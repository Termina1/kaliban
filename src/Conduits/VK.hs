{-# LANGUAGE DuplicateRecordFields, FlexibleContexts, GADTs, OverloadedStrings #-}

module Conduits.VK
  ( VKConduit(..)
  , initConduit
  ) where

import           Conduit
import           Control.Concurrent              (threadDelay)
import           Control.Concurrent.Async.Lifted
import           Control.Concurrent.MonadIO      (Chan, newChan, readChan, writeChan)
import           Control.Monad.IO.Class
import           Control.Monad.State
import           Data.List
import           Data.Optional
import           Data.String
import qualified Streaming.Prelude               as S
import           Util
import           VK.API
import           VK.API.Messages
import qualified VK.API.Users                    as Users
import           VK.LongpollStream
import           VK.ResponseTypes

data VKConduit = VKConduit
  { owner :: APIOwner
  }

data EventType =
  ForwardedAndText String [ForwardedMessage] |
  AudioMessage String |
  ForwardedAudioMessage String Int|
  SimpleText String |
  Unknown deriving (Show)

forwardedToEvent :: String -> [ForwardedMessage] -> EventType
forwardedToEvent "" [ForwardedMessage fromId _ _ _ (Specific [Attachment _ (DocC document)])] =
  case audio_msg (preview document) of
    Default       -> Unknown
    Specific prev -> ForwardedAudioMessage (link_ogg prev) fromId
forwardedToEvent text fwds = ForwardedAndText text fwds


messageToEventType :: Message -> EventType
messageToEventType (Message _ _ _ _ text _ (Specific fwd) _ _) = forwardedToEvent text fwd
messageToEventType (Message _ _ _ _ _ _ _ (Specific [Attachment _ (DocC document)]) _) =
  case audio_msg (preview document) of
    Default       -> Unknown
    Specific prev -> AudioMessage $ link_ogg prev
messageToEventType (Message _ _ _ _ text _ _ _ _) = SimpleText text

getName :: ForwardedMessage -> [Profile] -> String
getName (ForwardedMessage fromId _ _ _ _) profiles =
  let profm = find (\(Profile id _ _ _ _ _ _) -> fromId == id) profiles
  in case profm of
       Just prof -> (first_name prof) ++ " " ++ (last_name prof)
       Nothing   -> ""

getPersonification :: Int -> [Profile] -> String
getPersonification fromId profiles =
  let profileM = find (\(Profile id _ _ _ _ _ _) -> fromId == id) profiles in
  case profileM of
    Just profile ->
      let name = (first_name profile) ++ " " ++ (last_name profile) in
      case (sex profile) of
        Specific sex ->
          if sex == 1 then name ++ " сказала: "
                      else name ++ " сказал: "
        Default -> name ++ " сказал:"
    Nothing -> "Неизвестный сказал: "

forwardsToText :: [Profile] -> [ForwardedMessage] -> String
forwardsToText profiles fwds = intercalate "\n" $ map (forwardsToTextHelper profiles) fwds
  where
    forwardsToTextHelper profiles fwd =
      (getName fwd profiles) ++
      ": \n" ++ (getFMText fwd) ++ "\n" ++ (forwardsToText profiles $ defaultTo [] (forwarded fwd))

toEvent :: Message -> [Profile] -> ConduitEvent
toEvent msg profiles =
  case (messageToEventType msg) of
    Unknown                          -> ConduitEventIdle
    SimpleText text                  -> ConduitEventCommand text ""
    ForwardedAndText msgText fwd     -> ConduitEventCommand msgText (forwardsToText profiles fwd)
    AudioMessage url                 -> ConduitEventAudio url "Ты сказал: "
    ForwardedAudioMessage url fromId -> ConduitEventAudio url (getPersonification fromId profiles)

logAPIRequestError :: LogIO m => String -> APIResponse a -> m ()
logAPIRequestError method resp =
  case resp of
    APIError errorCode errorMessage -> do
      logErrorT $ fromString $ method ++ " (API error): " ++ (show errorCode) ++ " " ++ errorMessage
    APIRequestError message -> do
      logErrorT $ fromString $ method ++ " (server error): " ++ (show message)
    _ -> return ()

processResponse :: LogIO m => ConduitResponse -> APIOwner -> Int -> m ()
processResponse resp owner peerId =
  case resp of
    ConduitResponseUnknown -> do
      logDebugT "Got unknown response"
      liftIO $ send owner peerId "Чел, я не знаю"
      return ()
    ConduitResponseMessages message -> do
      logDebugT $ fromString $ "Got response message: " ++ (message)
      sendResponse <- liftIO $ send owner peerId message
      logAPIRequestError "messages.send" sendResponse
      return ()


listenResponse :: LogIO m => Int -> Chan ConduitResponse -> APIOwner -> m ()
listenResponse peerId chan owner = do
  logDebugT "Start listen for response"
  resp <- readChan chan
  processResponse resp owner peerId

requestNeededProfiles :: LogIO m => APIOwner -> Message -> m [Profile]
requestNeededProfiles owner message = let needed = (getFrom message) : walkForwards (defaultTo [] $ forwardedMesages message) in do
  result <- Users.get owner (nub needed)
  logAPIRequestError "Error requesting profiles" result
  case result of
    APIResult profiles -> return profiles
    _                  -> return []
  where
    foldOverForwards acc fwd = let tail = acc ++ walkForwards (defaultTo [] $ forwarded fwd) in
      (getFMFrom fwd) : tail
    walkForwards :: [ForwardedMessage] -> [Int]
    walkForwards fwdMessages = foldl foldOverForwards [] fwdMessages


proccessMessage :: LogIO m => APIOwner -> ConduitChannel -> LpContainer Message -> m ()
proccessMessage _ _ (Left err) = logErrorT (fromString $ (show err))
proccessMessage owner chan (Right message) =
  if (getFrom message) == (ownerId owner)
  then return ()
  else do
    respchan <- newChan
    logDebugT $ fromString $ "Got message: " ++ (show message)
    profiles <- requestNeededProfiles owner message
    async (listenResponse (VK.API.Messages.peerId message) respchan owner)
    writeChan chan ((toEvent message profiles), respchan)

instance Conduit VKConduit where
  initConduit vkcond chan = do
    logInfoT "Init VK conduit"
    runStateT (S.mapM_ (proccessMessage (owner vkcond) chan) (longpollStream (owner vkcond))) ""
    return ()
