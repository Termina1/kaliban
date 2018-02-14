{-# LANGUAGE DuplicateRecordFields, FlexibleContexts, GADTs, OverloadedStrings #-}

module Conduits.VK
  ( VKConduit(..)
  , initConduit
  ) where

import Conduit
import Control.Concurrent              (threadDelay)
import Control.Concurrent.Async.Lifted
import Control.Concurrent.MonadIO      (Chan, newChan, readChan, writeChan)
import Control.Monad.IO.Class
import Control.Monad.Log
import Data.List
import Data.Optional
import Data.String
import Util
import VK.API
import VK.API.Messages
import VK.ResponseTypes

doLater :: LogIO m => Double -> m () -> m ()
doLater ms io = do
  liftIO $ threadDelay (round (ms * 1000 * 1000))
  io
  return ()

data VKConduit = VKConduit
  { owner :: APIOwner
  }


getLpParams :: Int -> LpHistoryParams
getLpParams pts =
  LpHistoryParams
  { timestamp = Right pts
  , previewLength = Default
  , fields = Default
  , eventsLimit = Default
  , msgsLimit = Default
  , maxId = Default
  , lpVer = Specific 3
  , onlines = Default
  }

data EventType =
  ForwardedAndText String [ForwardedMessage] |
  AudioMessage String |
  ForwardedAudioMessage String Int|
  SimpleText |
  Unknown deriving (Show)

forwardedToEvent :: String -> [ForwardedMessage] -> EventType
forwardedToEvent "" [ForwardedMessage fromId _ _ _ (Specific [Attachment _ (DocC document)])] =
  case audio_msg (preview document) of
    Default       -> Unknown
    Specific prev -> ForwardedAudioMessage (link_ogg prev) fromId
forwardedToEvent text fwds = ForwardedAndText text fwds


messageToEventType :: Message -> EventType
messageToEventType (Message _ _ _ _ text _ (Specific fwd) _) = forwardedToEvent text fwd
messageToEventType (Message _ _ _ _ _ _ _ (Specific [Attachment _ (DocC document)])) =
  case audio_msg (preview document) of
    Default       -> Unknown
    Specific prev -> AudioMessage $ link_ogg prev
messageToEventType _ = Unknown

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
    SimpleText                       -> ConduitEventCommand (getMText msg) ""
    ForwardedAndText msgText fwd     -> ConduitEventCommand msgText (forwardsToText profiles fwd)
    AudioMessage url                 -> ConduitEventAudio url "Ты сказал: "
    ForwardedAudioMessage url fromId -> ConduitEventAudio url (getPersonification fromId profiles)

processLpResponse :: LogIO m => ConduitChannel -> LpHistoryResult -> APIOwner -> m ()
processLpResponse chan result owner = do
  sequence $
    map
      (\msg -> do
         respchan <- newChan
         logDebugT $ fromString $ "Got message: " ++ (show msg)
         async (listenResponse (VK.API.Messages.peerId msg) respchan owner)
         writeChan chan ((toEvent msg (defaultTo [] $ profiles result)), respchan))
      (filter (\mess -> (getFrom mess) /= (VK.API.ownerId owner)) (items (messages result)))
  return ()

startPollingApi :: LogIO m => APIOwner -> Int -> ConduitChannel -> m ()
startPollingApi owner pts chan = do
  result <- liftIO $ getLongpollHistory owner (getLpParams pts)
  logAPIRequestError "messages.getLongpollHistory" result
  case result of
    APIError errorCode errorMessage -> do
      doLater 3 (startPollingApi owner pts chan)
    APIRequestError message -> do
      doLater 1 (startPollingApi owner pts chan)
    APIResult response -> do
      processLpResponse chan response owner
      doLater 0.4 (startPollingApi owner (newPts response) chan)

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

instance Conduit VKConduit where
  initConduit vkcond chan = do
    ptsResp <- liftIO $ getLongpollServer (owner vkcond) True 3
    logAPIRequestError "messages.getLongpollServer" ptsResp
    logInfoT "Init VK conduit"
    case ptsResp of
      APIError errorCode errorMessage -> do
        doLater 3 (initConduit vkcond chan)
      APIRequestError message -> do
        doLater 3 (initConduit vkcond chan)
      APIResult response -> startPollingApi (owner vkcond) (defaultTo 0 (pts response)) chan
    return ()
