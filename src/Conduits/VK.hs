{-# LANGUAGE DuplicateRecordFields, FlexibleContexts, OverloadedStrings #-}

module Conduits.VK
  ( VKConduit(..)
  , initConduit
  ) where

import Conduit
import Data.List
import Data.Optional
import VK.API
import VK.API.Messages
import Control.Concurrent.Async.Lifted
import VK.ResponseTypes
import Control.Monad.IO.Class
import Control.Concurrent (threadDelay)
import Control.Concurrent.MonadIO (newChan, readChan, Chan, writeChan)
import Control.Monad.Log
import Util
import Data.String

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

getName :: ForwardedMessage -> [Profile] -> String
getName (ForwardedMessage fromId _ _ _) profiles =
  let profm = find (\(Profile id _ _ _ _ _ _) -> fromId == id) profiles
  in case profm of
       Just prof -> (first_name prof) ++ " " ++ (last_name prof)
       Nothing -> ""

forwardsToText :: [Profile] -> [ForwardedMessage] -> String
forwardsToText profiles fwds = intercalate "\n" $ map (forwardsToTextHelper profiles) fwds
  where
    forwardsToTextHelper profiles fwd =
      (getName fwd profiles) ++
      ": \n" ++ (getFMText fwd) ++ "\n" ++ (forwardsToText profiles $ defaultTo [] (forwarded fwd))

toEvent :: Message -> [Profile] -> ConduitEvent
toEvent msg profiles =
  case (forwardedMesages msg) of
    Default -> ConduitEventCommand (getMText msg) ""
    Specific fwd -> ConduitEventCommand (getMText msg) (forwardsToText profiles fwd)

processLpResponse :: LogIO m => ConduitChannel -> LpHistoryResult -> APIOwner -> m ()
processLpResponse chan result owner = do
  sequence $
    map
      (\msg -> do
         respchan <- newChan
         logDebug $ fromString $ "Got message: " ++ (show msg)
         async (listenResponse (VK.API.Messages.peerId msg) respchan owner)
         writeChan chan ((toEvent msg (defaultTo [] $ profiles result)), respchan))
      (filter (\mess -> (getFrom mess) /= (ownerId owner)) (items (messages result)))
  return ()

startPollingApi :: LogIO m => APIOwner -> Int -> ConduitChannel -> m ()
startPollingApi owner pts chan = do
  result <- liftIO $ getLongpollHistory owner (getLpParams pts)
  case result of
    APIError errorCode errorMessage -> do
      logError $ fromString $ "error: " ++ (show errorCode) ++ " " ++ errorMessage
      doLater 3 (startPollingApi owner pts chan)
    APIRequestError message -> do
      logError $ fromString $ "API error: " ++ (show message)
      doLater 1 (startPollingApi owner pts chan)
    APIResult response -> do
      processLpResponse chan response owner
      doLater 0.4 (startPollingApi owner (newPts response) chan)

processResponse :: LogIO m => ConduitResponse -> APIOwner -> Int -> m ()
processResponse resp owner peerId =
  case resp of
    ConduitResponseUnknown -> do
      logDebug "Got unknown response"
      liftIO $ send owner peerId "Чел, я не знаю"
      return ()
    ConduitResponseMessages message -> do
      logDebug $ fromString $ "Got response message: " ++ (message)
      liftIO $ send owner peerId message
      return ()


listenResponse :: LogIO m => Int -> Chan ConduitResponse -> APIOwner -> m ()
listenResponse peerId chan owner = do
  logDebug "Start listen for response"
  resp <- readChan chan
  processResponse resp owner peerId

instance Conduit VKConduit where
  initConduit vkcond chan = do
    ptsResp <- liftIO $ getLongpollServer (owner vkcond) True 3
    logInfo "Init VK conduit"
    case ptsResp of
      APIError errorCode errorMessage -> doLater 3 (initConduit vkcond chan)
      APIRequestError message -> doLater 3 (initConduit vkcond chan)
      APIResult response -> startPollingApi (owner vkcond) (defaultTo 0 (pts response)) chan
    return ()