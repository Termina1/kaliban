{-# LANGUAGE DuplicateRecordFields #-}

module Conduits.VK where

import Conduit
import Control.Concurrent
import Control.Concurrent.Chan
import Data.List
import Data.Optional
import VK.API
import VK.API.Messages
import VK.ResponseTypes
import Data.Optional

doLater :: Double -> IO () -> IO ()
doLater ms io = do
  threadDelay (round (ms * 1000 * 1000)) >> io
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
    Default -> ConduitEventIdle
    Specific fwd -> ConduitEventCommand (getMText msg) (forwardsToText profiles fwd)

processLpResponse :: ConduitChannel -> LpHistoryResult -> APIOwner -> IO ()
processLpResponse chan result owner = do
  sequence $ map (\msg -> do respchan <- newChan
                             putStrLn $ "Got message: " ++ (show msg)
                             forkIO (listenResponse (VK.API.Messages.peerId msg) respchan owner)
                             writeChan chan ((toEvent msg (defaultTo [] $ profiles result)), respchan)
                 ) (filter (\mess -> (getFrom mess) /= (ownerId owner)) (items (messages result)))
  return ()

startPollingApi :: APIOwner -> Int -> ConduitChannel -> IO ()
startPollingApi owner pts chan = do
  result <- getLongpollHistory owner (getLpParams pts)
  case result of
    APIError errorCode errorMessage -> do putStrLn $ "error: " ++ (show errorCode) ++ " " ++ errorMessage
                                          doLater 3 (startPollingApi owner pts chan)
    APIRequestError message -> do putStrLn $ "API error: " ++ (show message)
                                  doLater 1 (startPollingApi owner pts chan)
    APIResult response -> do
      processLpResponse chan response owner
      doLater 0.4 (startPollingApi owner (newPts response) chan)

processResponse :: ConduitResponse -> APIOwner -> Int -> IO ()
processResponse resp owner peerId = case resp of
                                         ConduitResponseUnknown -> do result <- send owner peerId "Чел, я не знаю"
                                                                      putStrLn (show result)
                                                                      return ()

listenResponse :: Int -> Chan ConduitResponse -> APIOwner -> IO ()
listenResponse peerId chan owner = do putStrLn "Start listen for response"
                                      resp <- readChan chan
                                      processResponse resp owner peerId
                       

instance Conduit VKConduit where
  initConduit vkcond chan = do
    ptsResp <- getLongpollServer (owner vkcond) True 3
    putStrLn "Init VK conduit"
    case ptsResp of
      APIError errorCode errorMessage -> doLater 3 (initConduit vkcond chan)
      APIRequestError message -> doLater 3 (initConduit vkcond chan)
      APIResult response -> startPollingApi (owner vkcond) (defaultTo 0 (pts response)) chan
    return ()
