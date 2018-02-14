{-# LANGUAGE ExistentialQuantification, FlexibleContexts, GADTs, StandaloneDeriving #-}

module Conduit where

import Control.Concurrent.Async.Lifted
import Control.Concurrent.MonadIO      (Chan, newChan, readChan, writeChan)
import Util

data ConduitEvent where
  ConduitEventIdle :: ConduitEvent
  ConduitEventAudio :: String -> String -> ConduitEvent
  ConduitEventCommand
    :: { commandText :: String
       , commandMeta :: String}
    -> ConduitEvent

data ConduitResponse where
  ConduitResponseMessages :: String -> ConduitResponse
  ConduitResponseUnknown :: ConduitResponse

deriving instance Show ConduitEvent

type ConduitChannel = Chan (ConduitEvent, (Chan ConduitResponse))

class Conduit a where
  initConduit :: LogIO m => a -> ConduitChannel -> m ()

data ConduitInstance =
  forall a. Conduit a =>
            MkConduitInstance a

initConduitInstance :: LogIO m => ConduitInstance -> m (ConduitChannel)
initConduitInstance (MkConduitInstance inst) = do
  chan <- newChan
  async $ initConduit inst chan
  return chan

composeChannels :: LogIO m => [ConduitChannel] -> m (ConduitChannel)
composeChannels chans = do
  composedChan <- newChan
  sequence (map (\chan -> async (composeHelper composedChan chan)) chans)
  return composedChan
  where
    composeHelper chanc chan = do
      val <- readChan chan
      writeChan chanc val
      composeHelper chanc chan

startConduits :: LogIO m => [ConduitInstance] -> m ConduitChannel
startConduits conduits =
  let instances = map initConduitInstance conduits
  in sequence instances >>= composeChannels
