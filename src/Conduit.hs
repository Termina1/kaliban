{-# LANGUAGE ExistentialQuantification, FlexibleContexts, GADTs, StandaloneDeriving #-}

module Conduit where

import Control.Concurrent.Async.Lifted
import Control.Concurrent.MonadIO (Chan, newChan, readChan, writeChan)
import Control.Exception
import Control.Monad
import Control.Monad.Catch
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

initConduitInstance :: LogIO m => (ConduitChannel, ConduitInstance) -> m ()
initConduitInstance (chan, (MkConduitInstance inst)) = initConduit inst chan

composeChannels :: LogIO m => ConduitChannel -> [ConduitChannel] -> m ()
composeChannels composedChan chans = do
  mapConcurrently_ (composeHelper composedChan) chans
  where
    composeHelper chanc chan = do
      val <- readChan chan
      writeChan chanc val
      composeHelper chanc chan

startConduits :: LogIO m => ConduitChannel -> [ConduitInstance] -> m ()
startConduits cchan conduits = do
  chans <- replicateM (length conduits) newChan
  concurrently (mapConcurrently_ initConduitInstance (zip chans conduits)) (composeChannels cchan chans)
  return ()
