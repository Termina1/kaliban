{-# LANGUAGE ExistentialQuantification, GADTs, StandaloneDeriving
  #-}

module Conduit where

import Control.Concurrent (forkIO)
import Control.Concurrent.Chan

data ConduitEvent where
  ConduitEventIdle :: ConduitEvent
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
  initConduit :: a -> ConduitChannel -> IO ()

data ConduitInstance =
  forall a. Conduit a =>
            MkConduitInstance a

initConduitInstance :: ConduitInstance -> IO (ConduitChannel)
initConduitInstance (MkConduitInstance inst) = do
  chan <- newChan
  forkIO $ initConduit inst chan
  return chan

composeChannels :: [ConduitChannel] -> IO (ConduitChannel)
composeChannels chans = do
  composedChan <- newChan
  sequence (map (\chan -> forkIO (composeHelper composedChan chan)) chans)
  return composedChan
  where
    composeHelper chanc chan = do
      val <- readChan chan
      writeChan chanc val
      composeHelper chanc chan

startConduits :: [ConduitInstance] -> IO ConduitChannel
startConduits conduits =
  let instances = map initConduitInstance conduits
  in sequence instances >>= composeChannels
