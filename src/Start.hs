{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Start where

import Brain
import Conduit
import Control.Concurrent.Chan
import Control.Concurrent.Async.Lifted
import Control.Monad.Log
import System.IO
import Util
import Control.Monad.IO.Class

startApp :: [ConduitInstance] -> BrainCells -> IO ()
startApp conds cells = do
  withFDHandler defaultBatchingOptions stdout 0.4 80 $ \logToStdout ->
      runLoggingT (startBot conds cells) (logControledSeverity logToStdout DefaultMode)

botLoop :: LogIO m => ConduitChannel -> BrainCells -> m ()
botLoop chan cells = do
    (event, pushback) <- liftIO $ readChan chan
    liftIO $ async (do response <- processRequest cells event
                       writeChan pushback response)
    botLoop chan cells

startBot :: LogIO m => [ConduitInstance] -> BrainCells -> m ()
startBot instances cells = do
    chan <- startConduits instances
    botLoop chan cells
    return ()