{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}

module Start where

import Brain
import Conduit
import Control.Concurrent.Async.Lifted
import Control.Concurrent.Chan
import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.Log
import Data.Time.Clock.POSIX
import System.IO
import Util

startApp :: [ConduitInstance] -> BrainCells -> IO ()
startApp conds cells = do
  time <- getPOSIXTime
  withFile ("/var/log/kaliban/data" ++ (show $ round time) ++ ".log") WriteMode $ \fhandle ->
    withFDHandler defaultBatchingOptions fhandle 0.4 80 $ \logToStdout ->
        runLoggingT (startBot conds cells) (logControledSeverity logToStdout VerboseMode)

botLoop :: LogIO m => ConduitChannel -> BrainCells -> m ()
botLoop chan cells = do
    (event, pushback) <- liftIO $ readChan chan
    act <- liftIO $ async (do response <- processRequest cells event
                              writeChan pushback response)
    liftIO $ link act
    botLoop chan cells

startBot :: LogIO m => [ConduitInstance] -> BrainCells -> m ()
startBot instances cells = do
  chan <- liftIO newChan
  withAsync (startConduits chan instances) $ \a -> do
    link a
    botLoop chan cells
