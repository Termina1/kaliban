{-# LANGUAGE FlexibleContexts, OverloadedStrings, DeriveGeneric #-}

module Start where

import Brain
import Brains.Server
import Conduit
import Control.Concurrent.Async.Lifted
import Control.Concurrent.Chan
import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.Log
import Data.Time.Clock.POSIX
import System.IO
import Util
import           Data.Aeson       hiding (json)
import           Web.Spock
import           Web.Spock.Config
import GHC.Generics
import Brains.Home (homeDetectPresence)

startApp :: [ConduitInstance] -> BrainCells -> IO ()
startApp conds cells = do
  time <- getPOSIXTime
  withFile ("/var/log/kaliban/data.log") AppendMode $ \fhandle ->
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
  withAsync (startConduits chan instances) $ \a2 -> do
    link a2
    botLoop chan cells

type Api = SpockM () () () ()

type ApiAction a = SpockAction () () () a

startServer:: ServerConfig -> IO ()
startServer config = do
  spockCfg <- defaultSpockCfg () PCNoDatabase ()
  runSpock 8080 (spock spockCfg (app config))

data StateUpdate = StateUpdate {
  deviceName :: String,
  state :: Bool,
  token :: String
} deriving (Show, Generic)

instance FromJSON StateUpdate

app :: ServerConfig -> Api
app config = do
  post "update_device_state" $ do
    maybeStateUpdate <- jsonBody :: ApiAction (Maybe StateUpdate)
    case maybeStateUpdate of
      Nothing -> json $ object ["error" .= String "invalid data passed"]
      Just stateUpdate -> do
        if (token stateUpdate) == (configToken config)
          then do result <- liftIO $ homeDetectPresence (deviceName stateUpdate) (state stateUpdate)
                  case result of
                    Left error -> json $ object ["error" .= error]
                    Right () -> json $ object ["result" .= String "ok"]
          else json $ object ["error" .= String "invalid token"]