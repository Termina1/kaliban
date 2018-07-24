{-# LANGUAGE ConstraintKinds, DataKinds, FlexibleContexts, MultiParamTypeClasses #-}

module Util where

import           Control.Applicative
import           Control.Monad.IO.Class
import           Control.Monad.Log
import           Control.Monad.Trans.Control
import           Data.Aeson
import           Data.Optional
import           Data.Time.Format
import qualified Network.Google               as Google
import           Text.PrettyPrint.Leijen.Text

instance FromJSON a => FromJSON (Optional a) where
  parseJSON val = (fmap Specific (parseJSON val)) <|> return Default

infixl 4 .:!?
(.:!?) o name = o .:? name .!= Default

type LogIO m = (MonadLog (WithTimestamp (WithSeverity Doc)) m, MonadIO m, MonadBaseControl IO m)
type GEnv = Google.Env '["https://www.googleapis.com/auth/cloud-platform"]

data LogginMode = DefaultMode | VerboseMode

logControledSeverity :: MonadIO io => Handler io Doc -> LogginMode -> (WithTimestamp (WithSeverity Doc)) -> io ()
logControledSeverity _ DefaultMode (WithTimestamp (WithSeverity Debug _) _) = liftIO $ return ()
logControledSeverity logFunc _ doc = let tsDoc = renderWithTimestamp (formatTime defaultTimeLocale rfc822DateFormat) discardSeverity doc in
                                     let tsSevDoc = renderWithSeverity Prelude.id (WithSeverity (msgSeverity $ discardTimestamp doc) tsDoc) in
                                     logFunc tsSevDoc


logInfoT :: (MonadLog (WithTimestamp (WithSeverity a)) m, MonadIO m) => a -> m ()
logInfoT doc = timestamp (WithSeverity Informational doc) >>= logMessage

logErrorT :: (MonadLog (WithTimestamp (WithSeverity a)) m, MonadIO m) => a -> m ()
logErrorT doc = timestamp (WithSeverity Control.Monad.Log.Error doc) >>= logMessage

logDebugT :: (MonadLog (WithTimestamp (WithSeverity a)) m, MonadIO m) => a -> m ()
logDebugT doc = timestamp (WithSeverity Debug doc) >>= logMessage

data Timeout = Timeout {
  currentTimeout :: Int,
  defaultTimeout :: Int,
  maxTimeout :: Int
}

backoff :: Timeout -> Timeout
backoff (Timeout currentTimeout defaultTimeout maxTimeout) =
  let nwCurrent = min (currentTimeout * 2) maxTimeout in
    Timeout nwCurrent defaultTimeout maxTimeout

reset :: Timeout -> Timeout
reset (Timeout currentTimeout defaultTimeout maxTimeout) = Timeout defaultTimeout defaultTimeout maxTimeout