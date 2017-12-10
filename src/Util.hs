{-# LANGUAGE ConstraintKinds, FlexibleContexts #-}

module Util where

import Data.Optional
import Data.Aeson
import Control.Applicative
import Control.Monad.Log
import Text.PrettyPrint.Leijen.Text
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Data.Time.Format

instance FromJSON a => FromJSON (Optional a) where
  parseJSON val = (fmap Specific (parseJSON val)) <|> return Default

infixl 4 .:!?
(.:!?) o name = o .:? name .!= Default

type LogIO m = (MonadLog (WithTimestamp (WithSeverity Doc)) m, MonadIO m, MonadBaseControl IO m)

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