{-# LANGUAGE ConstraintKinds, FlexibleContexts #-}

module Util where

import Data.Optional
import Data.Aeson
import Control.Applicative
import Control.Monad.Log
import Text.PrettyPrint.Leijen.Text
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Control.Monad.Trans.Class

instance FromJSON a => FromJSON (Optional a) where
  parseJSON val = (fmap Specific (parseJSON val)) <|> return Default

infixl 4 .:!?
(.:!?) o name = o .:? name .!= Default

type LogIO m = (MonadLog (WithSeverity Doc) m, MonadIO m, MonadBaseControl IO m)

data LogginMode = DefaultMode | VerboseMode

logControledSeverity :: MonadIO io => Handler io Doc -> LogginMode -> WithSeverity Doc -> io ()
logControledSeverity _ DefaultMode (WithSeverity Debug _) = liftIO $ return ()
logControledSeverity logFunc _ doc = (logFunc . renderWithSeverity Prelude.id) doc