module Util where

import Data.Optional
import Data.Aeson
import Control.Applicative

instance FromJSON a => FromJSON (Optional a) where
  parseJSON val = (fmap Specific (parseJSON val)) <|> return Default

infixl 4 .:!?

(.:!?) o name = o .:? name .!= Default