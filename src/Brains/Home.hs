{-# LANGUAGE OverloadedStrings #-}

module Brains.Home where
import Data.Aeson
import Network.HTTP.Conduit
import           Data.Time
import           Control.Monad.Catch

data HomeItem = HomeContact | HomeUnkown
data HomeContactState = ContactOpen | ContactClosed | ContactUnknown
  deriving (Show)


instance FromJSON HomeItem where
  parseJSON (String "Contact") = return HomeContact
  parseJSON act = return HomeUnkown

instance FromJSON HomeContactState where
  parseJSON (String "OPEN") = return ContactOpen
  parseJSON (String "CLOSED") = return ContactClosed
  parseJSON act = return ContactUnknown

data HomeResponse a = HomeResponse {
  link :: String,
  state :: a,
  homeType :: HomeItem,
  label :: String
}

instance (FromJSON a, Show a) => FromJSON (HomeResponse a) where
  parseJSON = withObject "item" $ \o -> do
    link <- o .: "link"
    state <- o .: "state"
    label <- o .: "label"
    ttype <- o .: "type"
    return $ HomeResponse link state ttype label

queryHome :: (FromJSON a, Show a) => String -> IO (Either String (HomeResponse a))
queryHome item = catch(do
  result <- simpleHttp ("http://192.168.255.7:8080/rest/items/" ++ item)
  return $ eitherDecode result) (\e -> return $ Left (show (e :: SomeException)))

homeIsDoorOpen :: IO (Either String HomeContactState)
homeIsDoorOpen = do
  result <- queryHome "EntranceDoor"
  return $ fmap state result

-- should move to friendly-time
homeDoorOpenTime :: IO (Either String LocalTime)
homeDoorOpenTime = do
  result <- queryHome "EntranceTime"
  return $ fmap (\resp -> utcToLocalTime (hoursToTimeZone 3) (state resp)) result