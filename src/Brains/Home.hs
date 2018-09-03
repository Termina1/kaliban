{-# LANGUAGE OverloadedStrings #-}

module Brains.Home where
import Data.Aeson
import Network.HTTP.Conduit
import           Control.Monad.Catch

data HomeItem = HomeContact | HomeUnkown
data HomeContactState = ContactOpen | ContactClosed | ContactUnknown

instance FromJSON HomeItem where
  parseJSON (String "Contact") = return HomeContact
  parseJSON act = return HomeUnkown

instance FromJSON HomeContactState where
  parseJSON (String "OPEN") = return ContactOpen
  parseJSON (String "CLOSED") = return ContactClosed
  parseJSON act = return ContactUnknown

data HomeResponse = HomeResponse {
  link :: String,
  state :: HomeContactState,
  homeType :: HomeItem,
  label :: String
}

instance FromJSON HomeResponse where
  parseJSON = withObject "item" $ \o -> do
    link <- o .: "link"
    state <- o .: "state"
    label <- o .: "label"
    ttype <- o .: "type"
    return $ HomeResponse link state ttype label

queryHome :: String -> IO (Either String HomeResponse)
queryHome item = catch(do
  result <- simpleHttp ("http://tvault:8080/rest/items/" ++ item)
  return $ eitherDecode result) (\e -> return $ Left (show (e :: SomeException)))

homeIsDoorOpen :: IO (Either String HomeContactState)
homeIsDoorOpen = do
  result <- queryHome "Entrance"
  return $ fmap state result
