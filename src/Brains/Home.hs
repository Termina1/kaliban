{-# LANGUAGE OverloadedStrings #-}

module Brains.Home where
import Data.Aeson
import Network.HTTP.Conduit
import           Data.Time
import           Control.Monad.Catch
import Data.ByteString.UTF8
import           Control.Monad.IO.Class

data HomeItem = HomeContact | HomeUnkown
data HomeContactState = ContactOpen | ContactClosed | ContactUnknown
  deriving (Show)

data HomeDweller = Myself | Tanya

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

homeToOpenhabItem :: HomeDweller -> String
homeToOpenhabItem Myself = "SlavaHome"
homeToOpenhabItem Tanya = "TanyaHome"

homeToRealName :: HomeDweller -> String
homeToRealName Myself = "Ты"
homeToRealName Tanya = "Таня"

homeDwellerToPhone :: HomeDweller -> String
homeDwellerToPhone Myself = "Terminal6s"
homeDwellerToPhone Tanya = "Tanya"

queryHome :: (FromJSON a, Show a) => String -> IO (Either String (HomeResponse a))
queryHome item = catch(do
  result <- simpleHttp ("http://192.168.255.7:8080/rest/items/" ++ item)
  return $ eitherDecode result) (\e -> return $ Left (show (e :: SomeException)))

updateHome :: String -> String -> IO (Either String ())
updateHome item cmd = catch(do
  request <- liftIO $ (parseRequest ("http://192.168.255.7:8080/rest/items/" ++ item))
  let putRequest = request {
    method = "POST",
    requestBody = (RequestBodyBS $ fromString cmd)
  }
  manager <- newManager tlsManagerSettings
  result <- httpLbs putRequest manager
  return $ Right ()) (\e -> return $ Left (show (e :: SomeException)))

homeIsDoorOpen :: IO (Either String HomeContactState)
homeIsDoorOpen = do
  result <- queryHome "EntranceDoor"
  return $ fmap state result

-- should move to friendly-time
homeDoorOpenTime :: IO (Either String LocalTime)
homeDoorOpenTime = do
  result <- queryHome "EntranceTime"
  return $ fmap (\resp -> utcToLocalTime (hoursToTimeZone 3) (state resp)) result

homeSetPresence :: String -> Bool -> IO (Either String ())
homeSetPresence itemName True = updateHome itemName "ON"
homeSetPresence itemName False = updateHome itemName "OFF"


homeDetectPresence :: String -> Bool -> IO (Either String ())
homeDetectPresence deviceName state =
  case deviceName of
    "Terminal6s" -> homeSetPresence (homeToOpenhabItem Myself) state
    "Tanya" -> homeSetPresence (homeToOpenhabItem Tanya) state
    _ -> return $ Right ()

homeQueryPresence :: HomeDweller -> IO (Either String Bool)
homeQueryPresence dweller = do
  result <- queryHome (homeToOpenhabItem dweller)
  return (result >>= mapState)
  where
    mapState :: HomeResponse String -> Either String Bool
    mapState resp =
      case state resp of
        "ON" -> Right True
        "OFF" -> Right False
        _ -> Left "Unknown state"

homeQueryPresenceMulti :: [HomeDweller] -> IO (Either String [(HomeDweller, Bool)])
homeQueryPresenceMulti dwellers = (sequence $ map queryPresence dwellers) >>= \lst -> return $ sequence lst
  where
    queryPresence dweller = do
      response <- homeQueryPresence dweller
      return $ fmap (\result -> (dweller, result)) response