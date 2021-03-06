{-# LANGUAGE GADTs, OverloadedStrings, RecordWildCards, StandaloneDeriving #-}

module Brains.AI where

import           Control.Monad.Catch
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.HashMap.Strict as HM
import           Data.Optional
import           Data.String
import           Data.Time
import           Network.HTTP.Simple
import           Network.URL
import           Util

baseUrl :: String
baseUrl = "https://api.api.ai/api/"

data AILang = AILangRu

instance FromJSON AILang where
  parseJSON "ru" = return AILangRu
  parseJSON lang = fail $ "unknown language: " ++ (show lang)

instance Show AILang where
  show AILangRu = "ru"

data APIOwner = APIOwner {
  aiToken :: String,
  lang    :: AILang,
  v       :: String
}

data AIAction =
  AIActionTask |
  AIActionChart |
  AIActionUnknown |
  AIActionDoorQuery |
  AIQueryDoorOpenTime |
  AIQueryWhoIsHome |
  AIQuerySetPresence
  deriving (Show)

instance FromJSON AIAction where
  parseJSON (String "create_task")    = return AIActionTask
  parseJSON (String "send_chart")     = return AIActionChart
  parseJSON (String "door_query")     = return AIActionDoorQuery
  parseJSON (String "when_door_open") = return AIQueryDoorOpenTime
  parseJSON (String "who_is_home")    = return AIQueryWhoIsHome
  parseJSON (String "set_presence")   = return AIQuerySetPresence
  parseJSON (String "input.unknown")  = return AIActionUnknown
  parseJSON act                       = return AIActionUnknown

data EntityComeGone = EntityCome | EntityGone
  deriving (Show, Eq)
instance FromJSON EntityComeGone where
  parseJSON (String "come") = return EntityCome
  parseJSON (String "gone") = return EntityGone
  parseJSON act = fail "Unknown verb"

data EntityDweller = DwellerSlava | DwellerTanya
  deriving (Show)
instance FromJSON EntityDweller where
  parseJSON (String "Slava") = return DwellerSlava
  parseJSON (String "Tanya") = return DwellerTanya
  parseJSON act = fail "Unknown dweller"

data AIIntentParams where
  IntentTaskParams :: {
    time :: Optional TimeOfDay,
    date :: Optional Day
  } ->  AIIntentParams
  IntentChartParams :: Int -> AIIntentParams
  IntentDoorQuery :: AIIntentParams
  IntentDoorOpenTimeQuery :: AIIntentParams
  IntentWhoIsHome :: AIIntentParams
  IntentSetPresence :: {
    verb :: EntityComeGone,
    dweller :: EntityDweller
  } -> AIIntentParams
  IntentUnknown :: AIIntentParams
  deriving (Show)

parseIntentParams :: AIAction -> Value -> Parser AIIntentParams
parseIntentParams AIActionTask = withObject "task action" $ \o -> do
  date <- o .: "date"
  time <- o .: "time"
  return $ createTime date time

  where
    createTime :: String -> String -> AIIntentParams
    createTime "" "" = IntentTaskParams Default Default
    createTime date "" = IntentTaskParams Default (parseTimeM False defaultTimeLocale "%0Y-%m-%d" date)
    createTime "" time = IntentTaskParams (fmap localTimeOfDay (parseTimeM False defaultTimeLocale "%H:%M:%S" time)) Default
    createTime date time = IntentTaskParams (fmap localTimeOfDay $ parseTimeM False defaultTimeLocale "%H:%M:%S" time)
                                            (parseTimeM False defaultTimeLocale "%0Y-%m-%d" date)
parseIntentParams AIActionDoorQuery = const $ return IntentDoorQuery
parseIntentParams AIQueryDoorOpenTime = const $ return IntentDoorOpenTimeQuery
parseIntentParams AIQueryWhoIsHome = const $ return IntentWhoIsHome
parseIntentParams AIQuerySetPresence = withObject "presence action" $ \o -> do
  dweller <- o .: "HomeDweller"
  verb <- o .: "gone_leave"
  return $ IntentSetPresence verb dweller
parseIntentParams AIActionUnknown = const $ return IntentUnknown
parseIntentParams action = \val -> fail $ "Unsupported action: " ++ (show action)

data AIResult = AIResult {
  source           :: String,
  resolvedQuery    :: String,
  actionIncomplete :: Bool,
  fulfillment      :: String,
  action           :: AIAction,
  parameters       :: AIIntentParams,
  score            :: Double
} deriving (Show)

instance FromJSON AIResult where
  parseJSON = withObject "meta" $ \o -> do
    source <- o .: "source"
    resolvedQuery <- o .: "resolvedQuery"
    actionIncomplete <- o .: "actionIncomplete"
    action <- o .: "action"
    speech <- o .: "fulfillment"
    fulfillment <- speech .: "speech"
    score <- o .: "score"
    case (HM.lookup "parameters" o) of
      Nothing -> fail "No parameters!"
      Just val -> do parameters <- parseIntentParams action val
                     return AIResult{..}


data AIStatus = AIStatus {
  code         :: Int,
  errorType    :: String,
  errorDetails :: Optional String
} deriving (Show)

instance FromJSON AIStatus where
  parseJSON = withObject "status" $ \o -> do
    code <- o .: "code"
    errorType <- o .: "errorType"
    errorDetails <- o .:!? "errorDetails"
    return AIStatus{..}

data AIResponse = AIResponse {
  id        :: String,
  timestamp :: UTCTime,
  result    :: Maybe AIResult,
  status    :: AIStatus,
  sessionId :: Maybe String
} deriving (Show)

instance FromJSON AIResponse where
  parseJSON = withObject "ai response" $ \o -> do
    id <- o .: "id"
    timestamp <- o .: "timestamp"
    status <- o .: "status"
    sessionId <- o .:? "sessionId"
    result <- o .:? "result"
    return AIResponse{..}

data AICommand where
  AICommand :: {
    params     :: AIIntentParams,
    response   :: String,
    actionName :: AIAction
  } -> AICommand
  AICommandError :: Int -> String -> AICommand

deriving instance Show AICommand

aiRequest :: APIOwner -> String -> String -> IO (Either String AIResponse)
aiRequest owner text sessionId =
  let req = fromString $ (getRequestUrl owner text sessionId) in
  let freq = setRequestHeaders [("Authorization", fromString $ "Bearer " ++ (aiToken owner)),
                                ("Accept", fromString $ "application/json")] req in
  catch (httpLBS freq >>= \res -> do
    let resp = getResponseBody res
    case eitherDecode resp of
      Left err  -> return $ Left err
      Right obj -> return $ Right obj) $ \e -> return $ Left $ show (e :: SomeException)
  where
    ownerToParams :: APIOwner -> [(String, String)]
    ownerToParams owner = [ ("lang", (show (lang owner))), ("v", (v owner)) ]
    getRequestUrl :: APIOwner -> String -> String -> String
    getRequestUrl owner query sessionId =
      baseUrl ++ "query" ++ "?" ++ (exportParams $ (ownerToParams owner) ++ [("query", query), ("sessionId", sessionId)])

toCommand :: AIResponse -> AICommand
toCommand resp =
  case (result resp) of
    Nothing -> AICommandError (code $ status resp) $ (errorType $ status resp) ++ ": " ++ (defaultTo "" $ errorDetails $ status resp)
    Just res -> AICommand {
      params = (parameters res),
      response = (fulfillment res),
      actionName = (action res)
    }

askAI :: APIOwner -> String -> String -> IO (Either String AICommand)
askAI owner text sessionId = do res <- aiRequest owner text sessionId
                                return (fmap toCommand res)
