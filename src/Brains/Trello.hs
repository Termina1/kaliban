{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Brains.Trello where

import           Data.Aeson
import qualified Data.HashMap.Strict as HM
import           Data.Optional
import           Data.String
import           Data.Time
import           Data.Time.ISO8601
import           Network.HTTP.Simple
import           Network.URL

data APIOwner = APIOwner
  { trelloToken :: String
  , trelloKey   :: String
  , boardId     :: String
  , listId      :: String
  }

data TrelloCard = TrelloCard
  { id :: String
  }

instance FromJSON TrelloCard where
  parseJSON =
    withObject "card" $ \o -> do
      id <- o .: "id"
      return TrelloCard {..}

baseUrl :: String
baseUrl = "https://api.trello.com/1/"

createTask :: APIOwner -> String -> Optional UTCTime -> IO (Either String TrelloCard)
createTask owner name due =
  let params = [("name", name), ("pos", "top"), ("idList", (listId owner))]
  in case due of
       Default -> aiRequest owner "POST" "cards" params
       Specific due ->
         let dueStr = formatISO8601 due
         in aiRequest owner "POST" "cards" (("due", dueStr) : params)

aiRequest :: FromJSON a => APIOwner -> String -> String -> [(String, String)] -> IO (Either String a)
aiRequest owner method resource params =
  let req = fromString $ (getRequestUrl owner resource params)
  in let freq = setRequestMethod (fromString method) req
     in httpLBS freq >>= \res -> do
          let resp = getResponseBody res
          case eitherDecode resp of
            Left err  -> return $ Left err
            Right obj -> return $ Right obj
  where
    ownerToParams :: APIOwner -> [(String, String)]
    ownerToParams owner = [("key", trelloKey owner), ("token", (trelloToken owner))]
    getRequestUrl :: APIOwner -> String -> [(String, String)] -> String
    getRequestUrl owner res params = baseUrl ++ res ++ "?" ++ (exportParams $ (ownerToParams owner) ++ params)
