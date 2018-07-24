{-# LANGUAGE DeriveGeneric, DuplicateRecordFields, ExistentialQuantification, GADTs, OverloadedStrings, RecordWildCards,
             StandaloneDeriving #-}

module VK.API.Groups where

import Data.Aeson
import Data.List
import Data.Optional
import VK.API
import GHC.Generics
import VK.ResponseTypes
import Control.Monad.IO.Class


data GroupLPServer = GroupLPServer {
  server :: String,
  key :: String,
  ts :: Int
} deriving (Show, Generic)

instance FromJSON GroupLPServer

getLongpollServer :: MonadIO m => APIOwner -> Int -> m (APIResponse GroupLPServer)
getLongpollServer owner groupId =
  liftIO $ apiRequest
    owner
    "groups.getLongPollServer"
    ([("group_id", (show groupId))] :: [(String, String)])