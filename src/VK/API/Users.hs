{-# LANGUAGE DeriveGeneric, DuplicateRecordFields, ExistentialQuantification, GADTs, OverloadedStrings, RecordWildCards,
             StandaloneDeriving #-}

module VK.API.Users where

import Control.Monad.IO.Class
import Data.List
import VK.API
import VK.API.Messages
import VK.ResponseTypes

get :: MonadIO m => APIOwner -> [Int] -> m (APIResponse [Profile])
get owner userIds = do
  liftIO $ apiRequest
    owner
    "users.get"
    ([("user_ids", (intercalate "," $ map show userIds))] :: [(String, String)])
