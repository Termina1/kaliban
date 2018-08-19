{-# LANGUAGE OverloadedStrings #-}

module VK.API (apiRequest, APIOwner(..)) where

import Data.Aeson          (FromJSON, eitherDecode)
import Data.String
import Network.HTTP.Simple
import Network.URL
import Control.Exception.Base

import VK.ResponseTypes

apiUrl :: String
apiUrl = "https://api.vk.com/"

data APIOwner = APIOwner
  { accessToken :: String
  , ownerId     :: Int
  , version     :: (Int, Int)
  }

apiRequest :: (FromJSON a, Paramable b) => APIOwner -> String -> b -> IO (APIResponse a)
apiRequest owner method params = catch doRequest $ \exc -> return $ APIRequestError $ show (exc :: SomeException)
  where
    doRequest =
      (httpLBS $ fromString $ getRequestUrl owner method (toParams params)) >>= \res -> do
        let resp = getResponseBody res
        case eitherDecode resp of
          Left err  -> return $ APIRequestError err
          Right obj -> return obj
    ownerToParams :: APIOwner -> [(String, String)]
    ownerToParams owner =
      [ ("access_token", accessToken owner)
      , ("v", ((show $ fst (version owner)) ++ "." ++ (show $ snd (version owner))))
      ]
    getRequestUrl :: APIOwner -> String -> [(String, String)] -> String
    getRequestUrl owner method params =
      apiUrl ++ "/method/" ++ method ++ "?" ++ (exportParams $ (ownerToParams owner) ++ params)
