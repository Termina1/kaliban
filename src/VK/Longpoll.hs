{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module VK.Longpoll
(
  makeLongpollRequest, 
  LongpollInstance(LongpollInstance)
) where

import Data.List
import Control.Monad
import Data.Aeson
import Data.ByteString.Lazy.UTF8
import Network.HTTP.Conduit
import VK.ResponseTypes

data LongpollInstance = LongpollInstance {
  key :: String,
  ts :: Int,
  server :: String,
  version :: Int,
  mode :: Int,
  wait :: Int
}

longpollToUrl :: LongpollInstance -> String
longpollToUrl lp = "https://" ++ (server lp) ++ "?" ++ renderParams lp
  where
    renderParams :: LongpollInstance -> String
    renderParams lph = let params = [
                            ("act", const "a_check"), 
                            ("ts", show . ts), 
                            ("version", show . version), 
                            ("mode", show . mode),
                            ("wait", show . wait),
                            ("key", key)] in
                          join $ intersperse ['&'] $ map (\(name, func) -> name ++ "=" ++ (func lph)) params


makeLongpollRequest :: LongpollInstance -> IO (Either String LongpollResponse)
makeLongpollRequest lp = do result <- simpleHttp $ longpollToUrl lp
                            case lpEventFromStr result of
                              Left str -> return $ Left $ "invalid JSON: " ++ str
                              Right resp -> return $ Right resp


  where
    lpEventFromStr :: ByteString -> Either String LongpollResponse
    lpEventFromStr body = eitherDecode body