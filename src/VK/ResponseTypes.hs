{-# LANGUAGE ExistentialQuantification, FlexibleInstances, GADTs,
  OverloadedStrings, StandaloneDeriving #-}

module VK.ResponseTypes where

import Control.Applicative
import Data.Aeson
import qualified Data.HashMap.Strict as HM
import Data.List
import Data.Optional
import qualified Data.Vector as V
import Util

toInt :: Optional Int -> Int
toInt (Specific i) = i
toInt Default = 0

toList :: Show a => [a] -> String
toList ls = intercalate "," $ map show ls

data APIResponse a where
  APIError
    :: { errorCode :: Int
       , erroMessage :: String}
    -> APIResponse a
  APIRequestError :: String -> APIResponse a
  APIResult :: a -> APIResponse a

deriving instance Show a => Show (APIResponse a)

instance FromJSON a => FromJSON (APIResponse a) where
  parseJSON =
    withObject "result" $ \obj ->
      case (HM.lookup "error" obj) of
        Nothing ->
          case (HM.lookup "response" obj) of
            Nothing -> fail $ "Invalid API response" ++ (show obj)
            Just resp -> fmap APIResult (parseJSON resp)
        Just errParams ->
          withObject
            "error"
            (\errObject -> do
               code <- errObject .: "error_code"
               msg <- errObject .: "error_msg"
               return (APIError code msg))
            errParams

data WithCount a where
  WithCount
    :: { totalCount :: Int
       , items :: [a]}
    -> WithCount a

deriving instance Show a => Show (WithCount a)

instance (FromJSON a, Show a) => FromJSON (WithCount a) where
  parseJSON =
    withObject "items with count" $ \obj -> do
      count <- obj .: "count"
      itemsp <- obj .: "items"
      return $ WithCount count itemsp

data LongpollEvent where
  LpNewMessage
    :: { id :: Int
       , flags :: Int
       , peerId :: Int
       , date :: Int
       , text :: String
       , kludges :: [(String, String)]
       , randomId :: Int}
    -> LongpollEvent
  LpNotSuppEvent :: LongpollEvent

deriving instance Show LongpollEvent

instance FromJSON LongpollEvent where
  parseJSON (Array xs) =
    if V.length xs < 3
      then fail $ "Invalid event"
      else (do num <- parseJSON (V.head xs)
               parseEvent (num :: Int) (V.tail xs))
    where
      parseKludges =
        withObject "kludges" $ \o -> do
          mapM
            (\(name, value) -> do
               val <- parseJSON value
               return ((show name), val :: String))
            (HM.toList o)
      parseEvent 4 arr =
        if (V.length arr) /= 7
          then fail "invalid new message event"
          else (do vmessageId <- parseJSON (arr V.! 0)
                   vflags <- parseJSON (arr V.! 1)
                   vpeerId <- parseJSON (arr V.! 2)
                   vdate <- parseJSON (arr V.! 3)
                   vtext <- parseJSON (arr V.! 4)
                   vkludges <- parseKludges (arr V.! 5)
                   vrandomId <- parseJSON (arr V.! 6)
                   return $ LpNewMessage vmessageId vflags vpeerId vdate vtext vkludges vrandomId)
      parseEvent _ arr = return LpNotSuppEvent
  parseJSON _ = return LpNotSuppEvent

data LongpollResponse
  = LPErr Int
  | LPResult (Int, [LongpollEvent])
  deriving (Show)

instance FromJSON LongpollResponse where
  parseJSON (Object obj) =
    (do err <- obj .: "failed"
        return (LPErr err)) <|>
    (do nwts <- obj .: "ts"
        updates <- obj .: "updates"
        return $ LPResult (nwts, updates))

class Paramable a where
  toParams :: a -> [(String, String)]

instance Paramable [(String, String)] where
  toParams a = a
