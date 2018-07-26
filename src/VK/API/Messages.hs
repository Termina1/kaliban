{-# LANGUAGE DeriveGeneric, DuplicateRecordFields, ExistentialQuantification, GADTs, OverloadedStrings, RecordWildCards,
             StandaloneDeriving #-}

module VK.API.Messages where

import Data.Aeson
import Data.List
import Data.Optional
import GHC.Generics
import Util
import VK.API
import VK.ResponseTypes

data AttachContainer a where
  DocC :: Document -> AttachContainer Document

deriving instance Show (AttachContainer a)

data Attachment = forall a. (FromJSON a, Show a) => Attachment
  { _type  :: String
  , attach :: AttachContainer a
  }

instance FromJSON Attachment where
  parseJSON =
    withObject "attach" $ \o -> do
      _type <- o .: "type"
      case _type of
        "doc" -> do
          attach <- o .: "doc"
          return $ Attachment _type $ DocC (attach :: Document)
        _ -> fail $ "Unkown attach type: " ++ _type

deriving instance Show Attachment

data AudioMessagePreview = AudioMessagePreview {
  duration :: Int,
  link_ogg :: String,
  link_mp3 :: String
} deriving (Show, Generic)

instance FromJSON AudioMessagePreview

data DocumentPreview = DocumentPreview {
  audio_msg :: Optional AudioMessagePreview
} deriving (Show, Generic)

instance FromJSON DocumentPreview

data Document = Document {
  id         :: Int,
  owner_id   :: Int,
  title      :: String,
  size       :: Int,
  ext        :: String,
  url        :: String,
  date       :: Int,
  access_key :: Optional String,
  preview    :: DocumentPreview
} deriving (Show, Generic)

instance FromJSON Document

data ForwardedMessage = ForwardedMessage
  { fromId      :: Int
  , date        :: Int
  , text        :: Optional String
  , forwarded   :: Optional [ForwardedMessage]
  , attachments :: Optional [Attachment]
  } deriving (Show)

getFMText :: ForwardedMessage -> String
getFMText (ForwardedMessage _ _ text _ _) = defaultTo "" text

getFMFrom :: ForwardedMessage -> Int
getFMFrom (ForwardedMessage fromId _ _ _ _) = fromId

instance FromJSON ForwardedMessage where
  parseJSON =
    withObject "fwd_message" $ \o -> do
      fromId <- o .: "from_id"
      date <- o .: "date"
      text <- o .:!? "text"
      forwarded <- o .:!? "fwd_messages"
      attachments <- o .:!? "attachments"
      return ForwardedMessage {..}

data Message = Message
  { id               :: Int
  , date             :: Int
  , peerId           :: Int
  , fromId           :: Int
  , text             :: String
  , randomId         :: Optional Int
  , forwardedMesages :: Optional [ForwardedMessage]
  , attachments      :: Optional [Attachment]
  , out              :: Int
  } deriving (Show)

getFrom :: Message -> Int
getFrom (Message _ _ _ fromId _ _ _ _ _) = fromId

getMText :: Message -> String
getMText (Message _ _ _ _ text _ _ _ _) = text

instance FromJSON Message where
  parseJSON =
    withObject "message" $ \o -> do
      id <- o .: "id"
      date <- o .: "date"
      peerId <- o .: "peer_id"
      fromId <- o .: "from_id"
      forwardedMesages <- o .:!? "fwd_messages"
      text <- o .: "text"
      randomId <- o .:!? "random_id"
      attachments <- o .:!? "attachments"
      out <- o .: "out"
      return Message {..}

getById :: APIOwner -> [Int] -> Optional Int -> IO (APIResponse (WithCount Message))
getById owner ids preview_length =
  apiRequest
    owner
    "messages.getById"
    ([("message_ids", toList ids), ("preview_length", (show $ toInt preview_length))] :: [(String, String)])

data LpServer = LpServer
  { key    :: String
  , server :: String
  , ts     :: Int
  , pts    :: Optional Int
  } deriving (Show)

instance FromJSON LpServer where
  parseJSON =
    withObject "lp server" $ \o -> do
      key <- o .: "key"
      server <- o .: "server"
      ts <- o .: "ts"
      pts <- o .:!? "pts"
      return LpServer {..}

getLongpollServer :: APIOwner -> Bool -> Int -> IO (APIResponse LpServer)
getLongpollServer owner pts lpVer =
  apiRequest
    owner
    "messages.getLongPollServer"
    ([("need_pts", (show pts)), ("lp_version", (show lpVer))] :: [(String, String)])

data LpHistoryParams = LpHistoryParams
  { timestamp     :: Either Int Int
  , previewLength :: Optional Int
  , fields        :: Optional [String]
  , eventsLimit   :: Optional Int
  , msgsLimit     :: Optional Int
  , maxId         :: Optional Int
  , lpVer         :: Optional Int
  , onlines       :: Optional Bool
  } deriving (Show)

lpHistoryParams :: Int -> LpHistoryParams
lpHistoryParams pts = LpHistoryParams (Right pts) Default Default Default Default Default (Specific 3) Default

fromBool :: Bool -> String
fromBool True  = "1"
fromBool False = "0"

instance Paramable LpHistoryParams where
  toParams lp =
    let params =
          [ ("preview_length", show $ defaultTo 0 $ previewLength lp)
          , ( "fields"
            , intercalate "," $ defaultTo ["photo", "photo_medium_rec", "sex", "online", "screen_name"] $ fields lp)
          , ("events_limit", show $ defaultTo 1000 $ eventsLimit lp)
          , ("msgs_limit", show $ defaultTo 200 $ msgsLimit lp)
          , ("max_msg_id", show $ defaultTo 0 $ maxId lp)
          , ("lp_version", show $ defaultTo 0 $ lpVer lp)
          , ("onlines", fromBool $ defaultTo False $ onlines lp)
          ]
    in case (timestamp lp) of
         Left its  -> ("ts", show its) : params
         Right pts -> ("pts", show pts) : params

data Profile = Profile
  { id               :: Int
  , first_name       :: String
  , last_name        :: String
  , sex              :: Optional Int
  , screen_name      :: Optional String
  , photo            :: Optional String
  , photo_medium_rec :: Optional String
  } deriving (Show)

instance FromJSON Profile where
  parseJSON =
    withObject "profile" $ \o -> do
      id <- o .: "id"
      first_name <- o .: "first_name"
      last_name <- o .: "last_name"
      sex <- o .:!? "sex"
      screen_name <- o .:!? "screen_name"
      photo <- o .:!? "photo"
      photo_medium_rec <- o .:!? "photo_medium_rec"
      return Profile {..}

data LpHistoryResult = LpHistoryResult
  { history  :: [[Int]]
  , messages :: WithCount Message
  , profiles :: Optional [Profile]
  , newPts   :: Int
  } deriving (Show)

instance FromJSON LpHistoryResult where
  parseJSON =
    withObject "lp response" $ \o -> do
      history <- o .: "history"
      messages <- o .: "messages"
      profiles <- o .:!? "profiles"
      newPts <- o .: "new_pts"
      return LpHistoryResult {..}

getLongpollHistory :: APIOwner -> LpHistoryParams -> IO (APIResponse LpHistoryResult)
getLongpollHistory owner params = apiRequest owner "messages.getLongPollHistory" params

send :: APIOwner -> Int -> String -> IO (APIResponse Int)
send owner peerId text =
  apiRequest owner "messages.send" ([("peer_id", show peerId), ("message", text)] :: [(String, String)])
