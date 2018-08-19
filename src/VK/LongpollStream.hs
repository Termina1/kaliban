{-# LANGUAGE DeriveGeneric, FlexibleContexts, GADTs, MultiParamTypeClasses, OverloadedStrings #-}

module VK.LongpollStream where

import           Control.Concurrent
import           Control.Monad.Catch
import           Control.Monad.State
import           Data.Aeson
import qualified Data.ByteString.Lazy.UTF8 as B
import qualified Data.HashMap.Strict       as HM
import           Data.String
import           Network.HTTP.Conduit
import           Network.URL
import           Streaming
import qualified Streaming.Prelude         as S
import           Util
import           VK.API
import qualified VK.API.Groups             as G
import           VK.API.Messages
import           VK.ResponseTypes


data HandleResult = HRetry | HRecreate | HPropogate | HOk

data LpError =
  LpCredError |
  LpUnknownCredError String |
  LPError String |
  LPKeyError String deriving (Show)

type LpContainer a = Either LpError a

data BotLongpollEvent = BotEventMessage Message | BotEventReply Message | BotEventUnknown

data BotLongpollResponse where
  BotResponseOk :: {
    tsOk :: String,
    updates :: [BotLongpollEvent]
  } -> BotLongpollResponse
  BotResponseError :: {
    failed :: Int,
    tsFail :: Maybe Int
  } -> BotLongpollResponse

instance FromJSON BotLongpollResponse where
  parseJSON = withObject "longpoll response" $ \o -> do
    case HM.lookup "failed" o of
      Just _ -> do
        failed <- o .: "failed"
        ts <- o .:? "ts"
        return $ BotResponseError failed ts
      Nothing -> do
        ts <- o .: "ts"
        updates <- o .: "updates"
        return $ BotResponseOk ts updates

instance FromJSON BotLongpollEvent where
  parseJSON = withObject "event" $ \o -> do
    evType <- o .: "type"
    case (evType :: String) of
      "message_new" -> do
        message <- o .: "object"
        return $ BotEventMessage message
      "message_reply" -> do
        message <- o .: "object"
        return $ BotEventReply message
      _ -> return BotEventUnknown

apiCredentialsStream :: Monad m => APIOwner -> Stream (Of APIOwner) m ()
apiCredentialsStream creds = S.repeat creds

lpCredentialsStream :: MonadIO m => Stream (Of APIOwner) m () -> Stream (Of (LpContainer G.GroupLPServer)) m ()
lpCredentialsStream credStream = S.mapM getLpCreds credStream
  where
    getLpCreds owner = do
      result <- G.getLongpollServer owner (negate (ownerId owner))
      case result of
        APIError code err   -> return $ Left (if code == 5 then LpCredError else LpUnknownCredError err)
        APIRequestError err -> return $ Left $ LpUnknownCredError err
        APIResult creds     -> return $ Right creds

lpRequestStream :: (MonadIO m, MonadState String m, MonadCatch m) => Stream (Of (LpContainer G.GroupLPServer)) m () -> Stream (Of (LpContainer Message)) m ()
lpRequestStream lpServerStream = effect $ do
  nextVal <- S.next lpServerStream
  case nextVal of
    Left ret -> return $ return ret
    Right (lpServerCont, nextStream) ->
      case lpServerCont of
        Left _ -> return $ return ()
        Right lpServer -> do
          storedTs <- get
          let currentTs = if storedTs == "" then show (G.ts lpServer) else storedTs
          let params = [("act", "a_check"), ("key", G.key lpServer), ("ts", currentTs), ("wait", "25")]
          let lpurl = (G.server lpServer) ++ "?" ++ exportParams params
          catch (do
            result <- simpleHttp lpurl
            case lpEventFromStr result of
              Left str -> return $ (S.yield $ Left $ LPError $ "invalid JSON: " ++ str) >> lpRequestStream nextStream
              Right (BotResponseError failed tsM) -> if failed == 1 then
                case tsM of
                  Nothing -> return $ (S.yield $ Left $ LPError "No ts in failed: 1 event") >> lpRequestStream nextStream
                  Just ts -> do
                    put (show ts)
                    return $ (S.yield $ Left $ LPError "ts expired") >> lpRequestStream nextStream
              else
                return $ (S.yield $ Left $ LPKeyError "key failed") >> lpRequestStream nextStream

              Right (BotResponseOk ts updates) -> do
                let messages = foldl accMessage [] updates
                put ts
                return $ (S.map Right (S.each messages)) >> lpRequestStream nextStream)
            (\e -> return $ (S.yield $ Left $ LPError $ "Exception: " ++ (show (e :: SomeException))) >> lpRequestStream nextStream)
  where
    accMessage acc (BotEventReply message)   = message : acc
    accMessage acc (BotEventMessage message) = message : acc
    accMessage acc BotEventUnknown           = acc

    lpEventFromStr :: B.ByteString -> Either String BotLongpollResponse
    lpEventFromStr body = eitherDecode body

repeaterStream :: MonadIO m => (a -> HandleResult) -> Timeout -> (() -> Stream (Of a) m ()) -> Stream (Of a) m ()
repeaterStream handle timeout streamCreator = repeaterStreamHelper (streamCreator ()) timeout
  where
    repeaterStreamHelper stream timeout = effect $ do
      streamVal <- S.next stream
      case streamVal of
        Left ret -> do
          return $ return $ ret
        Right (val, nextStream) ->
          case handle val of
            HRetry -> do
              liftIO $ threadDelay ((currentTimeout timeout) * 1000000)
              return $ repeaterStreamHelper nextStream (backoff timeout)
            HRecreate -> do
              liftIO $ threadDelay ((currentTimeout timeout) * 1000000)
              return $ repeaterStream handle (backoff timeout) streamCreator
            _ -> return $ S.yield val >> repeaterStreamHelper nextStream (reset timeout)

cacheStream :: Monad m => Stream (Of a) m r -> Stream (Of a) m r
cacheStream stream = effect (S.next stream >>= \t ->
  case t of
    Left r       -> return $ return r
    Right (a, _) -> return $ S.repeat a)

probe :: (MonadIO m, Show a, LogIO m) => String -> Stream (Of a) m r -> Stream (Of a) m r
probe tag stream = S.mapM (\val -> (logInfoT $ fromString $ (tag ++ ": " ++ (show val))) >> return val) stream

lpCanHandle :: LpContainer Message -> HandleResult
lpCanHandle (Left (LPKeyError _)) = HRecreate
lpCanHandle (Left _)              = HRetry
lpCanHandle (Right _)             = HOk

apiCanHandle :: LpContainer G.GroupLPServer -> HandleResult
apiCanHandle (Left LpCredError) = HPropogate
apiCanHandle (Left _)           = HRetry
apiCanHandle (Right _)          = HOk

timeout :: Timeout
timeout = Timeout 1 1 10

longpollStream :: LogIO m => APIOwner -> Stream (Of (LpContainer Message)) (StateT String m) ()
longpollStream owner = repeaterStream lpCanHandle timeout $
  (\_ -> probe "longpoll" $ lpRequestStream $ cacheStream $ repeaterStream apiCanHandle timeout $
  (\_ -> probe "credentials" $ lpCredentialsStream $ apiCredentialsStream owner))
