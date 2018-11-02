{-# LANGUAGE FlexibleContexts, OverloadedStrings, ScopedTypeVariables #-}

module Brains.Speech (speechToText) where

import           Control.Lens                 ((.~), (<&>), (^?), _1)
import           Control.Lens.Cons
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.Trans.Resource (runResourceT)
import qualified Data.ByteString              as BB
import           Data.ByteString.Base64
import           Data.ByteString.Lazy
import           Data.String
import qualified Data.Text                    as Text
import qualified Network.Google               as Google
import           Network.Google.Speech
import           Network.HTTP.Conduit
import           System.IO                    (stdout)
import           Util

performRequest :: BB.ByteString -> GEnv -> IO RecognizeResponse
performRequest body env = runResourceT . Google.runGoogle env $ do
  let audio = raContent .~ Just (encode body) $ recognitionAudio
  let sconfig = (rcLanguageCode .~ Just "ru-RU")
                . (rcEncoding .~ Just OggOpus)
                . (rcSampleRateHertz .~ Just 16000) $ recognitionConfig
  let speechReq = (rrConfig .~ Just sconfig) . (rrAudio .~ Just audio) $ recognizeRequest
  Google.send (speechRecognize speechReq)

runGoogleRequest :: String -> GEnv -> IO (Maybe RecognizeResponse)
runGoogleRequest url env = catch (do
  body <- simpleHttp url
  fmap Just $ performRequest (toStrict body) env) handleException
  where
    handleException :: SomeException -> IO (Maybe RecognizeResponse)
    handleException e = return Nothing

speechToText :: String -> IO (Maybe String)
speechToText url = do
  lgr  <- Google.newLogger Google.Debug stdout
  env  <- Google.newEnv <&>
    (Google.envLogger .~ lgr)
    . (Google.envScopes .~ cloudPlatformScope)
  recongniseResultM <- runGoogleRequest (fromString url) env
  case recongniseResultM of
    Nothing -> return Nothing
    Just recongniseResult ->
      return $ fmap Text.unpack
        $ join $ recongniseResult ^? rrResults
                                    . _Cons . _1
                                    . srrAlternatives
                                    . _Cons
                                    . _1
                                    . sraTranscript
