{-# LANGUAGE DataKinds, DeriveDataTypeable, DeriveGeneric, NoImplicitPrelude, OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.Google.Speech.Types
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.Google.Speech.Types
    (
    -- * Service Configuration
      speechService

    -- * OAuth Scopes
    , cloudPlatformScope

    -- * Status
    , Status
    , status
    , sDetails
    , sCode
    , sMessage

    -- * SpeechContext
    , SpeechContext
    , speechContext
    , scPhrases

    -- * RecognizeRequest
    , RecognizeRequest
    , recognizeRequest
    , rrConfig
    , rrAudio

    -- * Operation
    , Operation
    , operation
    , oDone
    , oError
    , oResponse
    , oName
    , oMetadata

    -- * SpeechRecognitionAlternative
    , SpeechRecognitionAlternative
    , speechRecognitionAlternative
    , sraConfidence
    , sraWords
    , sraTranscript

    -- * WordInfo
    , WordInfo
    , wordInfo
    , wiStartTime
    , wiEndTime
    , wiWord

    -- * StatusDetailsItem
    , StatusDetailsItem
    , statusDetailsItem
    , sdiAddtional

    -- * SpeechRecognitionResult
    , SpeechRecognitionResult
    , speechRecognitionResult
    , srrAlternatives

    -- * RecognitionAudio
    , RecognitionAudio
    , recognitionAudio
    , raURI
    , raContent

    -- * RecognizeResponse
    , RecognizeResponse
    , recognizeResponse
    , rrResults

    -- * Xgafv
    , Xgafv (..)

    -- * RecognitionConfig
    , RecognitionConfig
    , recognitionConfig
    , rcEnableWordTimeOffSets
    , rcSpeechContexts
    , rcLanguageCode
    , rcSampleRateHertz
    , rcMaxAlternatives
    , rcProfanityFilter
    , rcEncoding
    , rcEnableWordConfidence

    -- * LongRunningRecognizeRequest
    , LongRunningRecognizeRequest
    , longRunningRecognizeRequest
    , lrrrConfig
    , lrrrAudio

    -- * OperationMetadata
    , OperationMetadata
    , operationMetadata
    , omAddtional

    -- * RecognitionConfigEncoding
    , RecognitionConfigEncoding (..)

    -- * OperationResponse
    , OperationResponse
    , operationResponse
    , orAddtional
    ) where

import Network.Google.Prelude
import Network.Google.Speech.Types.Product
import Network.Google.Speech.Types.Sum

-- | Default request referring to version 'v1' of the Google Cloud Speech API. This contains the host and root path used as a starting point for constructing service requests.
speechService :: ServiceConfig
speechService
  = defaultService (ServiceId "speech:v1")
      "speech.googleapis.com"

-- | View and manage your data across Google Cloud Platform services
cloudPlatformScope :: Proxy '["https://www.googleapis.com/auth/cloud-platform"]
cloudPlatformScope = Proxy;
