{-# LANGUAGE DataKinds, DeriveDataTypeable, DeriveGeneric, FlexibleInstances, NoImplicitPrelude, OverloadedStrings,
             RecordWildCards, TypeFamilies, TypeOperators #-}

{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds      #-}
{-# OPTIONS_GHC -fno-warn-unused-imports    #-}

-- |
-- Module      : Network.Google.Resource.Speech.Speech.Longrunningrecognize
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Performs asynchronous speech recognition: receive results via the
-- google.longrunning.Operations interface. Returns either an
-- \`Operation.error\` or an \`Operation.response\` which contains a
-- \`LongRunningRecognizeResponse\` message.
--
-- /See:/ <https://cloud.google.com/speech/ Google Cloud Speech API Reference> for @speech.speech.longrunningrecognize@.
module Network.Google.Resource.Speech.Speech.Longrunningrecognize
    (
    -- * REST Resource
      SpeechLongrunningrecognizeResource

    -- * Creating a Request
    , speechLongrunningrecognize
    , SpeechLongrunningrecognize

    -- * Request Lenses
    , slXgafv
    , slUploadProtocol
    , slPp
    , slAccessToken
    , slUploadType
    , slPayload
    , slBearerToken
    , slCallback
    ) where

import Network.Google.Prelude
import Network.Google.Speech.Types

-- | A resource alias for @speech.speech.longrunningrecognize@ method which the
-- 'SpeechLongrunningrecognize' request conforms to.
type SpeechLongrunningrecognizeResource =
     "v1" :>
       "speech:longrunningrecognize" :>
         QueryParam "$.xgafv" Xgafv :>
           QueryParam "upload_protocol" Text :>
             QueryParam "pp" Bool :>
               QueryParam "access_token" Text :>
                 QueryParam "uploadType" Text :>
                   QueryParam "bearer_token" Text :>
                     QueryParam "callback" Text :>
                       QueryParam "alt" AltJSON :>
                         ReqBody '[JSON] LongRunningRecognizeRequest :>
                           Post '[JSON] Operation

-- | Performs asynchronous speech recognition: receive results via the
-- google.longrunning.Operations interface. Returns either an
-- \`Operation.error\` or an \`Operation.response\` which contains a
-- \`LongRunningRecognizeResponse\` message.
--
-- /See:/ 'speechLongrunningrecognize' smart constructor.
data SpeechLongrunningrecognize = SpeechLongrunningrecognize'
    { _slXgafv          :: !(Maybe Xgafv)
    , _slUploadProtocol :: !(Maybe Text)
    , _slPp             :: !Bool
    , _slAccessToken    :: !(Maybe Text)
    , _slUploadType     :: !(Maybe Text)
    , _slPayload        :: !LongRunningRecognizeRequest
    , _slBearerToken    :: !(Maybe Text)
    , _slCallback       :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'SpeechLongrunningrecognize' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'slXgafv'
--
-- * 'slUploadProtocol'
--
-- * 'slPp'
--
-- * 'slAccessToken'
--
-- * 'slUploadType'
--
-- * 'slPayload'
--
-- * 'slBearerToken'
--
-- * 'slCallback'
speechLongrunningrecognize
    :: LongRunningRecognizeRequest -- ^ 'slPayload'
    -> SpeechLongrunningrecognize
speechLongrunningrecognize pSlPayload_ =
    SpeechLongrunningrecognize'
    { _slXgafv = Nothing
    , _slUploadProtocol = Nothing
    , _slPp = True
    , _slAccessToken = Nothing
    , _slUploadType = Nothing
    , _slPayload = pSlPayload_
    , _slBearerToken = Nothing
    , _slCallback = Nothing
    }

-- | V1 error format.
slXgafv :: Lens' SpeechLongrunningrecognize (Maybe Xgafv)
slXgafv = lens _slXgafv (\ s a -> s{_slXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
slUploadProtocol :: Lens' SpeechLongrunningrecognize (Maybe Text)
slUploadProtocol
  = lens _slUploadProtocol
      (\ s a -> s{_slUploadProtocol = a})

-- | Pretty-print response.
slPp :: Lens' SpeechLongrunningrecognize Bool
slPp = lens _slPp (\ s a -> s{_slPp = a})

-- | OAuth access token.
slAccessToken :: Lens' SpeechLongrunningrecognize (Maybe Text)
slAccessToken
  = lens _slAccessToken
      (\ s a -> s{_slAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
slUploadType :: Lens' SpeechLongrunningrecognize (Maybe Text)
slUploadType
  = lens _slUploadType (\ s a -> s{_slUploadType = a})

-- | Multipart request metadata.
slPayload :: Lens' SpeechLongrunningrecognize LongRunningRecognizeRequest
slPayload
  = lens _slPayload (\ s a -> s{_slPayload = a})

-- | OAuth bearer token.
slBearerToken :: Lens' SpeechLongrunningrecognize (Maybe Text)
slBearerToken
  = lens _slBearerToken
      (\ s a -> s{_slBearerToken = a})

-- | JSONP
slCallback :: Lens' SpeechLongrunningrecognize (Maybe Text)
slCallback
  = lens _slCallback (\ s a -> s{_slCallback = a})

instance GoogleRequest SpeechLongrunningrecognize
         where
        type Rs SpeechLongrunningrecognize = Operation
        type Scopes SpeechLongrunningrecognize =
             '["https://www.googleapis.com/auth/cloud-platform"]
        requestClient SpeechLongrunningrecognize'{..}
          = go _slXgafv _slUploadProtocol (Just _slPp)
              _slAccessToken
              _slUploadType
              _slBearerToken
              _slCallback
              (Just AltJSON)
              _slPayload
              speechService
          where go
                  = buildClient
                      (Proxy :: Proxy SpeechLongrunningrecognizeResource)
                      mempty
