{-# LANGUAGE DataKinds, DeriveDataTypeable, DeriveGeneric, FlexibleInstances, NoImplicitPrelude, OverloadedStrings,
             RecordWildCards, TypeFamilies, TypeOperators #-}

{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds      #-}
{-# OPTIONS_GHC -fno-warn-unused-imports    #-}

-- |
-- Module      : Network.Google.Resource.Speech.Operations.Get
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the latest state of a long-running operation. Clients can use this
-- method to poll the operation result at intervals as recommended by the
-- API service.
--
-- /See:/ <https://cloud.google.com/speech/ Google Cloud Speech API Reference> for @speech.operations.get@.
module Network.Google.Resource.Speech.Operations.Get
    (
    -- * REST Resource
      OperationsGetResource

    -- * Creating a Request
    , operationsGet
    , OperationsGet

    -- * Request Lenses
    , ogXgafv
    , ogUploadProtocol
    , ogPp
    , ogAccessToken
    , ogUploadType
    , ogBearerToken
    , ogName
    , ogCallback
    ) where

import Network.Google.Prelude
import Network.Google.Speech.Types

-- | A resource alias for @speech.operations.get@ method which the
-- 'OperationsGet' request conforms to.
type OperationsGetResource =
     "v1" :>
       "operations" :>
         Capture "name" Text :>
           QueryParam "$.xgafv" Xgafv :>
             QueryParam "upload_protocol" Text :>
               QueryParam "pp" Bool :>
                 QueryParam "access_token" Text :>
                   QueryParam "uploadType" Text :>
                     QueryParam "bearer_token" Text :>
                       QueryParam "callback" Text :>
                         QueryParam "alt" AltJSON :> Get '[JSON] Operation

-- | Gets the latest state of a long-running operation. Clients can use this
-- method to poll the operation result at intervals as recommended by the
-- API service.
--
-- /See:/ 'operationsGet' smart constructor.
data OperationsGet = OperationsGet'
    { _ogXgafv          :: !(Maybe Xgafv)
    , _ogUploadProtocol :: !(Maybe Text)
    , _ogPp             :: !Bool
    , _ogAccessToken    :: !(Maybe Text)
    , _ogUploadType     :: !(Maybe Text)
    , _ogBearerToken    :: !(Maybe Text)
    , _ogName           :: !Text
    , _ogCallback       :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'OperationsGet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ogXgafv'
--
-- * 'ogUploadProtocol'
--
-- * 'ogPp'
--
-- * 'ogAccessToken'
--
-- * 'ogUploadType'
--
-- * 'ogBearerToken'
--
-- * 'ogName'
--
-- * 'ogCallback'
operationsGet
    :: Text -- ^ 'ogName'
    -> OperationsGet
operationsGet pOgName_ =
    OperationsGet'
    { _ogXgafv = Nothing
    , _ogUploadProtocol = Nothing
    , _ogPp = True
    , _ogAccessToken = Nothing
    , _ogUploadType = Nothing
    , _ogBearerToken = Nothing
    , _ogName = pOgName_
    , _ogCallback = Nothing
    }

-- | V1 error format.
ogXgafv :: Lens' OperationsGet (Maybe Xgafv)
ogXgafv = lens _ogXgafv (\ s a -> s{_ogXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
ogUploadProtocol :: Lens' OperationsGet (Maybe Text)
ogUploadProtocol
  = lens _ogUploadProtocol
      (\ s a -> s{_ogUploadProtocol = a})

-- | Pretty-print response.
ogPp :: Lens' OperationsGet Bool
ogPp = lens _ogPp (\ s a -> s{_ogPp = a})

-- | OAuth access token.
ogAccessToken :: Lens' OperationsGet (Maybe Text)
ogAccessToken
  = lens _ogAccessToken
      (\ s a -> s{_ogAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
ogUploadType :: Lens' OperationsGet (Maybe Text)
ogUploadType
  = lens _ogUploadType (\ s a -> s{_ogUploadType = a})

-- | OAuth bearer token.
ogBearerToken :: Lens' OperationsGet (Maybe Text)
ogBearerToken
  = lens _ogBearerToken
      (\ s a -> s{_ogBearerToken = a})

-- | The name of the operation resource.
ogName :: Lens' OperationsGet Text
ogName = lens _ogName (\ s a -> s{_ogName = a})

-- | JSONP
ogCallback :: Lens' OperationsGet (Maybe Text)
ogCallback
  = lens _ogCallback (\ s a -> s{_ogCallback = a})

instance GoogleRequest OperationsGet where
        type Rs OperationsGet = Operation
        type Scopes OperationsGet =
             '["https://www.googleapis.com/auth/cloud-platform"]
        requestClient OperationsGet'{..}
          = go _ogName _ogXgafv _ogUploadProtocol (Just _ogPp)
              _ogAccessToken
              _ogUploadType
              _ogBearerToken
              _ogCallback
              (Just AltJSON)
              speechService
          where go
                  = buildClient (Proxy :: Proxy OperationsGetResource)
                      mempty
