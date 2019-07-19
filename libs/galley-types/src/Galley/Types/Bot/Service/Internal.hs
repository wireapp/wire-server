{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module Galley.Types.Bot.Service.Internal where

import Imports
import Control.Lens (makeLenses)
import Data.Aeson
import Data.ByteString.Conversion
import Data.Id
import Data.Misc (Fingerprint, Rsa, HttpsUrl)
import Data.Text.Ascii
import Cassandra.CQL

-- ServiceRef -----------------------------------------------------------------

-- | A fully-qualified reference to a service.
data ServiceRef = ServiceRef
    { _serviceRefId       :: !ServiceId
    , _serviceRefProvider :: !ProviderId
    } deriving (Ord, Eq, Show, Generic)

makeLenses ''ServiceRef

newServiceRef :: ServiceId -> ProviderId -> ServiceRef
newServiceRef = ServiceRef

instance FromJSON ServiceRef where
    parseJSON = withObject "ServiceRef" $ \o ->
        ServiceRef <$> o .: "id" <*> o .: "provider"

instance ToJSON ServiceRef where
    toJSON r = object
        [ "id"       .= _serviceRefId r
        , "provider" .= _serviceRefProvider r
        ]

-- Service --------------------------------------------------------------------

-- | A /secret/ bearer token used to authenticate and authorise requests @towards@
-- a 'Service' via inclusion in the HTTP 'Authorization' header.
newtype ServiceToken = ServiceToken AsciiBase64Url
    deriving (Eq, Show, ToByteString, Cql, FromByteString, FromJSON, ToJSON, Generic)

-- | Service connection information that is needed by galley.
data Service = Service
    { _serviceRef          :: !ServiceRef
    , _serviceUrl          :: !HttpsUrl
    , _serviceToken        :: !ServiceToken
    , _serviceFingerprints :: ![Fingerprint Rsa]
    , _serviceEnabled      :: !Bool
    }

makeLenses ''Service

newService :: ServiceRef -> HttpsUrl -> ServiceToken -> [Fingerprint Rsa] -> Service
newService ref url tok fps = Service ref url tok fps True

instance FromJSON Service where
    parseJSON = withObject "Service" $ \o ->
        Service <$> o .: "ref"
                <*> o .: "base_url"
                <*> o .: "auth_token"
                <*> o .: "fingerprints"
                <*> o .: "enabled"

instance ToJSON Service where
    toJSON s = object
        [ "ref"          .= _serviceRef s
        , "base_url"     .= _serviceUrl s
        , "auth_token"   .= _serviceToken s
        , "fingerprints" .= _serviceFingerprints s
        , "enabled"      .= _serviceEnabled s
        ]
