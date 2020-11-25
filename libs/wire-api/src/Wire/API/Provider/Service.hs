{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

module Wire.API.Provider.Service
  ( -- * ServiceRef
    ServiceRef (..),
    newServiceRef,
    serviceRefId,
    serviceRefProvider,

    -- * ServiceKey
    ServiceKey (..),
    ServiceKeyType (..),
    ServiceKeyPEM (..),

    -- * Service
    Service (..),
    ServiceToken (..),
    ServiceProfile (..),
    ServiceProfilePage (..),

    -- * Create/Update/Delete Service
    NewService (..),
    NewServiceResponse (..),
    UpdateService (..),
    UpdateServiceConn (..),
    mkUpdateServiceConn,
    DeleteService (..),

    -- * UpdateServiceWhitelist
    UpdateServiceWhitelist (..),

    -- * Swagger
    modelServiceRef,
  )
where

import qualified Cassandra.CQL as Cql
import Control.Lens (makeLenses)
import Data.Aeson
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Conversion
import Data.Id
import Data.Json.Util ((#))
import Data.List1 (List1)
import Data.Misc (HttpsUrl (..), PlainTextPassword (..))
import Data.PEM (PEM, pemParseBS, pemWriteLBS)
import Data.Range (Range)
import Data.Swagger (ToSchema (..))
import qualified Data.Swagger.Build.Api as Doc
import Data.Text.Ascii
import qualified Data.Text.Encoding as Text
import Deriving.Swagger (CamelToSnake, CustomSwagger, FieldLabelModifier, StripPrefix)
import Imports
import Wire.API.Arbitrary (Arbitrary (arbitrary), GenericUniform (..))
import Wire.API.Provider.Service.Tag (ServiceTag (..))
import Wire.API.User.Profile (Asset, Name)

--------------------------------------------------------------------------------
-- ServiceRef

-- | A fully-qualified reference to a service.
data ServiceRef = ServiceRef
  { _serviceRefId :: ServiceId,
    _serviceRefProvider :: ProviderId
  }
  deriving stock (Ord, Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform ServiceRef)
  deriving (ToSchema) via (CustomSwagger '[FieldLabelModifier (StripPrefix "_serviceRef", CamelToSnake)] ServiceRef)

makeLenses ''ServiceRef

newServiceRef :: ServiceId -> ProviderId -> ServiceRef
newServiceRef = ServiceRef

modelServiceRef :: Doc.Model
modelServiceRef = Doc.defineModel "ServiceRef" $ do
  Doc.description "Service Reference"
  Doc.property "id" Doc.bytes' $
    Doc.description "Service ID"
  Doc.property "provider" Doc.bytes' $
    Doc.description "Provider ID"

instance FromJSON ServiceRef where
  parseJSON = withObject "ServiceRef" $ \o ->
    ServiceRef <$> o .: "id" <*> o .: "provider"

instance ToJSON ServiceRef where
  toJSON r =
    object
      [ "id" .= _serviceRefId r,
        "provider" .= _serviceRefProvider r
      ]

--------------------------------------------------------------------------------
-- ServiceKey

-- | A PEM-encoded public key of a service used to verify the
-- identity of the remote peer in every established TLS connection
-- towards the service (i.e. public key pinning to prevent MITM attacks
-- with forged certificates).
data ServiceKey = ServiceKey
  { serviceKeyType :: ServiceKeyType,
    serviceKeySize :: Int32,
    serviceKeyPEM :: ServiceKeyPEM
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform ServiceKey)

instance ToJSON ServiceKey where
  toJSON k =
    object
      [ "type" .= serviceKeyType k,
        "size" .= serviceKeySize k,
        "pem" .= serviceKeyPEM k
      ]

instance FromJSON ServiceKey where
  parseJSON = withObject "ServiceKey" $ \o ->
    ServiceKey
      <$> o .: "type"
      <*> o .: "size"
      <*> o .: "pem"

-- | Other types may be supported in the future.
data ServiceKeyType
  = RsaServiceKey
  deriving stock (Eq, Enum, Bounded, Show, Generic)
  deriving (Arbitrary) via (GenericUniform ServiceKeyType)

instance ToJSON ServiceKeyType where
  toJSON RsaServiceKey = String "rsa"

instance FromJSON ServiceKeyType where
  parseJSON (String "rsa") = pure RsaServiceKey
  parseJSON _ = fail "Invalid service key type. Expected string 'rsa'."

newtype ServiceKeyPEM = ServiceKeyPEM {unServiceKeyPEM :: PEM}
  deriving stock (Eq, Show)

instance ToByteString ServiceKeyPEM where
  builder = BB.lazyByteString . pemWriteLBS . unServiceKeyPEM

instance FromByteString ServiceKeyPEM where
  parser = do
    bs <- parser
    case pemParseBS bs of
      Left e -> fail e
      Right [k] -> pure (ServiceKeyPEM k)
      Right _ -> fail "Too many sections in PEM format. Expected 1."

instance ToJSON ServiceKeyPEM where
  toJSON = String . Text.decodeUtf8 . toByteString'

instance FromJSON ServiceKeyPEM where
  parseJSON =
    withText "ServiceKeyPEM" $
      either fail pure . runParser parser . Text.encodeUtf8

instance Arbitrary ServiceKeyPEM where
  arbitrary =
    case pemParseBS (BS.unlines key) of
      Right [k] -> pure $ ServiceKeyPEM k
      other -> error $ "arbitrary ServiceKeyPEM: unexpected error: " <> show other
    where
      key =
        [ "-----BEGIN PUBLIC KEY-----",
          "MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAu+Kg/PHHU3atXrUbKnw0",
          "G06FliXcNt3lMwl2os5twEDcPPFw/feGiAKymxp+7JqZDrseS5D9THGrW+OQRIPH",
          "WvUBdiLfGrZqJO223DB6D8K2Su/odmnjZJ2z23rhXoEArTplu+Dg9K+c2LVeXTKV",
          "VPOaOzgtAB21XKRiQ4ermqgi3/njr03rXyq/qNkuNd6tNcg+HAfGxfGvvCSYBfiS",
          "bUKr/BeArYRcjzr/h5m1In6fG/if9GEI6m8dxHT9JbY53wiksowy6ajCuqskIFg8",
          "7X883H+LA/d6X5CTiPv1VMxXdBUiGPuC9IT/6CNQ1/LFt0P37ax58+LGYlaFo7la",
          "nQIDAQAB",
          "-----END PUBLIC KEY-----"
        ]

--------------------------------------------------------------------------------
-- Service

-- | Full service definition as seen by the provider.
data Service = Service
  { serviceId :: ServiceId,
    serviceName :: Name,
    serviceSummary :: Text,
    serviceDescr :: Text,
    serviceUrl :: HttpsUrl,
    serviceTokens :: List1 ServiceToken,
    serviceKeys :: List1 ServiceKey,
    serviceAssets :: [Asset],
    serviceTags :: Set ServiceTag,
    serviceEnabled :: Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform Service)

instance ToJSON Service where
  toJSON s =
    object $
      "id" .= serviceId s
        # "name" .= serviceName s
        # "summary" .= serviceSummary s
        # "description" .= serviceDescr s
        # "base_url" .= serviceUrl s
        # "auth_tokens" .= serviceTokens s
        # "public_keys" .= serviceKeys s
        # "assets" .= serviceAssets s
        # "tags" .= serviceTags s
        # "enabled" .= serviceEnabled s
        # []

instance FromJSON Service where
  parseJSON = withObject "Service" $ \o ->
    Service
      <$> o .: "id"
      <*> o .: "name"
      <*> o .: "summary"
      <*> o .: "description"
      <*> o .: "base_url"
      <*> o .: "auth_tokens"
      <*> o .: "public_keys"
      <*> o .: "assets"
      <*> o .: "tags"
      <*> o .: "enabled"

-- | A /secret/ bearer token used to authenticate and authorise requests @towards@
-- a 'Service' via inclusion in the HTTP 'Authorization' header.
newtype ServiceToken = ServiceToken AsciiBase64Url
  deriving stock (Eq, Show, Generic)
  deriving newtype (ToByteString, FromByteString, ToJSON, FromJSON, Arbitrary)

deriving instance Cql.Cql ServiceToken

--------------------------------------------------------------------------------
-- ServiceProfile

-- | Public profile of a service as seen by users.
data ServiceProfile = ServiceProfile
  { serviceProfileId :: ServiceId,
    serviceProfileProvider :: ProviderId,
    serviceProfileName :: Name,
    serviceProfileSummary :: Text,
    serviceProfileDescr :: Text,
    serviceProfileAssets :: [Asset],
    serviceProfileTags :: Set ServiceTag,
    serviceProfileEnabled :: Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform ServiceProfile)

instance ToJSON ServiceProfile where
  toJSON s =
    object $
      "id" .= serviceProfileId s
        # "provider" .= serviceProfileProvider s
        # "name" .= serviceProfileName s
        # "summary" .= serviceProfileSummary s
        # "description" .= serviceProfileDescr s
        # "assets" .= serviceProfileAssets s
        # "tags" .= serviceProfileTags s
        # "enabled" .= serviceProfileEnabled s
        # []

instance FromJSON ServiceProfile where
  parseJSON = withObject "ServiceProfile" $ \o ->
    ServiceProfile
      <$> o .: "id"
      <*> o .: "provider"
      <*> o .: "name"
      <*> o .: "summary"
      <*> o .: "description"
      <*> o .: "assets"
      <*> o .: "tags"
      <*> o .: "enabled"

--------------------------------------------------------------------------------
-- ServiceProfilePage

data ServiceProfilePage = ServiceProfilePage
  { serviceProfilePageHasMore :: Bool,
    serviceProfilePageResults :: [ServiceProfile]
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform ServiceProfilePage)

instance ToJSON ServiceProfilePage where
  toJSON p =
    object
      [ "has_more" .= serviceProfilePageHasMore p,
        "services" .= serviceProfilePageResults p
      ]

instance FromJSON ServiceProfilePage where
  parseJSON = withObject "ServiceProfilePage" $ \o ->
    ServiceProfilePage
      <$> o .: "has_more"
      <*> o .: "services"

--------------------------------------------------------------------------------
-- NewService

-- | Input data for registering a new service.
data NewService = NewService
  { newServiceName :: Name,
    newServiceSummary :: Range 1 128 Text,
    newServiceDescr :: Range 1 1024 Text,
    newServiceUrl :: HttpsUrl,
    newServiceKey :: ServiceKeyPEM,
    newServiceToken :: Maybe ServiceToken,
    newServiceAssets :: [Asset],
    newServiceTags :: Range 1 3 (Set ServiceTag)
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform NewService)

instance ToJSON NewService where
  toJSON s =
    object $
      "name" .= newServiceName s
        # "summary" .= newServiceSummary s
        # "description" .= newServiceDescr s
        # "base_url" .= newServiceUrl s
        # "public_key" .= newServiceKey s
        # "auth_token" .= newServiceToken s
        # "assets" .= newServiceAssets s
        # "tags" .= newServiceTags s
        # []

instance FromJSON NewService where
  parseJSON = withObject "NewService" $ \o ->
    NewService
      <$> o .: "name"
      <*> o .: "summary"
      <*> o .: "description"
      <*> o .: "base_url"
      <*> o .: "public_key"
      <*> o .:? "auth_token"
      <*> o .:? "assets" .!= []
      <*> o .: "tags"

-- | Response data upon adding a new service.
data NewServiceResponse = NewServiceResponse
  { rsNewServiceId :: ServiceId,
    -- | The generated bearer token that we will use for
    -- authenticating requests towards the service, if none was
    -- provided in the 'NewService' request.
    rsNewServiceToken :: Maybe ServiceToken
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform NewServiceResponse)

instance ToJSON NewServiceResponse where
  toJSON r =
    object $
      "id" .= rsNewServiceId r
        # "auth_token" .= rsNewServiceToken r
        # []

instance FromJSON NewServiceResponse where
  parseJSON = withObject "NewServiceResponse" $ \o ->
    NewServiceResponse
      <$> o .: "id"
      <*> o .:? "auth_token"

--------------------------------------------------------------------------------
-- UpdateService

-- | Update service profile information.
data UpdateService = UpdateService
  { updateServiceName :: Maybe Name,
    updateServiceSummary :: Maybe (Range 1 128 Text),
    updateServiceDescr :: Maybe (Range 1 1024 Text),
    updateServiceAssets :: Maybe [Asset],
    updateServiceTags :: Maybe (Range 1 3 (Set ServiceTag))
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform UpdateService)

instance ToJSON UpdateService where
  toJSON u =
    object $
      "name" .= updateServiceName u
        # "summary" .= updateServiceSummary u
        # "description" .= updateServiceDescr u
        # "assets" .= updateServiceAssets u
        # "tags" .= updateServiceTags u
        # []

instance FromJSON UpdateService where
  parseJSON = withObject "UpdateService" $ \o ->
    UpdateService
      <$> o .:? "name"
      <*> o .:? "summary"
      <*> o .:? "description"
      <*> o .:? "assets"
      <*> o .:? "tags"

--------------------------------------------------------------------------------
-- UpdateServiceConn

-- | Update service connection information.
-- This operation requires re-authentication via password.
data UpdateServiceConn = UpdateServiceConn
  { updateServiceConnPassword :: PlainTextPassword,
    updateServiceConnUrl :: Maybe HttpsUrl,
    updateServiceConnKeys :: Maybe (Range 1 2 [ServiceKeyPEM]),
    updateServiceConnTokens :: Maybe (Range 1 2 [ServiceToken]),
    updateServiceConnEnabled :: Maybe Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform UpdateServiceConn)

mkUpdateServiceConn :: PlainTextPassword -> UpdateServiceConn
mkUpdateServiceConn pw = UpdateServiceConn pw Nothing Nothing Nothing Nothing

instance ToJSON UpdateServiceConn where
  toJSON u =
    object $
      "password" .= updateServiceConnPassword u
        # "base_url" .= updateServiceConnUrl u
        # "public_keys" .= updateServiceConnKeys u
        # "auth_tokens" .= updateServiceConnTokens u
        # "enabled" .= updateServiceConnEnabled u
        # []

instance FromJSON UpdateServiceConn where
  parseJSON = withObject "UpdateServiceConn" $ \o ->
    UpdateServiceConn
      <$> o .: "password"
      <*> o .:? "base_url"
      <*> o .:? "public_keys"
      <*> o .:? "auth_tokens"
      <*> o .:? "enabled"

--------------------------------------------------------------------------------
-- DeleteService

-- | Input data for a service deletion request.
newtype DeleteService = DeleteService
  {deleteServicePassword :: PlainTextPassword}
  deriving stock (Eq, Show)
  deriving newtype (Arbitrary)

instance ToJSON DeleteService where
  toJSON d =
    object
      [ "password" .= deleteServicePassword d
      ]

instance FromJSON DeleteService where
  parseJSON = withObject "DeleteService" $ \o ->
    DeleteService <$> o .: "password"

--------------------------------------------------------------------------------
-- UpdateServiceWhitelist

data UpdateServiceWhitelist = UpdateServiceWhitelist
  { updateServiceWhitelistProvider :: ProviderId,
    updateServiceWhitelistService :: ServiceId,
    updateServiceWhitelistStatus :: Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform UpdateServiceWhitelist)

instance ToJSON UpdateServiceWhitelist where
  toJSON u =
    object
      [ "provider" .= updateServiceWhitelistProvider u,
        "id" .= updateServiceWhitelistService u,
        "whitelisted" .= updateServiceWhitelistStatus u
      ]

instance FromJSON UpdateServiceWhitelist where
  parseJSON = withObject "UpdateServiceWhitelist" $ \o ->
    UpdateServiceWhitelist
      <$> o .: "provider"
      <*> o .: "id"
      <*> o .: "whitelisted"
