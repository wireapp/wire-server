{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
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
    UpdateServiceWhitelistResp (..),
  )
where

import Cassandra.CQL qualified as Cql
import Control.Lens (makeLenses, (?~))
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Aeson qualified as A
import Data.ByteString.Builder qualified as BB
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Conversion
import Data.Id
import Data.Json.Util ((#))
import Data.List1 (List1)
import Data.Misc (HttpsUrl (..), PlainTextPassword6)
import Data.OpenApi qualified as S
import Data.PEM (PEM, pemParseBS, pemWriteLBS)
import Data.Proxy
import Data.Range (Range, fromRange, rangedSchema)
import Data.SOP
import Data.Schema
import Data.Text qualified as Text
import Data.Text.Ascii
import Data.Text.Encoding qualified as Text
import Imports
import Wire.API.Provider.Service.Tag (ServiceTag (..))
import Wire.API.Routes.MultiVerb
import Wire.API.User.Profile (Asset, Name)
import Wire.Arbitrary (Arbitrary (arbitrary), GenericUniform (..))

--------------------------------------------------------------------------------
-- ServiceRef

-- | A fully-qualified reference to a service.
data ServiceRef = ServiceRef
  { _serviceRefId :: ServiceId,
    _serviceRefProvider :: ProviderId
  }
  deriving stock (Ord, Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform ServiceRef)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema ServiceRef

instance ToSchema ServiceRef where
  schema =
    object "ServiceRef" $
      ServiceRef
        <$> _serviceRefId .= field "id" schema
        <*> _serviceRefProvider .= field "provider" schema

makeLenses ''ServiceRef

newServiceRef :: ServiceId -> ProviderId -> ServiceRef
newServiceRef = ServiceRef

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
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema ServiceKey

instance ToSchema ServiceKey where
  schema =
    object "ServiceKey" $
      ServiceKey
        <$> serviceKeyType .= field "type" schema
        <*> serviceKeySize .= field "size" schema
        <*> serviceKeyPEM .= field "pem" schema

-- | Other types may be supported in the future.
data ServiceKeyType
  = RsaServiceKey
  deriving stock (Eq, Enum, Bounded, Show, Generic)
  deriving (Arbitrary) via (GenericUniform ServiceKeyType)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema ServiceKeyType

instance ToSchema ServiceKeyType where
  schema =
    enum @Text "ServiceKeyType" (element "rsa" RsaServiceKey)

newtype ServiceKeyPEM = ServiceKeyPEM {unServiceKeyPEM :: PEM}
  deriving stock (Eq, Show)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema ServiceKeyPEM

instance ToByteString ServiceKeyPEM where
  builder = BB.lazyByteString . pemWriteLBS . unServiceKeyPEM

instance FromByteString ServiceKeyPEM where
  parser = do
    bs <- parser
    case pemParseBS bs of
      Left e -> fail e
      Right [k] -> pure (ServiceKeyPEM k)
      Right _ -> fail "Too many sections in PEM format. Expected 1."

instance ToSchema ServiceKeyPEM where
  schema =
    S.schema . S.example ?~ pem $
      (Text.decodeUtf8 . toByteString')
        .= parsedText
          "ServiceKeyPEM"
          (runParser parser . Text.encodeUtf8)
    where
      pem =
        A.String . Text.unlines $
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
  deriving (S.ToSchema) via (Schema Service)

instance ToSchema Service where
  schema =
    object "Service" $
      Service
        <$> serviceId .= field "id" schema
        <*> serviceName .= field "name" schema
        <*> serviceSummary .= field "summary" schema
        <*> serviceDescr .= field "description" schema
        <*> serviceUrl .= field "base_url" schema
        <*> serviceTokens .= field "auth_tokens" schema
        <*> serviceKeys .= field "public_keys" schema
        <*> serviceAssets .= field "assets" (array schema)
        <*> serviceTags .= field "tags" (set schema)
        <*> serviceEnabled .= field "enabled" schema

instance ToJSON Service where
  toJSON s =
    A.object $
      "id" A..= serviceId s
        # "name" A..= serviceName s
        # "summary" A..= serviceSummary s
        # "description" A..= serviceDescr s
        # "base_url" A..= serviceUrl s
        # "auth_tokens" A..= serviceTokens s
        # "public_keys" A..= serviceKeys s
        # "assets" A..= serviceAssets s
        # "tags" A..= serviceTags s
        # "enabled" A..= serviceEnabled s
        # []

instance FromJSON Service where
  parseJSON = A.withObject "Service" $ \o ->
    Service
      <$> o A..: "id"
      <*> o A..: "name"
      <*> o A..: "summary"
      <*> o A..: "description"
      <*> o A..: "base_url"
      <*> o A..: "auth_tokens"
      <*> o A..: "public_keys"
      <*> o A..: "assets"
      <*> o A..: "tags"
      <*> o A..: "enabled"

-- | A /secret/ bearer token used to authenticate and authorise requests @towards@
-- a 'Service' via inclusion in the HTTP 'Authorization' header.
newtype ServiceToken = ServiceToken AsciiBase64Url
  deriving stock (Eq, Show, Generic)
  deriving newtype (ToByteString, FromByteString, ToJSON, FromJSON, Arbitrary, ToSchema)

instance S.ToSchema ServiceToken where
  declareNamedSchema _ = tweak $ S.declareNamedSchema (Proxy @Text)
    where
      tweak = fmap $ S.schema . S.example ?~ tok
      tok = "sometoken"

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
  deriving (S.ToSchema) via (Schema ServiceProfile)

instance ToJSON ServiceProfile where
  toJSON s =
    A.object $
      "id" A..= serviceProfileId s
        # "provider" A..= serviceProfileProvider s
        # "name" A..= serviceProfileName s
        # "summary" A..= serviceProfileSummary s
        # "description" A..= serviceProfileDescr s
        # "assets" A..= serviceProfileAssets s
        # "tags" A..= serviceProfileTags s
        # "enabled" A..= serviceProfileEnabled s
        # []

instance FromJSON ServiceProfile where
  parseJSON = A.withObject "ServiceProfile" $ \o ->
    ServiceProfile
      <$> o A..: "id"
      <*> o A..: "provider"
      <*> o A..: "name"
      <*> o A..: "summary"
      <*> o A..: "description"
      <*> o A..: "assets"
      <*> o A..: "tags"
      <*> o A..: "enabled"

instance ToSchema ServiceProfile where
  schema =
    object "ServiceProfile" $
      ServiceProfile
        <$> serviceProfileId .= field "id" schema
        <*> serviceProfileProvider .= field "provider" schema
        <*> serviceProfileName .= field "name" schema
        <*> serviceProfileSummary .= field "summary" schema
        <*> serviceProfileDescr .= field "description" schema
        <*> serviceProfileAssets .= field "assets" (array schema)
        <*> serviceProfileTags .= field "tags" (set schema)
        <*> serviceProfileEnabled .= field "enabled" schema

--------------------------------------------------------------------------------
-- ServiceProfilePage

data ServiceProfilePage = ServiceProfilePage
  { serviceProfilePageHasMore :: Bool,
    serviceProfilePageResults :: [ServiceProfile]
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform ServiceProfilePage)
  deriving (S.ToSchema) via (Schema ServiceProfilePage)

instance ToJSON ServiceProfilePage where
  toJSON p =
    A.object
      [ "has_more" A..= serviceProfilePageHasMore p,
        "services" A..= serviceProfilePageResults p
      ]

instance FromJSON ServiceProfilePage where
  parseJSON = A.withObject "ServiceProfilePage" $ \o ->
    ServiceProfilePage
      <$> o A..: "has_more"
      <*> o A..: "services"

instance ToSchema ServiceProfilePage where
  schema =
    object "ServiceProfile" $
      ServiceProfilePage
        <$> serviceProfilePageHasMore .= field "has_more" schema
        <*> serviceProfilePageResults .= field "services" (array schema)

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
  deriving (S.ToSchema) via (Schema NewService)

instance ToSchema NewService where
  schema =
    object "NewService" $
      NewService
        <$> newServiceName .= field "name" schema
        <*> newServiceSummary .= field "summary" schema
        <*> newServiceDescr .= field "description" schema
        <*> newServiceUrl .= field "base_url" schema
        <*> newServiceKey .= field "public_key" schema
        <*> newServiceToken .= maybe_ (optField "auth_token" schema)
        <*> newServiceAssets .= field "assets" (array schema)
        <*> newServiceTags .= field "tags" (fromRange .= rangedSchema (set schema))

instance ToJSON NewService where
  toJSON s =
    A.object $
      "name" A..= newServiceName s
        # "summary" A..= newServiceSummary s
        # "description" A..= newServiceDescr s
        # "base_url" A..= newServiceUrl s
        # "public_key" A..= newServiceKey s
        # "auth_token" A..= newServiceToken s
        # "assets" A..= newServiceAssets s
        # "tags" A..= newServiceTags s
        # []

instance FromJSON NewService where
  parseJSON = A.withObject "NewService" $ \o ->
    NewService
      <$> o A..: "name"
      <*> o A..: "summary"
      <*> o A..: "description"
      <*> o A..: "base_url"
      <*> o A..: "public_key"
      <*> o A..:? "auth_token"
      <*> o A..:? "assets" A..!= []
      <*> o A..: "tags"

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
  deriving (S.ToSchema) via (Schema NewServiceResponse)

instance ToSchema NewServiceResponse where
  schema =
    object "NewServiceResponse" $
      NewServiceResponse
        <$> rsNewServiceId .= field "id" schema
        <*> rsNewServiceToken .= maybe_ (optField "auth_token" schema)

instance ToJSON NewServiceResponse where
  toJSON r =
    A.object $
      "id" A..= rsNewServiceId r
        # "auth_token" A..= rsNewServiceToken r
        # []

instance FromJSON NewServiceResponse where
  parseJSON = A.withObject "NewServiceResponse" $ \o ->
    NewServiceResponse
      <$> o A..: "id"
      <*> o A..:? "auth_token"

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
  deriving (S.ToSchema) via (Schema UpdateService)

instance ToSchema UpdateService where
  schema =
    object "UpdateService" $
      UpdateService
        <$> updateServiceName .= maybe_ (optField "name" schema)
        <*> updateServiceSummary .= maybe_ (optField "summary" schema)
        <*> updateServiceDescr .= maybe_ (optField "description" schema)
        <*> updateServiceAssets .= maybe_ (optField "assets" $ array schema)
        <*> updateServiceTags .= maybe_ (optField "tags" (fromRange .= rangedSchema (set schema)))

instance ToJSON UpdateService where
  toJSON u =
    A.object $
      "name" A..= updateServiceName u
        # "summary" A..= updateServiceSummary u
        # "description" A..= updateServiceDescr u
        # "assets" A..= updateServiceAssets u
        # "tags" A..= updateServiceTags u
        # []

instance FromJSON UpdateService where
  parseJSON = A.withObject "UpdateService" $ \o ->
    UpdateService
      <$> o A..:? "name"
      <*> o A..:? "summary"
      <*> o A..:? "description"
      <*> o A..:? "assets"
      <*> o A..:? "tags"

--------------------------------------------------------------------------------
-- UpdateServiceConn

-- | Update service connection information.
-- This operation requires re-authentication via password.
data UpdateServiceConn = UpdateServiceConn
  { updateServiceConnPassword :: PlainTextPassword6,
    updateServiceConnUrl :: Maybe HttpsUrl,
    updateServiceConnKeys :: Maybe (Range 1 2 [ServiceKeyPEM]),
    updateServiceConnTokens :: Maybe (Range 1 2 [ServiceToken]),
    updateServiceConnEnabled :: Maybe Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform UpdateServiceConn)
  deriving (S.ToSchema) via (Schema UpdateServiceConn)

instance ToSchema UpdateServiceConn where
  schema =
    object "UpdateServiceConn" $
      UpdateServiceConn
        <$> updateServiceConnPassword .= field "password" schema
        <*> updateServiceConnUrl .= maybe_ (optField "base_url" schema)
        <*> updateServiceConnKeys .= maybe_ (optField "public_keys" (fromRange .= rangedSchema (array schema)))
        <*> updateServiceConnTokens .= maybe_ (optField "auth_tokens" (fromRange .= rangedSchema (array schema)))
        <*> updateServiceConnEnabled .= maybe_ (optField "enabled" schema)

mkUpdateServiceConn :: PlainTextPassword6 -> UpdateServiceConn
mkUpdateServiceConn pw = UpdateServiceConn pw Nothing Nothing Nothing Nothing

instance ToJSON UpdateServiceConn where
  toJSON u =
    A.object $
      "password" A..= updateServiceConnPassword u
        # "base_url" A..= updateServiceConnUrl u
        # "public_keys" A..= updateServiceConnKeys u
        # "auth_tokens" A..= updateServiceConnTokens u
        # "enabled" A..= updateServiceConnEnabled u
        # []

instance FromJSON UpdateServiceConn where
  parseJSON = A.withObject "UpdateServiceConn" $ \o ->
    UpdateServiceConn
      <$> o A..: "password"
      <*> o A..:? "base_url"
      <*> o A..:? "public_keys"
      <*> o A..:? "auth_tokens"
      <*> o A..:? "enabled"

--------------------------------------------------------------------------------
-- DeleteService

-- | Input data for a service deletion request.
newtype DeleteService = DeleteService
  {deleteServicePassword :: PlainTextPassword6}
  deriving stock (Eq, Show)
  deriving newtype (Arbitrary)
  deriving (S.ToSchema) via (Schema DeleteService)

instance ToSchema DeleteService where
  schema =
    object "DeleteService" $
      DeleteService
        <$> deleteServicePassword .= field "password" schema

instance ToJSON DeleteService where
  toJSON d =
    A.object
      [ "password" A..= deleteServicePassword d
      ]

instance FromJSON DeleteService where
  parseJSON = A.withObject "DeleteService" $ \o ->
    DeleteService <$> o A..: "password"

--------------------------------------------------------------------------------
-- UpdateServiceWhitelist

data UpdateServiceWhitelist = UpdateServiceWhitelist
  { updateServiceWhitelistProvider :: ProviderId,
    updateServiceWhitelistService :: ServiceId,
    updateServiceWhitelistStatus :: Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform UpdateServiceWhitelist)
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema UpdateServiceWhitelist)

instance ToSchema UpdateServiceWhitelist where
  schema =
    object "UpdateServiceWhitelist" $
      UpdateServiceWhitelist
        <$> updateServiceWhitelistProvider .= field "provider" schema
        <*> updateServiceWhitelistService .= field "id" schema
        <*> updateServiceWhitelistStatus .= field "whitelisted" schema

data UpdateServiceWhitelistResp
  = UpdateServiceWhitelistRespChanged
  | UpdateServiceWhitelistRespUnchanged

-- basically the same as the instance for CheckBlacklistResponse
instance AsUnion '[RespondEmpty 200 "UpdateServiceWhitelistRespChanged", RespondEmpty 204 "UpdateServiceWhitelistRespUnchanged"] UpdateServiceWhitelistResp where
  toUnion UpdateServiceWhitelistRespChanged = Z (I ())
  toUnion UpdateServiceWhitelistRespUnchanged = S (Z (I ()))
  fromUnion (Z (I ())) = UpdateServiceWhitelistRespChanged
  fromUnion (S (Z (I ()))) = UpdateServiceWhitelistRespUnchanged
  fromUnion (S (S x)) = case x of {}
