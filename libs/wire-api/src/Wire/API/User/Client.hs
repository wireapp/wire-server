{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData #-}

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

module Wire.API.User.Client
  ( -- * SupportedClientFeature
    SupportedClientFeature (..),
    SupportedClientFeatureList (..),

    -- * UserClients
    UserClientMap (..),
    QualifiedUserClientMap (..),
    UserClients (..),
    QualifiedUserClients (..),
    filterClients,

    -- * Client
    Client (..),
    PubClient (..),
    ClientType (..),
    ClientClass (..),

    -- * New/Update/Remove Client
    NewClient (..),
    newClient,
    UpdateClient (..),
    RmClient (..),

    -- * re-exports
    Location,
    location,
    latitude,
    longitude,
    Latitude (..),
    Longitude (..),

    -- * Swagger
    modelOtrClientMap,
    modelUserClients,
    modelNewClient,
    modelUpdateClient,
    modelClientSupportedFeatureList,
    typeSupportedClientFeature,
    modelDeleteClient,
    modelClient,
    modelSigkeys,
    modelLocation, -- re-export from types-common
  )
where

import qualified Cassandra as Cql
import Control.Lens ((?~), (^.))
import Data.Aeson
import Data.Domain (Domain)
import qualified Data.HashMap.Strict as HashMap
import Data.Id
import Data.Json.Util
import qualified Data.Map.Strict as Map
import Data.Misc (Latitude (..), Location, Longitude (..), PlainTextPassword (..), latitude, location, longitude, modelLocation)
import Data.Proxy (Proxy (..))
import Data.Swagger (HasExample (example), NamedSchema (..), ToSchema (..), declareSchema)
import qualified Data.Swagger as Swagger
import qualified Data.Swagger.Build.Api as Doc
import Data.Swagger.Schema (toSchema)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.E
import Data.Typeable (typeRep)
import Data.UUID (toASCIIBytes)
import Deriving.Swagger
  ( CamelToKebab,
    CamelToSnake,
    ConstructorTagModifier,
    CustomSwagger,
    FieldLabelModifier,
    LabelMapping ((:->)),
    LabelMappings,
    LowerCase,
    StripPrefix,
    StripSuffix,
  )
import Imports
import Wire.API.Arbitrary (Arbitrary (arbitrary), GenericUniform (..), generateExample, mapOf', setOf')
import Wire.API.User.Auth (CookieLabel)
import Wire.API.User.Client.Prekey as Prekey

----------------------------------------------------------------------
-- SupportedClientFeature, SupportedClientFeatureList

-- | Names of features clients can claim to support in order to be treated differently by the
-- backend.
--
-- **The cost of feature switches**
--
-- Avoid this wherever possible.  Adding feature switches in the backend code makes testing
-- exponentially more expensive (in principle, you should always test all combinations of
-- supported features.  But even if you only test those known to occur in the wild, it will
-- still make your life harder.)
--
-- Consider dropping support for clients without ancient features if you have "enough" clients
-- that are younger.  This will always be disruptive for a minority of users, but maybe this
-- can be mitigated by giving those users clear feedback that they need to upgrade in order to
-- get their expected UX back.
--
-- **An alternative design**
--
-- Consider replacing 'SupportedClientFeature' with platform and version in formation (I
-- played with @data Platform = Android | IOS | WebApp | TeamSettings | AccountPages@ and
-- @Version@ from the `semver` package in https://github.com/wireapp/wire-server/pull/1503,
-- but ended up deciding against it).  This data could be passed in a similar way as the
-- 'SupportedClientFeatureList' is now (similar end-point, different path, different body
-- type), and the two approaches could be used in parallel indefinitely.
--
-- Feature flags only reveal the minimum amount of information necessary to handle the client,
-- making it harder to fingerprint and track clients; they are straight-forward and
-- self-documenting (to an extent), and make it easier to release a feature on the backend and
-- clients independently.
--
-- Platform/version info is if you have many different feature switches, even though it
-- doesn't solve the problem of having to explore the entire feature space in your tests.
-- They give you a better idea of the time line, and how to gently discontinue support for
-- ancient features.
data SupportedClientFeature
  = -- | Clients have minimum support for LH, but not for explicit consent.  Implicit consent
    -- is granted via the galley server config (see '_setLegalHoldTeamsWhitelist').
    ClientSupportsLegalholdImplicitConsent
  deriving stock (Eq, Ord, Bounded, Enum, Show, Generic)
  deriving (Arbitrary) via (GenericUniform SupportedClientFeature)
  deriving (ToSchema) via (CustomSwagger '[ConstructorTagModifier (StripPrefix "ClientSupports", CamelToKebab)] SupportedClientFeature)

typeSupportedClientFeature :: Doc.DataType
typeSupportedClientFeature =
  Doc.string $
    Doc.enum
      [ "legalhold-implicit-consent"
      ]

instance ToJSON SupportedClientFeature where
  toJSON ClientSupportsLegalholdImplicitConsent = String "legalhold-implicit-consent"

instance FromJSON SupportedClientFeature where
  parseJSON (String "legalhold-implicit-consent") = pure ClientSupportsLegalholdImplicitConsent
  parseJSON _ = fail "SupportedClientFeature"

instance Cql.Cql SupportedClientFeature where
  ctype = Cql.Tagged Cql.IntColumn

  toCql ClientSupportsLegalholdImplicitConsent = Cql.CqlInt 1

  fromCql (Cql.CqlInt i) = case i of
    1 -> return ClientSupportsLegalholdImplicitConsent
    n -> Left $ "Unexpected SupportedClientFeature value: " ++ show n
  fromCql _ = Left "SupportedClientFeature value: int expected"

-- FUTUREWORK: add golden tests for this?
data SupportedClientFeatureList = SupportedClientFeatureList {fromSupportedClientFeatureList :: Set SupportedClientFeature}
  deriving stock (Eq, Ord, Show, Generic)
  deriving (Arbitrary) via (GenericUniform SupportedClientFeatureList)
  deriving (ToSchema) via (CustomSwagger '[FieldLabelModifier (StripPrefix "fromSupportedClient", CamelToSnake)] SupportedClientFeatureList)

modelClientSupportedFeatureList :: Doc.Model
modelClientSupportedFeatureList = Doc.defineModel "SupportedClientFeatureList" $ do
  Doc.description "Hints provided by the client for the backend so it can behavior in a backwards-compatible way."
  Doc.property "feature_list" (Doc.array typeSupportedClientFeature) $ do
    Doc.description "Array containing all supported features."

instance ToJSON SupportedClientFeatureList where
  toJSON (SupportedClientFeatureList l) = object ["feature_list" .= l]

instance FromJSON SupportedClientFeatureList where
  parseJSON = withObject "SupportedClientFeatureList" $ \obj -> SupportedClientFeatureList <$> obj .: "feature_list"

--------------------------------------------------------------------------------
-- UserClientMap

newtype UserClientMap a = UserClientMap
  { userClientMap :: Map UserId (Map ClientId a)
  }
  deriving stock (Eq, Show, Functor, Foldable, Traversable)
  deriving newtype (Semigroup, Monoid)

-- FUTUREWORK: Remove when 'NewOtrMessage' has ToSchema
modelOtrClientMap :: Doc.Model
modelOtrClientMap = Doc.defineModel "OtrClientMap" $ do
  Doc.description "Map of client IDs to OTR content."
  Doc.property "" Doc.bytes' $
    Doc.description "Mapping from client IDs to OTR content (Base64 in JSON)."

instance ToJSON a => ToJSON (UserClientMap a) where
  toJSON = toJSON . Map.foldrWithKey' f Map.empty . userClientMap
    where
      f (Id u) clients m =
        let key = Text.E.decodeLatin1 (toASCIIBytes u)
            val = Map.foldrWithKey' g Map.empty clients
         in Map.insert key val m
      g (ClientId c) a = Map.insert c (toJSON a)

instance FromJSON a => FromJSON (UserClientMap a) where
  parseJSON = withObject "user-client-map" $ \o ->
    UserClientMap <$> foldrM f Map.empty (HashMap.toList o)
    where
      f (k, v) m = do
        u <- parseJSON (String k)
        flip (withObject "client-value-map") v $ \c -> do
          e <- foldrM g Map.empty (HashMap.toList c)
          return (Map.insert u e m)
      g (k, v) m = do
        c <- parseJSON (String k)
        t <- parseJSON v
        return (Map.insert c t m)

instance Arbitrary a => Arbitrary (UserClientMap a) where
  arbitrary = UserClientMap <$> mapOf' arbitrary (mapOf' arbitrary arbitrary)

instance ToSchema (UserClientMap (Maybe Prekey)) where
  declareNamedSchema _ = do
    mapSch <- declareSchema (Proxy @(Map UserId (Map ClientId (Maybe Prekey))))
    let valueTypeName = Text.pack $ show $ typeRep $ Proxy @(Maybe Prekey)
    return $
      NamedSchema (Just $ "UserClientMap (" <> valueTypeName <> ")") $
        mapSch
          & example
            ?~ toJSON
              ( Map.singleton
                  (generateExample @UserId)
                  ( Map.singleton
                      (newClientId 4940483633899001999)
                      (Just (Prekey (PrekeyId 1) "pQABAQECoQBYIOjl7hw0D8YRNq..."))
                  )
              )

newtype QualifiedUserClientMap a = QualifiedUserClientMap
  { qualifiedUserClientMap :: Map Domain (UserClientMap a)
  }
  deriving stock (Eq, Show)
  deriving newtype (Semigroup, Monoid, ToJSON, FromJSON)

instance Arbitrary a => Arbitrary (QualifiedUserClientMap a) where
  arbitrary = QualifiedUserClientMap <$> mapOf' arbitrary arbitrary

instance (Typeable a, ToSchema (UserClientMap a)) => ToSchema (QualifiedUserClientMap a) where
  declareNamedSchema _ = do
    mapSch <- declareSchema (Proxy @(Map Domain (UserClientMap a)))
    let userMapSchema = toSchema (Proxy @(UserClientMap a))
    let valueTypeName = Text.pack $ show $ typeRep $ Proxy @a
    return $
      NamedSchema (Just $ "QualifiedUserClientMap (" <> valueTypeName <> ")") $
        mapSch
          & Swagger.description ?~ "Map of Domain to (UserMap (" <> valueTypeName <> "))."
          & example
            ?~ toJSON
              (Map.singleton ("domain1.example.com" :: Text) (userMapSchema ^. example))

--------------------------------------------------------------------------------
-- UserClients

-- TODO: check if example generated by swagger look okay (probably not)
newtype UserClients = UserClients
  { userClients :: Map UserId (Set ClientId)
  }
  deriving stock (Eq, Show, Generic)
  deriving newtype (Semigroup, Monoid)

instance ToSchema UserClients where
  declareNamedSchema _ = do
    mapSch <- declareSchema (Proxy @(Map UserId (Set ClientId)))
    return $
      NamedSchema (Just "UserClients") $
        mapSch
          & Swagger.description ?~ "Map of user id to list of client ids."
          & example
            ?~ toJSON
              ( Map.fromList
                  [ (generateExample @UserId, [newClientId 1684636986166846496, newClientId 4940483633899001999]),
                    (generateExample @UserId, [newClientId 6987438498444556166, newClientId 7940473633839002939])
                  ]
              )

-- FUTUREWORK: Remove when 'NewOtrMessage' has ToSchema
modelUserClients :: Doc.Model
modelUserClients =
  Doc.defineModel "UserClients" $
    Doc.property "" (Doc.unique $ Doc.array Doc.bytes') $
      Doc.description "Map of user IDs to sets of client IDs ({ UserId: [ClientId] })."

instance ToJSON UserClients where
  toJSON =
    toJSON . Map.foldrWithKey' fn Map.empty . userClients
    where
      fn u c m =
        let k = Text.E.decodeLatin1 (toASCIIBytes (toUUID u))
         in Map.insert k c m

instance FromJSON UserClients where
  parseJSON =
    withObject "UserClients" (fmap UserClients . foldrM fn Map.empty . HashMap.toList)
    where
      fn (k, v) m = Map.insert <$> parseJSON (String k) <*> parseJSON v <*> pure m

instance Arbitrary UserClients where
  arbitrary = UserClients <$> mapOf' arbitrary (setOf' arbitrary)

filterClients :: (Set ClientId -> Bool) -> UserClients -> UserClients
filterClients p (UserClients c) = UserClients $ Map.filter p c

newtype QualifiedUserClients = QualifiedUserClients
  { qualifiedUserClients :: Map Domain UserClients
  }
  deriving stock (Eq, Show, Generic)
  deriving newtype (Semigroup, Monoid, FromJSON, ToJSON)

instance Arbitrary QualifiedUserClients where
  arbitrary = QualifiedUserClients <$> mapOf' arbitrary arbitrary

instance ToSchema QualifiedUserClients where
  declareNamedSchema _ = do
    schema <- declareSchema (Proxy @(Map Domain UserClients))
    userClientsSchema <- declareSchema (Proxy @UserClients)
    return $
      NamedSchema (Just "QualifiedUserClients") $
        schema
          & Swagger.description ?~ "Map of Domain to UserClients"
          & example
            ?~ toJSON
              (Map.singleton ("domain1.example.com" :: Text) (userClientsSchema ^. example))

--------------------------------------------------------------------------------
-- Client

data Client = Client
  { clientId :: ClientId,
    clientType :: ClientType,
    clientTime :: UTCTimeMillis,
    clientClass :: Maybe ClientClass,
    clientLabel :: Maybe Text,
    clientCookie :: Maybe CookieLabel,
    clientLocation :: Maybe Location,
    clientModel :: Maybe Text
  }
  deriving stock (Eq, Show, Generic, Ord)
  deriving (Arbitrary) via (GenericUniform Client)
  deriving (ToSchema) via (CustomSwagger '[FieldLabelModifier (StripPrefix "client", LowerCase)] Client)

modelClient :: Doc.Model
modelClient = Doc.defineModel "Client" $ do
  Doc.description "A registered client."
  Doc.property "type" typeClientType $
    Doc.description "The client type."
  Doc.property "id" Doc.string' $
    Doc.description "The client ID."
  Doc.property "label" Doc.string' $ do
    Doc.description "An optional label associated with the client."
    Doc.optional
  Doc.property "time" Doc.dateTime' $
    Doc.description "The date and time when this client was registered."
  Doc.property "class" typeClientClass $
    Doc.description "The device class this client belongs to."
  Doc.property "cookie" Doc.string' $
    Doc.description "The cookie label of this client."
  Doc.property "address" Doc.string' $ do
    Doc.description "IP address from which this client has been registered"
    Doc.optional
  Doc.property "location" (Doc.ref modelLocation) $ do
    Doc.description "Location from which this client has been registered."
    Doc.optional
  Doc.property "model" Doc.string' $ do
    Doc.description "Optional model information of this client"
    Doc.optional

instance ToJSON Client where
  toJSON c =
    object $
      "id" .= clientId c
        # "type" .= clientType c
        # "label" .= clientLabel c
        # "class" .= clientClass c
        # "time" .= clientTime c
        # "cookie" .= clientCookie c
        # "location" .= clientLocation c
        # "model" .= clientModel c
        # []

instance FromJSON Client where
  parseJSON = withObject "Client" $ \o ->
    Client
      <$> o .: "id"
      <*> o .: "type"
      <*> o .: "time"
      <*> o .:? "class"
      <*> o .:? "label"
      <*> o .:? "cookie"
      <*> o .:? "location"
      <*> o .:? "model"

--------------------------------------------------------------------------------
-- PubClient

data PubClient = PubClient
  { pubClientId :: ClientId,
    pubClientClass :: Maybe ClientClass
  }
  deriving stock (Eq, Show, Generic, Ord)
  deriving (Arbitrary) via (GenericUniform PubClient)
  deriving (ToSchema) via (CustomSwagger '[FieldLabelModifier (StripPrefix "pubClient", LowerCase)] PubClient)

instance ToJSON PubClient where
  toJSON c =
    object $
      "id" .= pubClientId c
        # "class" .= pubClientClass c
        # []

instance FromJSON PubClient where
  parseJSON = withObject "PubClient" $ \o ->
    PubClient
      <$> o .: "id"
      <*> o .:? "class"

--------------------------------------------------------------------------------
-- Client Type/Class

-- [Note: LegalHold]
--
-- Short feature description:
-- LegalHold is an enterprise feature, enabled on a per-team basis, and within a
-- team on a per-user basis
--
-- A LegalHoldClient is a client outside that user's control (but under the
-- control of that team's business)
--
-- Users need to click "accept" before a LegalHoldClient is added to their
-- account.
--
-- Any user interacting with a user which has a LegalHoldClient will upon
-- first interaction receive a warning, have the option of cancelling the
-- interaction, and on an ongoing basis see a visual indication in all
-- conversations where such a device is active.

-- | Strategy to translate enums in this module to schema.
type EnumToSchemaStrategy suffix ty =
  ( CustomSwagger
      '[ ConstructorTagModifier
           ( StripSuffix suffix,
             CamelToSnake,
             LabelMappings '["legal_hold" ':-> "legalhold"]
           )
       ]
      ty
  )

data ClientType
  = TemporaryClientType
  | PermanentClientType
  | LegalHoldClientType -- see Note [LegalHold]
  deriving stock (Eq, Ord, Show, Generic)
  deriving (Arbitrary) via (GenericUniform ClientType)
  deriving (ToSchema) via EnumToSchemaStrategy "ClientType" ClientType

typeClientType :: Doc.DataType
typeClientType =
  Doc.string $
    Doc.enum
      [ "permanent",
        "temporary",
        "legalhold"
      ]

instance ToJSON ClientType where
  toJSON TemporaryClientType = String "temporary"
  toJSON PermanentClientType = String "permanent"
  toJSON LegalHoldClientType = String "legalhold"

instance FromJSON ClientType where
  parseJSON = withText "ClientType" $ \txt -> case txt of
    "temporary" -> return TemporaryClientType
    "permanent" -> return PermanentClientType
    "legalhold" -> return LegalHoldClientType
    _ -> fail "Must be one of {'temporary', 'permanent', 'legalhold'}."

data ClientClass
  = PhoneClient
  | TabletClient
  | DesktopClient
  | LegalHoldClient -- see Note [LegalHold]
  deriving stock (Eq, Ord, Show, Generic)
  deriving (Arbitrary) via (GenericUniform ClientClass)
  deriving (ToSchema) via EnumToSchemaStrategy "Client" ClientClass

typeClientClass :: Doc.DataType
typeClientClass =
  Doc.string $
    Doc.enum
      [ "phone",
        "tablet",
        "desktop",
        "legalhold"
      ]

instance ToJSON ClientClass where
  toJSON PhoneClient = String "phone"
  toJSON TabletClient = String "tablet"
  toJSON DesktopClient = String "desktop"
  toJSON LegalHoldClient = String "legalhold"

instance FromJSON ClientClass where
  parseJSON = withText "ClientClass" $ \txt -> case txt of
    "phone" -> return PhoneClient
    "tablet" -> return TabletClient
    "desktop" -> return DesktopClient
    "legalhold" -> return LegalHoldClient
    _ -> fail "Must be one of {'phone', 'tablet', 'desktop', 'legalhold'}."

--------------------------------------------------------------------------------
-- NewClient

data NewClient = NewClient
  { newClientPrekeys :: [Prekey],
    newClientLastKey :: LastPrekey,
    newClientType :: ClientType,
    newClientLabel :: Maybe Text,
    newClientClass :: Maybe ClientClass,
    newClientCookie :: Maybe CookieLabel,
    newClientPassword :: Maybe PlainTextPassword,
    newClientModel :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform NewClient)

modelNewClient :: Doc.Model
modelNewClient = Doc.defineModel "NewClient" $ do
  Doc.description "The registration data for a new client."
  Doc.property "type" typeClientType $
    Doc.description
      "The type of client to register. A user may have no more than \
      \7 (seven) permanent clients and 1 (one) temporary client. When the \
      \limit of permanent clients is reached, an error is returned. \
      \When a temporary client already exists, it is replaced."
  Doc.property "password" Doc.string' $ do
    Doc.description
      "The password of the authenticated user for verification. \
      \Note: Required for registration of the 2nd, 3rd, ... client."
    Doc.optional
  Doc.property "prekeys" (Doc.array (Doc.ref modelPrekey)) $
    Doc.description "Prekeys for other clients to establish OTR sessions."
  Doc.property "lastkey" (Doc.ref modelPrekey) $
    Doc.description
      "The last resort prekey for other clients to establish OTR sessions. \
      \This key must have the ID 0xFFFF and is never deleted."
  -- FUTUREWORK: sigkeys don't seem to be used anymore
  Doc.property "sigkeys" (Doc.ref modelSigkeys) $
    Doc.description
      "The signaling keys to use for encryption and signing of OTR native push \
      \notifications (APNS, GCM)."
  Doc.property "label" Doc.string' $ do
    Doc.description "An optional label to associate with the client."
    Doc.optional
  Doc.property "class" typeClientClass $
    Doc.description "The device class this client belongs to. Either 'phone', 'tablet', or 'desktop'."
  Doc.property "cookie" Doc.string' $
    Doc.description "The cookie label, i.e. the label used when logging in."
  Doc.property "model" Doc.string' $ do
    Doc.description "Optional model information of this client"
    Doc.optional

newClient :: ClientType -> LastPrekey -> NewClient
newClient t k =
  NewClient
    { newClientPrekeys = [],
      newClientLastKey = k,
      newClientType = t,
      newClientLabel = Nothing,
      newClientClass = if t == LegalHoldClientType then Just LegalHoldClient else Nothing,
      newClientCookie = Nothing,
      newClientPassword = Nothing,
      newClientModel = Nothing
    }

instance ToJSON NewClient where
  toJSON c =
    object $
      "type" .= newClientType c
        # "prekeys" .= newClientPrekeys c
        # "lastkey" .= newClientLastKey c
        # "label" .= newClientLabel c
        # "class" .= newClientClass c
        # "cookie" .= newClientCookie c
        # "password" .= newClientPassword c
        # "model" .= newClientModel c
        # []

instance FromJSON NewClient where
  parseJSON = withObject "NewClient" $ \o ->
    NewClient
      <$> o .: "prekeys"
      <*> o .: "lastkey"
      <*> o .: "type"
      <*> o .:? "label"
      <*> o .:? "class"
      <*> o .:? "cookie"
      <*> o .:? "password"
      <*> o .:? "model"

--------------------------------------------------------------------------------
-- UpdateClient

data UpdateClient = UpdateClient
  { updateClientPrekeys :: [Prekey],
    updateClientLastKey :: Maybe LastPrekey,
    updateClientLabel :: Maybe Text,
    -- | see haddocks for 'SupportedClientFeature'
    updateClientSupportedFeatures :: Maybe (Set SupportedClientFeature)
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform UpdateClient)

modelUpdateClient :: Doc.Model
modelUpdateClient = Doc.defineModel "UpdateClient" $ do
  Doc.description "The new data for the registered client."
  Doc.property "prekeys" (Doc.array (Doc.ref modelPrekey)) $ do
    Doc.description "New prekeys for other clients to establish OTR sessions."
    Doc.optional
  Doc.property "lastkey" (Doc.ref modelPrekey) $ do
    Doc.description "New last-resort prekey."
    Doc.optional
  -- FUTUREWORK: sigkeys don't seem to be used anymore, remove?
  Doc.property "sigkeys" (Doc.ref modelSigkeys) $ do
    Doc.description
      "New signaling keys to use for encryption and signing of OTR native push \
      \notifications (APNS, GCM)."
    Doc.optional
  Doc.property "label" Doc.string' $ do
    Doc.description "A new name for this client."
    Doc.optional
  Doc.property "supported-features" typeSupportedClientFeature $ do
    Doc.description "Hints for the backend so it can behave in a backwards-compatible way."
    Doc.optional

instance ToJSON UpdateClient where
  toJSON c =
    object $
      "prekeys" .= updateClientPrekeys c
        # "lastkey" .= updateClientLastKey c
        # "label" .= updateClientLabel c
        # "supported-features" .= updateClientSupportedFeatures c
        # []

instance FromJSON UpdateClient where
  parseJSON = withObject "RefreshClient" $ \o ->
    UpdateClient
      <$> o .:? "prekeys" .!= []
      <*> o .:? "lastkey"
      <*> o .:? "label"
      <*> o .:? "supported-features"

--------------------------------------------------------------------------------
-- RmClient

newtype RmClient = RmClient
  { rmPassword :: Maybe PlainTextPassword
  }
  deriving stock (Eq, Show, Generic)
  deriving newtype (Arbitrary)

modelDeleteClient :: Doc.Model
modelDeleteClient = Doc.defineModel "DeleteClient" $ do
  Doc.description "Required information for client deletion."
  Doc.property "password" Doc.string' $ do
    Doc.description
      "The password of the authenticated user for verification. \
      \The password is not required for deleting temporary clients."
    Doc.optional

instance ToJSON RmClient where
  toJSON (RmClient pw) = object ["password" .= pw]

instance FromJSON RmClient where
  parseJSON = withObject "RmClient" $ \o ->
    RmClient <$> o .:? "password"

--------------------------------------------------------------------------------
-- other models

modelSigkeys :: Doc.Model
modelSigkeys = Doc.defineModel "SignalingKeys" $ do
  Doc.description "Signaling keys for encryption and signing of native push notifications (APNS, GCM)."
  Doc.property "enckey" Doc.bytes' $
    Doc.description "The base64-encoded, 256 bit encryption key."
  Doc.property "mackey" Doc.bytes' $
    Doc.description "The base64-encoded, 256 bit MAC key."
