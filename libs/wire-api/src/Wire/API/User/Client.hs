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
  ( -- * ClientCapability
    ClientCapability (..),
    ClientCapabilityList (..),

    -- * UserClients
    UserClientMap (..),
    UserClientPrekeyMap (..),
    mkUserClientPrekeyMap,
    QualifiedUserClientMap (..),
    QualifiedUserClientPrekeyMap (..),
    mkQualifiedUserClientPrekeyMap,
    UserClientsFull (..),
    userClientsFullToUserClients,
    UserClients (..),
    QualifiedUserClients (..),
    filterClients,
    filterClientsFull,

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
    modelClientCapabilityList,
    typeClientCapability,
    modelDeleteClient,
    modelClient,
    modelSigkeys,
    modelLocation, -- re-export from types-common
  )
where

import qualified Cassandra as Cql
import Control.Lens ((?~), (^.))
import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Aeson as A
import Data.Coerce
import Data.Domain (Domain)
import qualified Data.HashMap.Strict as HashMap
import Data.Id
import Data.Json.Util
import qualified Data.Map.Strict as Map
import Data.Misc (Latitude (..), Location, Longitude (..), PlainTextPassword (..), latitude, location, longitude, modelLocation)
import Data.Proxy (Proxy (..))
import Data.Schema
import qualified Data.Set as Set
import qualified Data.Swagger as Swagger
import qualified Data.Swagger.Build.Api as Doc
import qualified Data.Text.Encoding as Text.E
import Data.UUID (toASCIIBytes)
import Deriving.Swagger
  ( CamelToSnake,
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
-- ClientCapability, ClientCapabilityList

-- | Names of capabilities clients can claim to support in order to be treated differently by
-- the backend.
--
-- **The cost of capability keywords**
--
-- Avoid this wherever possible.  Adding capability keywords in the backend code makes testing
-- exponentially more expensive (in principle, you should always test all combinations of
-- supported capabilitiess.  But even if you only test those known to occur in the wild, it will
-- still make your life harder.)
--
-- Consider dropping support for clients without ancient capabilitiess if you have "enough" clients
-- that are younger.  This will always be disruptive for a minority of users, but maybe this
-- can be mitigated by giving those users clear feedback that they need to upgrade in order to
-- get their expected UX back.
--
-- **An alternative design**
--
-- Consider replacing 'ClientCapability' with platform and version in formation (I
-- played with @data Platform = Android | IOS | WebApp | TeamSettings | AccountPages@ and
-- @Version@ from the `semver` package in https://github.com/wireapp/wire-server/pull/1503,
-- but ended up deciding against it).  This data could be passed in a similar way as the
-- 'ClientCapabilityList' is now (similar end-point, different path, different body
-- type), and the two approaches could be used in parallel indefinitely.
--
-- Capability keywords reveal the minimum amount of information necessary to handle the client,
-- making it harder to fingerprint and track clients; they are straight-forward and
-- self-documenting (to an extent), and make it easier to release a capability on the backend and
-- clients independently.
--
-- Platform/version info is if you have many different capability keywords, even though it
-- doesn't solve the problem of having to explore the entire capability space in your tests.
-- They give you a better idea of the time line, and how to gently discontinue support for
-- ancient capabilities.
data ClientCapability
  = -- | Clients have minimum support for LH, but not for explicit consent.  Implicit consent
    -- is granted via the galley server config (see '_setLegalHoldTeamsWhitelist').
    ClientSupportsLegalholdImplicitConsent
  deriving stock (Eq, Ord, Bounded, Enum, Show, Generic)
  deriving (Arbitrary) via (GenericUniform ClientCapability)
  deriving (ToJSON, FromJSON, Swagger.ToSchema) via Schema ClientCapability

instance ToSchema ClientCapability where
  schema =
    enum @Text "ClientCapability" $
      element "legalhold-implicit-consent" ClientSupportsLegalholdImplicitConsent

typeClientCapability :: Doc.DataType
typeClientCapability =
  Doc.string $
    Doc.enum
      [ "legalhold-implicit-consent"
      ]

instance Cql.Cql ClientCapability where
  ctype = Cql.Tagged Cql.IntColumn

  toCql ClientSupportsLegalholdImplicitConsent = Cql.CqlInt 1

  fromCql (Cql.CqlInt i) = case i of
    1 -> return ClientSupportsLegalholdImplicitConsent
    n -> Left $ "Unexpected ClientCapability value: " ++ show n
  fromCql _ = Left "ClientCapability value: int expected"

-- FUTUREWORK: add golden tests for this?
data ClientCapabilityList = ClientCapabilityList {fromClientCapabilityList :: Set ClientCapability}
  deriving stock (Eq, Ord, Show, Generic)
  deriving (Arbitrary) via (GenericUniform ClientCapabilityList)
  deriving (ToJSON, FromJSON, Swagger.ToSchema) via (Schema ClientCapabilityList)

instance ToSchema ClientCapabilityList where
  schema =
    objectWithDocModifier "ClientCapabilityList" mods $
      ClientCapabilityList
        <$> (Set.toList . fromClientCapabilityList)
          .= field "capabilities" (Set.fromList <$> array schema)
    where
      mods = description ?~ ("Hints provided by the client for the backend so it can behavior in a backwards-compatible way." :: Text)

modelClientCapabilityList :: Doc.Model
modelClientCapabilityList = Doc.defineModel "ClientCapabilityList" $ do
  Doc.description "Hints provided by the client for the backend so it can behavior in a backwards-compatible way."
  Doc.property "capabilities" (Doc.array typeClientCapability) $ do
    Doc.description "Array containing all capabilities supported by a client."

--------------------------------------------------------------------------------
-- UserClientMap

newtype UserClientMap a = UserClientMap
  { userClientMap :: Map UserId (Map ClientId a)
  }
  deriving stock (Eq, Show, Functor, Foldable, Traversable)
  deriving newtype (Semigroup, Monoid)
  deriving (FromJSON, ToJSON, Swagger.ToSchema) via Schema (UserClientMap a)

-- FUTUREWORK: Remove when 'NewOtrMessage' has ToSchema
modelOtrClientMap :: Doc.Model
modelOtrClientMap = Doc.defineModel "OtrClientMap" $ do
  Doc.description "Map of client IDs to OTR content."
  Doc.property "" Doc.bytes' $
    Doc.description "Mapping from client IDs to OTR content (Base64 in JSON)."

instance ToSchema a => ToSchema (UserClientMap a) where
  schema = userClientMapSchema schema

userClientMapSchema ::
  ValueSchema NamedSwaggerDoc a ->
  ValueSchema NamedSwaggerDoc (UserClientMap a)
userClientMapSchema sch =
  named nm $
    UserClientMap <$> userClientMap .= map_ (map_ sch)
  where
    nm = "UserClientMap" <> maybe "" (" " <>) (getName (schemaDoc sch))

newtype UserClientPrekeyMap = UserClientPrekeyMap
  {getUserClientPrekeyMap :: UserClientMap (Maybe Prekey)}
  deriving stock (Eq, Show)
  deriving newtype (Arbitrary, Semigroup, Monoid)
  deriving (FromJSON, ToJSON, Swagger.ToSchema) via Schema UserClientPrekeyMap

mkUserClientPrekeyMap :: Map UserId (Map ClientId (Maybe Prekey)) -> UserClientPrekeyMap
mkUserClientPrekeyMap = coerce

instance ToSchema UserClientPrekeyMap where
  schema = UserClientPrekeyMap <$> getUserClientPrekeyMap .= addDoc sch
    where
      sch =
        named "UserClientPrekeyMap" . unnamed $
          userClientMapSchema (optWithDefault A.Null schema)
      addDoc =
        Swagger.schema . Swagger.example
          ?~ toJSON
            ( Map.singleton
                (generateExample @UserId)
                ( Map.singleton
                    (newClientId 4940483633899001999)
                    (Just (Prekey (PrekeyId 1) "pQABAQECoQBYIOjl7hw0D8YRNq..."))
                )
            )

instance Arbitrary a => Arbitrary (UserClientMap a) where
  arbitrary = UserClientMap <$> mapOf' arbitrary (mapOf' arbitrary arbitrary)

newtype QualifiedUserClientMap a = QualifiedUserClientMap
  { qualifiedUserClientMap :: Map Domain (UserClientMap a)
  }
  deriving stock (Eq, Show)
  deriving newtype (Semigroup, Monoid)
  deriving (FromJSON, ToJSON, Swagger.ToSchema) via Schema (QualifiedUserClientMap a)

instance Arbitrary a => Arbitrary (QualifiedUserClientMap a) where
  arbitrary = QualifiedUserClientMap <$> mapOf' arbitrary arbitrary

instance ToSchema a => ToSchema (QualifiedUserClientMap a) where
  schema = qualifiedUserClientMapSchema schema

qualifiedUserClientMapSchema ::
  ValueSchema NamedSwaggerDoc a ->
  ValueSchema NamedSwaggerDoc (QualifiedUserClientMap a)
qualifiedUserClientMapSchema sch =
  addDoc . named nm $
    QualifiedUserClientMap <$> qualifiedUserClientMap .= map_ innerSchema
  where
    innerSchema = userClientMapSchema sch
    nm = "QualifiedUserClientMap" <> maybe "" (" " <>) (getName (schemaDoc sch))
    addDoc =
      Swagger.schema . Swagger.example
        ?~ toJSON
          ( Map.singleton
              ("domain1.example.com" :: Text)
              (schemaDoc innerSchema ^. Swagger.schema . Swagger.example)
          )

newtype QualifiedUserClientPrekeyMap = QualifiedUserClientPrekeyMap
  {getQualifiedUserClientPrekeyMap :: QualifiedUserClientMap (Maybe Prekey)}
  deriving stock (Eq, Show)
  deriving newtype (Arbitrary, Semigroup, Monoid)
  deriving (FromJSON, ToJSON, Swagger.ToSchema) via Schema QualifiedUserClientPrekeyMap

instance ToSchema QualifiedUserClientPrekeyMap where
  schema =
    named "QualifiedUserClientPrekeyMap" $
      mkQualifiedUserClientPrekeyMap <$> unmk .= map_ schema
    where
      unmk :: QualifiedUserClientPrekeyMap -> Map Domain UserClientPrekeyMap
      unmk = coerce

mkQualifiedUserClientPrekeyMap :: Map Domain UserClientPrekeyMap -> QualifiedUserClientPrekeyMap
mkQualifiedUserClientPrekeyMap = coerce

--------------------------------------------------------------------------------
-- UserClients

newtype UserClientsFull = UserClientsFull
  { userClientsFull :: Map UserId (Set Client)
  }
  deriving stock (Eq, Show, Generic)
  deriving newtype (Semigroup, Monoid)
  deriving (Arbitrary) via (GenericUniform UserClientsFull)

userClientsFullToUserClients :: UserClientsFull -> UserClients
userClientsFullToUserClients (UserClientsFull mp) = UserClients $ Set.map clientId <$> mp

-- TODO: check if example generated by swagger look okay (probably not)
newtype UserClients = UserClients
  { userClients :: Map UserId (Set ClientId)
  }
  deriving stock (Eq, Show, Generic)
  deriving newtype (Semigroup, Monoid)

instance Swagger.ToSchema UserClients where
  declareNamedSchema _ = do
    mapSch <- Swagger.declareSchema (Proxy @(Map UserId (Set ClientId)))
    return $
      Swagger.NamedSchema (Just "UserClients") $
        mapSch
          & Swagger.description ?~ "Map of user id to list of client ids."
          & Swagger.example
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
    A.withObject "UserClients" (fmap UserClients . foldrM fn Map.empty . HashMap.toList)
    where
      fn (k, v) m = Map.insert <$> parseJSON (A.String k) <*> parseJSON v <*> pure m

instance Arbitrary UserClients where
  arbitrary = UserClients <$> mapOf' arbitrary (setOf' arbitrary)

filterClients :: (Set ClientId -> Bool) -> UserClients -> UserClients
filterClients p (UserClients c) = UserClients $ Map.filter p c

filterClientsFull :: (Set Client -> Bool) -> UserClientsFull -> UserClientsFull
filterClientsFull p (UserClientsFull c) = UserClientsFull $ Map.filter p c

newtype QualifiedUserClients = QualifiedUserClients
  { qualifiedUserClients :: Map Domain UserClients
  }
  deriving stock (Eq, Show, Generic)
  deriving newtype (Semigroup, Monoid, FromJSON, ToJSON)

instance Arbitrary QualifiedUserClients where
  arbitrary = QualifiedUserClients <$> mapOf' arbitrary arbitrary

instance Swagger.ToSchema QualifiedUserClients where
  declareNamedSchema _ = do
    sch <- Swagger.declareSchema (Proxy @(Map Domain UserClients))
    userClientsSchema <- Swagger.declareSchema (Proxy @UserClients)
    return $
      Swagger.NamedSchema (Just "QualifiedUserClients") $
        sch
          & Swagger.description ?~ "Map of Domain to UserClients"
          & Swagger.example
            ?~ toJSON
              (Map.singleton ("domain1.example.com" :: Text) (userClientsSchema ^. Swagger.example))

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
  deriving (Swagger.ToSchema) via (CustomSwagger '[FieldLabelModifier (StripPrefix "client", LowerCase)] Client)

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
    A.object $
      "id" A..= clientId c
        # "type" A..= clientType c
        # "label" A..= clientLabel c
        # "class" A..= clientClass c
        # "time" A..= clientTime c
        # "cookie" A..= clientCookie c
        # "location" A..= clientLocation c
        # "model" A..= clientModel c
        # []

instance FromJSON Client where
  parseJSON = A.withObject "Client" $ \o ->
    Client
      <$> o A..: "id"
      <*> o A..: "type"
      <*> o A..: "time"
      <*> o A..:? "class"
      <*> o A..:? "label"
      <*> o A..:? "cookie"
      <*> o A..:? "location"
      <*> o A..:? "model"

--------------------------------------------------------------------------------
-- PubClient

data PubClient = PubClient
  { pubClientId :: ClientId,
    pubClientClass :: Maybe ClientClass
  }
  deriving stock (Eq, Show, Generic, Ord)
  deriving (Arbitrary) via (GenericUniform PubClient)
  deriving (Swagger.ToSchema) via (CustomSwagger '[FieldLabelModifier (StripPrefix "pubClient", LowerCase)] PubClient)

instance ToJSON PubClient where
  toJSON c =
    A.object $
      "id" A..= pubClientId c
        # "class" A..= pubClientClass c
        # []

instance FromJSON PubClient where
  parseJSON = A.withObject "PubClient" $ \o ->
    PubClient
      <$> o A..: "id"
      <*> o A..:? "class"

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
  deriving (Swagger.ToSchema) via EnumToSchemaStrategy "ClientType" ClientType

typeClientType :: Doc.DataType
typeClientType =
  Doc.string $
    Doc.enum
      [ "permanent",
        "temporary",
        "legalhold"
      ]

instance ToJSON ClientType where
  toJSON TemporaryClientType = A.String "temporary"
  toJSON PermanentClientType = A.String "permanent"
  toJSON LegalHoldClientType = A.String "legalhold"

instance FromJSON ClientType where
  parseJSON = A.withText "ClientType" $ \txt -> case txt of
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
  deriving (Swagger.ToSchema) via EnumToSchemaStrategy "Client" ClientClass

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
  toJSON PhoneClient = A.String "phone"
  toJSON TabletClient = A.String "tablet"
  toJSON DesktopClient = A.String "desktop"
  toJSON LegalHoldClient = A.String "legalhold"

instance FromJSON ClientClass where
  parseJSON = A.withText "ClientClass" $ \txt -> case txt of
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
    A.object $
      "type" A..= newClientType c
        # "prekeys" A..= newClientPrekeys c
        # "lastkey" A..= newClientLastKey c
        # "label" A..= newClientLabel c
        # "class" A..= newClientClass c
        # "cookie" A..= newClientCookie c
        # "password" A..= newClientPassword c
        # "model" A..= newClientModel c
        # []

instance FromJSON NewClient where
  parseJSON = A.withObject "NewClient" $ \o ->
    NewClient
      <$> o A..: "prekeys"
      <*> o A..: "lastkey"
      <*> o A..: "type"
      <*> o A..:? "label"
      <*> o A..:? "class"
      <*> o A..:? "cookie"
      <*> o A..:? "password"
      <*> o A..:? "model"

--------------------------------------------------------------------------------
-- UpdateClient

data UpdateClient = UpdateClient
  { updateClientPrekeys :: [Prekey],
    updateClientLastKey :: Maybe LastPrekey,
    updateClientLabel :: Maybe Text,
    -- | see haddocks for 'ClientCapability'
    updateClientCapabilities :: Maybe (Set ClientCapability)
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
  Doc.property "capabilities" typeClientCapability $ do
    Doc.description "Hints for the backend so it can behave in a backwards-compatible way."
    Doc.optional

instance ToJSON UpdateClient where
  toJSON c =
    A.object $
      "prekeys" A..= updateClientPrekeys c
        # "lastkey" A..= updateClientLastKey c
        # "label" A..= updateClientLabel c
        # "capabilities" A..= updateClientCapabilities c
        # []

instance FromJSON UpdateClient where
  parseJSON = A.withObject "RefreshClient" $ \o ->
    UpdateClient
      <$> o A..:? "prekeys" A..!= []
      <*> o A..:? "lastkey"
      <*> o A..:? "label"
      <*> o A..:? "capabilities"

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
  toJSON (RmClient pw) = A.object ["password" A..= pw]

instance FromJSON RmClient where
  parseJSON = A.withObject "RmClient" $ \o ->
    RmClient <$> o A..:? "password"

--------------------------------------------------------------------------------
-- other models

modelSigkeys :: Doc.Model
modelSigkeys = Doc.defineModel "SignalingKeys" $ do
  Doc.description "Signaling keys for encryption and signing of native push notifications (APNS, GCM)."
  Doc.property "enckey" Doc.bytes' $
    Doc.description "The base64-encoded, 256 bit encryption key."
  Doc.property "mackey" Doc.bytes' $
    Doc.description "The base64-encoded, 256 bit MAC key."
