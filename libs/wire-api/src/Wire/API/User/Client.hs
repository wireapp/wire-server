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
    mkUserClients,
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
import Data.Bifunctor (second)
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
  ( CustomSwagger,
    FieldLabelModifier,
    LowerCase,
    StripPrefix,
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
    -- is granted via the galley server config and cassandra table `galley.legalhold_whitelisted`.
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
newtype ClientCapabilityList = ClientCapabilityList {fromClientCapabilityList :: Set ClientCapability}
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (Semigroup, Monoid)
  deriving (Arbitrary) via (GenericUniform ClientCapabilityList)
  deriving (ToJSON, FromJSON, Swagger.ToSchema) via (Schema ClientCapabilityList)

instance ToSchema ClientCapabilityList where
  schema =
    object "ClientCapabilityList" $
      ClientCapabilityList <$> fromClientCapabilityList .= capabilitiesFieldSchema

capabilitiesFieldSchema :: ObjectSchema SwaggerDoc (Set ClientCapability)
capabilitiesFieldSchema =
  Set.toList
    .= fieldWithDocModifier "capabilities" mods (Set.fromList <$> array schema)
  where
    mods =
      description
        ?~ "Hints provided by the client for the backend so it can \
           \behave in a backwards-compatible way."

modelClientCapabilityList :: Doc.Model
modelClientCapabilityList = Doc.defineModel "ClientCapabilityList" $ do
  Doc.description "Hints provided by the client for the backend so it can behave in a backwards-compatible way."
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

instance ToJSON UserClientsFull where
  toJSON =
    toJSON . Map.foldrWithKey' fn Map.empty . userClientsFull
    where
      fn u c m =
        let k = Text.E.decodeLatin1 (toASCIIBytes (toUUID u))
         in Map.insert k c m

instance FromJSON UserClientsFull where
  parseJSON =
    A.withObject "UserClientsFull" (fmap UserClientsFull . foldrM fn Map.empty . HashMap.toList)
    where
      fn (k, v) m = Map.insert <$> parseJSON (A.String k) <*> parseJSON v <*> pure m

instance Arbitrary UserClientsFull where
  arbitrary = UserClientsFull <$> mapOf' arbitrary (setOf' arbitrary)

userClientsFullToUserClients :: UserClientsFull -> UserClients
userClientsFullToUserClients (UserClientsFull mp) = UserClients $ Set.map clientId <$> mp

newtype UserClients = UserClients
  { userClients :: Map UserId (Set ClientId)
  }
  deriving stock (Eq, Show, Generic)
  deriving newtype (Semigroup, Monoid)
  deriving (ToJSON, FromJSON, Swagger.ToSchema) via Schema UserClients

mkUserClients :: [(UserId, [ClientId])] -> UserClients
mkUserClients xs = UserClients $ Map.fromList (xs <&> second Set.fromList)

instance ToSchema UserClients where
  schema =
    addDoc . named "UserClients" $ UserClients <$> userClients .= map_ (set schema)
    where
      addDoc sch =
        sch
          & Swagger.schema . Swagger.description ?~ "Map of user id to list of client ids."
          & Swagger.schema . Swagger.example
            ?~ toJSON
              ( Map.fromList
                  [ (generateExample @UserId, [newClientId 1684636986166846496, newClientId 4940483633899001999]),
                    (generateExample @UserId, [newClientId 6987438498444556166, newClientId 7940473633839002939])
                  ]
              )

-- FUTUREWORK: Remove when proto endpoint for sending messages is moved to
-- servant
modelUserClients :: Doc.Model
modelUserClients =
  Doc.defineModel "UserClients" $
    Doc.property "" (Doc.unique $ Doc.array Doc.bytes') $
      Doc.description "Map of user IDs to sets of client IDs ({ UserId: [ClientId] })."

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
    clientModel :: Maybe Text,
    clientCapabilities :: ClientCapabilityList
  }
  deriving stock (Eq, Show, Generic, Ord)
  deriving (Arbitrary) via (GenericUniform Client)
  deriving (FromJSON, ToJSON, Swagger.ToSchema) via Schema Client

instance ToSchema Client where
  schema =
    object "Client" $
      Client
        <$> clientId .= field "id" schema
        <*> clientType .= field "type" schema
        <*> clientTime .= field "time" schema
        <*> clientClass .= opt (field "class" schema)
        <*> clientLabel .= opt (field "label" schema)
        <*> clientCookie .= opt (field "cookie" schema)
        <*> clientLocation .= opt (field "location" schema)
        <*> clientModel .= opt (field "model" schema)
        <*> clientCapabilities .= (field "capabilities" schema <|> pure mempty)

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

data ClientType
  = TemporaryClientType
  | PermanentClientType
  | LegalHoldClientType -- see Note [LegalHold]
  deriving stock (Eq, Ord, Show, Generic)
  deriving (Arbitrary) via (GenericUniform ClientType)
  deriving (FromJSON, ToJSON, Swagger.ToSchema) via Schema ClientType

instance ToSchema ClientType where
  schema =
    enum @Text "ClientType" $
      element "temporary" TemporaryClientType
        <> element "permanent" PermanentClientType
        <> element "legalhold" LegalHoldClientType

typeClientType :: Doc.DataType
typeClientType =
  Doc.string $
    Doc.enum
      [ "permanent",
        "temporary",
        "legalhold"
      ]

data ClientClass
  = PhoneClient
  | TabletClient
  | DesktopClient
  | LegalHoldClient -- see Note [LegalHold]
  deriving stock (Eq, Ord, Show, Generic)
  deriving (Arbitrary) via (GenericUniform ClientClass)
  deriving (FromJSON, ToJSON, Swagger.ToSchema) via Schema ClientClass

instance ToSchema ClientClass where
  schema =
    enum @Text "ClientClass" $
      element "phone" PhoneClient
        <> element "tablet" TabletClient
        <> element "desktop" DesktopClient
        <> element "legalhold" LegalHoldClient

typeClientClass :: Doc.DataType
typeClientClass =
  Doc.string $
    Doc.enum
      [ "phone",
        "tablet",
        "desktop",
        "legalhold"
      ]

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
    newClientModel :: Maybe Text,
    newClientCapabilities :: Maybe (Set ClientCapability)
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform NewClient)
  deriving (FromJSON, ToJSON, Swagger.ToSchema) via Schema NewClient

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
  Doc.property "capabilities" typeClientCapability $ do
    Doc.description "Hints for the backend so it can behave in a backwards-compatible way."
    Doc.optional

instance ToSchema NewClient where
  schema =
    object "NewClient" $
      NewClient
        <$> newClientPrekeys
          .= fieldWithDocModifier
            "prekeys"
            (description ?~ "Prekeys for other clients to establish OTR sessions.")
            (array schema)
        <*> newClientLastKey
          .= fieldWithDocModifier
            "lastkey"
            ( description
                ?~ "The last resort prekey for other clients to establish OTR sessions. \
                   \This key must have the ID 0xFFFF and is never deleted."
            )
            schema
        <*> newClientType
          .= fieldWithDocModifier
            "type"
            ( description
                ?~ "The type of client to register. A user may have no more than \
                   \7 (seven) permanent clients and 1 (one) temporary client. When the \
                   \limit of permanent clients is reached, an error is returned. \
                   \When a temporary client already exists, it is replaced."
            )
            schema
        <*> newClientLabel .= opt (field "label" schema)
        <*> newClientClass
          .= opt
            ( fieldWithDocModifier
                "class"
                ( description
                    ?~ "The device class this client belongs to. \
                       \Either 'phone', 'tablet', or 'desktop'."
                )
                schema
            )
        <*> newClientCookie
          .= opt
            ( fieldWithDocModifier
                "cookie"
                (description ?~ "The cookie label, i.e. the label used when logging in.")
                schema
            )
        <*> newClientPassword
          .= opt
            ( fieldWithDocModifier
                "password"
                ( description
                    ?~ "The password of the authenticated user for verification. \
                       \Note: Required for registration of the 2nd, 3rd, ... client."
                )
                schema
            )
        <*> newClientModel .= opt (field "model" schema)
        <*> newClientCapabilities .= opt capabilitiesFieldSchema

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
      newClientModel = Nothing,
      newClientCapabilities = Nothing
    }

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
  deriving (FromJSON, ToJSON, Swagger.ToSchema) via Schema UpdateClient

instance ToSchema UpdateClient where
  schema =
    object "UpdateClient" $
      UpdateClient
        <$> (Just . updateClientPrekeys)
          .= ( fromMaybe []
                 <$> opt
                   ( fieldWithDocModifier
                       "prekeys"
                       (description ?~ "New prekeys for other clients to establish OTR sessions.")
                       (array schema)
                   )
             )
        <*> updateClientLastKey
          .= opt
            ( fieldWithDocModifier
                "lastkey"
                (description ?~ "New last-resort prekey.")
                schema
            )
        <*> updateClientLabel
          .= opt
            ( fieldWithDocModifier
                "label"
                (description ?~ "A new name for this client.")
                schema
            )
        <*> updateClientCapabilities .= opt capabilitiesFieldSchema

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

--------------------------------------------------------------------------------
-- RmClient

newtype RmClient = RmClient
  { rmPassword :: Maybe PlainTextPassword
  }
  deriving stock (Eq, Show, Generic)
  deriving newtype (Arbitrary)
  deriving (FromJSON, ToJSON, Swagger.ToSchema) via Schema RmClient

instance ToSchema RmClient where
  schema =
    object "DeleteClient" $
      RmClient
        <$> rmPassword
          .= fieldWithDocModifier
            "password"
            ( description
                ?~ "The password of the authenticated user for verification. \
                   \The password is not required for deleting temporary clients."
            )
            (optWithDefault A.Null schema)

modelDeleteClient :: Doc.Model
modelDeleteClient = Doc.defineModel "DeleteClient" $ do
  Doc.description "Required information for client deletion."
  Doc.property "password" Doc.string' $ do
    Doc.description
      "The password of the authenticated user for verification. \
      \The password is not required for deleting temporary clients."
    Doc.optional

--------------------------------------------------------------------------------
-- other models

modelSigkeys :: Doc.Model
modelSigkeys = Doc.defineModel "SignalingKeys" $ do
  Doc.description "Signaling keys for encryption and signing of native push notifications (APNS, GCM)."
  Doc.property "enckey" Doc.bytes' $
    Doc.description "The base64-encoded, 256 bit encryption key."
  Doc.property "mackey" Doc.bytes' $
    Doc.description "The base64-encoded, 256 bit MAC key."
