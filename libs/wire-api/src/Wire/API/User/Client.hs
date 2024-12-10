{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData #-}

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

module Wire.API.User.Client
  ( -- * ClientCapability
    ClientCapability (..),
    ClientCapabilityList (..),

    -- * ClientInfo
    ClientInfo (..),

    -- * UserClients
    UserClientMap (..),
    UserClientPrekeyMap (..),
    mkUserClientPrekeyMap,
    QualifiedUserClientMap (..),
    QualifiedUserClientPrekeyMap (..),
    QualifiedUserClientPrekeyMapV4 (..),
    mkQualifiedUserClientPrekeyMap,
    qualifiedUserClientPrekeyMapFromList,
    UserClientsFull (..),
    UserClients (..),
    mkUserClients,
    QualifiedUserClients (..),
    qualifiedUserClientsValueSchema,
    filterClients,
    filterClientsFull,

    -- * Client
    Client (..),
    clientSchema,
    PubClient (..),
    ClientType (..),
    ClientClass (..),
    MLSPublicKeys,

    -- * New/Update/Remove Client
    NewClient (..),
    newClient,
    UpdateClient (..),
    defUpdateClient,
    RmClient (..),

    -- * re-exports
    Latitude (..),
    Longitude (..),

    -- * List of MLS client ids
    ClientList (..),
  )
where

import Cassandra qualified as C
import Control.Applicative
import Control.Lens hiding (element, enum, set, (#), (.=))
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Aeson qualified as A
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Bifunctor (second)
import Data.Code qualified as Code
import Data.Coerce
import Data.Domain (Domain)
import Data.Id
import Data.Json.Util
import Data.Map.Strict qualified as Map
import Data.Misc (Latitude (..), Longitude (..), PlainTextPassword6)
import Data.OpenApi hiding (Schema, ToSchema, nullable, schema)
import Data.OpenApi qualified as Swagger hiding (nullable)
import Data.Qualified
import Data.SOP hiding (fn)
import Data.Schema
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Text.Encoding qualified as T
import Data.Time.Clock
import Data.UUID (toASCIIBytes)
import Deriving.Swagger
  ( CustomSwagger,
    FieldLabelModifier,
    LowerCase,
    StripPrefix,
  )
import Imports
import Wire.API.MLS.CipherSuite
import Wire.API.Routes.MultiVerb
import Wire.API.Routes.Version
import Wire.API.Routes.Versioned
import Wire.API.User.Auth
import Wire.API.User.Client.Prekey as Prekey
import Wire.Arbitrary (Arbitrary (arbitrary), GenericUniform (..), generateExample, mapOf', setOf')

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
  | ClientSupportsConsumableNotifications
  deriving stock (Eq, Ord, Bounded, Enum, Show, Generic)
  deriving (Arbitrary) via (GenericUniform ClientCapability)
  deriving (ToJSON, FromJSON, Swagger.ToSchema) via Schema ClientCapability

instance ToSchema ClientCapability where
  schema =
    enum @Text "ClientCapability" $
      element "legalhold-implicit-consent" ClientSupportsLegalholdImplicitConsent
        <> element "consumable-notifications" ClientSupportsConsumableNotifications

instance C.Cql ClientCapability where
  ctype = C.Tagged C.IntColumn

  toCql ClientSupportsLegalholdImplicitConsent = C.CqlInt 1
  toCql ClientSupportsConsumableNotifications = C.CqlInt 2

  fromCql (C.CqlInt i) = case i of
    1 -> pure ClientSupportsLegalholdImplicitConsent
    2 -> pure ClientSupportsConsumableNotifications
    n -> Left $ "Unexpected ClientCapability value: " ++ show n
  fromCql _ = Left "ClientCapability value: int expected"

-- FUTUREWORK: add golden tests for this?
newtype ClientCapabilityList = ClientCapabilityList {fromClientCapabilityList :: Set ClientCapability}
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (Semigroup, Monoid)
  deriving (Arbitrary) via (GenericUniform ClientCapabilityList)
  deriving (ToJSON, FromJSON, Swagger.ToSchema) via (Schema ClientCapabilityList)

instance ToSchema ClientCapabilityList where
  schema = capabilitiesSchema Nothing

instance ToSchema (Versioned V6 ClientCapabilityList) where
  schema =
    object "ClientCapabilityListV6Wrapper" $
      Versioned
        <$> unVersioned .= field "capabilities" (capabilitiesSchema (Just V6))

instance ToSchema (Versioned V7 ClientCapabilityList) where
  schema =
    Versioned
      <$> unVersioned .= capabilitiesSchema (Just V7)

capabilitiesSchema ::
  Maybe Version ->
  ValueSchema NamedSwaggerDoc ClientCapabilityList
capabilitiesSchema mVersion =
  named objName $
    ClientCapabilityList
      <$> (Set.toList . dropIncompatibleCapabilities . fromClientCapabilityList) .= (Set.fromList <$> array schema)
  where
    objName =
      case mVersion of
        Nothing -> "ClientCapabilityList"
        Just v -> "ClientCapabilityList" <> Text.pack (show v)
    dropIncompatibleCapabilities :: Set ClientCapability -> Set ClientCapability
    dropIncompatibleCapabilities caps =
      case mVersion of
        Just v | v <= V7 -> Set.delete ClientSupportsConsumableNotifications caps
        _ -> caps

--------------------------------------------------------------------------------
-- UserClientMap

newtype UserClientMap a = UserClientMap
  { userClientMap :: Map UserId (Map ClientId a)
  }
  deriving stock (Eq, Show, Functor, Foldable, Traversable)
  deriving newtype (Semigroup, Monoid)
  deriving (FromJSON, ToJSON, Swagger.ToSchema) via Schema (UserClientMap a)

instance (ToSchema a) => ToSchema (UserClientMap a) where
  schema = userClientMapSchema schema

class WrapName doc where
  wrapName :: doc -> (Text -> Text) -> SwaggerDoc -> doc

instance WrapName SwaggerDoc where
  wrapName _ _ = id

instance WrapName NamedSwaggerDoc where
  wrapName d f = fmap (Swagger.NamedSchema (Just (f (maybe "" ("_" <>) (getName d)))))

userClientMapSchema ::
  (WrapName doc, HasSchemaRef doc) =>
  ValueSchema doc a ->
  ValueSchema doc (UserClientMap a)
userClientMapSchema sch =
  over doc (wrapName (schemaDoc sch) ("UserClientMap" <>)) $
    UserClientMap <$> userClientMap .= map_ (map_ sch)

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
        named "UserClientPrekeyMap" $
          userClientMapSchema (nullable (unnamed schema))
      addDoc =
        Swagger.schema . Swagger.example
          ?~ toJSON
            ( Map.singleton
                (generateExample @UserId)
                ( Map.singleton
                    (ClientId 4940483633899001999)
                    (Just (Prekey (PrekeyId 1) "pQABAQECoQBYIOjl7hw0D8YRNq..."))
                )
            )

instance (Arbitrary a) => Arbitrary (UserClientMap a) where
  arbitrary = UserClientMap <$> mapOf' arbitrary (mapOf' arbitrary arbitrary)

newtype QualifiedUserClientMap a = QualifiedUserClientMap
  { qualifiedUserClientMap :: Map Domain (Map UserId (Map ClientId a))
  }
  deriving stock (Eq, Show, Functor)
  deriving (FromJSON, ToJSON, Swagger.ToSchema) via Schema (QualifiedUserClientMap a)

instance (Semigroup a) => Semigroup (QualifiedUserClientMap a) where
  (QualifiedUserClientMap m1) <> (QualifiedUserClientMap m2) =
    QualifiedUserClientMap $ Map.unionWith (Map.unionWith (Map.unionWith (<>))) m1 m2

instance (Semigroup (QualifiedUserClientMap a)) => Monoid (QualifiedUserClientMap a) where
  mempty = QualifiedUserClientMap mempty

instance (Arbitrary a) => Arbitrary (QualifiedUserClientMap a) where
  arbitrary = QualifiedUserClientMap <$> mapOf' arbitrary (mapOf' arbitrary (mapOf' arbitrary arbitrary))

instance (ToSchema a) => ToSchema (QualifiedUserClientMap a) where
  schema = qualifiedUserClientMapSchema schema

qualifiedUserClientMapSchema ::
  ValueSchema NamedSwaggerDoc a ->
  ValueSchema NamedSwaggerDoc (QualifiedUserClientMap a)
qualifiedUserClientMapSchema sch =
  addDoc . named nm $
    QualifiedUserClientMap <$> qualifiedUserClientMap .= map_ (map_ (map_ sch))
  where
    innerSchema = userClientMapSchema sch
    nm = "QualifiedUserClientMap" <> maybe "" ("_" <>) (getName (schemaDoc sch))
    addDoc =
      Swagger.schema . Swagger.example
        ?~ toJSON
          ( Map.singleton
              ("domain1.example.com" :: Text)
              (schemaDoc innerSchema ^. Swagger.schema . Swagger.example)
          )

data QualifiedUserClientPrekeyMapV4 = QualifiedUserClientPrekeyMapV4
  { qualifiedUserClientPrekeys :: QualifiedUserClientMap (Maybe Prekey),
    failedToList :: Maybe [Qualified UserId]
  }
  deriving stock (Eq, Show)
  deriving (FromJSON, ToJSON, Swagger.ToSchema) via Schema QualifiedUserClientPrekeyMapV4

instance Arbitrary QualifiedUserClientPrekeyMapV4 where
  arbitrary =
    QualifiedUserClientPrekeyMapV4
      <$> arbitrary
      <*> arbitrary

instance ToSchema QualifiedUserClientPrekeyMapV4 where
  schema =
    object "QualifiedUserClientPrekeyMapV4" $
      QualifiedUserClientPrekeyMapV4
        <$> fmap to' (from' .= field "qualified_user_client_prekeys" (map_ schema))
        <*> failedToList .= maybe_ (optField "failed_to_list" (array schema))
    where
      from' :: QualifiedUserClientPrekeyMapV4 -> Map Domain UserClientPrekeyMap
      from' = coerce . qualifiedUserClientPrekeys
      to' :: Map Domain UserClientPrekeyMap -> QualifiedUserClientMap (Maybe Prekey)
      to' = coerce

newtype QualifiedUserClientPrekeyMap = QualifiedUserClientPrekeyMap
  { getQualifiedUserClientPrekeyMap :: QualifiedUserClientMap (Maybe Prekey)
  }
  deriving stock (Eq, Show)
  deriving newtype (Arbitrary)
  deriving (FromJSON, ToJSON, Swagger.ToSchema) via Schema QualifiedUserClientPrekeyMap
  deriving (Semigroup, Monoid) via (QualifiedUserClientMap (Alt Maybe Prekey))

instance ToSchema QualifiedUserClientPrekeyMap where
  schema =
    named "QualifiedUserClientPrekeyMap" $
      mkQualifiedUserClientPrekeyMap <$> unmk .= map_ schema
    where
      unmk :: QualifiedUserClientPrekeyMap -> Map Domain UserClientPrekeyMap
      unmk = coerce

mkQualifiedUserClientPrekeyMap :: Map Domain UserClientPrekeyMap -> QualifiedUserClientPrekeyMap
mkQualifiedUserClientPrekeyMap = coerce

qualifiedUserClientPrekeyMapFromList ::
  [Qualified UserClientPrekeyMap] ->
  QualifiedUserClientPrekeyMap
qualifiedUserClientPrekeyMapFromList =
  mkQualifiedUserClientPrekeyMap . Map.fromList . map qToPair

--------------------------------------------------------------------------------
-- ClientInfo

-- | A client, together with extra information about it.
data ClientInfo = ClientInfo
  { -- | The ID of this client.
    ciId :: ClientId,
    -- | Whether this client is MLS-capable.
    ciMLS :: Bool
  }
  deriving stock (Eq, Ord, Show)
  deriving (Swagger.ToSchema, FromJSON, ToJSON) via Schema ClientInfo

instance ToSchema ClientInfo where
  schema =
    object "ClientInfo" $
      ClientInfo
        <$> ciId .= field "id" schema
        <*> ciMLS .= field "mls" schema

--------------------------------------------------------------------------------
-- UserClients

newtype UserClientsFull = UserClientsFull
  { userClientsFull :: Map UserId (Set Client)
  }
  deriving stock (Eq, Show, Generic)
  deriving newtype (Semigroup, Monoid)

-- | Json rendering of `UserClientsFull` is dynamic in the object fields, so it's unclear how
-- machine-generated swagger would look like.  We just leave the manual aeson instances in
-- place and write something in English into the docs here.
instance Swagger.ToSchema UserClientsFull where
  declareNamedSchema _ = do
    pure $
      NamedSchema (Just "UserClientsFull") $
        mempty
          & type_ ?~ OpenApiObject
          & description ?~ "Dictionary object of `Client` objects indexed by `UserId`."
          & example ?~ "{\"1355c55a-0ac8-11ee-97ee-db1a6351f093\": <Client object>, ...}"

instance ToJSON UserClientsFull where
  toJSON =
    toJSON . Map.foldrWithKey' fn Map.empty . userClientsFull
    where
      fn u c m =
        let k = T.decodeLatin1 (toASCIIBytes (toUUID u))
         in Map.insert k c m

instance FromJSON UserClientsFull where
  parseJSON =
    A.withObject "UserClientsFull" (fmap UserClientsFull . foldrM fn Map.empty . KeyMap.toList)
    where
      fn (k, v) m = Map.insert <$> parseJSON (A.String $ Key.toText k) <*> parseJSON v <*> pure m

instance Arbitrary UserClientsFull where
  arbitrary = UserClientsFull <$> mapOf' arbitrary (setOf' arbitrary)

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
                  [ (generateExample @UserId, [ClientId 1684636986166846496, ClientId 4940483633899001999]),
                    (generateExample @UserId, [ClientId 6987438498444556166, ClientId 7940473633839002939])
                  ]
              )

instance Arbitrary UserClients where
  arbitrary = UserClients <$> mapOf' arbitrary (setOf' arbitrary)

filterClients :: (Set ClientId -> Bool) -> UserClients -> UserClients
filterClients p (UserClients c) = UserClients $ Map.filter p c

filterClientsFull :: (Set Client -> Bool) -> UserClientsFull -> UserClientsFull
filterClientsFull p (UserClientsFull c) = UserClientsFull $ Map.filter p c

newtype QualifiedUserClients = QualifiedUserClients
  { qualifiedUserClients :: Map Domain (Map UserId (Set ClientId))
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON, Swagger.ToSchema) via (Schema QualifiedUserClients)

instance Semigroup QualifiedUserClients where
  (QualifiedUserClients m1) <> (QualifiedUserClients m2) =
    QualifiedUserClients $ Map.unionWith (Map.unionWith (<>)) m1 m2

instance Monoid QualifiedUserClients where
  mempty = QualifiedUserClients mempty

instance Arbitrary QualifiedUserClients where
  arbitrary = QualifiedUserClients <$> mapOf' arbitrary (mapOf' arbitrary (setOf' arbitrary))

qualifiedUserClientsValueSchema :: ValueSchema SwaggerDoc QualifiedUserClients
qualifiedUserClientsValueSchema =
  QualifiedUserClients <$> qualifiedUserClients .= map_ (map_ (set schema))

instance ToSchema QualifiedUserClients where
  schema =
    addDoc . named "QualifiedUserClients" $ qualifiedUserClientsValueSchema
    where
      addDoc sch =
        sch
          & Swagger.schema . Swagger.description ?~ "Map of Domain to UserClients"
          & Swagger.schema . Swagger.example
            ?~ toJSON
              ( Map.singleton
                  ("domain1.example.com" :: Text)
                  (view (Swagger.schema . Swagger.example) (schema @UserClients))
              )

--------------------------------------------------------------------------------
-- Client

data Client = Client
  { clientId :: ClientId,
    clientType :: ClientType,
    clientTime :: UTCTimeMillis,
    clientClass :: Maybe ClientClass,
    clientLabel :: Maybe Text,
    clientCookie :: Maybe CookieLabel,
    clientModel :: Maybe Text,
    clientCapabilities :: ClientCapabilityList,
    clientMLSPublicKeys :: MLSPublicKeys,
    clientLastActive :: Maybe UTCTime
  }
  deriving stock (Eq, Show, Generic, Ord)
  deriving (Arbitrary) via (GenericUniform Client)
  deriving (FromJSON, ToJSON, Swagger.ToSchema) via Schema Client

type MLSPublicKeys = Map SignatureSchemeTag ByteString

mlsPublicKeysSchema :: ValueSchema NamedSwaggerDoc MLSPublicKeys
mlsPublicKeysSchema =
  mapSchema
    & doc
      %~ ( (description ?~ "Mapping from signature scheme (tags) to public key data")
             . (example ?~ toJSON (Map.fromList $ map (,exampleValue) keys))
         )
    & named "MLSPublicKeys"
  where
    keys :: [SignatureSchemeTag]
    keys = [minBound .. maxBound]

    exampleValue :: A.Value
    exampleValue = fromMaybe (toJSON ("base64==" :: Text)) (base64Schema ^. doc . example)

    mapSchema :: ValueSchema SwaggerDoc MLSPublicKeys
    mapSchema = map_ base64Schema

clientSchema :: Maybe Version -> ValueSchema NamedSwaggerDoc Client
clientSchema mVersion =
  object "Client" $
    Client
      <$> clientId .= field "id" schema
      <*> clientType .= field "type" schema
      <*> clientTime .= field "time" schema
      <*> clientClass .= maybe_ (optField "class" schema)
      <*> clientLabel .= maybe_ (optField "label" schema)
      <*> clientCookie .= maybe_ (optField "cookie" schema)
      <*> clientModel .= maybe_ (optField "model" schema)
      <*> clientCapabilities .= (fromMaybe mempty <$> optField "capabilities" caps)
      <*> clientMLSPublicKeys .= mlsPublicKeysFieldSchema
      <*> clientLastActive .= maybe_ (optField "last_active" utcTimeSchema)
  where
    caps :: ValueSchema NamedSwaggerDoc ClientCapabilityList
    caps = case mVersion of
      -- broken capability serialisation for backwards compatibility
      Just v
        | v <= V6 ->
            dimap Versioned unVersioned $ schema @(Versioned V6 ClientCapabilityList)
        | v == V7 ->
            dimap Versioned unVersioned $ schema @(Versioned V7 ClientCapabilityList)
      _ -> schema @ClientCapabilityList

instance ToSchema Client where
  schema = clientSchema Nothing

instance ToSchema (Versioned 'V6 Client) where
  schema = Versioned <$> unVersioned .= clientSchema (Just V6)

instance ToSchema (Versioned 'V7 Client) where
  schema = Versioned <$> unVersioned .= clientSchema (Just V7)

instance {-# OVERLAPPING #-} ToSchema (Versioned 'V6 [Client]) where
  schema =
    Versioned
      <$> unVersioned
        .= named "ClientListV6" (array (clientSchema (Just V6)))

instance {-# OVERLAPPING #-} ToSchema (Versioned 'V7 [Client]) where
  schema =
    Versioned
      <$> unVersioned
        .= named "ClientListV7" (array (clientSchema (Just V7)))

mlsPublicKeysFieldSchema :: ObjectSchema SwaggerDoc MLSPublicKeys
mlsPublicKeysFieldSchema = fromMaybe mempty <$> optField "mls_public_keys" mlsPublicKeysSchema

instance AsHeaders '[ClientId] Client Client where
  toHeaders c = (I (clientId c) :* Nil, c)
  fromHeaders = snd

--------------------------------------------------------------------------------
-- ClientList

-- | Client list for internal API.
data ClientList = ClientList {clClients :: [ClientId]}
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform ClientList)
  deriving (FromJSON, ToJSON, Swagger.ToSchema) via Schema ClientList

instance ToSchema ClientList where
  schema =
    object "ClientList" $
      ClientList
        <$> clClients
          .= field "client_ids" (array schema)

--------------------------------------------------------------------------------
-- PubClient

data PubClient = PubClient
  { pubClientId :: ClientId,
    pubClientClass :: Maybe ClientClass
  }
  deriving stock (Eq, Show, Generic, Ord)
  deriving (Arbitrary) via (GenericUniform PubClient)
  deriving (Swagger.ToSchema) via (CustomSwagger '[FieldLabelModifier (StripPrefix "pubClient", LowerCase)] PubClient)
  deriving (FromJSON, ToJSON) via Schema PubClient

instance ToSchema PubClient where
  schema =
    object "PubClient" $
      PubClient
        <$> pubClientId .= field "id" schema
        <*> pubClientClass .= maybe_ (optField "class" schema)

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

instance C.Cql ClientType where
  ctype = C.Tagged C.IntColumn
  toCql TemporaryClientType = C.CqlInt 0
  toCql PermanentClientType = C.CqlInt 1
  toCql LegalHoldClientType = C.CqlInt 2

  fromCql (C.CqlInt 0) = pure TemporaryClientType
  fromCql (C.CqlInt 1) = pure PermanentClientType
  fromCql (C.CqlInt 2) = pure LegalHoldClientType
  fromCql _ = Left "ClientType: Int [0, 2] expected"

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

instance C.Cql ClientClass where
  ctype = C.Tagged C.IntColumn
  toCql PhoneClient = C.CqlInt 0
  toCql TabletClient = C.CqlInt 1
  toCql DesktopClient = C.CqlInt 2
  toCql LegalHoldClient = C.CqlInt 3

  fromCql (C.CqlInt 0) = pure PhoneClient
  fromCql (C.CqlInt 1) = pure TabletClient
  fromCql (C.CqlInt 2) = pure DesktopClient
  fromCql (C.CqlInt 3) = pure LegalHoldClient
  fromCql _ = Left "ClientClass: Int [0, 3] expected"

--------------------------------------------------------------------------------
-- NewClient

data NewClient = NewClient
  { newClientPrekeys :: [Prekey],
    newClientLastKey :: LastPrekey,
    newClientType :: ClientType,
    newClientLabel :: Maybe Text,
    newClientClass :: Maybe ClientClass,
    newClientCookie :: Maybe CookieLabel,
    newClientPassword :: Maybe PlainTextPassword6,
    newClientModel :: Maybe Text,
    newClientCapabilities :: Maybe ClientCapabilityList,
    newClientMLSPublicKeys :: MLSPublicKeys,
    newClientVerificationCode :: Maybe Code.Value
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform NewClient)
  deriving (FromJSON, ToJSON, Swagger.ToSchema) via Schema NewClient

newClientSchema :: Maybe Version -> ValueSchema NamedSwaggerDoc NewClient
newClientSchema mVersion =
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
      <*> newClientLabel .= maybe_ (optField "label" schema)
      <*> newClientClass
        .= maybe_
          ( optFieldWithDocModifier
              "class"
              ( description
                  ?~ "The device class this client belongs to. \
                     \Either 'phone', 'tablet', or 'desktop'."
              )
              schema
          )
      <*> newClientCookie
        .= maybe_
          ( optFieldWithDocModifier
              "cookie"
              (description ?~ "The cookie label, i.e. the label used when logging in.")
              schema
          )
      <*> newClientPassword
        .= maybe_
          ( optFieldWithDocModifier
              "password"
              ( description
                  ?~ "The password of the authenticated user for verification. \
                     \Note: Required for registration of the 2nd, 3rd, ... client."
              )
              schema
          )
      <*> newClientModel .= maybe_ (optField "model" schema)
      <*> newClientCapabilities
        .= maybe_
          ( optFieldWithDocModifier
              "capabilities"
              ( description
                  ?~ "Hints provided by the client for the backend so it can \
                     \behave in a backwards-compatible way."
              )
              (capabilitiesSchema mVersion)
          )
      <*> newClientMLSPublicKeys .= mlsPublicKeysFieldSchema
      <*> newClientVerificationCode .= maybe_ (optField "verification_code" schema)

instance ToSchema NewClient where
  schema = newClientSchema Nothing

instance ToSchema (Versioned 'V6 NewClient) where
  schema = Versioned <$> unVersioned .= newClientSchema (Just V6)

instance ToSchema (Versioned 'V7 NewClient) where
  schema = Versioned <$> unVersioned .= newClientSchema (Just V7)

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
      newClientCapabilities = Nothing,
      newClientMLSPublicKeys = mempty,
      newClientVerificationCode = Nothing
    }

--------------------------------------------------------------------------------
-- UpdateClient

data UpdateClient = UpdateClient
  { updateClientPrekeys :: [Prekey],
    updateClientLastKey :: Maybe LastPrekey,
    updateClientLabel :: Maybe Text,
    -- | see haddocks for 'ClientCapability'
    updateClientCapabilities :: Maybe ClientCapabilityList,
    updateClientMLSPublicKeys :: MLSPublicKeys
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform UpdateClient)
  deriving (FromJSON, ToJSON, Swagger.ToSchema) via Schema UpdateClient

defUpdateClient :: UpdateClient
defUpdateClient =
  UpdateClient
    { updateClientPrekeys = [],
      updateClientLastKey = Nothing,
      updateClientLabel = Nothing,
      updateClientCapabilities = Nothing,
      updateClientMLSPublicKeys = mempty
    }

updateClientSchema :: Maybe Version -> ValueSchema NamedSwaggerDoc UpdateClient
updateClientSchema mVersion =
  object "UpdateClient" $
    UpdateClient
      <$> updateClientPrekeys
        .= ( fromMaybe []
               <$> optFieldWithDocModifier
                 "prekeys"
                 (description ?~ "New prekeys for other clients to establish OTR sessions.")
                 (array schema)
           )
      <*> updateClientLastKey
        .= maybe_
          ( optFieldWithDocModifier
              "lastkey"
              (description ?~ "New last-resort prekey.")
              schema
          )
      <*> updateClientLabel
        .= maybe_
          ( optFieldWithDocModifier
              "label"
              (description ?~ "A new name for this client.")
              schema
          )
      <*> updateClientCapabilities
        .= maybe_
          ( optFieldWithDocModifier
              "capabilities"
              (description ?~ "Hints provided by the client for the backend so it can behave in a backwards-compatible way.")
              (capabilitiesSchema mVersion)
          )
      <*> updateClientMLSPublicKeys .= mlsPublicKeysFieldSchema

instance ToSchema UpdateClient where
  schema = updateClientSchema Nothing

instance ToSchema (Versioned 'V6 UpdateClient) where
  schema = Versioned <$> unVersioned .= updateClientSchema (Just V6)

instance ToSchema (Versioned 'V7 UpdateClient) where
  schema = Versioned <$> unVersioned .= updateClientSchema (Just V7)

--------------------------------------------------------------------------------
-- RmClient

newtype RmClient = RmClient
  { rmPassword :: Maybe PlainTextPassword6
  }
  deriving stock (Eq, Show, Generic)
  deriving newtype (Arbitrary)
  deriving (FromJSON, ToJSON, Swagger.ToSchema) via Schema RmClient

instance ToSchema RmClient where
  schema =
    object "DeleteClient" $
      RmClient
        <$> rmPassword
          .= optFieldWithDocModifier
            "password"
            ( description
                ?~ "The password of the authenticated user for verification. \
                   \The password is not required for deleting temporary clients."
            )
            (maybeWithDefault A.Null schema)
