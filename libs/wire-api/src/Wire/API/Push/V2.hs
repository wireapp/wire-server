{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Wire.API.Push.V2
  ( Push (..),
    newPush,
    pushRecipients,
    pushOrigin,
    pushConnections,
    pushOriginConnection,
    pushTransient,
    pushNativeIncludeOrigin,
    pushNativeEncrypt,
    pushNativeAps,
    pushNativePriority,
    pushPayload,
    pushIsCellsEvent,
    singletonPayload,
    Recipient (..),
    RecipientClients (..),
    recipient,
    recipientId,
    recipientRoute,
    recipientClients,
    Route (..),
    ApsData,
    ApsLocKey (..),
    ApsSound (..),
    apsData,
    apsLocKey,
    apsLocArgs,
    apsSound,
    apsBadge,

    -- * Priority (re-export)
    Priority (..),

    -- * PushToken (re-export)
    PushTokenList (..),
    PushToken,
    pushToken,
    tokenTransport,
    tokenApp,
    tokenClient,
    token,

    -- * PushToken fields (re-export)
    Token (..),
    Transport (..),
    AppName (..),
  )
where

import Control.Lens (makeLenses, (?~))
import Data.Aeson (FromJSON (..), Object, ToJSON (..))
import Data.Aeson qualified as A
import Data.Aeson.Types qualified as A
import Data.Id
import Data.Json.Util
import Data.List1
import Data.List1 qualified as List1
import Data.OpenApi qualified as S
import Data.Range
import Data.Schema
import Data.Set qualified as Set
import Imports
import Wire.API.Message (Priority (..))
import Wire.API.Push.V2.Token
import Wire.Arbitrary

-----------------------------------------------------------------------------
-- Route

data Route
  = -- | Sends notification on all channels including push notifications to
    -- mobile clients. Note that transient messages never cause a push
    -- notification.
    RouteAny
  | -- | Avoids causing push notification for mobile clients.
    RouteDirect
  deriving (Eq, Ord, Enum, Bounded, Show, Generic)
  deriving (Arbitrary) via GenericUniform Route
  deriving (FromJSON, ToJSON, S.ToSchema) via (Schema Route)

instance ToSchema Route where
  schema =
    enum @Text "Route" $
      mconcat
        [ element "any" RouteAny,
          element "direct" RouteDirect
        ]

-----------------------------------------------------------------------------
-- Recipient

-- FUTUREWORK: this is a duplicate of the type in "Wire.NotificationSubsystem" (even though
-- the latter lacks a few possibly deprecated fields). consolidate!
data Recipient = Recipient
  { _recipientId :: !UserId,
    _recipientRoute :: !Route,
    _recipientClients :: !RecipientClients
  }
  deriving (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON, S.ToSchema) via (Schema Recipient)

data RecipientClients
  = -- | All clients of some user
    RecipientClientsAll
  | -- | An explicit list of clients
    RecipientClientsSome (List1 ClientId)
  deriving (Eq, Show, Ord, Generic)
  deriving (Arbitrary) via GenericUniform RecipientClients
  deriving (FromJSON, ToJSON, S.ToSchema) via (Schema RecipientClients)

instance Semigroup RecipientClients where
  RecipientClientsAll <> _ = RecipientClientsAll
  _ <> RecipientClientsAll = RecipientClientsAll
  RecipientClientsSome cs1 <> RecipientClientsSome cs2 =
    RecipientClientsSome (cs1 <> cs2)

instance ToSchema Recipient where
  schema =
    object "Recipient" $
      Recipient
        <$> _recipientId .= field "user_id" schema
        <*> _recipientRoute .= field "route" schema
        <*> _recipientClients .= field "clients" schema

instance ToSchema RecipientClients where
  schema = mkSchema d i o
    where
      d :: NamedSwaggerDoc
      d =
        swaggerDoc @[ClientId]
          & (S.schema . S.type_ ?~ S.OpenApiArray)
          & (S.schema . S.description ?~ "List of clientIds. Empty means `all clients`.")

      i :: A.Value -> A.Parser RecipientClients
      i v =
        parseJSON @[ClientId] v >>= \case
          [] -> pure RecipientClientsAll
          c : cs -> pure (RecipientClientsSome (list1 c cs))

      o :: RecipientClients -> Maybe A.Value
      o =
        pure . toJSON . \case
          RecipientClientsSome cs -> toList cs
          RecipientClientsAll -> []

makeLenses ''Recipient

recipient :: UserId -> Route -> Recipient
recipient u r = Recipient u r RecipientClientsAll

-----------------------------------------------------------------------------
-- ApsData

newtype ApsSound = ApsSound {fromSound :: Text}
  deriving (Eq, Show, ToJSON, FromJSON, Arbitrary)

instance ToSchema ApsSound where
  schema =
    mkSchema d i o
    where
      d =
        swaggerDoc @Text
          & (S.schema . S.type_ ?~ S.OpenApiString)
          & (S.schema . S.description ?~ "ApsSound")

      i = A.withText "ApsSound" (pure . ApsSound)
      o = pure . A.String . fromSound

newtype ApsLocKey = ApsLocKey {fromLocKey :: Text}
  deriving (Eq, Show, ToJSON, FromJSON, Arbitrary)

instance ToSchema ApsLocKey where
  schema =
    mkSchema d i o
    where
      d =
        swaggerDoc @Text
          & (S.schema . S.type_ ?~ S.OpenApiString)
          & (S.schema . S.description ?~ "ApsLocKey")

      i = A.withText "ApsLocKey" (pure . ApsLocKey)
      o = pure . A.String . fromLocKey

data ApsData = ApsData
  { _apsLocKey :: !ApsLocKey,
    _apsLocArgs :: [Text],
    _apsSound :: !(Maybe ApsSound),
    _apsBadge :: !Bool
  }
  deriving (Eq, Show, Generic)
  deriving (Arbitrary) via GenericUniform ApsData
  deriving (FromJSON, ToJSON, S.ToSchema) via (Schema ApsData)

apsData :: ApsLocKey -> [Text] -> ApsData
apsData lk la = ApsData lk la Nothing True

instance ToSchema ApsData where
  schema =
    object "ApsData" $
      ApsData
        <$> _apsLocKey .= field "loc_key" schema
        <*> withDefault "loc_args" _apsLocArgs (array schema) []
        <*> _apsSound .= optField "sound" (maybeWithDefault A.Null schema) -- keep null for backwards compat
        <*> withDefault "badge" _apsBadge schema True
    where
      withDefault fn f s def = ((Just . f) .= maybe_ (optField fn s)) <&> fromMaybe def

makeLenses ''ApsData

-----------------------------------------------------------------------------
-- Push

-- FUTUREWORK: this is a duplicate of the type in "Wire.NotificationSubsystem" (even though
-- the latter lacks a few possibly deprecated fields). consolidate!
data Push = Push
  { -- | Recipients
    --
    -- REFACTOR: '_pushRecipients' should be @Set (Recipient, Maybe (NonEmptySet ConnId))@, and
    -- '_pushConnections' should go away.  Rationale: the current setup only works under the
    -- assumption that no 'ConnId' is used by two 'Recipient's.  This is *probably* correct, but
    -- not in any contract.  (Changing this may require a new version module, since we need to
    -- support both the old and the new data type simultaneously during upgrade.)
    _pushRecipients :: Range 1 1024 (Set Recipient),
    -- | Originating user
    --
    -- 'Nothing' here means that the originating user is on another backend.
    --
    -- REFACTOR: where is this required, and for what?  or can it be removed?  (see also: #531)
    _pushOrigin :: !(Maybe UserId),
    -- | Destination connections.  If empty, ignore.  Otherwise, filter the connections derived
    -- from '_pushRecipients' and only push to those contained in this set.
    --
    -- REFACTOR: change this to @_pushConnectionWhitelist :: Maybe (Set ConnId)@.
    _pushConnections :: !(Set ConnId),
    -- | Originating connection, if any.
    _pushOriginConnection :: !(Maybe ConnId),
    -- | Transient payloads are not forwarded to the notification stream.
    _pushTransient :: !Bool,
    -- | Whether to send native notifications to other clients
    -- of the originating user, if he is among the recipients.
    _pushNativeIncludeOrigin :: !Bool,
    -- | Should native push payloads be encrypted?
    --
    -- REFACTOR: this make no sense any more since native push notifications have no more payload.
    -- https://github.com/wireapp/wire-server/pull/546
    _pushNativeEncrypt :: !Bool,
    -- | APNs-specific metadata (needed eg. in "Brig.IO.Intra.toApsData").
    _pushNativeAps :: !(Maybe ApsData),
    -- | Native push priority.
    _pushNativePriority :: !Priority,
    -- | Opaque payload
    _pushPayload :: !(List1 Object),
    _pushIsCellsEvent :: !Bool
  }
  deriving (Eq, Show)
  deriving (FromJSON, ToJSON, S.ToSchema) via (Schema Push)

newPush :: Maybe UserId -> Range 1 1024 (Set Recipient) -> List1 Object -> Push
newPush from to pload =
  Push
    { _pushRecipients = to,
      _pushOrigin = from,
      _pushConnections = Set.empty,
      _pushOriginConnection = Nothing,
      _pushTransient = False,
      _pushNativeIncludeOrigin = True,
      _pushNativeEncrypt = True,
      _pushNativeAps = Nothing,
      _pushNativePriority = HighPriority,
      _pushPayload = pload,
      _pushIsCellsEvent = False
    }

singletonPayload :: (ToJSONObject a) => a -> List1 Object
singletonPayload = List1.singleton . toJSONObject

instance ToSchema Push where
  schema =
    object "Push" $
      Push
        <$> (fromRange . _pushRecipients) .= field "recipients" (rangedSchema (set schema))
        <*> _pushOrigin .= maybe_ (optField "origin" schema)
        <*> (ifNot Set.null . _pushConnections)
          .= maybe_ (fmap (fromMaybe mempty) (optField "connections" (set schema)))
        <*> _pushOriginConnection .= maybe_ (optField "origin_connection" schema)
        <*> (ifNot not . _pushTransient)
          .= maybe_
            (fmap (fromMaybe False) (optField "transient" schema))
        <*> (ifNot id . _pushNativeIncludeOrigin)
          .= maybe_ (fmap (fromMaybe True) (optField "native_include_origin" schema))
        <*> (ifNot id . _pushNativeEncrypt)
          .= maybe_ (fmap (fromMaybe True) (optField "native_encrypt" schema))
        <*> _pushNativeAps .= maybe_ (optField "native_aps" schema)
        <*> (ifNot (== HighPriority) . _pushNativePriority)
          .= maybe_ (fromMaybe HighPriority <$> optField "native_priority" schema)
        <*> _pushPayload .= field "payload" schema
        <*> _pushIsCellsEvent .= field "is_cells_event" schema
    where
      ifNot f a = if f a then Nothing else Just a

makeLenses ''Push
