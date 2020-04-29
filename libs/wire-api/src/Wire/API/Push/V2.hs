{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

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
    singletonRecipient,
    singletonPayload,
    Recipient (..),
    RecipientClients (..),
    recipient,
    recipientId,
    recipientRoute,
    recipientClients,
    Priority (..),
    Route (..),
    Transport (..),
    ApsData,
    ApsPreference (..),
    ApsLocKey (..),
    ApsSound (..),
    apsData,
    apsLocKey,
    apsLocArgs,
    apsSound,
    apsPreference,
    apsBadge,
    PushToken,
    Token (..),
    AppName (..),
    pushToken,
    tokenTransport,
    tokenApp,
    tokenClient,
    token,
    PushTokenList (..),
  )
where

import Control.Lens (makeLenses)
import Data.Aeson
import Data.Attoparsec.ByteString (takeByteString)
import Data.ByteString.Conversion
import Data.Id
import Data.Json.Util
import Data.List1
import qualified Data.List1 as List1
import Data.Range
import qualified Data.Range as Range
import qualified Data.Set as Set
import Imports

-----------------------------------------------------------------------------
-- Route

data Route
  = RouteAny
  | -- | 'RouteDirect' messages are different from transient messages: they do not
    -- trigger native pushes if the web socket is unavaiable, but they are stored in
    -- cassandra for later pickup.
    RouteDirect
  | -- | REFACTOR: this can probably be removed.
    RouteNative
  deriving (Eq, Ord, Enum, Bounded, Show)

instance FromJSON Route where
  parseJSON (String "any") = return RouteAny
  parseJSON (String "direct") = return RouteDirect
  parseJSON (String "native") = return RouteNative
  parseJSON x = fail $ "Invalid routing: " ++ show (encode x)

instance ToJSON Route where
  toJSON RouteAny = String "any"
  toJSON RouteDirect = String "direct"
  toJSON RouteNative = String "native"

-----------------------------------------------------------------------------
-- Recipient

data Recipient = Recipient
  { _recipientId :: !UserId,
    _recipientRoute :: !Route,
    _recipientClients :: !RecipientClients
  }
  deriving (Show)

instance Eq Recipient where
  (Recipient uid1 _ _) == (Recipient uid2 _ _) = uid1 == uid2

instance Ord Recipient where
  compare r r' = compare (_recipientId r) (_recipientId r')

data RecipientClients
  = -- | All clients of some user
    RecipientClientsAll
  | -- | An explicit list of clients
    RecipientClientsSome (List1 ClientId)
  deriving (Eq, Show)

makeLenses ''Recipient

recipient :: UserId -> Route -> Recipient
recipient u r = Recipient u r RecipientClientsAll

instance FromJSON Recipient where
  parseJSON = withObject "Recipient" $ \p ->
    Recipient <$> p .: "user_id"
      <*> p .: "route"
      <*> p .:? "clients" .!= RecipientClientsAll

instance ToJSON Recipient where
  toJSON (Recipient u r c) =
    object $
      "user_id" .= u
        # "route" .= r
        # "clients" .= c
        # []

-- "All clients" is encoded in the API as an empty list.
instance FromJSON RecipientClients where
  parseJSON x = parseJSON @[ClientId] x >>= \case
    [] -> pure RecipientClientsAll
    c : cs -> pure (RecipientClientsSome (list1 c cs))

instance ToJSON RecipientClients where
  toJSON = toJSON . \case
    RecipientClientsAll -> []
    RecipientClientsSome cs -> toList cs

-----------------------------------------------------------------------------
-- ApsData

newtype ApsSound = ApsSound {fromSound :: Text}
  deriving (Eq, Show, ToJSON, FromJSON)

newtype ApsLocKey = ApsLocKey {fromLocKey :: Text}
  deriving (Eq, Show, ToJSON, FromJSON)

data ApsPreference
  = ApsStdPreference
  | ApsVoIPPreference
  deriving (Eq, Show)

instance ToJSON ApsPreference where
  toJSON ApsVoIPPreference = "voip"
  toJSON ApsStdPreference = "std"

instance FromJSON ApsPreference where
  parseJSON = withText "ApsPreference" $ \case
    "voip" -> pure ApsVoIPPreference
    "std" -> pure ApsStdPreference
    x -> fail $ "Invalid preference: " ++ show x

data ApsData = ApsData
  { _apsLocKey :: !ApsLocKey,
    _apsLocArgs :: [Text],
    _apsSound :: !(Maybe ApsSound),
    _apsPreference :: !(Maybe ApsPreference),
    _apsBadge :: !Bool
  }
  deriving (Eq, Show)

makeLenses ''ApsData

apsData :: ApsLocKey -> [Text] -> ApsData
apsData lk la = ApsData lk la Nothing Nothing True

instance ToJSON ApsData where
  toJSON (ApsData k a s p b) =
    object $
      "loc_key" .= k
        # "loc_args" .= a
        # "sound" .= s
        # "preference" .= p
        # "badge" .= b
        # []

instance FromJSON ApsData where
  parseJSON = withObject "ApsData" $ \o ->
    ApsData <$> o .: "loc_key"
      <*> o .:? "loc_args" .!= []
      <*> o .:? "sound"
      <*> o .:? "preference"
      <*> o .:? "badge" .!= True

-----------------------------------------------------------------------------
-- Priority

-- | REFACTOR: do we ever use LowPriority?  to test, (a) remove the constructor and see what goes
-- wrong; (b) log use of 'LowPriority' by clients in production and watch it a few days.  if it is
-- not used anywhere, consider removing the entire type, or just the unused constructor.
--
-- @neongreen writes: [...] nobody seems to ever set `native_priority` in the client code. Exhibits
-- A1 and A2:
--
-- * <https://github.com/search?q=org%3Awireapp+native_priority&type=Code>
-- * <https://sourcegraph.com/search?q=native_priority+repo:^github\.com/wireapp/+#1>
--
-- see also: 'Wire.API.Push.Proto.Priority'.
data Priority = LowPriority | HighPriority
  deriving (Eq, Show, Ord, Enum)

instance ToJSON Priority where
  toJSON LowPriority = String "low"
  toJSON HighPriority = String "high"

instance FromJSON Priority where
  parseJSON = withText "Priority" $ \case
    "low" -> pure LowPriority
    "high" -> pure HighPriority
    x -> fail $ "Invalid push priority: " ++ show x

-----------------------------------------------------------------------------
-- Push

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
    -- REFACTOR: where is this required, and for what?  or can it be removed?  (see also: #531)
    _pushOrigin :: !UserId,
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
    -- | APNs-specific metadata.  REFACTOR: can this be removed?
    _pushNativeAps :: !(Maybe ApsData),
    -- | Native push priority.
    _pushNativePriority :: !Priority,
    -- | Opaque payload
    _pushPayload :: !(List1 Object)
  }
  deriving (Eq, Show)

makeLenses ''Push

newPush :: UserId -> Range 1 1024 (Set Recipient) -> List1 Object -> Push
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
      _pushPayload = pload
    }

singletonRecipient :: Recipient -> Range 1 1024 (Set Recipient)
singletonRecipient = Range.unsafeRange . Set.singleton

singletonPayload :: ToJSONObject a => a -> List1 Object
singletonPayload = List1.singleton . toJSONObject

instance FromJSON Push where
  parseJSON = withObject "Push" $ \p ->
    Push <$> p .: "recipients"
      <*> p .: "origin"
      <*> p .:? "connections" .!= Set.empty
      <*> p .:? "origin_connection"
      <*> p .:? "transient" .!= False
      <*> p .:? "native_include_origin" .!= True
      <*> p .:? "native_encrypt" .!= True
      <*> p .:? "native_aps"
      <*> p .:? "native_priority" .!= HighPriority
      <*> p .: "payload"

instance ToJSON Push where
  toJSON p =
    object $
      "recipients" .= _pushRecipients p
        # "origin" .= _pushOrigin p
        # "connections" .= ifNot Set.null (_pushConnections p)
        # "origin_connection" .= _pushOriginConnection p
        # "transient" .= ifNot (== False) (_pushTransient p)
        # "native_include_origin" .= ifNot (== True) (_pushNativeIncludeOrigin p)
        # "native_encrypt" .= ifNot (== True) (_pushNativeEncrypt p)
        # "native_aps" .= _pushNativeAps p
        # "native_priority" .= ifNot (== HighPriority) (_pushNativePriority p)
        # "payload" .= _pushPayload p
        # []
    where
      ifNot f a = if f a then Nothing else Just a

-----------------------------------------------------------------------------
-- Transport

data Transport
  = GCM
  | APNS
  | APNSSandbox
  | APNSVoIP
  | APNSVoIPSandbox
  deriving (Eq, Ord, Show, Bounded, Enum)

instance ToJSON Transport where
  toJSON GCM = "GCM"
  toJSON APNS = "APNS"
  toJSON APNSSandbox = "APNS_SANDBOX"
  toJSON APNSVoIP = "APNS_VOIP"
  toJSON APNSVoIPSandbox = "APNS_VOIP_SANDBOX"

instance FromJSON Transport where
  parseJSON = withText "transport" $ \case
    "GCM" -> return GCM
    "APNS" -> return APNS
    "APNS_SANDBOX" -> return APNSSandbox
    "APNS_VOIP" -> return APNSVoIP
    "APNS_VOIP_SANDBOX" -> return APNSVoIPSandbox
    x -> fail $ "Invalid push transport: " ++ show x

instance FromByteString Transport where
  parser = takeByteString >>= \case
    "GCM" -> return GCM
    "APNS" -> return APNS
    "APNS_SANDBOX" -> return APNSSandbox
    "APNS_VOIP" -> return APNSVoIP
    "APNS_VOIP_SANDBOX" -> return APNSVoIPSandbox
    x -> fail $ "Invalid push transport: " <> show x

-----------------------------------------------------------------------------
-- PushToken

newtype Token = Token
  { tokenText :: Text
  }
  deriving (Eq, Ord, Show, FromJSON, ToJSON, FromByteString, ToByteString)

newtype AppName = AppName
  { appNameText :: Text
  }
  deriving (Eq, Ord, Show, FromJSON, ToJSON, IsString)

data PushToken = PushToken
  { _tokenTransport :: !Transport,
    _tokenApp :: !AppName,
    _token :: !Token,
    _tokenClient :: !ClientId
  }
  deriving (Eq, Ord, Show)

makeLenses ''PushToken

pushToken :: Transport -> AppName -> Token -> ClientId -> PushToken
pushToken tp an tk cl = PushToken tp an tk cl

instance ToJSON PushToken where
  toJSON p =
    object $
      "transport" .= _tokenTransport p
        # "app" .= _tokenApp p
        # "token" .= _token p
        # "client" .= _tokenClient p
        # []

instance FromJSON PushToken where
  parseJSON = withObject "PushToken" $ \p ->
    PushToken <$> p .: "transport"
      <*> p .: "app"
      <*> p .: "token"
      <*> p .: "client"

newtype PushTokenList = PushTokenList
  { pushTokens :: [PushToken]
  }
  deriving (Eq, Show)

instance FromJSON PushTokenList where
  parseJSON = withObject "PushTokenList" $ \p ->
    PushTokenList <$> p .: "tokens"

instance ToJSON PushTokenList where
  toJSON (PushTokenList t) = object ["tokens" .= t]
