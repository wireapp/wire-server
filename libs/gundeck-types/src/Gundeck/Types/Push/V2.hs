{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

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

module Gundeck.Types.Push.V2
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
    Route (..),
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

import Control.Lens (makeLenses)
import Data.Aeson
import Data.Id
import Data.Json.Util
import Data.List1
import Data.List1 qualified as List1
import Data.Range
import Data.Range qualified as Range
import Data.Set qualified as Set
import Imports hiding (cs)
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

instance FromJSON Route where
  parseJSON (String "any") = pure RouteAny
  parseJSON (String "direct") = pure RouteDirect
  parseJSON x = fail $ "Invalid routing: " ++ show (encode x)

instance ToJSON Route where
  toJSON RouteAny = String "any"
  toJSON RouteDirect = String "direct"

-----------------------------------------------------------------------------
-- Recipient

data Recipient = Recipient
  { _recipientId :: !UserId,
    _recipientRoute :: !Route,
    _recipientClients :: !RecipientClients
  }
  deriving (Show, Eq, Ord, Generic)

data RecipientClients
  = -- | All clients of some user
    RecipientClientsAll
  | -- | An explicit list of clients
    RecipientClientsSome (List1 ClientId)
  deriving (Eq, Show, Ord, Generic)
  deriving (Arbitrary) via GenericUniform RecipientClients

instance Semigroup RecipientClients where
  RecipientClientsAll <> _ = RecipientClientsAll
  _ <> RecipientClientsAll = RecipientClientsAll
  RecipientClientsSome cs1 <> RecipientClientsSome cs2 =
    RecipientClientsSome (cs1 <> cs2)

makeLenses ''Recipient

recipient :: UserId -> Route -> Recipient
recipient u r = Recipient u r RecipientClientsAll

instance FromJSON Recipient where
  parseJSON = withObject "Recipient" $ \p ->
    Recipient
      <$> p .: "user_id"
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
  parseJSON x =
    parseJSON @[ClientId] x >>= \case
      [] -> pure RecipientClientsAll
      c : cs -> pure (RecipientClientsSome (list1 c cs))

instance ToJSON RecipientClients where
  toJSON =
    toJSON . \case
      RecipientClientsAll -> []
      RecipientClientsSome cs -> toList cs

-----------------------------------------------------------------------------
-- ApsData

newtype ApsSound = ApsSound {fromSound :: Text}
  deriving (Eq, Show, ToJSON, FromJSON, Arbitrary)

newtype ApsLocKey = ApsLocKey {fromLocKey :: Text}
  deriving (Eq, Show, ToJSON, FromJSON, Arbitrary)

data ApsPreference
  = ApsStdPreference
  | ApsVoIPPreference
  deriving (Eq, Show, Generic)
  deriving (Arbitrary) via GenericUniform ApsPreference

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
  deriving (Eq, Show, Generic)
  deriving (Arbitrary) via GenericUniform ApsData

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
    ApsData
      <$> o .: "loc_key"
      <*> o .:? "loc_args" .!= []
      <*> o .:? "sound"
      <*> o .:? "preference"
      <*> o .:? "badge" .!= True

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
    _pushPayload :: !(List1 Object)
  }
  deriving (Eq, Show)

makeLenses ''Push

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
      _pushPayload = pload
    }

singletonRecipient :: Recipient -> Range 1 1024 (Set Recipient)
singletonRecipient = Range.unsafeRange . Set.singleton

singletonPayload :: ToJSONObject a => a -> List1 Object
singletonPayload = List1.singleton . toJSONObject

instance FromJSON Push where
  parseJSON = withObject "Push" $ \p ->
    Push
      <$> p .: "recipients"
      <*> p .:? "origin"
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
        # "transient" .= ifNot not (_pushTransient p)
        # "native_include_origin" .= ifNot id (_pushNativeIncludeOrigin p)
        # "native_encrypt" .= ifNot id (_pushNativeEncrypt p)
        # "native_aps" .= _pushNativeAps p
        # "native_priority" .= ifNot (== HighPriority) (_pushNativePriority p)
        # "payload" .= _pushPayload p
        # []
    where
      ifNot f a = if f a then Nothing else Just a
