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

module Test.Wire.API.Golden.Manual.Push
  ( testObject_Push_1,
    testObject_Push_2,
  )
where

import Data.Aeson as A
import Data.Aeson.KeyMap qualified as KM
import Data.Id
import Data.Json.Util ((#))
import Data.List1
import Data.OpenApi qualified as S
import Data.Range
import Data.Schema qualified as SPF
import Data.Set qualified as Set
import Data.UUID qualified as UUID
import Imports

newtype ApsSound = ApsSound {fromSound :: Text}
  deriving (Eq, Show, A.ToJSON, A.FromJSON)

newtype ApsLocKey = ApsLocKey {fromLocKey :: Text}
  deriving (Eq, Show, A.ToJSON, A.FromJSON)

data ApsData = ApsData
  { _apsLocKey :: !ApsLocKey,
    _apsLocArgs :: [Text],
    _apsSound :: !(Maybe ApsSound),
    _apsBadge :: !Bool
  }
  deriving (Eq, Show, Generic)

apsData :: ApsLocKey -> [Text] -> ApsData
apsData lk la = ApsData lk la Nothing True

instance A.ToJSON ApsData where
  toJSON (ApsData k a s b) =
    A.object $
      [ "loc_key" A..= k,
        "loc_args" A..= a,
        "sound" A..= s,
        "badge" A..= b
      ]

instance A.FromJSON ApsData where
  parseJSON = A.withObject "ApsData" $ \o ->
    ApsData
      <$> o A..: "loc_key"
      <*> o A..:? "loc_args" A..!= []
      <*> o A..:? "sound"
      <*> o A..:? "badge" A..!= True

data Priority = LowPriority | HighPriority
  deriving stock (Eq, Show, Ord, Enum, Generic)
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via SPF.Schema Priority

instance SPF.ToSchema Priority where
  schema =
    SPF.enum @Text "Priority" $
      mconcat
        [ SPF.element "low" LowPriority,
          SPF.element "high" HighPriority
        ]

data Route = RouteAny | RouteDirect
  deriving (Eq, Ord, Enum, Bounded, Show, Generic)

instance A.FromJSON Route where
  parseJSON (A.String "any") = pure RouteAny
  parseJSON (A.String "direct") = pure RouteDirect
  parseJSON x = fail $ "Invalid routing: " ++ show (A.encode x)

instance A.ToJSON Route where
  toJSON RouteAny = A.String "any"
  toJSON RouteDirect = A.String "direct"

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

instance Semigroup RecipientClients where
  RecipientClientsAll <> _ = RecipientClientsAll
  _ <> RecipientClientsAll = RecipientClientsAll
  RecipientClientsSome cs1 <> RecipientClientsSome cs2 =
    RecipientClientsSome (cs1 <> cs2)

instance A.FromJSON Recipient where
  parseJSON = A.withObject "Recipient" $ \p ->
    Recipient
      <$> p A..: "user_id"
      <*> p A..: "route"
      <*> p A..:? "clients" A..!= RecipientClientsAll

instance A.ToJSON Recipient where
  toJSON (Recipient u r c) =
    A.object $
      "user_id"
        A..= u
        # "route"
        A..= r
        # "clients"
        A..= c
        # []

-- "All clients" is encoded in the API as an empty list.
instance A.FromJSON RecipientClients where
  parseJSON x =
    A.parseJSON @[ClientId] x >>= \case
      [] -> pure RecipientClientsAll
      c : cs -> pure (RecipientClientsSome (list1 c cs))

instance A.ToJSON RecipientClients where
  toJSON =
    A.toJSON . \case
      RecipientClientsAll -> []
      RecipientClientsSome cs -> toList cs

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
    _pushPayload :: !(List1 A.Object)
  }
  deriving (Eq, Show)

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
      "recipients"
        .= _pushRecipients p
        # "origin"
        .= _pushOrigin p
        # "connections"
        .= ifNot Set.null (_pushConnections p)
        # "origin_connection"
        .= _pushOriginConnection p
        # "transient"
        .= ifNot not (_pushTransient p)
        # "native_include_origin"
        .= ifNot id (_pushNativeIncludeOrigin p)
        # "native_encrypt"
        .= ifNot id (_pushNativeEncrypt p)
        # "native_aps"
        .= _pushNativeAps p
        # "native_priority"
        .= ifNot (== HighPriority) (_pushNativePriority p)
        # "payload"
        .= _pushPayload p
        # []
    where
      ifNot f a = if f a then Nothing else Just a

rcp1, rcp2, rcp3 :: Recipient
rcp1 =
  Recipient
    (Id . fromJust $ UUID.fromString "15441ff8-7f14-11ef-aeec-bbe21dc8a204")
    RouteAny
    RecipientClientsAll
rcp2 =
  Recipient
    (Id . fromJust $ UUID.fromString "2e18540e-7f14-11ef-9886-d3c2ff21d3d1")
    RouteDirect
    (RecipientClientsSome (list1 (ClientId 0) []))
rcp3 =
  Recipient
    (Id . fromJust $ UUID.fromString "316924ee-7f14-11ef-b6a2-036a4f646914")
    RouteDirect
    (RecipientClientsSome (list1 (ClientId 234) [ClientId 123]))

testObject_Push_1 :: Push
testObject_Push_1 =
  Push
    { _pushRecipients = unsafeRange (Set.fromList [rcp1]),
      _pushOrigin = Nothing,
      _pushConnections = mempty,
      _pushOriginConnection = Nothing,
      _pushTransient = False,
      _pushNativeIncludeOrigin = False,
      _pushNativeEncrypt = True,
      _pushNativeAps = Nothing,
      _pushNativePriority = HighPriority,
      _pushPayload = singleton mempty
    }

testObject_Push_2 :: Push
testObject_Push_2 =
  Push
    { _pushRecipients = unsafeRange (Set.fromList [rcp2, rcp3]),
      _pushOrigin = Just (Id . fromJust $ UUID.fromString "dec9b47a-7f12-11ef-b634-6710e7ae3d33"),
      _pushConnections = Set.fromList [ConnId "sdf", ConnId "mempty", ConnId "wire-client"],
      _pushOriginConnection = Just (ConnId "123"),
      _pushTransient = True,
      _pushNativeIncludeOrigin = True,
      _pushNativeEncrypt = False,
      _pushNativeAps = Just (apsData (ApsLocKey "asdf") ["1", "22", "333"]),
      _pushNativePriority = LowPriority,
      _pushPayload =
        list1
          (KM.fromList [("foo" :: KM.Key) A..= '3', "bar" A..= True])
          [KM.fromList [], KM.fromList ["growl" A..= ("foooood" :: Text)], KM.fromList ["lunchtime" A..= ("imminent" :: Text)]]
    }
