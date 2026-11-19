{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2026 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option)
-- any later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

module Wire.API.Notification.FanIn where

import Control.Lens (makePrisms, _1)
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as A
import Data.Aeson.Types (Pair)
import Data.Id
import Data.List.NonEmpty (NonEmpty)
import Data.OpenApi qualified as S
import Data.Qualified (Qualified)
import Data.Schema
import Imports
import Wire.API.Event.WebSocketProtocol (AckData (..), EventData)
import Wire.API.MLS.Epoch (Epoch)
import Wire.API.MLS.Group (GroupId)
import Wire.API.Notification (NotificationId)
import Wire.API.Push.V2 (ApsData, Priority, Route)

-- | Targets describe who can consume a stored event, without enumerating team
-- or epoch members. Constructor names and record fields form the internal JSON
-- contract; no legacy push JSON representation is reused.
data FanInTarget
  = FanInTargetUser UserId
  | FanInTargetUserClients UserId (NonEmpty ClientId)
  | FanInTargetTeam TeamId
  | FanInTargetEpoch GroupId Epoch
  | FanInTargetConnections (Qualified UserId)
  deriving stock (Eq, Show, Generic)

makePrisms ''FanInTarget

data FanInTargetTag = UserTarget | ClientsTarget | TeamTarget | EpochTarget | ConnectionsTarget
  deriving stock (Eq, Enum, Bounded)

instance ToSchema FanInTargetTag where
  schema =
    enum @Text $
      mconcat
        [ element "FanInTargetUser" UserTarget,
          element "FanInTargetUserClients" ClientsTarget,
          element "FanInTargetTeam" TeamTarget,
          element "FanInTargetEpoch" EpochTarget,
          element "FanInTargetConnections" ConnectionsTarget
        ]

instance ToSchema FanInTarget where
  schema =
    object $
      snd
        <$> toTagged
          .= bind
            (fst .= field "tag" (schema @FanInTargetTag))
            (snd .= fieldOver _1 "contents" contentsSchema)
    where
      toTagged target = (targetTag target, target)
      targetTag = \case
        FanInTargetUser {} -> UserTarget
        FanInTargetUserClients {} -> ClientsTarget
        FanInTargetTeam {} -> TeamTarget
        FanInTargetEpoch {} -> EpochTarget
        FanInTargetConnections {} -> ConnectionsTarget
      contentsSchema = dispatch $ \case
        UserTarget -> tag _FanInTargetUser (unnamed schema)
        ClientsTarget -> tag _FanInTargetUserClients (unnamed genericToSchema)
        TeamTarget -> tag _FanInTargetTeam (unnamed schema)
        EpochTarget -> tag _FanInTargetEpoch (unnamed genericToSchema)
        ConnectionsTarget -> tag _FanInTargetConnections (unnamed schema)

deriving via (Schema FanInTarget) instance ToJSON FanInTarget

deriving via (Schema FanInTarget) instance FromJSON FanInTarget

deriving via (Schema FanInTarget) instance S.ToSchema FanInTarget

-- | The request id must be a UUIDv7 notification id, allocated once by the
-- producer and reused on retries. Gundeck validates it before persistence.
-- Transient pushes use the same envelope but are never stored.
data FanInPush = FanInPush
  { notificationId :: NotificationId,
    targets :: NonEmpty FanInTarget,
    conn :: Maybe ConnId,
    transient :: Bool,
    route :: Route,
    nativePriority :: Maybe Priority,
    origin :: Maybe UserId,
    json :: A.Object,
    apsData :: Maybe ApsData,
    isCellsEvent :: Bool
  }
  deriving stock (Eq, Show, Generic)

instance ToSchema FanInPush where
  schema =
    object $
      FanInPush
        <$> (.notificationId) .= field "notificationId" schema
        <*> (.targets) .= field "targets" (nonEmptyArray schema)
        <*> (.conn) .= (join <$> optField' "conn" (nullable (unnamed schema)))
        <*> (.transient) .= field "transient" schema
        <*> (.route) .= field "route" schema
        <*> (.nativePriority) .= (join <$> optField' "nativePriority" (nullable (unnamed schema)))
        <*> (.origin) .= (join <$> optField' "origin" (nullable (unnamed schema)))
        <*> (.json) .= field "json" jsonObject
        <*> (.apsData) .= (join <$> optField' "apsData" (nullable (unnamed schema)))
        <*> (.isCellsEvent) .= field "isCellsEvent" schema

deriving via (Schema FanInPush) instance ToJSON FanInPush

deriving via (Schema FanInPush) instance FromJSON FanInPush

deriving via (Schema FanInPush) instance S.ToSchema FanInPush

data FanInMember = FanInMember
  { userId :: UserId,
    clientId :: ClientId
  }
  deriving stock (Eq, Show, Generic)

instance ToSchema FanInMember where
  schema =
    object $
      FanInMember
        <$> (.userId) .= field "userId" schema
        <*> (.clientId) .= field "clientId" schema

deriving via (Schema FanInMember) instance ToJSON FanInMember

deriving via (Schema FanInMember) instance FromJSON FanInMember

deriving via (Schema FanInMember) instance S.ToSchema FanInMember

-- | Proposed Galley -> Gundeck contract. Repeating an identical epoch is a
-- no-op; conflicting membership for that epoch or a stale epoch is rejected.
-- An empty member list retires a group while retaining its previous history.
-- Membership must be durable before notifications targeting that epoch.
data FanInMembershipUpdate = FanInMembershipUpdate
  { groupId :: GroupId,
    epoch :: Epoch,
    members :: [FanInMember]
  }
  deriving stock (Eq, Show, Generic)

instance ToSchema FanInMembershipUpdate where
  schema =
    object $
      FanInMembershipUpdate
        <$> (.groupId) .= field "groupId" schema
        <*> (.epoch) .= field "epoch" schema
        <*> (.members) .= field "members" (array schema)

deriving via (Schema FanInMembershipUpdate) instance ToJSON FanInMembershipUpdate

deriving via (Schema FanInMembershipUpdate) instance FromJSON FanInMembershipUpdate

deriving via (Schema FanInMembershipUpdate) instance S.ToSchema FanInMembershipUpdate

-- | Public reset payload from the RFC. Omitted cursors are left unchanged.
-- Authorization and stream membership are checked before any reset is applied.
data FanInAcknowledgements = FanInAcknowledgements
  { user :: Maybe NotificationId,
    client :: Maybe NotificationId,
    team :: Maybe NotificationId,
    mls :: [FanInMLSAck]
  }
  deriving stock (Eq, Show, Generic)

instance ToSchema FanInAcknowledgements where
  schema =
    object $
      FanInAcknowledgements
        <$> (.user) .= optField "user" (maybeWithDefault A.Null schema)
        <*> (.client) .= optField "client" (maybeWithDefault A.Null schema)
        <*> (.team) .= optField "team" (maybeWithDefault A.Null schema)
        <*> (.mls) .= field "mls" (array schema)

deriving via (Schema FanInAcknowledgements) instance ToJSON FanInAcknowledgements

deriving via (Schema FanInAcknowledgements) instance FromJSON FanInAcknowledgements

deriving via (Schema FanInAcknowledgements) instance S.ToSchema FanInAcknowledgements

data FanInMLSAck = FanInMLSAck
  { groupId :: GroupId,
    epoch :: Epoch,
    notificationId :: NotificationId,
    ackPreviousEpochs :: Bool
  }
  deriving stock (Eq, Show, Generic)

instance ToSchema FanInMLSAck where
  schema =
    object $
      FanInMLSAck
        <$> (.groupId) .= field "groupId" schema
        <*> (.epoch) .= field "epoch" schema
        <*> (.notificationId) .= field "notificationId" schema
        <*> (.ackPreviousEpochs) .= field "ackPreviousEpochs" schema

deriving via (Schema FanInMLSAck) instance ToJSON FanInMLSAck

deriving via (Schema FanInMLSAck) instance FromJSON FanInMLSAck

deriving via (Schema FanInMLSAck) instance S.ToSchema FanInMLSAck

-- | Session-local delivery tags map to both the notification and its stream.
-- There is no global ordering across streams. The synchronization message has
-- no marker or delivery tag. AckData retains the existing wire representation.
data FanInServerMessage
  = FanInEvent EventData
  | FanInNotificationsMissed
  | FanInSynchronization
  deriving stock (Eq, Show)

makePrisms ''FanInServerMessage

data FanInServerMessageTag = EventTag | MissedTag | SynchronizationTag
  deriving stock (Eq, Enum, Bounded)

instance ToSchema FanInServerMessageTag where
  schema =
    enum @Text $
      mconcat
        [ element "event" EventTag,
          element "notifications_missed" MissedTag,
          element "synchronization" SynchronizationTag
        ]

instance ToSchema FanInServerMessage where
  schema =
    object $
      snd
        <$> toTagged
          .= bind
            (fst .= field "type" (schema @FanInServerMessageTag))
            (snd .= messageSchema)
    where
      toTagged message = (messageTag message, message)
      messageTag = \case
        FanInEvent {} -> EventTag
        FanInNotificationsMissed -> MissedTag
        FanInSynchronization -> SynchronizationTag
      messageSchema :: SchemaP SwaggerDoc (A.Object, FanInServerMessageTag) [Pair] FanInServerMessage FanInServerMessage
      messageSchema = dispatch $ \case
        EventTag -> tag _FanInEvent (field "data" schema)
        MissedTag -> tag _FanInNotificationsMissed (pure ())
        SynchronizationTag -> tag _FanInSynchronization (pure ())

deriving via (Schema FanInServerMessage) instance ToJSON FanInServerMessage

deriving via (Schema FanInServerMessage) instance FromJSON FanInServerMessage

deriving via (Schema FanInServerMessage) instance S.ToSchema FanInServerMessage

data FanInClientMessage = FanInAck AckData | FanInAckFullSync
  deriving stock (Eq, Show)

makePrisms ''FanInClientMessage

data FanInClientMessageTag = AckTag | FullSyncTag
  deriving stock (Eq, Enum, Bounded)

instance ToSchema FanInClientMessageTag where
  schema =
    enum @Text $
      mconcat
        [ element "ack" AckTag,
          element "ack_full_sync" FullSyncTag
        ]

instance ToSchema FanInClientMessage where
  schema =
    object $
      snd
        <$> toTagged
          .= bind
            (fst .= field "type" (schema @FanInClientMessageTag))
            (snd .= messageSchema)
    where
      toTagged message = (messageTag message, message)
      messageTag = \case
        FanInAck {} -> AckTag
        FanInAckFullSync -> FullSyncTag
      messageSchema :: SchemaP SwaggerDoc (A.Object, FanInClientMessageTag) [Pair] FanInClientMessage FanInClientMessage
      messageSchema = dispatch $ \case
        AckTag ->
          tag _FanInAck $
            AckData
              <$> (.deliveryTag) .= field "delivery_tag" schema
              <*> (.multiple) .= field "multiple" schema
        FullSyncTag -> tag _FanInAckFullSync (pure ())

deriving via (Schema FanInClientMessage) instance ToJSON FanInClientMessage

deriving via (Schema FanInClientMessage) instance FromJSON FanInClientMessage

deriving via (Schema FanInClientMessage) instance S.ToSchema FanInClientMessage

-- | Background job -> Cannon. Fan-in sessions treat persistent payloads only
-- as wake-ups and read their backlog from the DB. Transient payloads are
-- forwarded directly to fan-in sessions.
data FanInCannonRequest = FanInCannonRequest
  { recipients :: NonEmpty FanInMember,
    push :: FanInPush
  }
  deriving stock (Eq, Show, Generic)

instance ToSchema FanInCannonRequest where
  schema =
    object $
      FanInCannonRequest
        <$> (.recipients) .= field "recipients" (nonEmptyArray schema)
        <*> (.push) .= field "push" schema

deriving via (Schema FanInCannonRequest) instance ToJSON FanInCannonRequest

deriving via (Schema FanInCannonRequest) instance FromJSON FanInCannonRequest

deriving via (Schema FanInCannonRequest) instance S.ToSchema FanInCannonRequest
