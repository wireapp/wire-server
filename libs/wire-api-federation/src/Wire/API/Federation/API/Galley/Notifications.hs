{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2023 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.API.Federation.API.Galley.Notifications where

import Data.Aeson
import Data.Id
import Data.Json.Util
import Data.List.NonEmpty
import Data.OpenApi (ToSchema)
import Data.Qualified
import Data.Range
import Data.Time.Clock
import Imports
import Servant.API
import Wire.API.Conversation
import Wire.API.Conversation.Action
import Wire.API.Federation.Component
import Wire.API.Federation.Endpoint
import Wire.API.Federation.HasNotificationEndpoint
import Wire.API.Federation.Version
import Wire.API.MLS.SubConversation
import Wire.API.Message
import Wire.API.Routes.Version (From, Until)
import Wire.API.Util.Aeson
import Wire.Arbitrary

data GalleyNotificationTag
  = OnClientRemovedTag
  | OnMessageSentTag
  | OnMLSMessageSentTag
  | OnConversationUpdatedTagV0
  | OnConversationUpdatedTag
  | OnUserDeletedConversationsTag
  deriving (Show, Eq, Generic, Bounded, Enum)

instance IsNotificationTag GalleyNotificationTag where
  type NotificationComponent _ = 'Galley

instance HasNotificationEndpoint 'OnClientRemovedTag where
  type Payload 'OnClientRemovedTag = ClientRemovedRequest
  type NotificationPath 'OnClientRemovedTag = "on-client-removed"

-- used to notify this backend that a new message has been posted to a
-- remote conversation
instance HasNotificationEndpoint 'OnMessageSentTag where
  type Payload 'OnMessageSentTag = RemoteMessage ConvId
  type NotificationPath 'OnMessageSentTag = "on-message-sent"

instance HasNotificationEndpoint 'OnMLSMessageSentTag where
  type Payload 'OnMLSMessageSentTag = RemoteMLSMessage
  type NotificationPath 'OnMLSMessageSentTag = "on-mls-message-sent"

-- used by the backend that owns a conversation to inform this backend of
-- changes to the conversation
instance HasNotificationEndpoint 'OnConversationUpdatedTagV0 where
  type Payload 'OnConversationUpdatedTagV0 = ConversationUpdateV0
  type NotificationPath 'OnConversationUpdatedTagV0 = "on-conversation-updated"
  type NotificationVersionTag 'OnConversationUpdatedTagV0 = 'Just 'V0
  type NotificationMods 'OnConversationUpdatedTagV0 = '[Until 'V1]

instance HasNotificationEndpoint 'OnConversationUpdatedTag where
  type Payload 'OnConversationUpdatedTag = ConversationUpdate
  type NotificationPath 'OnConversationUpdatedTag = "on-conversation-updated"
  type NotificationMods 'OnConversationUpdatedTag = '[From 'V1]

instance HasNotificationEndpoint 'OnUserDeletedConversationsTag where
  type Payload 'OnUserDeletedConversationsTag = UserDeletedConversationsNotification
  type NotificationPath 'OnUserDeletedConversationsTag = "on-user-deleted-conversations"

-- | All the notification endpoints return an 'EmptyResponse'.
type GalleyNotificationAPI =
  NotificationFedEndpoint 'OnClientRemovedTag
    :<|> NotificationFedEndpoint 'OnMessageSentTag
    :<|> NotificationFedEndpoint 'OnMLSMessageSentTag
    :<|> NotificationFedEndpoint 'OnConversationUpdatedTagV0
    :<|> NotificationFedEndpoint 'OnConversationUpdatedTag
    :<|> NotificationFedEndpoint 'OnUserDeletedConversationsTag

data ClientRemovedRequest = ClientRemovedRequest
  { user :: UserId,
    client :: ClientId,
    convs :: [ConvId]
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform ClientRemovedRequest)
  deriving (FromJSON, ToJSON) via (CustomEncoded ClientRemovedRequest)

instance ToSchema ClientRemovedRequest

-- Note: this is parametric in the conversation type to allow it to be used
-- both for conversations with a fixed known domain (e.g. as the argument of the
-- federation RPC), and for conversations with an arbitrary Qualified or Remote id
-- (e.g. as the argument of the corresponding handler).
data RemoteMessage conv = RemoteMessage
  { time :: UTCTime,
    _data :: Maybe Text,
    sender :: Qualified UserId,
    senderClient :: ClientId,
    conversation :: conv,
    priority :: Maybe Priority,
    push :: Bool,
    transient :: Bool,
    recipients :: UserClientMap Text
  }
  deriving stock (Eq, Show, Generic, Functor)
  deriving (Arbitrary) via (GenericUniform (RemoteMessage conv))
  deriving (ToJSON, FromJSON) via (CustomEncodedLensable (RemoteMessage conv))

instance (ToSchema a) => ToSchema (RemoteMessage a)

data RemoteMLSMessage = RemoteMLSMessage
  { time :: UTCTime,
    metadata :: MessageMetadata,
    sender :: Qualified UserId,
    conversation :: ConvId,
    subConversation :: Maybe SubConvId,
    recipients :: Map UserId (NonEmpty ClientId),
    message :: Base64ByteString
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform RemoteMLSMessage)
  deriving (ToJSON, FromJSON) via (CustomEncoded RemoteMLSMessage)

instance ToSchema RemoteMLSMessage

data ConversationUpdateV0 = ConversationUpdateV0
  { cuTime :: UTCTime,
    cuOrigUserId :: Qualified UserId,
    -- | The unqualified ID of the conversation where the update is happening.
    -- The ID is local to the sender to prevent putting arbitrary domain that
    -- is different than that of the backend making a conversation membership
    -- update request.
    cuConvId :: ConvId,
    -- | A list of users from the receiving backend that need to be sent
    -- notifications about this change. This is required as we do not expect a
    -- non-conversation owning backend to have an indexed mapping of
    -- conversation to users.
    cuAlreadyPresentUsers :: [UserId],
    -- | Information on the specific action that caused the update.
    cuAction :: SomeConversationAction
  }
  deriving (Eq, Show, Generic)

instance ToJSON ConversationUpdateV0

instance FromJSON ConversationUpdateV0

instance ToSchema ConversationUpdateV0

data ConversationUpdate = ConversationUpdate
  { time :: UTCTime,
    origUserId :: Qualified UserId,
    -- | The unqualified ID of the conversation where the update is happening.
    -- The ID is local to the sender to prevent putting arbitrary domain that
    -- is different than that of the backend making a conversation membership
    -- update request.
    convId :: ConvId,
    -- | A list of users from the receiving backend that need to be sent
    -- notifications about this change. This is required as we do not expect a
    -- non-conversation owning backend to have an indexed mapping of
    -- conversation to users.
    alreadyPresentUsers :: [UserId],
    -- | Information on the specific action that caused the update.
    action :: SomeConversationAction,
    extraConversationData :: Maybe ExtraConversationData
  }
  deriving (Eq, Show, Generic)

instance ToJSON ConversationUpdate

instance FromJSON ConversationUpdate

instance ToSchema ConversationUpdate

conversationUpdateToV0 :: ConversationUpdate -> ConversationUpdateV0
conversationUpdateToV0 cu =
  ConversationUpdateV0
    { cuTime = cu.time,
      cuOrigUserId = cu.origUserId,
      cuConvId = cu.convId,
      cuAlreadyPresentUsers = cu.alreadyPresentUsers,
      cuAction = cu.action
    }

conversationUpdateFromV0 :: ConversationUpdateV0 -> ConversationUpdate
conversationUpdateFromV0 cu =
  ConversationUpdate
    { time = cu.cuTime,
      origUserId = cu.cuOrigUserId,
      convId = cu.cuConvId,
      alreadyPresentUsers = cu.cuAlreadyPresentUsers,
      action = cu.cuAction,
      extraConversationData = Nothing
    }

type UserDeletedNotificationMaxConvs = 1000

data UserDeletedConversationsNotification = UserDeletedConversationsNotification
  { -- | This is qualified implicitly by the origin domain
    user :: UserId,
    -- | These are qualified implicitly by the target domain
    conversations :: Range 1 UserDeletedNotificationMaxConvs [ConvId]
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform UserDeletedConversationsNotification)
  deriving (FromJSON, ToJSON) via (CustomEncoded UserDeletedConversationsNotification)

instance ToSchema UserDeletedConversationsNotification
