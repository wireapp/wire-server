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
import Data.Kind
import Data.List.NonEmpty
import Data.Qualified
import Data.Range
import Data.Time.Clock
import GHC.TypeLits
import Imports
import Servant.API
import Wire.API.Conversation.Action
import Wire.API.Federation.Endpoint
import Wire.API.MLS.SubConversation
import Wire.API.MakesFederatedCall
import Wire.API.Message
import Wire.API.Util.Aeson
import Wire.Arbitrary

data GalleyNotificationTag
  = OnClientRemovedTag
  | OnMessageSentTag
  | OnMLSMessageSentTag
  | OnConversationUpdatedTag
  | OnUserDeletedConversationsTag
  deriving (Show, Eq, Generic, Bounded, Enum)

type family GalleyNotification (tag :: GalleyNotificationTag) :: Type where
  GalleyNotification 'OnClientRemovedTag = ClientRemovedRequest
  GalleyNotification 'OnMessageSentTag = RemoteMessage ConvId
  GalleyNotification 'OnMLSMessageSentTag = RemoteMLSMessage
  GalleyNotification 'OnConversationUpdatedTag = ConversationUpdate
  GalleyNotification 'OnUserDeletedConversationsTag = UserDeletedConversationsNotification

-- | The central path component of a Galley notification endpoint
type family GNPath (tag :: GalleyNotificationTag) :: Symbol where
  GNPath 'OnClientRemovedTag = "on-client-removed"
  GNPath 'OnMessageSentTag = "on-message-sent"
  GNPath 'OnMLSMessageSentTag = "on-mls-message-sent"
  GNPath 'OnConversationUpdatedTag = "on-conversation-updated"
  GNPath 'OnUserDeletedConversationsTag = "on-user-deleted-conversations"

type GalleyNotifEndpoint (tag :: GalleyNotificationTag) =
  NotificationFedEndpoint (GNPath tag) (GalleyNotification tag)

type family GalleyNotificationToServantAPI (gn :: GalleyNotificationTag) :: Type where
  GalleyNotificationToServantAPI 'OnClientRemovedTag =
    NotificationFedEndpointWithMods
      '[ MakesFederatedCall 'Galley "on-mls-message-sent"
       ]
      (GNPath 'OnClientRemovedTag)
      (GalleyNotification 'OnClientRemovedTag)
  -- used to notify this backend that a new message has been posted to a
  -- remote conversation
  GalleyNotificationToServantAPI 'OnMessageSentTag = GalleyNotifEndpoint 'OnMessageSentTag
  GalleyNotificationToServantAPI 'OnMLSMessageSentTag = GalleyNotifEndpoint 'OnMLSMessageSentTag
  -- used by the backend that owns a conversation to inform this backend of
  -- changes to the conversation
  GalleyNotificationToServantAPI 'OnConversationUpdatedTag =
    GalleyNotifEndpoint 'OnConversationUpdatedTag
  GalleyNotificationToServantAPI 'OnUserDeletedConversationsTag =
    NotificationFedEndpointWithMods
      '[ MakesFederatedCall 'Galley "on-mls-message-sent",
         MakesFederatedCall 'Galley "on-conversation-updated",
         MakesFederatedCall 'Brig "api-version"
       ]
      (GNPath 'OnUserDeletedConversationsTag)
      (GalleyNotification 'OnUserDeletedConversationsTag)

-- | All the notification endpoints return an 'EmptyResponse'.
type NotificationAPI =
  GalleyNotificationToServantAPI 'OnClientRemovedTag
    :<|> GalleyNotificationToServantAPI 'OnMessageSentTag
    :<|> GalleyNotificationToServantAPI 'OnMLSMessageSentTag
    :<|> GalleyNotificationToServantAPI 'OnConversationUpdatedTag
    :<|> GalleyNotificationToServantAPI 'OnUserDeletedConversationsTag

data ClientRemovedRequest = ClientRemovedRequest
  { user :: UserId,
    client :: ClientId,
    convs :: [ConvId]
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform ClientRemovedRequest)
  deriving (FromJSON, ToJSON) via (CustomEncoded ClientRemovedRequest)

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

data ConversationUpdate = ConversationUpdate
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

instance ToJSON ConversationUpdate

instance FromJSON ConversationUpdate

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
