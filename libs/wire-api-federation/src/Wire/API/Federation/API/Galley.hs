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

module Wire.API.Federation.API.Galley where

import Data.Aeson (FromJSON, ToJSON)
import Data.Id (ClientId, ConvId, UserId)
import Data.Json.Util (Base64ByteString)
import Data.Misc (Milliseconds)
import Data.Qualified
import Data.Range
import Data.Time.Clock (UTCTime)
import Imports
import Servant.API
import Wire.API.Arbitrary (Arbitrary, GenericUniform (..))
import Wire.API.Conversation
  ( Access,
    AccessRoleV2,
    ConvType,
    ConversationMetadata,
    ReceiptMode,
  )
import Wire.API.Conversation.Action
import Wire.API.Conversation.Member (OtherMember)
import Wire.API.Conversation.Role (RoleName)
import Wire.API.Federation.API.Common
import Wire.API.Federation.Endpoint
import Wire.API.Message (MessageNotSent, MessageSendingStatus, PostOtrResponse, Priority)
import Wire.API.User.Client (UserClientMap)
import Wire.API.Util.Aeson (CustomEncoded (..))

-- FUTUREWORK: data types, json instances, more endpoints. See
-- https://wearezeta.atlassian.net/wiki/spaces/CORE/pages/356090113/Federation+Galley+Conversation+API
-- for the current list we need.

-- | For conventions see /docs/developer/federation-api-conventions.md
type GalleyApi =
  -- | Register a new conversation
  FedEndpoint "on-conversation-created" (NewRemoteConversation ConvId) ()
    :<|> FedEndpoint "get-conversations" GetConversationsRequest GetConversationsResponse
    -- used by the backend that owns a conversation to inform this backend of
    -- changes to the conversation
    :<|> FedEndpoint "on-conversation-updated" ConversationUpdate ()
    :<|> FedEndpoint "leave-conversation" LeaveConversationRequest LeaveConversationResponse
    -- used to notify this backend that a new message has been posted to a
    -- remote conversation
    :<|> FedEndpoint "on-message-sent" (RemoteMessage ConvId) ()
    -- used by a remote backend to send a message to a conversation owned by
    -- this backend
    :<|> FedEndpoint "send-message" MessageSendRequest MessageSendResponse
    :<|> FedEndpoint "on-user-deleted-conversations" UserDeletedConversationsNotification EmptyResponse
    :<|> FedEndpoint "update-conversation" ConversationUpdateRequest ConversationUpdateResponse

data GetConversationsRequest = GetConversationsRequest
  { gcrUserId :: UserId,
    gcrConvIds :: [ConvId]
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform GetConversationsRequest)
  deriving (ToJSON, FromJSON) via (CustomEncoded GetConversationsRequest)

data RemoteConvMembers = RemoteConvMembers
  { rcmSelfRole :: RoleName,
    rcmOthers :: [OtherMember]
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform RemoteConvMembers)
  deriving (FromJSON, ToJSON) via (CustomEncoded RemoteConvMembers)

-- | A conversation hosted on a remote backend. This contains the same
-- information as a 'Conversation', with the exception that conversation status
-- fields (muted/archived/hidden) are omitted, since they are not known by the
-- remote backend.
data RemoteConversation = RemoteConversation
  { -- | Id of the conversation, implicitly qualified with the domain of the
    -- backend that created this value.
    rcnvId :: ConvId,
    rcnvMetadata :: ConversationMetadata,
    rcnvMembers :: RemoteConvMembers
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform RemoteConversation)
  deriving (FromJSON, ToJSON) via (CustomEncoded RemoteConversation)

newtype GetConversationsResponse = GetConversationsResponse
  { gcresConvs :: [RemoteConversation]
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform GetConversationsResponse)
  deriving (ToJSON, FromJSON) via (CustomEncoded GetConversationsResponse)

-- | A record type describing a new federated conversation
--
-- FUTUREWORK: Think about extracting common conversation metadata into a
-- separarate data type that can be reused in several data types in this module.
data NewRemoteConversation conv = NewRemoteConversation
  { -- | The time when the conversation was created
    rcTime :: UTCTime,
    -- | The user that created the conversation. This is implicitly qualified
    -- by the requesting domain, since it is impossible to create a regular/group
    -- conversation on a remote backend.
    rcOrigUserId :: UserId,
    -- | The conversation ID, local to the backend invoking the RPC
    rcCnvId :: conv,
    -- | The conversation type
    rcCnvType :: ConvType,
    rcCnvAccess :: [Access],
    rcCnvAccessRoles :: Set AccessRoleV2,
    -- | The conversation name,
    rcCnvName :: Maybe Text,
    -- | Members of the conversation apart from the creator
    rcNonCreatorMembers :: Set OtherMember,
    rcMessageTimer :: Maybe Milliseconds,
    rcReceiptMode :: Maybe ReceiptMode
  }
  deriving stock (Eq, Show, Generic, Functor)
  deriving (ToJSON, FromJSON) via (CustomEncoded (NewRemoteConversation conv))

rcRemoteOrigUserId :: NewRemoteConversation (Remote ConvId) -> Remote UserId
rcRemoteOrigUserId rc = qualifyAs (rcCnvId rc) (rcOrigUserId rc)

data ConversationUpdate = ConversationUpdate
  { cuTime :: UTCTime,
    cuOrigUserId :: Qualified UserId,
    -- | The unqualified ID of the conversation where the update is happening.
    -- The ID is local to prevent putting arbitrary domain that is different
    -- than that of the backend making a conversation membership update request.
    cuConvId :: ConvId,
    -- | A list of users from a remote backend that need to be sent
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

data LeaveConversationRequest = LeaveConversationRequest
  { -- | The conversation is assumed to be owned by the target domain, which
    -- allows us to protect against relay attacks
    lcConvId :: ConvId,
    -- | The leaver is assumed to be owned by the origin domain, which allows us
    -- to protect against spoofing attacks
    lcLeaver :: UserId
  }
  deriving stock (Generic, Eq, Show)
  deriving (ToJSON, FromJSON) via (CustomEncoded LeaveConversationRequest)

-- | Error outcomes of the leave-conversation RPC.
data RemoveFromConversationError
  = RemoveFromConversationErrorRemovalNotAllowed
  | RemoveFromConversationErrorNotFound
  | RemoveFromConversationErrorUnchanged
  deriving stock (Eq, Show, Generic)
  deriving
    (ToJSON, FromJSON)
    via (CustomEncoded RemoveFromConversationError)

-- Note: this is parametric in the conversation type to allow it to be used
-- both for conversations with a fixed known domain (e.g. as the argument of the
-- federation RPC), and for conversations with an arbitrary Qualified or Remote id
-- (e.g. as the argument of the corresponding handler).
data RemoteMessage conv = RemoteMessage
  { rmTime :: UTCTime,
    rmData :: Maybe Text,
    rmSender :: Qualified UserId,
    rmSenderClient :: ClientId,
    rmConversation :: conv,
    rmPriority :: Maybe Priority,
    rmPush :: Bool,
    rmTransient :: Bool,
    rmRecipients :: UserClientMap Text
  }
  deriving stock (Eq, Show, Generic, Functor)
  deriving (Arbitrary) via (GenericUniform (RemoteMessage conv))
  deriving (ToJSON, FromJSON) via (CustomEncoded (RemoteMessage conv))

data MessageSendRequest = MessageSendRequest
  { -- | Conversation is assumed to be owned by the target domain, this allows
    -- us to protect against relay attacks
    msrConvId :: ConvId,
    -- | Sender is assumed to be owned by the origin domain, this allows us to
    -- protect against spoofing attacks
    msrSender :: UserId,
    msrRawMessage :: Base64ByteString
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform MessageSendRequest)
  deriving (ToJSON, FromJSON) via (CustomEncoded MessageSendRequest)

newtype MessageSendResponse = MessageSendResponse
  {msResponse :: PostOtrResponse MessageSendingStatus}
  deriving stock (Eq, Show)
  deriving
    (ToJSON, FromJSON)
    via ( Either
            (CustomEncoded (MessageNotSent MessageSendingStatus))
            MessageSendingStatus
        )

newtype LeaveConversationResponse = LeaveConversationResponse
  {leaveResponse :: Either RemoveFromConversationError ()}
  deriving stock (Eq, Show)
  deriving
    (ToJSON, FromJSON)
    via (Either (CustomEncoded RemoveFromConversationError) ())

type UserDeletedNotificationMaxConvs = 1000

data UserDeletedConversationsNotification = UserDeletedConversationsNotification
  { -- | This is qualified implicitly by the origin domain
    udcvUser :: UserId,
    -- | These are qualified implicitly by the target domain
    udcvConversations :: Range 1 UserDeletedNotificationMaxConvs [ConvId]
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform UserDeletedConversationsNotification)
  deriving (FromJSON, ToJSON) via (CustomEncoded UserDeletedConversationsNotification)

data ConversationUpdateRequest = ConversationUpdateRequest
  { -- | The user that is attempting to perform the action. This is qualified
    -- implicitly by the origin domain
    curUser :: UserId,
    -- | Id of conversation the action should be performed on. The is qualified
    -- implicity by the owning backend which receives this request.
    curConvId :: ConvId,
    curAction :: SomeConversationAction
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform ConversationUpdateRequest)
  deriving (FromJSON, ToJSON) via (CustomEncoded ConversationUpdateRequest)

data ConversationUpdateError
  = InsufficientPrivileges
  | TODO
  deriving stock (Eq, Show, Generic)
  deriving (ToJSON, FromJSON) via (CustomEncoded ConversationUpdateError)

newtype ConversationUpdateResponse = ConversationUpdateResponse
  { conversationUpdateResponse :: Either ConversationUpdateError ConversationUpdate
  }
  deriving stock (Eq, Show)
  deriving
    (ToJSON, FromJSON)
    via (Either (CustomEncoded ConversationUpdateError) ConversationUpdate)
