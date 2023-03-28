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
import Data.Id
import Data.Json.Util
import Data.Misc (Milliseconds)
import Data.Qualified
import Data.Range
import Data.Time.Clock (UTCTime)
import Imports
import qualified Network.Wai.Utilities.Error as Wai
import Servant.API
import Wire.API.Conversation
import Wire.API.Conversation.Action
import Wire.API.Conversation.Protocol
import Wire.API.Conversation.Role (RoleName)
import Wire.API.Conversation.Typing
import Wire.API.Error.Galley
import Wire.API.Federation.API.Common
import Wire.API.Federation.Endpoint
import Wire.API.MLS.Message
import Wire.API.MLS.SubConversation
import Wire.API.MakesFederatedCall
import Wire.API.Message
import Wire.API.Routes.Public.Galley.Messaging
import Wire.API.Util.Aeson (CustomEncoded (..))
import Wire.Arbitrary (Arbitrary, GenericUniform (..))

-- FUTUREWORK: data types, json instances, more endpoints. See
-- https://wearezeta.atlassian.net/wiki/spaces/CORE/pages/356090113/Federation+Galley+Conversation+API
-- for the current list we need.

-- | For conventions see /docs/developer/federation-api-conventions.md
type GalleyApi =
  -- | Register a new conversation. This is only called on backends of users
  -- that are part of a conversation at creation time. Since MLS conversations
  -- are always created empty (i.e. they only contain the creator), this RPC is
  -- never invoked for such conversations.
  FedEndpoint "on-conversation-created" (ConversationCreated ConvId) ()
    -- This endpoint is called the first time a user from this backend is
    -- added to a remote conversation.
    :<|> FedEndpoint "on-new-remote-conversation" NewRemoteConversation EmptyResponse
    :<|> FedEndpoint "get-conversations" GetConversationsRequest GetConversationsResponse
    -- used by the backend that owns a conversation to inform this backend of
    -- changes to the conversation
    :<|> FedEndpoint "on-conversation-updated" ConversationUpdate ()
    :<|> FedEndpointWithMods
           '[ MakesFederatedCall 'Galley "on-conversation-updated",
              MakesFederatedCall 'Galley "on-mls-message-sent",
              MakesFederatedCall 'Galley "on-new-remote-conversation"
            ]
           "leave-conversation"
           LeaveConversationRequest
           LeaveConversationResponse
    -- used to notify this backend that a new message has been posted to a
    -- remote conversation
    :<|> FedEndpoint "on-message-sent" (RemoteMessage ConvId) ()
    -- used by a remote backend to send a message to a conversation owned by
    -- this backend
    :<|> FedEndpointWithMods
           '[ MakesFederatedCall 'Galley "on-message-sent",
              MakesFederatedCall 'Brig "get-user-clients"
            ]
           "send-message"
           ProteusMessageSendRequest
           MessageSendResponse
    :<|> FedEndpointWithMods
           '[ MakesFederatedCall 'Galley "on-mls-message-sent",
              MakesFederatedCall 'Galley "on-conversation-updated",
              MakesFederatedCall 'Galley "on-new-remote-conversation"
            ]
           "on-user-deleted-conversations"
           UserDeletedConversationsNotification
           EmptyResponse
    :<|> FedEndpointWithMods
           '[ MakesFederatedCall 'Galley "on-conversation-updated",
              MakesFederatedCall 'Galley "on-mls-message-sent",
              MakesFederatedCall 'Galley "on-new-remote-conversation"
            ]
           "update-conversation"
           ConversationUpdateRequest
           ConversationUpdateResponse
    :<|> FedEndpoint "mls-welcome" MLSWelcomeRequest MLSWelcomeResponse
    :<|> FedEndpoint "on-mls-message-sent" RemoteMLSMessage RemoteMLSMessageResponse
    :<|> FedEndpointWithMods
           '[ MakesFederatedCall 'Galley "on-conversation-updated",
              MakesFederatedCall 'Galley "on-mls-message-sent",
              MakesFederatedCall 'Galley "on-new-remote-conversation",
              MakesFederatedCall 'Galley "send-mls-message",
              MakesFederatedCall 'Brig "get-mls-clients"
            ]
           "send-mls-message"
           MLSMessageSendRequest
           MLSMessageResponse
    :<|> FedEndpointWithMods
           '[ MakesFederatedCall 'Galley "mls-welcome",
              MakesFederatedCall 'Galley "on-conversation-updated",
              MakesFederatedCall 'Galley "on-mls-message-sent",
              MakesFederatedCall 'Galley "on-new-remote-conversation",
              MakesFederatedCall 'Galley "send-mls-commit-bundle",
              MakesFederatedCall 'Brig "get-mls-clients"
            ]
           "send-mls-commit-bundle"
           MLSMessageSendRequest
           MLSMessageResponse
    :<|> FedEndpoint "query-group-info" GetGroupInfoRequest GetGroupInfoResponse
    :<|> FedEndpointWithMods
           '[ MakesFederatedCall 'Galley "on-mls-message-sent"
            ]
           "on-client-removed"
           ClientRemovedRequest
           EmptyResponse
    :<|> FedEndpointWithMods
           '[ MakesFederatedCall 'Galley "on-typing-indicator-updated"
            ]
           "update-typing-indicator"
           TypingDataUpdateRequest
           TypingDataUpdateResponse
    :<|> FedEndpoint "on-typing-indicator-updated" TypingDataUpdated EmptyResponse

data TypingDataUpdateRequest = TypingDataUpdateRequest
  { tdurTypingStatus :: TypingStatus,
    tdurUserId :: UserId,
    tdurConvId :: ConvId
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via (CustomEncoded TypingDataUpdateRequest)

data TypingDataUpdateResponse
  = TypingDataUpdateSuccess TypingDataUpdated
  | TypingDataUpdateError GalleyError
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via (CustomEncoded TypingDataUpdateResponse)

data TypingDataUpdated = TypingDataUpdated
  { tudTime :: UTCTime,
    tudOrigUserId :: Qualified UserId,
    -- | Implicitely qualified by sender's domain
    tudConvId :: ConvId,
    -- | Implicitely qualified by receiver's domain
    tudUsersInConv :: [UserId],
    tudTypingStatus :: TypingStatus
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via (CustomEncoded TypingDataUpdated)

data ClientRemovedRequest = ClientRemovedRequest
  { crrUser :: UserId,
    crrClient :: ClientId,
    crrConvs :: [ConvId]
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform ClientRemovedRequest)
  deriving (FromJSON, ToJSON) via (CustomEncoded ClientRemovedRequest)

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
-- fields (muted\/archived\/hidden) are omitted, since they are not known by the
-- remote backend.
data RemoteConversation = RemoteConversation
  { -- | Id of the conversation, implicitly qualified with the domain of the
    -- backend that created this value.
    rcnvId :: ConvId,
    rcnvMetadata :: ConversationMetadata,
    rcnvMembers :: RemoteConvMembers,
    rcnvProtocol :: Protocol
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
data ConversationCreated conv = ConversationCreated
  { -- | The time when the conversation was created
    ccTime :: UTCTime,
    -- | The user that created the conversation. This is implicitly qualified
    -- by the requesting domain, since it is impossible to create a regular/group
    -- conversation on a remote backend.
    ccOrigUserId :: UserId,
    -- | The conversation ID, local to the backend invoking the RPC
    ccCnvId :: conv,
    -- | The conversation type
    ccCnvType :: ConvType,
    ccCnvAccess :: [Access],
    ccCnvAccessRoles :: Set AccessRole,
    -- | The conversation name,
    ccCnvName :: Maybe Text,
    -- | Members of the conversation apart from the creator
    ccNonCreatorMembers :: Set OtherMember,
    ccMessageTimer :: Maybe Milliseconds,
    ccReceiptMode :: Maybe ReceiptMode,
    ccProtocol :: Protocol
  }
  deriving stock (Eq, Show, Generic, Functor)
  deriving (ToJSON, FromJSON) via (CustomEncoded (ConversationCreated conv))

ccRemoteOrigUserId :: ConversationCreated (Remote ConvId) -> Remote UserId
ccRemoteOrigUserId cc = qualifyAs (ccCnvId cc) (ccOrigUserId cc)

data NewRemoteConversation = NewRemoteConversation
  { -- | The conversation ID, local to the backend invoking the RPC.
    nrcConvId :: ConvId,
    -- | The conversation protocol.
    nrcProtocol :: Protocol
  }
  deriving stock (Eq, Show, Generic)
  deriving (ToJSON, FromJSON) via (CustomEncoded NewRemoteConversation)

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

data RemoteMLSMessage = RemoteMLSMessage
  { rmmTime :: UTCTime,
    rmmMetadata :: MessageMetadata,
    rmmSender :: Qualified UserId,
    rmmConversation :: ConvId,
    rmmRecipients :: [(UserId, ClientId)],
    rmmMessage :: Base64ByteString
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform RemoteMLSMessage)
  deriving (ToJSON, FromJSON) via (CustomEncoded RemoteMLSMessage)

data RemoteMLSMessageResponse
  = RemoteMLSMessageOk
  | RemoteMLSMessageMLSNotEnabled
  deriving stock (Eq, Show, Generic)
  deriving (ToJSON, FromJSON) via (CustomEncoded RemoteMLSMessageResponse)

data ProteusMessageSendRequest = ProteusMessageSendRequest
  { -- | Conversation is assumed to be owned by the target domain, this allows
    -- us to protect against relay attacks
    pmsrConvId :: ConvId,
    -- | Sender is assumed to be owned by the origin domain, this allows us to
    -- protect against spoofing attacks
    pmsrSender :: UserId,
    pmsrRawMessage :: Base64ByteString
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform ProteusMessageSendRequest)
  deriving (ToJSON, FromJSON) via (CustomEncoded ProteusMessageSendRequest)

data MLSMessageSendRequest = MLSMessageSendRequest
  { -- | Conversation (or sub conversation) is assumed to be owned by the target
    -- domain, this allows us to protect against relay attacks
    mmsrConvOrSubId :: ConvOrSubConvId,
    -- | Sender is assumed to be owned by the origin domain, this allows us to
    -- protect against spoofing attacks
    mmsrSender :: UserId,
    mmsrRawMessage :: Base64ByteString
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform MLSMessageSendRequest)
  deriving (ToJSON, FromJSON) via (CustomEncoded MLSMessageSendRequest)

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

data ConversationUpdateResponse
  = ConversationUpdateResponseError GalleyError
  | ConversationUpdateResponseUpdate ConversationUpdate
  | ConversationUpdateResponseNoChanges
  deriving stock (Eq, Show, Generic)
  deriving
    (ToJSON, FromJSON)
    via (CustomEncoded ConversationUpdateResponse)

-- | A wrapper around a raw welcome message
newtype MLSWelcomeRequest = MLSWelcomeRequest
  { unMLSWelcomeRequest :: Base64ByteString
  }
  deriving stock (Eq, Generic, Show)
  deriving (Arbitrary) via (GenericUniform MLSWelcomeRequest)
  deriving (FromJSON, ToJSON) via (CustomEncoded MLSWelcomeRequest)

data MLSWelcomeResponse
  = MLSWelcomeSent
  | MLSWelcomeMLSNotEnabled
  deriving stock (Eq, Generic, Show)
  deriving (FromJSON, ToJSON) via (CustomEncoded MLSWelcomeResponse)

data MLSMessageResponse
  = MLSMessageResponseError GalleyError
  | MLSMessageResponseProtocolError Text
  | MLSMessageResponseProposalFailure Wai.Error
  | MLSMessageResponseUpdates [ConversationUpdate] UnreachableUserList
  deriving stock (Eq, Show, Generic)
  deriving (ToJSON, FromJSON) via (CustomEncoded MLSMessageResponse)

data GetGroupInfoRequest = GetGroupInfoRequest
  { -- | Conversation is assumed to be owned by the target domain, this allows
    -- us to protect against relay attacks
    ggireqConv :: ConvId,
    -- | Sender is assumed to be owned by the origin domain, this allows us to
    -- protect against spoofing attacks
    ggireqSender :: UserId
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform GetGroupInfoRequest)
  deriving (ToJSON, FromJSON) via (CustomEncoded GetGroupInfoRequest)

data GetGroupInfoResponse
  = GetGroupInfoResponseError GalleyError
  | GetGroupInfoResponseState Base64ByteString
  deriving stock (Eq, Show, Generic)
  deriving (ToJSON, FromJSON) via (CustomEncoded GetGroupInfoResponse)
