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
import Data.Domain
import Data.Id
import Data.Json.Util
import Data.List.NonEmpty (NonEmpty)
import Data.Misc (Milliseconds)
import Data.OpenApi (OpenApi, ToSchema)
import Data.Proxy (Proxy (Proxy))
import Data.Qualified
import Data.Range
import Data.Time.Clock (UTCTime)
import Imports
import Network.Wai.Utilities.JSONResponse
import Servant.API
import Servant.OpenApi (HasOpenApi (toOpenApi))
import Wire.API.Conversation
import Wire.API.Conversation.Action
import Wire.API.Conversation.Protocol
import Wire.API.Conversation.Role (RoleName)
import Wire.API.Conversation.Typing
import Wire.API.Error.Galley
import Wire.API.Federation.API.Common
import Wire.API.Federation.Endpoint
import Wire.API.MLS.SubConversation
import Wire.API.MakesFederatedCall
import Wire.API.Message
import Wire.API.Routes.Public.Galley.Messaging
import Wire.API.Util.Aeson (CustomEncoded (..), CustomEncodedLensable (..))
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
  FedEndpoint "on-conversation-created" (ConversationCreated ConvId) EmptyResponse
    -- This endpoint is called the first time a user from this backend is
    -- added to a remote conversation.
    :<|> FedEndpoint "get-conversations" GetConversationsRequest GetConversationsResponse
    -- used by the backend that owns a conversation to inform this backend of
    -- changes to the conversation
    :<|> FedEndpoint "on-conversation-updated" ConversationUpdate EmptyResponse
    :<|> FedEndpointWithMods
           '[ MakesFederatedCall 'Galley "on-conversation-updated",
              MakesFederatedCall 'Galley "on-mls-message-sent",
              MakesFederatedCall 'Brig "get-users-by-ids",
              MakesFederatedCall 'Brig "api-version"
            ]
           "leave-conversation"
           LeaveConversationRequest
           LeaveConversationResponse
    -- used to notify this backend that a new message has been posted to a
    -- remote conversation
    :<|> FedEndpoint "on-message-sent" (RemoteMessage ConvId) EmptyResponse
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
              MakesFederatedCall 'Brig "api-version"
            ]
           "on-user-deleted-conversations"
           UserDeletedConversationsNotification
           EmptyResponse
    :<|> FedEndpointWithMods
           '[ MakesFederatedCall 'Galley "on-conversation-updated",
              MakesFederatedCall 'Galley "on-mls-message-sent",
              MakesFederatedCall 'Brig "get-users-by-ids",
              MakesFederatedCall 'Galley "on-mls-message-sent"
            ]
           "update-conversation"
           ConversationUpdateRequest
           ConversationUpdateResponse
    :<|> FedEndpoint "mls-welcome" MLSWelcomeRequest MLSWelcomeResponse
    :<|> FedEndpoint "on-mls-message-sent" RemoteMLSMessage EmptyResponse
    :<|> FedEndpointWithMods
           '[ MakesFederatedCall 'Galley "on-conversation-updated",
              MakesFederatedCall 'Galley "on-mls-message-sent",
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
              MakesFederatedCall 'Galley "send-mls-commit-bundle",
              MakesFederatedCall 'Brig "get-mls-clients",
              MakesFederatedCall 'Brig "get-users-by-ids",
              MakesFederatedCall 'Brig "api-version"
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
    :<|> FedEndpoint "get-sub-conversation" GetSubConversationsRequest GetSubConversationsResponse
    :<|> FedEndpointWithMods
           '[
            ]
           "delete-sub-conversation"
           DeleteSubConversationFedRequest
           DeleteSubConversationResponse
    :<|> FedEndpointWithMods
           '[ MakesFederatedCall 'Galley "on-mls-message-sent"
            ]
           "leave-sub-conversation"
           LeaveSubConversationRequest
           LeaveSubConversationResponse
    :<|> FedEndpoint
           "get-one2one-conversation"
           GetOne2OneConversationRequest
           GetOne2OneConversationResponse

data TypingDataUpdateRequest = TypingDataUpdateRequest
  { typingStatus :: TypingStatus,
    userId :: UserId,
    convId :: ConvId
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via (CustomEncoded TypingDataUpdateRequest)

instance ToSchema TypingDataUpdateRequest

data TypingDataUpdateResponse
  = TypingDataUpdateSuccess TypingDataUpdated
  | TypingDataUpdateError GalleyError
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via (CustomEncoded TypingDataUpdateResponse)

instance ToSchema TypingDataUpdateResponse

data TypingDataUpdated = TypingDataUpdated
  { time :: UTCTime,
    origUserId :: Qualified UserId,
    -- | Implicitely qualified by sender's domain
    convId :: ConvId,
    -- | Implicitely qualified by receiver's domain
    usersInConv :: [UserId],
    typingStatus :: TypingStatus
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via (CustomEncoded TypingDataUpdated)

instance ToSchema TypingDataUpdated

data ClientRemovedRequest = ClientRemovedRequest
  { user :: UserId,
    client :: ClientId,
    convs :: [ConvId]
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform ClientRemovedRequest)
  deriving (FromJSON, ToJSON) via (CustomEncoded ClientRemovedRequest)

instance ToSchema ClientRemovedRequest

data GetConversationsRequest = GetConversationsRequest
  { userId :: UserId,
    convIds :: [ConvId]
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform GetConversationsRequest)
  deriving (ToJSON, FromJSON) via (CustomEncoded GetConversationsRequest)

instance ToSchema GetConversationsRequest

data GetOne2OneConversationRequest = GetOne2OneConversationRequest
  { -- The user on the sender's domain
    goocSenderUser :: UserId,
    -- The user on the receiver's domain
    goocReceiverUser :: UserId
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform GetOne2OneConversationRequest)
  deriving (ToJSON, FromJSON) via (CustomEncoded GetOne2OneConversationRequest)

instance ToSchema GetOne2OneConversationRequest

data RemoteConvMembers = RemoteConvMembers
  { selfRole :: RoleName,
    others :: [OtherMember]
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform RemoteConvMembers)
  deriving (FromJSON, ToJSON) via (CustomEncoded RemoteConvMembers)

instance ToSchema RemoteConvMembers

-- | A conversation hosted on a remote backend. This contains the same
-- information as a 'Conversation', with the exception that conversation status
-- fields (muted\/archived\/hidden) are omitted, since they are not known by the
-- remote backend.
data RemoteConversation = RemoteConversation
  { -- | Id of the conversation, implicitly qualified with the domain of the
    -- backend that created this value.
    id :: ConvId,
    metadata :: ConversationMetadata,
    members :: RemoteConvMembers,
    protocol :: Protocol
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform RemoteConversation)
  deriving (FromJSON, ToJSON) via (CustomEncoded RemoteConversation)

instance ToSchema RemoteConversation

newtype GetConversationsResponse = GetConversationsResponse
  { convs :: [RemoteConversation]
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform GetConversationsResponse)
  deriving (ToJSON, FromJSON) via (CustomEncoded GetConversationsResponse)

instance ToSchema GetConversationsResponse

data GetOne2OneConversationResponse
  = GetOne2OneConversationOk RemoteConversation
  | -- | This is returned when the local backend is asked for a 1-1 conversation
    -- that should reside on the other backend.
    GetOne2OneConversationBackendMismatch
  | -- | This is returned when a 1-1 conversation between two unconnected users
    -- is requested.
    GetOne2OneConversationNotConnected
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform GetOne2OneConversationResponse)
  deriving (ToJSON, FromJSON) via (CustomEncoded GetOne2OneConversationResponse)

instance ToSchema GetOne2OneConversationResponse

-- | A record type describing a new federated conversation
--
-- FUTUREWORK: Think about extracting common conversation metadata into a
-- separarate data type that can be reused in several data types in this module.
data ConversationCreated conv = ConversationCreated
  { -- | The time when the conversation was created
    time :: UTCTime,
    -- | The user that created the conversation. This is implicitly qualified
    -- by the requesting domain, since it is impossible to create a regular/group
    -- conversation on a remote backend.
    origUserId :: UserId,
    -- | The conversation ID, local to the backend invoking the RPC
    cnvId :: conv,
    -- | The conversation type
    cnvType :: ConvType,
    cnvAccess :: [Access],
    cnvAccessRoles :: Set AccessRole,
    -- | The conversation name,
    cnvName :: Maybe Text,
    -- | Members of the conversation apart from the creator
    nonCreatorMembers :: Set OtherMember,
    messageTimer :: Maybe Milliseconds,
    receiptMode :: Maybe ReceiptMode,
    protocol :: Protocol
  }
  deriving stock (Eq, Show, Generic, Functor)
  deriving (ToJSON, FromJSON) via (CustomEncoded (ConversationCreated conv))

instance (ToSchema a) => ToSchema (ConversationCreated a)

ccRemoteOrigUserId :: ConversationCreated (Remote ConvId) -> Remote UserId
ccRemoteOrigUserId cc = qualifyAs cc.cnvId cc.origUserId

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

instance ToSchema ConversationUpdate

data LeaveConversationRequest = LeaveConversationRequest
  { -- | The conversation is assumed to be owned by the target domain, which
    -- allows us to protect against relay attacks
    convId :: ConvId,
    -- | The leaver is assumed to be owned by the origin domain, which allows us
    -- to protect against spoofing attacks
    leaver :: UserId
  }
  deriving stock (Generic, Eq, Show)
  deriving (ToJSON, FromJSON) via (CustomEncoded LeaveConversationRequest)

instance ToSchema LeaveConversationRequest

-- | Error outcomes of the leave-conversation RPC.
data RemoveFromConversationError
  = RemoveFromConversationErrorRemovalNotAllowed
  | RemoveFromConversationErrorNotFound
  | RemoveFromConversationErrorUnchanged
  deriving stock (Eq, Show, Generic)
  deriving (ToJSON, FromJSON) via (CustomEncoded RemoveFromConversationError)

instance ToSchema RemoveFromConversationError

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

data RemoteMLSMessageResponse
  = RemoteMLSMessageOk
  | RemoteMLSMessageMLSNotEnabled
  deriving stock (Eq, Show, Generic)
  deriving (ToJSON, FromJSON) via (CustomEncoded RemoteMLSMessageResponse)

instance ToSchema RemoteMLSMessageResponse

data ProteusMessageSendRequest = ProteusMessageSendRequest
  { -- | Conversation is assumed to be owned by the target domain, this allows
    -- us to protect against relay attacks
    convId :: ConvId,
    -- | Sender is assumed to be owned by the origin domain, this allows us to
    -- protect against spoofing attacks
    sender :: UserId,
    rawMessage :: Base64ByteString
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform ProteusMessageSendRequest)
  deriving (ToJSON, FromJSON) via (CustomEncoded ProteusMessageSendRequest)

instance ToSchema ProteusMessageSendRequest

data MLSMessageSendRequest = MLSMessageSendRequest
  { -- | Conversation (or sub conversation) is assumed to be owned by the target
    -- domain, this allows us to protect against relay attacks
    convOrSubId :: ConvOrSubConvId,
    -- | Sender is assumed to be owned by the origin domain, this allows us to
    -- protect against spoofing attacks
    sender :: UserId,
    senderClient :: ClientId,
    rawMessage :: Base64ByteString
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform MLSMessageSendRequest)
  deriving (ToJSON, FromJSON) via (CustomEncoded MLSMessageSendRequest)

instance ToSchema MLSMessageSendRequest

newtype MessageSendResponse = MessageSendResponse
  {response :: PostOtrResponse MessageSendingStatus}
  deriving stock (Eq, Show, Generic)
  deriving
    (ToJSON, FromJSON)
    via ( Either
            (CustomEncoded (MessageNotSent MessageSendingStatus))
            MessageSendingStatus
        )

instance ToSchema MessageSendResponse

newtype LeaveConversationResponse = LeaveConversationResponse
  {response :: Either RemoveFromConversationError ()}
  deriving stock (Eq, Show, Generic)
  deriving
    (ToJSON, FromJSON)
    via (Either (CustomEncoded RemoveFromConversationError) ())

instance ToSchema LeaveConversationResponse

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

data ConversationUpdateRequest = ConversationUpdateRequest
  { -- | The user that is attempting to perform the action. This is qualified
    -- implicitly by the origin domain
    user :: UserId,
    -- | Id of conversation the action should be performed on. The is qualified
    -- implicity by the owning backend which receives this request.
    convId :: ConvId,
    action :: SomeConversationAction
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform ConversationUpdateRequest)
  deriving (FromJSON, ToJSON) via (CustomEncoded ConversationUpdateRequest)

instance ToSchema ConversationUpdateRequest

data ConversationUpdateResponse
  = ConversationUpdateResponseError GalleyError
  | ConversationUpdateResponseUpdate ConversationUpdate
  | ConversationUpdateResponseNoChanges
  | ConversationUpdateResponseNonFederatingBackends NonFederatingBackends
  | ConversationUpdateResponseUnreachableBackends UnreachableBackends
  deriving stock (Eq, Show, Generic)
  deriving
    (ToJSON, FromJSON)
    via (CustomEncoded ConversationUpdateResponse)

instance ToSchema ConversationUpdateResponse

-- | A wrapper around a raw welcome message
data MLSWelcomeRequest = MLSWelcomeRequest
  { -- | Implicitely qualified by origin domain
    originatingUser :: UserId,
    -- | A serialised welcome message.
    welcomeMessage :: Base64ByteString,
    -- | Recipients local to the target backend.
    recipients :: [(UserId, ClientId)],
    -- | The conversation id, qualified to the owning domain
    qualifiedConvId :: Qualified ConvId
  }
  deriving stock (Eq, Generic, Show)
  deriving (Arbitrary) via (GenericUniform MLSWelcomeRequest)
  deriving (FromJSON, ToJSON) via (CustomEncoded MLSWelcomeRequest)

instance ToSchema MLSWelcomeRequest

data MLSWelcomeResponse
  = MLSWelcomeSent
  | MLSWelcomeMLSNotEnabled
  deriving stock (Eq, Generic, Show)
  deriving (FromJSON, ToJSON) via (CustomEncoded MLSWelcomeResponse)

instance ToSchema MLSWelcomeResponse

data MLSMessageResponse
  = MLSMessageResponseError GalleyError
  | MLSMessageResponseProtocolError Text
  | MLSMessageResponseProposalFailure JSONResponse
  | -- | The conversation-owning backend could not reach some of the backends that
    -- have users in the conversation when processing a commit.
    MLSMessageResponseUnreachableBackends (Set Domain)
  | -- | If the list of unreachable users is non-empty, it corresponds to users
    -- that an application message could not be sent to.
    MLSMessageResponseUpdates [ConversationUpdate]
  | MLSMessageResponseNonFederatingBackends NonFederatingBackends
  deriving stock (Eq, Show, Generic)
  deriving (ToJSON, FromJSON) via (CustomEncoded MLSMessageResponse)

instance ToSchema MLSMessageResponse

data GetGroupInfoRequest = GetGroupInfoRequest
  { -- | Conversation (or subconversation) is assumed to be owned by the target
    -- domain, this allows us to protect against relay attacks
    conv :: ConvOrSubConvId,
    -- | Sender is assumed to be owned by the origin domain, this allows us to
    -- protect against spoofing attacks
    sender :: UserId
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform GetGroupInfoRequest)
  deriving (ToJSON, FromJSON) via (CustomEncoded GetGroupInfoRequest)

instance ToSchema GetGroupInfoRequest

data GetGroupInfoResponse
  = GetGroupInfoResponseError GalleyError
  | GetGroupInfoResponseState Base64ByteString
  deriving stock (Eq, Show, Generic)
  deriving (ToJSON, FromJSON) via (CustomEncoded GetGroupInfoResponse)

instance ToSchema GetGroupInfoResponse

data GetSubConversationsRequest = GetSubConversationsRequest
  { gsreqUser :: UserId,
    gsreqConv :: ConvId,
    gsreqSubConv :: SubConvId
  }
  deriving stock (Eq, Show, Generic)
  deriving (ToJSON, FromJSON) via (CustomEncoded GetSubConversationsRequest)

instance ToSchema GetSubConversationsRequest

data GetSubConversationsResponse
  = GetSubConversationsResponseError GalleyError
  | GetSubConversationsResponseSuccess PublicSubConversation
  deriving stock (Eq, Show, Generic)
  deriving (ToJSON, FromJSON) via (CustomEncoded GetSubConversationsResponse)

instance ToSchema GetSubConversationsResponse

data LeaveSubConversationRequest = LeaveSubConversationRequest
  { lscrUser :: UserId,
    lscrClient :: ClientId,
    lscrConv :: ConvId,
    lscrSubConv :: SubConvId
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform LeaveSubConversationRequest)
  deriving (ToJSON, FromJSON) via (CustomEncoded LeaveSubConversationRequest)

instance ToSchema LeaveSubConversationRequest

data LeaveSubConversationResponse
  = LeaveSubConversationResponseError GalleyError
  | LeaveSubConversationResponseProtocolError Text
  | LeaveSubConversationResponseOk
  deriving stock (Eq, Show, Generic)
  deriving (ToJSON, FromJSON) via (CustomEncoded LeaveSubConversationResponse)

instance ToSchema LeaveSubConversationResponse

data DeleteSubConversationFedRequest = DeleteSubConversationFedRequest
  { dscreqUser :: UserId,
    dscreqConv :: ConvId,
    dscreqSubConv :: SubConvId,
    dscreqGroupId :: GroupId,
    dscreqEpoch :: Epoch
  }
  deriving stock (Eq, Show, Generic)
  deriving (ToJSON, FromJSON) via (CustomEncoded DeleteSubConversationFedRequest)

instance ToSchema DeleteSubConversationFedRequest

data DeleteSubConversationResponse
  = DeleteSubConversationResponseError GalleyError
  | DeleteSubConversationResponseSuccess
  deriving stock (Eq, Show, Generic)
  deriving (ToJSON, FromJSON) via (CustomEncoded DeleteSubConversationResponse)

instance ToSchema DeleteSubConversationResponse

swaggerDoc :: OpenApi
swaggerDoc = toOpenApi (Proxy @GalleyApi)
