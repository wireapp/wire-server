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

module Wire.API.Federation.API.Galley
  ( module Wire.API.Federation.API.Galley,
    module Notifications,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Domain
import Data.Id
import Data.Json.Util
import Data.Misc (Milliseconds)
import Data.OpenApi (OpenApi, ToSchema)
import Data.Proxy (Proxy (Proxy))
import Data.Qualified
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
import Wire.API.Federation.API.Galley.Notifications as Notifications
import Wire.API.Federation.Endpoint
import Wire.API.Federation.Version
import Wire.API.MLS.Keys
import Wire.API.MLS.SubConversation
import Wire.API.Message
import Wire.API.Routes.Named
import Wire.API.Routes.Public.Galley.Messaging
import Wire.API.Routes.SpecialiseToVersion
import Wire.API.Routes.Version qualified as ClientAPI
import Wire.API.Routes.Versioned qualified as ClientAPI
import Wire.API.Util.Aeson (CustomEncoded (..))
import Wire.API.VersionInfo
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
    :<|> Named
           "get-conversations@v1"
           ( UnnamedFedEndpointWithMods
               '[Until 'V2]
               "get-conversations"
               GetConversationsRequest
               GetConversationsResponse
           )
    :<|> FedEndpointWithMods
           '[From 'V2]
           "get-conversations"
           GetConversationsRequest
           GetConversationsResponseV2
    :<|> FedEndpoint
           "leave-conversation"
           LeaveConversationRequest
           LeaveConversationResponse
    -- used by a remote backend to send a message to a conversation owned by
    -- this backend
    :<|> FedEndpoint
           "send-message"
           ProteusMessageSendRequest
           MessageSendResponse
    :<|> FedEndpoint
           "update-conversation"
           ConversationUpdateRequest
           ConversationUpdateResponse
    :<|> FedEndpoint "mls-welcome" MLSWelcomeRequest MLSWelcomeResponse
    :<|> FedEndpoint
           "send-mls-message"
           MLSMessageSendRequest
           MLSMessageResponse
    :<|> FedEndpoint
           "send-mls-commit-bundle"
           MLSMessageSendRequest
           MLSMessageResponse
    :<|> FedEndpoint "query-group-info" GetGroupInfoRequest GetGroupInfoResponse
    :<|> FedEndpointWithMods
           '[
            ]
           "update-typing-indicator"
           TypingDataUpdateRequest
           TypingDataUpdateResponse
    :<|> FedEndpoint "on-typing-indicator-updated" TypingDataUpdated EmptyResponse
    :<|> FedEndpointWithMods
           '[ From 'V1
            ]
           "get-sub-conversation"
           GetSubConversationsRequest
           GetSubConversationsResponse
    :<|> FedEndpointWithMods
           '[ From 'V1
            ]
           "delete-sub-conversation"
           DeleteSubConversationFedRequest
           DeleteSubConversationResponse
    :<|> FedEndpointWithMods
           '[ From 'V1
            ]
           "leave-sub-conversation"
           LeaveSubConversationRequest
           LeaveSubConversationResponse
    :<|> Named
           "get-one2one-conversation@v1"
           ( UnnamedFedEndpointWithMods
               '[From 'V1, Until 'V2]
               "get-one2one-conversation"
               GetOne2OneConversationRequest
               GetOne2OneConversationResponse
           )
    :<|> FedEndpointWithMods
           '[From 'V2]
           "get-one2one-conversation"
           GetOne2OneConversationRequest
           GetOne2OneConversationResponseV2
    -- All the notification endpoints that go through the queue-based
    -- federation client ('fedQueueClient').
    :<|> GalleyNotificationAPI

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
    protocol :: ClientAPI.Versioned 'ClientAPI.V5 Protocol
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform RemoteConversation)
  deriving (FromJSON, ToJSON) via (CustomEncoded RemoteConversation)

instance ToSchema RemoteConversation

-- | A conversation hosted on a remote backend. This contains the same
-- information as a 'Conversation', with the exception that conversation status
-- fields (muted\/archived\/hidden) are omitted, since they are not known by the
-- remote backend.
data RemoteConversationV2 = RemoteConversationV2
  { -- | Id of the conversation, implicitly qualified with the domain of the
    -- backend that created this value.
    id :: ConvId,
    metadata :: ConversationMetadata,
    members :: RemoteConvMembers,
    protocol :: Protocol
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform RemoteConversationV2)
  deriving (FromJSON, ToJSON) via (CustomEncoded RemoteConversationV2)

instance ToSchema RemoteConversationV2

remoteConversationFromV2 :: RemoteConversationV2 -> RemoteConversation
remoteConversationFromV2 rc =
  RemoteConversation
    { id = rc.id,
      metadata = rc.metadata,
      members = rc.members,
      protocol = ClientAPI.Versioned rc.protocol
    }

remoteConversationToV2 :: RemoteConversation -> RemoteConversationV2
remoteConversationToV2 rc =
  RemoteConversationV2
    { id = rc.id,
      metadata = rc.metadata,
      members = rc.members,
      protocol = rc.protocol.unVersioned
    }

newtype GetConversationsResponse = GetConversationsResponse
  { convs :: [RemoteConversation]
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform GetConversationsResponse)
  deriving (ToJSON, FromJSON) via (CustomEncoded GetConversationsResponse)

instance ToSchema GetConversationsResponse

newtype GetConversationsResponseV2 = GetConversationsResponseV2
  { convs :: [RemoteConversationV2]
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform GetConversationsResponseV2)
  deriving (ToJSON, FromJSON) via (CustomEncoded GetConversationsResponseV2)

instance ToSchema GetConversationsResponseV2

getConversationsResponseToV2 :: GetConversationsResponse -> GetConversationsResponseV2
getConversationsResponseToV2 res = GetConversationsResponseV2 (map remoteConversationToV2 res.convs)

getConversationsResponseFromV2 :: GetConversationsResponseV2 -> GetConversationsResponse
getConversationsResponseFromV2 res = GetConversationsResponse (map remoteConversationFromV2 res.convs)

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

data GetOne2OneConversationResponseV2
  = GetOne2OneConversationV2Ok RemoteMLSOne2OneConversation
  | -- | This is returned when the local backend is asked for a 1-1 conversation
    -- that should reside on the other backend.
    GetOne2OneConversationV2BackendMismatch
  | -- | This is returned when a 1-1 conversation between two unconnected users
    -- is requested.
    GetOne2OneConversationV2NotConnected
  | GetOne2OneConversationV2MLSNotEnabled
  deriving stock (Eq, Show, Generic)
  deriving (ToJSON, FromJSON) via (CustomEncoded GetOne2OneConversationResponseV2)

instance ToSchema GetOne2OneConversationResponseV2

data RemoteMLSOne2OneConversation = RemoteMLSOne2OneConversation
  { conversation :: RemoteConversationV2,
    publicKeys :: MLSKeysByPurpose MLSPublicKeys
  }
  deriving stock (Eq, Show, Generic)
  deriving (ToJSON, FromJSON) via (CustomEncoded RemoteMLSOne2OneConversation)

instance ToSchema RemoteMLSOne2OneConversation

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
swaggerDoc = toOpenApi (Proxy @(SpecialiseToVersion 'V1 GalleyApi))
