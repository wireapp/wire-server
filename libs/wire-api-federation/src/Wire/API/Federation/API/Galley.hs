{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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

module Wire.API.Federation.API.Galley where

import Control.Monad.Except (MonadError (..))
import Data.Aeson (FromJSON, ToJSON)
import Data.Id (ClientId, ConvId, UserId)
import Data.Json.Util (Base64ByteString)
import Data.List1 (List1)
import Data.Misc (Milliseconds)
import Data.Qualified (Qualified)
import Data.Time.Clock (UTCTime)
import Imports
import Servant.API (JSON, Post, ReqBody, StdMethod (DELETE), Summary, (:>))
import Servant.API.Generic ((:-))
import Servant.Client.Generic (AsClientT, genericClient)
import Wire.API.Arbitrary (Arbitrary, GenericUniform (..))
import Wire.API.Conversation (Access, AccessRole, ConvType, Conversation, ReceiptMode)
import Wire.API.Conversation.Member (OtherMember)
import Wire.API.Conversation.Role (RoleName)
import Wire.API.Federation.Client (FederationClientFailure, FederatorClient)
import Wire.API.Federation.Domain (OriginDomainHeader)
import qualified Wire.API.Federation.GRPC.Types as Proto
import Wire.API.Federation.Util.Aeson (CustomEncoded (..))
import Wire.API.Message (MessageNotSent, MessageSendingStatus, PostOtrResponse, Priority)
import Wire.API.Routes.MultiVerb (MultiVerb)
import Wire.API.Routes.Public.Galley.Responses (RemoveFromConversation, RemoveFromConversationResponses)
import Wire.API.User.Client (UserClientMap)

-- FUTUREWORK: data types, json instances, more endpoints. See
-- https://wearezeta.atlassian.net/wiki/spaces/CORE/pages/356090113/Federation+Galley+Conversation+API
-- for the current list we need.

data Api routes = Api
  { -- | Register a new conversation
    registerConversation ::
      routes
        :- "federation"
        :> Summary "Register users to be in a new remote conversation"
        :> "register-conversation"
        :> ReqBody '[JSON] RegisterConversation
        :> Post '[JSON] (),
    getConversations ::
      routes
        :- "federation"
        :> "get-conversations"
        :> ReqBody '[JSON] GetConversationsRequest
        :> Post '[JSON] GetConversationsResponse,
    -- used by backend that owns the conversation to inform the backend about
    -- add/removal of its users to the conversation
    updateConversationMemberships ::
      routes
        :- "federation"
        :> "update-conversation-memberships"
        :> ReqBody '[JSON] ConversationMemberUpdate
        :> Post '[JSON] (),
    leaveConversation ::
      routes
        :- "federation"
        :> "leave-conversation"
        :> OriginDomainHeader
        :> ReqBody '[JSON] LeaveConversation
        :> MultiVerb 'DELETE '[JSON] RemoveFromConversationResponses RemoveFromConversation,
    -- used to notify this backend that a new message has been posted to a
    -- remote conversation
    receiveMessage ::
      routes
        :- "federation"
        :> "receive-message"
        :> OriginDomainHeader
        :> ReqBody '[JSON] (RemoteMessage ConvId)
        :> Post '[JSON] (),
    -- used by a remote backend to send a message to a conversation owned by
    -- this backend
    sendMessage ::
      routes
        :- "federation"
        :> "send-message"
        :> OriginDomainHeader
        :> ReqBody '[JSON] MessageSendRequest
        :> Post '[JSON] MessageSendResponse,
    removeMembers ::
      routes
        :- "federation"
        :> "remove-members"
        :> OriginDomainHeader
        :> ReqBody '[JSON] RemoveMembersRequest
        :> MultiVerb 'DELETE '[JSON] RemoveFromConversationResponses RemoveFromConversation
  }
  deriving (Generic)

data GetConversationsRequest = GetConversationsRequest
  { gcrUserId :: Qualified UserId,
    gcrConvIds :: [ConvId]
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform GetConversationsRequest)
  deriving (ToJSON, FromJSON) via (CustomEncoded GetConversationsRequest)

newtype GetConversationsResponse = GetConversationsResponse
  { gcresConvs :: [Conversation]
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform GetConversationsResponse)
  deriving (ToJSON, FromJSON) via (CustomEncoded GetConversationsResponse)

-- | A record type describing a new federated conversation
--
-- FUTUREWORK: Think about extracting common conversation metadata into a
-- separarate data type that can be reused in several data types in this module.
data RegisterConversation = MkRegisterConversation
  { -- | The time when the conversation was created
    rcTime :: UTCTime,
    -- | The user that created the conversation
    rcOrigUserId :: Qualified UserId,
    -- | The qualified conversation ID
    rcCnvId :: Qualified ConvId,
    -- | The conversation type
    rcCnvType :: ConvType,
    rcCnvAccess :: [Access],
    rcCnvAccessRole :: AccessRole,
    -- | The conversation name,
    rcCnvName :: Maybe Text,
    -- | Members of the conversation
    rcMembers :: Set OtherMember,
    rcMessageTimer :: Maybe Milliseconds,
    rcReceiptMode :: Maybe ReceiptMode
  }
  deriving stock (Eq, Show, Generic)
  deriving (ToJSON, FromJSON) via (CustomEncoded RegisterConversation)

-- | A conversation membership update, as given by ' ConversationMemberUpdate',
-- can be either a member addition or removal.
data ConversationMembersAction
  = ConversationMembersActionAdd (List1 (Qualified UserId, RoleName))
  | ConversationMembersActionRemove (List1 (Qualified UserId))
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform ConversationMembersAction)
  deriving (ToJSON, FromJSON) via (CustomEncoded ConversationMembersAction)

data ConversationMemberUpdate = ConversationMemberUpdate
  { cmuTime :: UTCTime,
    cmuOrigUserId :: Qualified UserId,
    cmuConvId :: Qualified ConvId,
    -- | A list of users from a remote backend that need to be sent
    -- notifications about this change. This is required as we do not expect a
    -- non-conversation owning backend to have an indexed mapping of
    -- conversation to users.
    cmuAlreadyPresentUsers :: [UserId],
    -- | Users that got either added to or removed from the conversation.
    cmuAction :: ConversationMembersAction
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform ConversationMemberUpdate)
  deriving (ToJSON, FromJSON) via (CustomEncoded ConversationMemberUpdate)

data LeaveConversation = LeaveConversation
  { -- | The conversation is assumed to be owned by the target domain, which
    -- allows us to protect against relay attacks
    lcConvId :: ConvId,
    -- | The leaver is assumed to be owned by the origin domain, which allows us
    -- to protect against spoofing attacks
    lcLeaver :: UserId
  }
  deriving stock (Generic)
  deriving (ToJSON, FromJSON) via (CustomEncoded LeaveConversation)

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

data RemoveMembersRequest = RemoveMembersRequest
  { -- | The converastion is assumed to be owned by the target domain, which
    -- allows us to protect against relay attacks
    rmrConvId :: ConvId,
    -- | The remover is assumed to be owned by the origin domain, which allows
    -- us to protect against spoofing attacks
    rmrRemover :: UserId,
    -- | The set of conversation members to be removed
    rmrMemberList :: Set (Qualified UserId)
  }
  deriving stock (Generic)
  deriving (ToJSON, FromJSON) via (CustomEncoded RemoveMembersRequest)

clientRoutes :: (MonadError FederationClientFailure m, MonadIO m) => Api (AsClientT (FederatorClient 'Proto.Galley m))
clientRoutes = genericClient
