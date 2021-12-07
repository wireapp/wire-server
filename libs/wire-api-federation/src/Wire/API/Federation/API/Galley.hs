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

import Control.Lens (_Left, _Right)
import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Aeson as A
import Data.Id (ClientId, ConvId, UserId)
import Data.Json.Util
import Data.Misc (Milliseconds)
import Data.Proxy
import Data.Qualified
import Data.Range
import Data.Schema
import Data.Singletons (sing)
import qualified Data.Swagger as S
import Data.Time.Clock (UTCTime)
import Imports
import Servant.API (JSON, Post, ReqBody, Summary, (:>))
import Servant.API.Generic
import Servant.Swagger
import Wire.API.Arbitrary (Arbitrary, GenericUniform (..))
import Wire.API.Conversation
  ( Access,
    AccessRole,
    ConvType,
    ConversationMetadata,
    ReceiptMode,
  )
import Wire.API.Conversation.Action
import Wire.API.Conversation.Member (OtherMember)
import Wire.API.Conversation.Role (RoleName)
import Wire.API.Federation.API.Common
import Wire.API.Federation.Domain (OriginDomainHeader)
import Wire.API.Message (MessageNotSent, MessageSendingStatus, PostOtrResponse, Priority)
import Wire.API.User.Client (UserClientMap)

-- FUTUREWORK: data types, json instances, more endpoints. See
-- https://wearezeta.atlassian.net/wiki/spaces/CORE/pages/356090113/Federation+Galley+Conversation+API
-- for the current list we need.

-- | For conventions see /docs/developer/federation-api-conventions.md
data GalleyApi routes = GalleyApi
  { -- | Register a new conversation
    onConversationCreated ::
      routes
        :- Summary "Register users to be in a new remote conversation"
        :> "on-conversation-created"
        :> OriginDomainHeader
        :> ReqBody '[JSON] (NewRemoteConversation ConvId)
        :> Post '[JSON] (),
    getConversations ::
      routes
        :- Summary "Get a user's conversations"
        :> "get-conversations"
        :> OriginDomainHeader
        :> ReqBody '[JSON] GetConversationsRequest
        :> Post '[JSON] GetConversationsResponse,
    -- used by the backend that owns a conversation to inform this backend of
    -- changes to the conversation
    onConversationUpdated ::
      routes
        :- Summary "Notify of a conversation update"
        :> "on-conversation-updated"
        :> OriginDomainHeader
        :> ReqBody '[JSON] ConversationUpdate
        :> Post '[JSON] (),
    leaveConversation ::
      routes
        :- Summary "Leave a conversation hosted on a remote backend"
        :> "leave-conversation"
        :> OriginDomainHeader
        :> ReqBody '[JSON] LeaveConversationRequest
        :> Post '[JSON] LeaveConversationResponse,
    -- used to notify this backend that a new message has been posted to a
    -- remote conversation
    onMessageSent ::
      routes
        :- Summary "Notify a backend that a new message has been posted to a remote conversation"
        :> "on-message-sent"
        :> OriginDomainHeader
        :> ReqBody '[JSON] (RemoteMessage ConvId)
        :> Post '[JSON] (),
    -- used by a remote backend to send a message to a conversation owned by
    -- this backend
    sendMessage ::
      routes
        :- Summary "Send a message to a conversation owned by another backend"
        :> "send-message"
        :> OriginDomainHeader
        :> ReqBody '[JSON] MessageSendRequest
        :> Post '[JSON] MessageSendResponse,
    onUserDeleted ::
      routes
        :- Summary "Notify a remote backend's conversations that a user account got deleted"
        :- "on-user-deleted-conversations"
        :> "conversations"
        :> OriginDomainHeader
        :> ReqBody '[JSON] UserDeletedConversationsNotification
        :> Post '[JSON] EmptyResponse
  }
  deriving (Generic)

data GetConversationsRequest = GetConversationsRequest
  { gcrUserId :: UserId,
    gcrConvIds :: [ConvId]
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform GetConversationsRequest)
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema GetConversationsRequest)

instance ToSchema GetConversationsRequest where
  schema =
    object "GetConversationsRequest" $
      GetConversationsRequest
        <$> gcrUserId .= field "user_id" schema
        <*> gcrConvIds .= field "conversation_ids" (array schema)

data RemoteConvMembers = RemoteConvMembers
  { rcmSelfRole :: RoleName,
    rcmOthers :: [OtherMember]
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform RemoteConvMembers)
  deriving (FromJSON, ToJSON, S.ToSchema) via (Schema RemoteConvMembers)

instance ToSchema RemoteConvMembers where
  schema =
    object "RemoteConvMembers" $
      RemoteConvMembers
        <$> rcmSelfRole .= field "self_role" schema
        <*> rcmOthers .= field "other_members" (array schema)

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
  deriving (FromJSON, ToJSON, S.ToSchema) via (Schema RemoteConversation)

instance ToSchema RemoteConversation where
  schema =
    object "RemoteConversation" $
      RemoteConversation
        <$> rcnvId .= field "conversation_id" schema
        <*> rcnvMetadata .= field "metadata" schema
        <*> rcnvMembers .= field "members" schema

newtype GetConversationsResponse = GetConversationsResponse
  { gcresConvs :: [RemoteConversation]
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform GetConversationsResponse)
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema GetConversationsResponse)

instance ToSchema GetConversationsResponse where
  schema =
    object "GetConversationsResponse" $
      GetConversationsResponse <$> gcresConvs .= field "conversations" (array schema)

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
    rcCnvAccessRole :: AccessRole,
    -- | The conversation name,
    rcCnvName :: Maybe Text,
    -- | Members of the conversation apart from the creator
    rcNonCreatorMembers :: Set OtherMember,
    rcMessageTimer :: Maybe Milliseconds,
    rcReceiptMode :: Maybe ReceiptMode
  }
  deriving stock (Eq, Show, Generic, Functor)
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema (NewRemoteConversation conv))

instance ToSchema conv => ToSchema (NewRemoteConversation conv) where
  schema =
    object "NewRemoteConversation" $
      NewRemoteConversation
        <$> (toUTCTimeMillis . rcTime) .= fmap fromUTCTimeMillis (field "time" schema)
        <*> rcOrigUserId .= field "creator_user_id" schema
        <*> rcCnvId .= field "conversation_id" schema
        <*> rcCnvType .= field "conversation_type" schema
        <*> rcCnvAccess .= field "access" (array schema)
        <*> rcCnvAccessRole .= field "access_role" schema
        <*> rcCnvName .= lax (field "conversation_name" (optWithDefault A.Null schema))
        <*> rcNonCreatorMembers .= field "non_creator_members" (set schema)
        <*> rcMessageTimer .= opt (field "message_timer" schema)
        <*> rcReceiptMode .= opt (field "receipt_mode" schema)

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
    cuAction :: ConversationAction
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform ConversationUpdate)
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema ConversationUpdate)

instance ToSchema ConversationUpdate where
  schema =
    object "ConversationUpdate" $
      ConversationUpdate
        <$> (toUTCTimeMillis . cuTime) .= fmap fromUTCTimeMillis (field "time" schema)
        <*> cuOrigUserId .= field "qualified_creator_user_id" schema
        <*> cuConvId .= field "conversation_id" schema
        <*> cuAlreadyPresentUsers .= field "already_present_users" (array schema)
        <*> cuAction .= field "action" schema

data LeaveConversationRequest = LeaveConversationRequest
  { -- | The conversation is assumed to be owned by the target domain, which
    -- allows us to protect against relay attacks
    lcConvId :: ConvId,
    -- | The leaver is assumed to be owned by the origin domain, which allows us
    -- to protect against spoofing attacks
    lcLeaver :: UserId
  }
  deriving stock (Generic, Eq, Show)
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema LeaveConversationRequest)

instance ToSchema LeaveConversationRequest where
  schema =
    object "LeaveConversationRequest" $
      LeaveConversationRequest
        <$> lcConvId .= field "conversation_id" schema
        <*> lcLeaver .= field "leaver_id" schema

-- | Error outcomes of the leave-conversation RPC.
data RemoveFromConversationError
  = RemoveFromConversationErrorRemovalNotAllowed
  | RemoveFromConversationErrorNotFound
  | RemoveFromConversationErrorUnchanged
  deriving stock (Eq, Show, Generic)
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema RemoveFromConversationError)

instance ToSchema RemoveFromConversationError where
  schema =
    enum @Text "RemoveFromConversationError" $
      mconcat
        [ element
            "removal-not-allowed"
            RemoveFromConversationErrorRemovalNotAllowed,
          element
            "not-found"
            RemoveFromConversationErrorNotFound,
          element
            "unchanged"
            RemoveFromConversationErrorUnchanged
        ]

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
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema (RemoteMessage conv))

instance ToSchema conv => ToSchema (RemoteMessage conv) where
  schema =
    object "RemoteMessage" $
      RemoteMessage
        <$> (toUTCTimeMillis . rmTime) .= fmap fromUTCTimeMillis (field "time" schema)
        <*> rmData .= opt (field "data" schema)
        <*> rmSender .= field "qualified_sender_id" schema
        <*> rmSenderClient .= field "sender_client" schema
        <*> rmConversation .= field "conversation" schema
        <*> rmPriority .= opt (field "priority" schema)
        <*> rmPush .= field "push" schema
        <*> rmTransient .= field "transient" schema
        <*> rmRecipients .= field "recipients" schema

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
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema MessageSendRequest)

instance ToSchema MessageSendRequest where
  schema =
    object "MessageSendRequest" $
      MessageSendRequest
        <$> msrConvId .= field "conversation_id" schema
        <*> msrSender .= field "sender_id" schema
        <*> msrRawMessage .= field "raw_message" schema

newtype MessageSendResponse = MessageSendResponse
  {msResponse :: PostOtrResponse MessageSendingStatus}
  deriving stock (Eq, Show)
  deriving
    (ToJSON, FromJSON, S.ToSchema)
    via ( Either
            (Schema (MessageNotSent MessageSendingStatus))
            MessageSendingStatus
        )

newtype LeaveConversationResponse = LeaveConversationResponse
  {leaveResponse :: Either RemoveFromConversationError ()}
  deriving stock (Eq, Show)
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema LeaveConversationResponse)

instance ToSchema LeaveConversationResponse where
  schema =
    LeaveConversationResponse
      <$> leaveResponse
        .= named
          "LeaveConversationResponse"
          (tag _Left (unnamed schema) <> tag _Right null_)

type UserDeletedNotificationMaxConvs = 1000

data UserDeletedConversationsNotification = UserDeletedConversationsNotification
  { -- | This is qualified implicitly by the origin domain
    udcvUser :: UserId,
    -- | These are qualified implicitly by the target domain
    udcvConversations :: Range 1 UserDeletedNotificationMaxConvs [ConvId]
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform UserDeletedConversationsNotification)
  deriving (FromJSON, ToJSON, S.ToSchema) via (Schema UserDeletedConversationsNotification)

instance ToSchema UserDeletedConversationsNotification where
  schema =
    object "UserDeletedConversationsNotification" $
      UserDeletedConversationsNotification
        <$> udcvUser .= field "user_id" schema
        <*> (fromRange . udcvConversations)
          .= field "conversation_ids" (rangedSchema sing sing (array schema))

type ServantAPI = ToServantApi GalleyApi

swaggerDoc :: S.Swagger
swaggerDoc = toSwagger (Proxy @ServantAPI)
