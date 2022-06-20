{-# LANGUAGE DerivingVia #-}
{-# OPTIONS_GHC -Wno-orphans #-}

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

module Wire.API.Routes.Public.Galley where

import qualified Data.Code as Code
import Data.CommaSeparatedList
import Data.Domain (Domain)
import Data.Id (ConvId, TeamId, UserId)
import Data.Qualified (Qualified (..))
import Data.Range
import Data.SOP
import qualified Data.Swagger as Swagger
import GHC.TypeLits (AppendSymbol)
import qualified Generics.SOP as GSOP
import Imports hiding (head)
import Servant hiding (WithStatus)
import Servant.Swagger.Internal
import Servant.Swagger.Internal.Orphans ()
import Wire.API.Conversation
import Wire.API.Conversation.Role
import Wire.API.CustomBackend (CustomBackend)
import Wire.API.Error
import qualified Wire.API.Error.Brig as BrigError
import Wire.API.Error.Galley
import Wire.API.Event.Conversation
import Wire.API.MLS.Message
import Wire.API.MLS.Serialisation
import Wire.API.MLS.Servant
import Wire.API.MLS.Welcome
import Wire.API.Message
import Wire.API.Routes.CSV
import Wire.API.Routes.LowLevelStream
import Wire.API.Routes.MultiVerb
import Wire.API.Routes.Named
import Wire.API.Routes.Public
import Wire.API.Routes.Public.Util
import Wire.API.Routes.QualifiedCapture
import Wire.API.Routes.Version
import Wire.API.ServantProto (Proto, RawProto)
import Wire.API.Team
import Wire.API.Team.Conversation
import Wire.API.Team.Feature
import Wire.API.Team.LegalHold
import Wire.API.Team.Member
import Wire.API.Team.Permission (Perm (..))
import Wire.API.Team.SearchVisibility (TeamSearchVisibilityView)
import qualified Wire.API.User as User

instance AsHeaders '[ConvId] Conversation Conversation where
  toHeaders c = (I (qUnqualified (cnvQualifiedId c)) :* Nil, c)
  fromHeaders = snd

type ConversationResponse = ResponseForExistedCreated Conversation

type ConversationHeaders = '[DescHeader "Location" "Conversation ID" ConvId]

type ConversationVerb =
  MultiVerb
    'POST
    '[JSON]
    '[ WithHeaders
         ConversationHeaders
         Conversation
         (Respond 200 "Conversation existed" Conversation),
       WithHeaders
         ConversationHeaders
         Conversation
         (Respond 201 "Conversation created" Conversation)
     ]
    ConversationResponse

type CreateConversationCodeVerb =
  MultiVerb
    'POST
    '[JSON]
    '[ Respond 200 "Conversation code already exists." ConversationCode,
       Respond 201 "Conversation code created." Event
     ]
    AddCodeResult

instance
  (ResponseType r1 ~ ConversationCode, ResponseType r2 ~ Event) =>
  AsUnion '[r1, r2] AddCodeResult
  where
  toUnion (CodeAlreadyExisted c) = Z (I c)
  toUnion (CodeAdded e) = S (Z (I e))

  fromUnion (Z (I c)) = CodeAlreadyExisted c
  fromUnion (S (Z (I e))) = CodeAdded e
  fromUnion (S (S x)) = case x of

type ConvUpdateResponses = UpdateResponses "Conversation unchanged" "Conversation updated" Event

type ConvJoinResponses = UpdateResponses "Conversation unchanged" "Conversation joined" Event

data MessageNotSent a
  = MessageNotSentConversationNotFound
  | MessageNotSentUnknownClient
  | MessageNotSentLegalhold
  | MessageNotSentClientMissing a
  deriving stock (Eq, Show, Generic, Functor)
  deriving
    (AsUnion (MessageNotSentResponses a))
    via (GenericAsUnion (MessageNotSentResponses a) (MessageNotSent a))

instance GSOP.Generic (MessageNotSent a)

type RemoveFromConversationVerb =
  MultiVerb
    'DELETE
    '[JSON]
    '[ RespondEmpty 204 "No change",
       Respond 200 "Member removed" Event
     ]
    (Maybe Event)

type MessageNotSentResponses a =
  '[ ErrorResponse 'ConvNotFound,
     ErrorResponse 'BrigError.UnknownClient,
     ErrorResponse 'BrigError.MissingLegalholdConsent,
     Respond 412 "Missing clients" a
   ]

type PostOtrResponses a =
  MessageNotSentResponses a
    .++ '[Respond 201 "Message sent" a]

type PostOtrResponse a = Either (MessageNotSent a) a

instance
  ( rs ~ (MessageNotSentResponses a .++ '[r]),
    a ~ ResponseType r
  ) =>
  AsUnion rs (PostOtrResponse a)
  where
  toUnion =
    eitherToUnion
      (toUnion @(MessageNotSentResponses a))
      (Z . I)

  fromUnion =
    eitherFromUnion
      (fromUnion @(MessageNotSentResponses a))
      (unI . unZ)

type ServantAPI =
  ConversationAPI
    :<|> TeamConversationAPI
    :<|> MessagingAPI
    :<|> BotAPI
    :<|> TeamAPI
    :<|> FeatureAPI
    :<|> MLSAPI
    :<|> CustomBackendAPI
    :<|> LegalHoldAPI
    :<|> TeamMemberAPI

type ConversationAPI =
  Named
    "get-unqualified-conversation"
    ( Summary "Get a conversation by ID"
        :> CanThrow 'ConvNotFound
        :> CanThrow 'ConvAccessDenied
        :> ZLocalUser
        :> "conversations"
        :> Capture "cnv" ConvId
        :> Get '[Servant.JSON] Conversation
    )
    :<|> Named
           "get-conversation"
           ( Summary "Get a conversation by ID"
               :> CanThrow 'ConvNotFound
               :> CanThrow 'ConvAccessDenied
               :> ZLocalUser
               :> "conversations"
               :> QualifiedCapture "cnv" ConvId
               :> Get '[Servant.JSON] Conversation
           )
    :<|> Named
           "get-conversation-roles"
           ( Summary "Get existing roles available for the given conversation"
               :> CanThrow 'ConvNotFound
               :> CanThrow 'ConvAccessDenied
               :> ZLocalUser
               :> "conversations"
               :> Capture "cnv" ConvId
               :> "roles"
               :> Get '[Servant.JSON] ConversationRolesList
           )
    :<|> Named
           "list-conversation-ids-unqualified"
           ( Summary "[deprecated] Get all local conversation IDs."
               -- FUTUREWORK: add bounds to swagger schema for Range
               :> ZLocalUser
               :> "conversations"
               :> "ids"
               :> QueryParam'
                    [ Optional,
                      Strict,
                      Description "Conversation ID to start from (exclusive)"
                    ]
                    "start"
                    ConvId
               :> QueryParam'
                    [ Optional,
                      Strict,
                      Description "Maximum number of IDs to return"
                    ]
                    "size"
                    (Range 1 1000 Int32)
               :> Get '[Servant.JSON] (ConversationList ConvId)
           )
    :<|> Named
           "list-conversation-ids"
           ( Summary "Get all conversation IDs."
               :> Description PaginationDocs
               :> ZLocalUser
               :> "conversations"
               :> "list-ids"
               :> ReqBody '[Servant.JSON] GetPaginatedConversationIds
               :> Post '[Servant.JSON] ConvIdsPage
           )
    :<|> Named
           "get-conversations"
           ( Summary "Get all *local* conversations."
               :> Description
                    "Will not return remote conversations.\n\n\
                    \Use `POST /conversations/list-ids` followed by \
                    \`POST /conversations/list` instead."
               :> ZLocalUser
               :> "conversations"
               :> QueryParam'
                    [ Optional,
                      Strict,
                      Description "Mutually exclusive with 'start' (at most 32 IDs per request)"
                    ]
                    "ids"
                    (Range 1 32 (CommaSeparatedList ConvId))
               :> QueryParam'
                    [ Optional,
                      Strict,
                      Description "Conversation ID to start from (exclusive)"
                    ]
                    "start"
                    ConvId
               :> QueryParam'
                    [ Optional,
                      Strict,
                      Description "Maximum number of conversations to return"
                    ]
                    "size"
                    (Range 1 500 Int32)
               :> Get '[Servant.JSON] (ConversationList Conversation)
           )
    :<|> Named
           "list-conversations-v1"
           ( Summary "Get conversation metadata for a list of conversation ids"
               :> Until 'V2
               :> ZLocalUser
               :> "conversations"
               :> "list"
               :> "v2"
               :> ReqBody '[Servant.JSON] ListConversations
               :> Post '[Servant.JSON] ConversationsResponse
           )
    :<|> Named
           "list-conversations"
           ( Summary "Get conversation metadata for a list of conversation ids"
               :> From 'V2
               :> ZLocalUser
               :> "conversations"
               :> "list"
               :> ReqBody '[Servant.JSON] ListConversations
               :> Post '[Servant.JSON] ConversationsResponse
           )
    -- This endpoint can lead to the following events being sent:
    -- - ConvCreate event to members
    :<|> Named
           "get-conversation-by-reusable-code"
           ( Summary "Get limited conversation information by key/code pair"
               :> CanThrow 'CodeNotFound
               :> CanThrow 'ConvNotFound
               :> CanThrow 'ConvAccessDenied
               :> CanThrow 'GuestLinksDisabled
               :> CanThrow 'NotATeamMember
               :> ZLocalUser
               :> "conversations"
               :> "join"
               :> QueryParam' [Required, Strict] "key" Code.Key
               :> QueryParam' [Required, Strict] "code" Code.Value
               :> Get '[Servant.JSON] ConversationCoverView
           )
    :<|> Named
           "create-group-conversation"
           ( Summary "Create a new conversation"
               :> CanThrow 'ConvAccessDenied
               :> CanThrow 'MLSNonEmptyMemberList
               :> CanThrow 'NotConnected
               :> CanThrow 'NotATeamMember
               :> CanThrow OperationDenied
               :> CanThrow 'MissingLegalholdConsent
               :> Description "This returns 201 when a new conversation is created, and 200 when the conversation already existed"
               :> ZLocalUser
               :> ZConn
               :> "conversations"
               :> ReqBody '[Servant.JSON] NewConv
               :> ConversationVerb
           )
    :<|> Named
           "create-self-conversation"
           ( Summary "Create a self-conversation"
               :> ZLocalUser
               :> "conversations"
               :> "self"
               :> ConversationVerb
           )
    -- This endpoint can lead to the following events being sent:
    -- - ConvCreate event to members
    -- TODO: add note: "On 201, the conversation ID is the `Location` header"
    :<|> Named
           "create-one-to-one-conversation"
           ( Summary "Create a 1:1 conversation"
               :> CanThrow 'ConvAccessDenied
               :> CanThrow 'InvalidOperation
               :> CanThrow 'NoBindingTeamMembers
               :> CanThrow 'NonBindingTeam
               :> CanThrow 'NotATeamMember
               :> CanThrow 'NotConnected
               :> CanThrow OperationDenied
               :> CanThrow 'TeamNotFound
               :> CanThrow 'MissingLegalholdConsent
               :> ZLocalUser
               :> ZConn
               :> "conversations"
               :> "one2one"
               :> ReqBody '[Servant.JSON] NewConv
               :> ConversationVerb
           )
    -- This endpoint can lead to the following events being sent:
    -- - MemberJoin event to members
    :<|> Named
           "add-members-to-conversation-unqualified"
           ( Summary "Add members to an existing conversation (deprecated)"
               :> Until 'V2
               :> CanThrow ('ActionDenied 'AddConversationMember)
               :> CanThrow ('ActionDenied 'LeaveConversation)
               :> CanThrow 'ConvNotFound
               :> CanThrow 'InvalidOperation
               :> CanThrow 'TooManyMembers
               :> CanThrow 'ConvAccessDenied
               :> CanThrow 'NotATeamMember
               :> CanThrow 'NotConnected
               :> CanThrow 'MissingLegalholdConsent
               :> ZLocalUser
               :> ZConn
               :> "conversations"
               :> Capture "cnv" ConvId
               :> "members"
               :> ReqBody '[JSON] Invite
               :> MultiVerb 'POST '[JSON] ConvUpdateResponses (UpdateResult Event)
           )
    :<|> Named
           "add-members-to-conversation-unqualified2"
           ( Summary "Add qualified members to an existing conversation."
               :> Until 'V2
               :> CanThrow ('ActionDenied 'AddConversationMember)
               :> CanThrow ('ActionDenied 'LeaveConversation)
               :> CanThrow 'ConvNotFound
               :> CanThrow 'InvalidOperation
               :> CanThrow 'TooManyMembers
               :> CanThrow 'ConvAccessDenied
               :> CanThrow 'NotATeamMember
               :> CanThrow 'NotConnected
               :> CanThrow 'MissingLegalholdConsent
               :> ZLocalUser
               :> ZConn
               :> "conversations"
               :> Capture "cnv" ConvId
               :> "members"
               :> "v2"
               :> ReqBody '[Servant.JSON] InviteQualified
               :> MultiVerb 'POST '[Servant.JSON] ConvUpdateResponses (UpdateResult Event)
           )
    :<|> Named
           "add-members-to-conversation"
           ( Summary "Add qualified members to an existing conversation."
               :> From 'V2
               :> CanThrow ('ActionDenied 'AddConversationMember)
               :> CanThrow ('ActionDenied 'LeaveConversation)
               :> CanThrow 'ConvNotFound
               :> CanThrow 'InvalidOperation
               :> CanThrow 'TooManyMembers
               :> CanThrow 'ConvAccessDenied
               :> CanThrow 'NotATeamMember
               :> CanThrow 'NotConnected
               :> CanThrow 'MissingLegalholdConsent
               :> ZLocalUser
               :> ZConn
               :> "conversations"
               :> QualifiedCapture "cnv" ConvId
               :> "members"
               :> ReqBody '[Servant.JSON] InviteQualified
               :> MultiVerb 'POST '[Servant.JSON] ConvUpdateResponses (UpdateResult Event)
           )
    -- This endpoint can lead to the following events being sent:
    -- - MemberJoin event to members
    :<|> Named
           "join-conversation-by-id-unqualified"
           ( Summary "Join a conversation by its ID (if link access enabled)"
               :> CanThrow 'ConvAccessDenied
               :> CanThrow 'ConvNotFound
               :> CanThrow 'InvalidOperation
               :> CanThrow 'NotATeamMember
               :> CanThrow 'TooManyMembers
               :> ZLocalUser
               :> ZConn
               :> "conversations"
               :> Capture' '[Description "Conversation ID"] "cnv" ConvId
               :> "join"
               :> MultiVerb 'POST '[Servant.JSON] ConvJoinResponses (UpdateResult Event)
           )
    -- This endpoint can lead to the following events being sent:
    -- - MemberJoin event to members
    :<|> Named
           "join-conversation-by-code-unqualified"
           ( Summary
               "Join a conversation using a reusable code.\
               \If the guest links team feature is disabled, this will fail with 409 GuestLinksDisabled.\
               \Note that this is currently inconsistent (for backwards compatibility reasons) with `POST /conversations/code-check` which responds with 404 CodeNotFound if guest links are disabled."
               :> CanThrow 'CodeNotFound
               :> CanThrow 'ConvAccessDenied
               :> CanThrow 'ConvNotFound
               :> CanThrow 'GuestLinksDisabled
               :> CanThrow 'InvalidOperation
               :> CanThrow 'NotATeamMember
               :> CanThrow 'TooManyMembers
               :> ZLocalUser
               :> ZConn
               :> "conversations"
               :> "join"
               :> ReqBody '[Servant.JSON] ConversationCode
               :> MultiVerb 'POST '[Servant.JSON] ConvJoinResponses (UpdateResult Event)
           )
    :<|> Named
           "code-check"
           ( Summary
               "Check validity of a conversation code.\
               \If the guest links team feature is disabled, this will fail with 404 CodeNotFound.\
               \Note that this is currently inconsistent (for backwards compatibility reasons) with `POST /conversations/join` which responds with 409 GuestLinksDisabled if guest links are disabled."
               :> CanThrow 'CodeNotFound
               :> CanThrow 'ConvNotFound
               :> "conversations"
               :> "code-check"
               :> ReqBody '[Servant.JSON] ConversationCode
               :> MultiVerb
                    'POST
                    '[JSON]
                    '[RespondEmpty 200 "Valid"]
                    ()
           )
    -- this endpoint can lead to the following events being sent:
    -- - ConvCodeUpdate event to members, if code didn't exist before
    :<|> Named
           "create-conversation-code-unqualified"
           ( Summary "Create or recreate a conversation code"
               :> CanThrow 'ConvAccessDenied
               :> CanThrow 'ConvNotFound
               :> CanThrow 'GuestLinksDisabled
               :> ZUser
               :> ZConn
               :> "conversations"
               :> Capture' '[Description "Conversation ID"] "cnv" ConvId
               :> "code"
               :> CreateConversationCodeVerb
           )
    :<|> Named
           "get-conversation-guest-links-status"
           ( Summary "Get the status of the guest links feature for a conversation that potentially has been created by someone from another team."
               :> CanThrow 'ConvAccessDenied
               :> CanThrow 'ConvNotFound
               :> ZUser
               :> "conversations"
               :> Capture' '[Description "Conversation ID"] "cnv" ConvId
               :> "features"
               :> FeatureSymbol GuestLinksConfig
               :> Get '[Servant.JSON] (WithStatus GuestLinksConfig)
           )
    -- This endpoint can lead to the following events being sent:
    -- - ConvCodeDelete event to members
    :<|> Named
           "remove-code-unqualified"
           ( Summary "Delete conversation code"
               :> CanThrow 'ConvAccessDenied
               :> CanThrow 'ConvNotFound
               :> ZLocalUser
               :> ZConn
               :> "conversations"
               :> Capture' '[Description "Conversation ID"] "cnv" ConvId
               :> "code"
               :> MultiVerb
                    'DELETE
                    '[JSON]
                    '[Respond 200 "Conversation code deleted." Event]
                    Event
           )
    :<|> Named
           "get-code"
           ( Summary "Get existing conversation code"
               :> CanThrow 'CodeNotFound
               :> CanThrow 'ConvAccessDenied
               :> CanThrow 'ConvNotFound
               :> CanThrow 'GuestLinksDisabled
               :> ZLocalUser
               :> "conversations"
               :> Capture' '[Description "Conversation ID"] "cnv" ConvId
               :> "code"
               :> MultiVerb
                    'GET
                    '[JSON]
                    '[Respond 200 "Conversation Code" ConversationCode]
                    ConversationCode
           )
    -- This endpoint can lead to the following events being sent:
    -- - Typing event to members
    :<|> Named
           "member-typing-unqualified"
           ( Summary "Sending typing notifications"
               :> CanThrow 'ConvNotFound
               :> ZLocalUser
               :> ZConn
               :> "conversations"
               :> Capture' '[Description "Conversation ID"] "cnv" ConvId
               :> "typing"
               :> ReqBody '[JSON] TypingData
               :> MultiVerb 'POST '[JSON] '[RespondEmpty 200 "Notification sent"] ()
           )
    -- This endpoint can lead to the following events being sent:
    -- - MemberLeave event to members
    :<|> Named
           "remove-member-unqualified"
           ( Summary "Remove a member from a conversation (deprecated)"
               :> ZLocalUser
               :> ZConn
               :> CanThrow ('ActionDenied 'RemoveConversationMember)
               :> CanThrow 'ConvNotFound
               :> CanThrow 'InvalidOperation
               :> "conversations"
               :> Capture' '[Description "Conversation ID"] "cnv" ConvId
               :> "members"
               :> Capture' '[Description "Target User ID"] "usr" UserId
               :> RemoveFromConversationVerb
           )
    -- This endpoint can lead to the following events being sent:
    -- - MemberLeave event to members
    :<|> Named
           "remove-member"
           ( Summary "Remove a member from a conversation"
               :> ZLocalUser
               :> ZConn
               :> CanThrow ('ActionDenied 'RemoveConversationMember)
               :> CanThrow 'ConvNotFound
               :> CanThrow 'InvalidOperation
               :> "conversations"
               :> QualifiedCapture' '[Description "Conversation ID"] "cnv" ConvId
               :> "members"
               :> QualifiedCapture' '[Description "Target User ID"] "usr" UserId
               :> RemoveFromConversationVerb
           )
    -- This endpoint can lead to the following events being sent:
    -- - MemberStateUpdate event to members
    :<|> Named
           "update-other-member-unqualified"
           ( Summary "Update membership of the specified user (deprecated)"
               :> Description "Use `PUT /conversations/:cnv_domain/:cnv/members/:usr_domain/:usr` instead"
               :> ZLocalUser
               :> ZConn
               :> CanThrow 'ConvNotFound
               :> CanThrow 'ConvMemberNotFound
               :> CanThrow ('ActionDenied 'ModifyOtherConversationMember)
               :> CanThrow 'InvalidTarget
               :> CanThrow 'InvalidOperation
               :> "conversations"
               :> Capture' '[Description "Conversation ID"] "cnv" ConvId
               :> "members"
               :> Capture' '[Description "Target User ID"] "usr" UserId
               :> ReqBody '[JSON] OtherMemberUpdate
               :> MultiVerb
                    'PUT
                    '[JSON]
                    '[RespondEmpty 200 "Membership updated"]
                    ()
           )
    :<|> Named
           "update-other-member"
           ( Summary "Update membership of the specified user"
               :> Description "**Note**: at least one field has to be provided."
               :> ZLocalUser
               :> ZConn
               :> CanThrow 'ConvNotFound
               :> CanThrow 'ConvMemberNotFound
               :> CanThrow ('ActionDenied 'ModifyOtherConversationMember)
               :> CanThrow 'InvalidTarget
               :> CanThrow 'InvalidOperation
               :> "conversations"
               :> QualifiedCapture' '[Description "Conversation ID"] "cnv" ConvId
               :> "members"
               :> QualifiedCapture' '[Description "Target User ID"] "usr" UserId
               :> ReqBody '[JSON] OtherMemberUpdate
               :> MultiVerb
                    'PUT
                    '[JSON]
                    '[RespondEmpty 200 "Membership updated"]
                    ()
           )
    -- This endpoint can lead to the following events being sent:
    -- - ConvRename event to members
    :<|> Named
           "update-conversation-name-deprecated"
           ( Summary "Update conversation name (deprecated)"
               :> Description "Use `/conversations/:domain/:conv/name` instead."
               :> CanThrow ('ActionDenied 'ModifyConversationName)
               :> CanThrow 'ConvNotFound
               :> CanThrow 'InvalidOperation
               :> ZLocalUser
               :> ZConn
               :> "conversations"
               :> Capture' '[Description "Conversation ID"] "cnv" ConvId
               :> ReqBody '[JSON] ConversationRename
               :> MultiVerb
                    'PUT
                    '[JSON]
                    (UpdateResponses "Name unchanged" "Name updated" Event)
                    (UpdateResult Event)
           )
    :<|> Named
           "update-conversation-name-unqualified"
           ( Summary "Update conversation name (deprecated)"
               :> Description "Use `/conversations/:domain/:conv/name` instead."
               :> CanThrow ('ActionDenied 'ModifyConversationName)
               :> CanThrow 'ConvNotFound
               :> CanThrow 'InvalidOperation
               :> ZLocalUser
               :> ZConn
               :> "conversations"
               :> Capture' '[Description "Conversation ID"] "cnv" ConvId
               :> "name"
               :> ReqBody '[JSON] ConversationRename
               :> MultiVerb
                    'PUT
                    '[JSON]
                    (UpdateResponses "Name unchanged" "Name updated" Event)
                    (UpdateResult Event)
           )
    :<|> Named
           "update-conversation-name"
           ( Summary "Update conversation name"
               :> CanThrow ('ActionDenied 'ModifyConversationName)
               :> CanThrow 'ConvNotFound
               :> CanThrow 'InvalidOperation
               :> ZLocalUser
               :> ZConn
               :> "conversations"
               :> QualifiedCapture' '[Description "Conversation ID"] "cnv" ConvId
               :> "name"
               :> ReqBody '[JSON] ConversationRename
               :> MultiVerb
                    'PUT
                    '[JSON]
                    (UpdateResponses "Name updated" "Name unchanged" Event)
                    (UpdateResult Event)
           )
    -- This endpoint can lead to the following events being sent:
    -- - ConvMessageTimerUpdate event to members
    :<|> Named
           "update-conversation-message-timer-unqualified"
           ( Summary "Update the message timer for a conversation (deprecated)"
               :> Description "Use `/conversations/:domain/:cnv/message-timer` instead."
               :> ZLocalUser
               :> ZConn
               :> CanThrow ('ActionDenied 'ModifyConversationMessageTimer)
               :> CanThrow 'ConvAccessDenied
               :> CanThrow 'ConvNotFound
               :> CanThrow 'InvalidOperation
               :> "conversations"
               :> Capture' '[Description "Conversation ID"] "cnv" ConvId
               :> "message-timer"
               :> ReqBody '[JSON] ConversationMessageTimerUpdate
               :> MultiVerb
                    'PUT
                    '[JSON]
                    (UpdateResponses "Message timer unchanged" "Message timer updated" Event)
                    (UpdateResult Event)
           )
    :<|> Named
           "update-conversation-message-timer"
           ( Summary "Update the message timer for a conversation"
               :> ZLocalUser
               :> ZConn
               :> CanThrow ('ActionDenied 'ModifyConversationMessageTimer)
               :> CanThrow 'ConvAccessDenied
               :> CanThrow 'ConvNotFound
               :> CanThrow 'InvalidOperation
               :> "conversations"
               :> QualifiedCapture' '[Description "Conversation ID"] "cnv" ConvId
               :> "message-timer"
               :> ReqBody '[JSON] ConversationMessageTimerUpdate
               :> MultiVerb
                    'PUT
                    '[JSON]
                    (UpdateResponses "Message timer unchanged" "Message timer updated" Event)
                    (UpdateResult Event)
           )
    -- This endpoint can lead to the following events being sent:
    -- - ConvReceiptModeUpdate event to members
    :<|> Named
           "update-conversation-receipt-mode-unqualified"
           ( Summary "Update receipt mode for a conversation (deprecated)"
               :> Description "Use `PUT /conversations/:domain/:cnv/receipt-mode` instead."
               :> ZLocalUser
               :> ZConn
               :> CanThrow ('ActionDenied 'ModifyConversationReceiptMode)
               :> CanThrow 'ConvAccessDenied
               :> CanThrow 'ConvNotFound
               :> CanThrow 'InvalidOperation
               :> "conversations"
               :> Capture' '[Description "Conversation ID"] "cnv" ConvId
               :> "receipt-mode"
               :> ReqBody '[JSON] ConversationReceiptModeUpdate
               :> MultiVerb
                    'PUT
                    '[JSON]
                    (UpdateResponses "Receipt mode unchanged" "Receipt mode updated" Event)
                    (UpdateResult Event)
           )
    :<|> Named
           "update-conversation-receipt-mode"
           ( Summary "Update receipt mode for a conversation"
               :> ZLocalUser
               :> ZConn
               :> CanThrow ('ActionDenied 'ModifyConversationReceiptMode)
               :> CanThrow 'ConvAccessDenied
               :> CanThrow 'ConvNotFound
               :> CanThrow 'InvalidOperation
               :> "conversations"
               :> QualifiedCapture' '[Description "Conversation ID"] "cnv" ConvId
               :> "receipt-mode"
               :> ReqBody '[JSON] ConversationReceiptModeUpdate
               :> MultiVerb
                    'PUT
                    '[JSON]
                    (UpdateResponses "Receipt mode unchanged" "Receipt mode updated" Event)
                    (UpdateResult Event)
           )
    -- This endpoint can lead to the following events being sent:
    -- - MemberLeave event to members, if members get removed
    -- - ConvAccessUpdate event to members
    :<|> Named
           "update-conversation-access-unqualified"
           ( Summary "Update access modes for a conversation (deprecated)"
               :> Description "Use PUT `/conversations/:domain/:cnv/access` instead."
               :> ZLocalUser
               :> ZConn
               :> CanThrow ('ActionDenied 'ModifyConversationAccess)
               :> CanThrow ('ActionDenied 'RemoveConversationMember)
               :> CanThrow 'ConvAccessDenied
               :> CanThrow 'ConvNotFound
               :> CanThrow 'InvalidOperation
               :> CanThrow 'InvalidTargetAccess
               :> "conversations"
               :> Capture' '[Description "Conversation ID"] "cnv" ConvId
               :> "access"
               :> ReqBody '[JSON] ConversationAccessData
               :> MultiVerb
                    'PUT
                    '[JSON]
                    (UpdateResponses "Access unchanged" "Access updated" Event)
                    (UpdateResult Event)
           )
    :<|> Named
           "update-conversation-access"
           ( Summary "Update access modes for a conversation"
               :> ZLocalUser
               :> ZConn
               :> CanThrow ('ActionDenied 'ModifyConversationAccess)
               :> CanThrow ('ActionDenied 'RemoveConversationMember)
               :> CanThrow 'ConvAccessDenied
               :> CanThrow 'ConvNotFound
               :> CanThrow 'InvalidOperation
               :> CanThrow 'InvalidTargetAccess
               :> "conversations"
               :> QualifiedCapture' '[Description "Conversation ID"] "cnv" ConvId
               :> "access"
               :> ReqBody '[JSON] ConversationAccessData
               :> MultiVerb
                    'PUT
                    '[JSON]
                    (UpdateResponses "Access unchanged" "Access updated" Event)
                    (UpdateResult Event)
           )
    :<|> Named
           "get-conversation-self-unqualified"
           ( Summary "Get self membership properties (deprecated)"
               :> ZLocalUser
               :> "conversations"
               :> Capture' '[Description "Conversation ID"] "cnv" ConvId
               :> "self"
               :> Get '[JSON] (Maybe Member)
           )
    :<|> Named
           "update-conversation-self-unqualified"
           ( Summary "Update self membership properties (deprecated)"
               :> Description "Use `/conversations/:domain/:conv/self` instead."
               :> CanThrow 'ConvNotFound
               :> ZLocalUser
               :> ZConn
               :> "conversations"
               :> Capture' '[Description "Conversation ID"] "cnv" ConvId
               :> "self"
               :> ReqBody '[JSON] MemberUpdate
               :> MultiVerb
                    'PUT
                    '[JSON]
                    '[RespondEmpty 200 "Update successful"]
                    ()
           )
    :<|> Named
           "update-conversation-self"
           ( Summary "Update self membership properties"
               :> Description "**Note**: at least one field has to be provided."
               :> CanThrow 'ConvNotFound
               :> ZLocalUser
               :> ZConn
               :> "conversations"
               :> QualifiedCapture' '[Description "Conversation ID"] "cnv" ConvId
               :> "self"
               :> ReqBody '[JSON] MemberUpdate
               :> MultiVerb
                    'PUT
                    '[JSON]
                    '[RespondEmpty 200 "Update successful"]
                    ()
           )

type TeamConversationAPI =
  Named
    "get-team-conversation-roles"
    ( Summary "Get existing roles available for the given team"
        :> CanThrow 'NotATeamMember
        :> ZUser
        :> "teams"
        :> Capture "tid" TeamId
        :> "conversations"
        :> "roles"
        :> Get '[Servant.JSON] ConversationRolesList
    )
    :<|> Named
           "get-team-conversations"
           ( Summary "Get team conversations"
               :> CanThrow OperationDenied
               :> CanThrow 'NotATeamMember
               :> ZUser
               :> "teams"
               :> Capture "tid" TeamId
               :> "conversations"
               :> Get '[Servant.JSON] TeamConversationList
           )
    :<|> Named
           "get-team-conversation"
           ( Summary "Get one team conversation"
               :> CanThrow 'ConvNotFound
               :> CanThrow OperationDenied
               :> CanThrow 'NotATeamMember
               :> ZUser
               :> "teams"
               :> Capture "tid" TeamId
               :> "conversations"
               :> Capture "cid" ConvId
               :> Get '[Servant.JSON] TeamConversation
           )
    :<|> Named
           "delete-team-conversation"
           ( Summary "Remove a team conversation"
               :> CanThrow ('ActionDenied 'DeleteConversation)
               :> CanThrow 'ConvNotFound
               :> CanThrow 'InvalidOperation
               :> CanThrow 'NotATeamMember
               :> ZLocalUser
               :> ZConn
               :> "teams"
               :> Capture "tid" TeamId
               :> "conversations"
               :> Capture "cid" ConvId
               :> MultiVerb 'DELETE '[JSON] '[RespondEmpty 200 "Conversation deleted"] ()
           )

type TeamAPI =
  Named
    "create-non-binding-team"
    ( Summary "Create a new non binding team"
        :> ZUser
        :> ZConn
        :> CanThrow 'NotConnected
        :> CanThrow 'UserBindingExists
        :> "teams"
        :> ReqBody '[Servant.JSON] NonBindingNewTeam
        :> MultiVerb
             'POST
             '[JSON]
             '[ WithHeaders
                  '[DescHeader "Location" "Team ID" TeamId]
                  TeamId
                  (RespondEmpty 201 "Team ID as `Location` header value")
              ]
             TeamId
    )
    :<|> Named
           "update-team"
           ( Summary "Update team properties"
               :> ZUser
               :> ZConn
               :> CanThrow 'NotATeamMember
               :> CanThrow ('MissingPermission ('Just 'SetTeamData))
               :> "teams"
               :> Capture "tid" TeamId
               :> ReqBody '[JSON] TeamUpdateData
               :> MultiVerb
                    'PUT
                    '[JSON]
                    '[RespondEmpty 200 "Team updated"]
                    ()
           )
    :<|> Named
           "get-teams"
           ( Summary "Get teams (deprecated); use `GET /teams/:tid`"
               :> ZUser
               :> "teams"
               :> Get '[JSON] TeamList
           )
    :<|> Named
           "get-team"
           ( Summary "Get a team by ID"
               :> ZUser
               :> CanThrow 'TeamNotFound
               :> "teams"
               :> Capture "tid" TeamId
               :> Get '[JSON] Team
           )
    :<|> Named
           "delete-team"
           ( Summary "Delete a team"
               :> ZUser
               :> ZConn
               :> CanThrow 'TeamNotFound
               :> CanThrow ('MissingPermission ('Just 'DeleteTeam))
               :> CanThrow 'NotATeamMember
               :> CanThrow OperationDenied
               :> CanThrow 'DeleteQueueFull
               :> CanThrow AuthenticationError
               :> "teams"
               :> Capture "tid" TeamId
               :> ReqBody '[Servant.JSON] TeamDeleteData
               :> MultiVerb 'DELETE '[JSON] '[RespondEmpty 202 "Team is scheduled for removal"] ()
           )

type MessagingAPI =
  Named
    "post-otr-message-unqualified"
    ( Summary "Post an encrypted message to a conversation (accepts JSON or Protobuf)"
        :> Description PostOtrDescriptionUnqualified
        :> ZLocalUser
        :> ZConn
        :> "conversations"
        :> Capture "cnv" ConvId
        :> "otr"
        :> "messages"
        :> QueryParam "ignore_missing" IgnoreMissing
        :> QueryParam "report_missing" ReportMissing
        :> ReqBody '[JSON, Proto] NewOtrMessage
        :> MultiVerb
             'POST
             '[Servant.JSON]
             (PostOtrResponses ClientMismatch)
             (PostOtrResponse ClientMismatch)
    )
    :<|> Named
           "post-otr-broadcast-unqualified"
           ( Summary "Broadcast an encrypted message to all team members and all contacts (accepts JSON or Protobuf)"
               :> Description PostOtrDescriptionUnqualified
               :> ZLocalUser
               :> ZConn
               :> CanThrow 'TeamNotFound
               :> CanThrow 'BroadcastLimitExceeded
               :> CanThrow 'NonBindingTeam
               :> "broadcast"
               :> "otr"
               :> "messages"
               :> QueryParam "ignore_missing" IgnoreMissing
               :> QueryParam "report_missing" ReportMissing
               :> ReqBody '[JSON, Proto] NewOtrMessage
               :> MultiVerb
                    'POST
                    '[JSON]
                    (PostOtrResponses ClientMismatch)
                    (PostOtrResponse ClientMismatch)
           )
    :<|> Named
           "post-proteus-message"
           ( Summary "Post an encrypted message to a conversation (accepts only Protobuf)"
               :> Description PostOtrDescription
               :> ZLocalUser
               :> ZConn
               :> "conversations"
               :> QualifiedCapture "cnv" ConvId
               :> "proteus"
               :> "messages"
               :> ReqBody '[Proto] (RawProto QualifiedNewOtrMessage)
               :> MultiVerb
                    'POST
                    '[Servant.JSON]
                    (PostOtrResponses MessageSendingStatus)
                    (Either (MessageNotSent MessageSendingStatus) MessageSendingStatus)
           )
    :<|> Named
           "post-proteus-broadcast"
           ( Summary "Post an encrypted message to all team members and all contacts (accepts only Protobuf)"
               :> Description PostOtrDescription
               :> ZLocalUser
               :> ZConn
               :> CanThrow 'TeamNotFound
               :> CanThrow 'BroadcastLimitExceeded
               :> CanThrow 'NonBindingTeam
               :> "broadcast"
               :> "proteus"
               :> "messages"
               :> ReqBody '[Proto] QualifiedNewOtrMessage
               :> MultiVerb
                    'POST
                    '[JSON]
                    (PostOtrResponses MessageSendingStatus)
                    (Either (MessageNotSent MessageSendingStatus) MessageSendingStatus)
           )

type BotAPI =
  Named
    "post-bot-message-unqualified"
    ( ZBot
        :> ZConversation
        :> CanThrow 'ConvNotFound
        :> "bot"
        :> "messages"
        :> QueryParam "ignore_missing" IgnoreMissing
        :> QueryParam "report_missing" ReportMissing
        :> ReqBody '[JSON] NewOtrMessage
        :> MultiVerb
             'POST
             '[Servant.JSON]
             (PostOtrResponses ClientMismatch)
             (PostOtrResponse ClientMismatch)
    )

type FeatureAPI =
  FeatureStatusGet SSOConfig
    :<|> FeatureStatusGet LegalholdConfig
    :<|> FeatureStatusPut
           '( 'ActionDenied 'RemoveConversationMember,
              '( AuthenticationError,
                 '( 'CannotEnableLegalHoldServiceLargeTeam,
                    '( 'LegalHoldNotEnabled,
                       '( 'LegalHoldDisableUnimplemented,
                          '( 'LegalHoldServiceNotRegistered,
                             '( 'UserLegalHoldIllegalOperation,
                                '( 'LegalHoldCouldNotBlockConnections, '())
                              )
                           )
                        )
                     )
                  )
               )
            )
           LegalholdConfig
    :<|> FeatureStatusGet SearchVisibilityAvailableConfig
    :<|> FeatureStatusPut '() SearchVisibilityAvailableConfig
    :<|> FeatureStatusDeprecatedGet "This endpoint is potentially used the old Android client. It is not used by iOS, team management, or webapp as of June 2022" SearchVisibilityAvailableConfig
    :<|> FeatureStatusDeprecatedPut "This endpoint is potentially used the old Android client. It is not used by iOS, team management, or webapp as of June 2022" SearchVisibilityAvailableConfig
    :<|> SearchVisibilityGet
    :<|> SearchVisibilitySet
    :<|> FeatureStatusGet ValidateSAMLEmailsConfig
    :<|> FeatureStatusDeprecatedGet "This endpoint is potentially used the old Android client. It is not used by iOS, team management, or webapp as of June 2022" ValidateSAMLEmailsConfig
    :<|> FeatureStatusGet DigitalSignaturesConfig
    :<|> FeatureStatusDeprecatedGet "This endpoint was removed in iOS in version 3.101. It is potentially used by the old Android client. It is not used by team management, or webapp as of June 2022" DigitalSignaturesConfig
    :<|> FeatureStatusGet AppLockConfig
    :<|> FeatureStatusPut '() AppLockConfig
    :<|> FeatureStatusGet FileSharingConfig
    :<|> FeatureStatusPut '() FileSharingConfig
    :<|> FeatureStatusGet ClassifiedDomainsConfig
    :<|> FeatureStatusGet ConferenceCallingConfig
    :<|> FeatureStatusGet SelfDeletingMessagesConfig
    :<|> FeatureStatusPut '() SelfDeletingMessagesConfig
    :<|> FeatureStatusGet GuestLinksConfig
    :<|> FeatureStatusPut '() GuestLinksConfig
    :<|> FeatureStatusGet SndFactorPasswordChallengeConfig
    :<|> FeatureStatusPut '() SndFactorPasswordChallengeConfig
    :<|> AllFeatureConfigsUserGet
    :<|> AllFeatureConfigsTeamGet
    :<|> FeatureConfigDeprecatedGet "This endpoint was removed in iOS in version 3.101. It is not used by team management, or webapp, and is potentially used by the old Android client as of June 2022" LegalholdConfig
    :<|> FeatureConfigDeprecatedGet "This endpoint was removed in iOS in version 3.101. It is used by team management, webapp, and potentially the old Android client as of June 2022" SSOConfig
    :<|> FeatureConfigDeprecatedGet "This endpoint was removed in iOS in version 3.101. It is not used by team management, or webapp, and is potentially used by the old Android client as of June 2022" SearchVisibilityAvailableConfig
    :<|> FeatureConfigDeprecatedGet "This endpoint was removed in iOS in version 3.101. It is not used by team management, or webapp, and is potentially used by the old Android client as of June 2022" ValidateSAMLEmailsConfig
    :<|> FeatureConfigDeprecatedGet "This endpoint was removed in iOS in version 3.101. It is used by team management, webapp, and potentially the old Android client as of June 2022" DigitalSignaturesConfig
    :<|> FeatureConfigDeprecatedGet "This endpoint was removed in iOS in version 3.101. It is used by team management, webapp, and potentially the old Android client as of June 2022" AppLockConfig
    :<|> FeatureConfigDeprecatedGet "This endpoint was removed in iOS in version 3.101. It is used by team management, webapp, and potentially the old Android client as of June 2022" FileSharingConfig
    :<|> FeatureConfigDeprecatedGet "This endpoint was removed in iOS in version 3.101. It is not used by team management, or webapp, and is potentially used by the old Android client as of June 2022" ClassifiedDomainsConfig
    :<|> FeatureConfigDeprecatedGet "This endpoint was removed in iOS in version 3.101. It is not used by team management, or webapp, and is potentially used by the old Android client as of June 2022" ConferenceCallingConfig
    :<|> FeatureConfigDeprecatedGet "This endpoint was removed in iOS in version 3.101. It is not used by team management, or webapp, and is potentially used by the old Android client as of June 2022" SelfDeletingMessagesConfig
    :<|> FeatureConfigDeprecatedGet "This endpoint was removed in iOS in version 3.101. It is used by team management, webapp, and potentially the old Android client as of June 2022" GuestLinksConfig
    :<|> FeatureConfigDeprecatedGet "This endpoint was removed in iOS in version 3.101. It is used by team management, webapp, and potentially the old Android client as of June 2022" SndFactorPasswordChallengeConfig

type FeatureStatusGet f =
  Named
    '("get", f)
    (ZUser :> FeatureStatusBaseGet f)

type FeatureStatusPut errs f =
  Named
    '("put", f)
    (ZUser :> FeatureStatusBasePutPublic errs f)

type FeatureStatusDeprecatedGet d f =
  Named
    '("get-deprecated", f)
    (ZUser :> FeatureStatusBaseDeprecatedGet d f)

type FeatureStatusDeprecatedPut d f =
  Named
    '("put-deprecated", f)
    (ZUser :> FeatureStatusBaseDeprecatedPut d f)

type FeatureStatusBaseGet featureConfig =
  Summary (AppendSymbol "Get config for " (FeatureSymbol featureConfig))
    :> CanThrow OperationDenied
    :> CanThrow 'NotATeamMember
    :> CanThrow 'TeamNotFound
    :> "teams"
    :> Capture "tid" TeamId
    :> "features"
    :> FeatureSymbol featureConfig
    :> Get '[Servant.JSON] (WithStatus featureConfig)

type FeatureStatusBasePutPublic errs featureConfig =
  Summary (AppendSymbol "Put config for " (FeatureSymbol featureConfig))
    :> CanThrow OperationDenied
    :> CanThrow 'NotATeamMember
    :> CanThrow 'TeamNotFound
    :> CanThrow TeamFeatureError
    :> CanThrowMany errs
    :> "teams"
    :> Capture "tid" TeamId
    :> "features"
    :> FeatureSymbol featureConfig
    :> ReqBody '[Servant.JSON] (WithStatusNoLock featureConfig)
    :> Put '[Servant.JSON] (WithStatus featureConfig)

-- | A type for a GET endpoint for a feature with a deprecated path
type FeatureStatusBaseDeprecatedGet desc featureConfig =
  ( Summary
      (AppendSymbol "[deprecated] Get config for " (FeatureSymbol featureConfig))
      :> Until 'V2
      :> Description
           ( "Deprecated. Please use `GET /teams/:tid/features/"
               `AppendSymbol` FeatureSymbol featureConfig
               `AppendSymbol` "` instead.\n"
               `AppendSymbol` desc
           )
      :> CanThrow 'NotATeamMember
      :> CanThrow OperationDenied
      :> CanThrow 'TeamNotFound
      :> "teams"
      :> Capture "tid" TeamId
      :> "features"
      :> DeprecatedFeatureName featureConfig
      :> Get '[Servant.JSON] (WithStatus featureConfig)
  )

-- | A type for a PUT endpoint for a feature with a deprecated path
type FeatureStatusBaseDeprecatedPut desc featureConfig =
  Summary
    (AppendSymbol "[deprecated] Get config for " (FeatureSymbol featureConfig))
    :> Until 'V2
    :> Description
         ( "Deprecated. Please use `PUT /teams/:tid/features/"
             `AppendSymbol` FeatureSymbol featureConfig
             `AppendSymbol` "` instead.\n"
             `AppendSymbol` desc
         )
    :> CanThrow 'NotATeamMember
    :> CanThrow OperationDenied
    :> CanThrow 'TeamNotFound
    :> CanThrow TeamFeatureError
    :> "teams"
    :> Capture "tid" TeamId
    :> "features"
    :> DeprecatedFeatureName featureConfig
    :> ReqBody '[Servant.JSON] (WithStatusNoLock featureConfig)
    :> Put '[Servant.JSON] (WithStatus featureConfig)

type FeatureConfigDeprecatedGet desc featureConfig =
  Named
    '("get-config", featureConfig)
    ( Summary (AppendSymbol "[deprecated] Get feature config for feature " (FeatureSymbol featureConfig))
        :> Until 'V2
        :> Description ("Deprecated. Please use `GET /feature-configs` instead.\n" `AppendSymbol` desc)
        :> ZUser
        :> CanThrow 'NotATeamMember
        :> CanThrow OperationDenied
        :> CanThrow 'TeamNotFound
        :> "feature-configs"
        :> FeatureSymbol featureConfig
        :> Get '[Servant.JSON] (WithStatus featureConfig)
    )

type AllFeatureConfigsUserGet =
  Named
    "get-all-feature-configs-for-user"
    ( Summary
        "Gets feature configs for a user"
        :> Description
             "Gets feature configs for a user. If the user is a member of a team and has the required permissions, this will return the team's feature configs.\
             \If the user is not a member of a team, this will return the personal feature configs (the server defaults)."
        :> ZUser
        :> CanThrow 'NotATeamMember
        :> CanThrow OperationDenied
        :> CanThrow 'TeamNotFound
        :> "feature-configs"
        :> Get '[Servant.JSON] AllFeatureConfigs
    )

type AllFeatureConfigsTeamGet =
  Named
    "get-all-feature-configs-for-team"
    ( Summary "Gets feature configs for a team"
        :> Description "Gets feature configs for a team. User must be a member of the team and have permission to view team features."
        :> CanThrow 'NotATeamMember
        :> CanThrow OperationDenied
        :> CanThrow 'TeamNotFound
        :> ZLocalUser
        :> "teams"
        :> Capture "tid" TeamId
        :> "features"
        :> Get '[JSON] AllFeatureConfigs
    )

type SearchVisibilityGet =
  Named
    "get-search-visibility"
    ( Summary "Shows the value for search visibility"
        :> CanThrow 'NotATeamMember
        :> CanThrow OperationDenied
        :> ZLocalUser
        :> "teams"
        :> Capture "tid" TeamId
        :> "search-visibility"
        :> Get '[JSON] TeamSearchVisibilityView
    )

type SearchVisibilitySet =
  Named
    "set-search-visibility"
    ( Summary "Sets the search visibility for the whole team"
        :> CanThrow 'NotATeamMember
        :> CanThrow OperationDenied
        :> CanThrow 'TeamSearchVisibilityNotEnabled
        :> CanThrow 'TeamNotFound
        :> CanThrow TeamFeatureError
        :> ZLocalUser
        :> "teams"
        :> Capture "tid" TeamId
        :> "search-visibility"
        :> ReqBody '[JSON] TeamSearchVisibilityView
        :> MultiVerb 'PUT '[JSON] '[RespondEmpty 204 "Search visibility set"] ()
    )

type MLSMessagingAPI =
  Named
    "mls-welcome-message"
    ( Summary "Post an MLS welcome message"
        :> CanThrow 'MLSKeyPackageRefNotFound
        :> "welcome"
        :> ZConn
        :> ReqBody '[MLS] (RawMLS Welcome)
        :> MultiVerb1 'POST '[JSON] (RespondEmpty 201 "Welcome message sent")
    )
    :<|> Named
           "mls-message"
           ( Summary "Post an MLS message"
               :> CanThrow 'ConvNotFound
               :> CanThrow 'MLSKeyPackageRefNotFound
               :> CanThrow 'MLSClientMismatch
               :> CanThrow 'MLSProtocolErrorTag
               :> CanThrow 'MLSStaleMessage
               :> CanThrow MLSProposalFailure
               :> CanThrow 'MLSProposalNotFound
               :> CanThrow 'MLSUnsupportedMessage
               :> CanThrow 'MLSUnsupportedProposal
               :> CanThrow 'LegalHoldNotEnabled
               :> CanThrow 'MissingLegalholdConsent
               :> "messages"
               :> ZConn
               :> ReqBody '[MLS] (RawMLS SomeMessage)
               :> MultiVerb1 'POST '[JSON] (Respond 201 "Message sent" [Event])
           )

type MLSAPI = LiftNamed (ZLocalUser :> "mls" :> MLSMessagingAPI)

type CustomBackendAPI =
  Named
    "get-custom-backend-by-domain"
    ( Summary "Shows information about custom backends related to a given email domain"
        :> CanThrow 'CustomBackendNotFound
        :> "custom-backend"
        :> "by-domain"
        :> Capture' '[Description "URL-encoded email domain"] "domain" Domain
        :> Get '[JSON] CustomBackend
    )

type LegalHoldAPI =
  Named
    "create-legal-hold-settings"
    ( Summary "Create legal hold service settings"
        :> CanThrow 'NotATeamMember
        :> CanThrow OperationDenied
        :> CanThrow 'LegalHoldNotEnabled
        :> CanThrow 'LegalHoldServiceInvalidKey
        :> CanThrow 'LegalHoldServiceBadResponse
        :> ZLocalUser
        :> "teams"
        :> Capture "tid" TeamId
        :> "legalhold"
        :> "settings"
        :> ReqBody '[JSON] NewLegalHoldService
        :> MultiVerb1 'POST '[JSON] (Respond 201 "Legal hold service settings created" ViewLegalHoldService)
    )
    :<|> Named
           "get-legal-hold-settings"
           ( Summary "Get legal hold service settings"
               :> CanThrow 'NotATeamMember
               :> CanThrow OperationDenied
               :> ZLocalUser
               :> "teams"
               :> Capture "tid" TeamId
               :> "legalhold"
               :> "settings"
               :> Get '[JSON] ViewLegalHoldService
           )
    :<|> Named
           "delete-legal-hold-settings"
           ( Summary "Delete legal hold service settings"
               :> CanThrow AuthenticationError
               :> CanThrow OperationDenied
               :> CanThrow 'NotATeamMember
               :> CanThrow ('ActionDenied 'RemoveConversationMember)
               :> CanThrow 'InvalidOperation
               :> CanThrow 'LegalHoldNotEnabled
               :> CanThrow 'LegalHoldDisableUnimplemented
               :> CanThrow 'LegalHoldServiceNotRegistered
               :> CanThrow 'UserLegalHoldIllegalOperation
               :> CanThrow 'LegalHoldCouldNotBlockConnections
               :> Description
                    "This endpoint can lead to the following events being sent:\n\
                    \- ClientRemoved event to members with a legalhold client (via brig)\n\
                    \- UserLegalHoldDisabled event to contacts of members with a legalhold client (via brig)"
               :> ZLocalUser
               :> "teams"
               :> Capture "tid" TeamId
               :> "legalhold"
               :> "settings"
               :> ReqBody '[JSON] RemoveLegalHoldSettingsRequest
               :> MultiVerb1 'DELETE '[JSON] (RespondEmpty 204 "Legal hold service settings deleted")
           )
    :<|> Named
           "get-legal-hold"
           ( Summary "Get legal hold status"
               :> CanThrow 'TeamMemberNotFound
               :> ZLocalUser
               :> "teams"
               :> Capture "tid" TeamId
               :> "legalhold"
               :> Capture "uid" UserId
               :> Get '[JSON] UserLegalHoldStatusResponse
           )
    :<|> Named
           "consent-to-legal-hold"
           ( Summary "Consent to legal hold"
               :> CanThrow ('ActionDenied 'RemoveConversationMember)
               :> CanThrow 'InvalidOperation
               :> CanThrow 'TeamMemberNotFound
               :> CanThrow 'UserLegalHoldIllegalOperation
               :> CanThrow 'LegalHoldCouldNotBlockConnections
               :> ZLocalUser
               :> "teams"
               :> Capture "tid" TeamId
               :> "legalhold"
               :> "consent"
               :> MultiVerb 'POST '[JSON] GrantConsentResultResponseTypes GrantConsentResult
           )
    :<|> Named
           "request-legal-hold-device"
           ( Summary "Request legal hold device"
               :> CanThrow ('ActionDenied 'RemoveConversationMember)
               :> CanThrow 'NotATeamMember
               :> CanThrow OperationDenied
               :> CanThrow 'TeamMemberNotFound
               :> CanThrow 'LegalHoldNotEnabled
               :> CanThrow 'UserLegalHoldAlreadyEnabled
               :> CanThrow 'NoUserLegalHoldConsent
               :> CanThrow 'LegalHoldServiceBadResponse
               :> CanThrow 'LegalHoldServiceNotRegistered
               :> CanThrow 'LegalHoldCouldNotBlockConnections
               :> CanThrow 'UserLegalHoldIllegalOperation
               :> Description
                    "This endpoint can lead to the following events being sent:\n\
                    \- LegalHoldClientRequested event to contacts of the user the device is requested for, if they didn't already have a legalhold client (via brig)"
               :> ZLocalUser
               :> "teams"
               :> Capture "tid" TeamId
               :> "legalhold"
               :> Capture "uid" UserId
               :> MultiVerb
                    'POST
                    '[JSON]
                    RequestDeviceResultResponseType
                    RequestDeviceResult
           )
    :<|> Named
           "disable-legal-hold-for-user"
           ( Summary "Disable legal hold for user"
               :> CanThrow AuthenticationError
               :> CanThrow ('ActionDenied 'RemoveConversationMember)
               :> CanThrow 'NotATeamMember
               :> CanThrow OperationDenied
               :> CanThrow 'LegalHoldServiceNotRegistered
               :> CanThrow 'UserLegalHoldIllegalOperation
               :> CanThrow 'LegalHoldCouldNotBlockConnections
               :> Description
                    "This endpoint can lead to the following events being sent:\n\
                    \- ClientRemoved event to the user owning the client (via brig)\n\
                    \- UserLegalHoldDisabled event to contacts of the user owning the client (via brig)"
               :> ZLocalUser
               :> "teams"
               :> Capture "tid" TeamId
               :> "legalhold"
               :> Capture "uid" UserId
               :> ReqBody '[JSON] DisableLegalHoldForUserRequest
               :> MultiVerb
                    'DELETE
                    '[JSON]
                    DisableLegalHoldForUserResponseType
                    DisableLegalHoldForUserResponse
           )
    :<|> Named
           "approve-legal-hold-device"
           ( Summary "Approve legal hold device"
               :> CanThrow AuthenticationError
               :> CanThrow 'AccessDenied
               :> CanThrow ('ActionDenied 'RemoveConversationMember)
               :> CanThrow 'NotATeamMember
               :> CanThrow 'LegalHoldNotEnabled
               :> CanThrow 'UserLegalHoldNotPending
               :> CanThrow 'NoLegalHoldDeviceAllocated
               :> CanThrow 'LegalHoldServiceNotRegistered
               :> CanThrow 'UserLegalHoldAlreadyEnabled
               :> CanThrow 'UserLegalHoldIllegalOperation
               :> CanThrow 'LegalHoldCouldNotBlockConnections
               :> Description
                    "This endpoint can lead to the following events being sent:\n\
                    \- ClientAdded event to the user owning the client (via brig)\n\
                    \- UserLegalHoldEnabled event to contacts of the user owning the client (via brig)\n\
                    \- ClientRemoved event to the user, if removing old client due to max number (via brig)"
               :> ZLocalUser
               :> ZConn
               :> "teams"
               :> Capture "tid" TeamId
               :> "legalhold"
               :> Capture "uid" UserId
               :> "approve"
               :> ReqBody '[JSON] ApproveLegalHoldForUserRequest
               :> MultiVerb1 'PUT '[JSON] (RespondEmpty 200 "Legal hold approved")
           )

type RequestDeviceResultResponseType =
  '[ RespondEmpty 201 "Request device successful",
     RespondEmpty 204 "Request device already pending"
   ]

data RequestDeviceResult
  = RequestDeviceSuccess
  | RequestDeviceAlreadyPending
  deriving (Generic)
  deriving (AsUnion RequestDeviceResultResponseType) via GenericAsUnion RequestDeviceResultResponseType RequestDeviceResult

instance GSOP.Generic RequestDeviceResult

type DisableLegalHoldForUserResponseType =
  '[ RespondEmpty 200 "Disable legal hold successful",
     RespondEmpty 204 "Legal hold was not enabled"
   ]

data DisableLegalHoldForUserResponse
  = DisableLegalHoldSuccess
  | DisableLegalHoldWasNotEnabled
  deriving (Generic)
  deriving (AsUnion DisableLegalHoldForUserResponseType) via GenericAsUnion DisableLegalHoldForUserResponseType DisableLegalHoldForUserResponse

instance GSOP.Generic DisableLegalHoldForUserResponse

type GrantConsentResultResponseTypes =
  '[ RespondEmpty 201 "Grant consent successful",
     RespondEmpty 204 "Consent already granted"
   ]

data GrantConsentResult
  = GrantConsentSuccess
  | GrantConsentAlreadyGranted
  deriving (Generic)
  deriving (AsUnion GrantConsentResultResponseTypes) via GenericAsUnion GrantConsentResultResponseTypes GrantConsentResult

instance GSOP.Generic GrantConsentResult

type TeamMemberAPI =
  Named
    "get-team-members"
    ( Summary "Get team members"
        :> CanThrow 'NotATeamMember
        :> ZLocalUser
        :> "teams"
        :> Capture "tid" TeamId
        :> "members"
        :> QueryParam'
             [ Optional,
               Strict,
               Description "Maximum results to be returned"
             ]
             "maxResults"
             (Range 1 HardTruncationLimit Int32)
        :> Get '[JSON] TeamMemberListOptPerms
    )
    :<|> Named
           "get-team-member"
           ( Summary "Get single team member"
               :> CanThrow 'NotATeamMember
               :> CanThrow 'TeamMemberNotFound
               :> ZLocalUser
               :> "teams"
               :> Capture "tid" TeamId
               :> "members"
               :> Capture "uid" UserId
               :> Get '[JSON] TeamMemberOptPerms
           )
    :<|> Named
           "get-team-members-by-ids"
           ( Summary "Get team members by user id list"
               :> Description "The `has_more` field in the response body is always `false`."
               :> CanThrow 'NotATeamMember
               :> CanThrow 'BulkGetMemberLimitExceeded
               :> ZLocalUser
               :> "teams"
               :> Capture "tid" TeamId
               :> "get-members-by-ids-using-post"
               :> QueryParam'
                    [ Optional,
                      Strict,
                      Description "Maximum results to be returned"
                    ]
                    "maxResults"
                    (Range 1 HardTruncationLimit Int32)
               :> ReqBody '[JSON] User.UserIdList
               :> Post '[JSON] TeamMemberListOptPerms
           )
    :<|> Named
           "add-team-member"
           ( Summary "Add a new team member"
               :> CanThrow 'InvalidPermissions
               :> CanThrow 'NoAddToBinding
               :> CanThrow 'NotATeamMember
               :> CanThrow 'NotConnected
               :> CanThrow OperationDenied
               :> CanThrow 'TeamNotFound
               :> CanThrow 'TooManyTeamMembers
               :> CanThrow 'UserBindingExists
               :> CanThrow 'TooManyTeamMembersOnTeamWithLegalhold
               :> ZLocalUser
               :> ZConn
               :> "teams"
               :> Capture "tid" TeamId
               :> "members"
               :> ReqBody '[JSON] NewTeamMember
               :> MultiVerb1
                    'POST
                    '[JSON]
                    (RespondEmpty 200 "")
           )
    :<|> Named
           "delete-team-member"
           ( Summary "Remove an existing team member"
               :> CanThrow AuthenticationError
               :> CanThrow 'AccessDenied
               :> CanThrow 'TeamMemberNotFound
               :> CanThrow 'TeamNotFound
               :> CanThrow 'NotATeamMember
               :> CanThrow OperationDenied
               :> ZLocalUser
               :> ZConn
               :> "teams"
               :> Capture "tid" TeamId
               :> "members"
               :> Capture "uid" UserId
               :> ReqBody '[JSON] TeamMemberDeleteData
               :> MultiVerb
                    'DELETE
                    '[JSON]
                    TeamMemberDeleteResultResponseType
                    TeamMemberDeleteResult
           )
    :<|> Named
           "delete-non-binding-team-member"
           ( Summary "Remove an existing team member"
               :> CanThrow AuthenticationError
               :> CanThrow 'AccessDenied
               :> CanThrow 'TeamMemberNotFound
               :> CanThrow 'TeamNotFound
               :> CanThrow 'NotATeamMember
               :> CanThrow OperationDenied
               :> ZLocalUser
               :> ZConn
               :> "teams"
               :> Capture "tid" TeamId
               :> "members"
               :> Capture "uid" UserId
               :> MultiVerb
                    'DELETE
                    '[JSON]
                    TeamMemberDeleteResultResponseType
                    TeamMemberDeleteResult
           )
    :<|> Named
           "update-team-member"
           ( Summary "Update an existing team member"
               :> CanThrow 'AccessDenied
               :> CanThrow 'InvalidPermissions
               :> CanThrow 'TeamNotFound
               :> CanThrow 'TeamMemberNotFound
               :> CanThrow 'NotATeamMember
               :> CanThrow OperationDenied
               :> ZLocalUser
               :> ZConn
               :> "teams"
               :> Capture "tid" TeamId
               :> "members"
               :> ReqBody '[JSON] NewTeamMember
               :> MultiVerb1
                    'PUT
                    '[JSON]
                    (RespondEmpty 200 "")
           )
    :<|> Named
           "get-team-members-csv"
           ( Summary "Get all members of the team as a CSV file"
               :> CanThrow 'AccessDenied
               :> Description
                    "The endpoint returns data in chunked transfer encoding.\
                    \ Internal server errors might result in a failed transfer\
                    \ instead of a 500 response."
               :> ZLocalUser
               :> "teams"
               :> Capture "tid" TeamId
               :> "members"
               :> "csv"
               :> LowLevelStream
                    'GET
                    200
                    '[ '( "Content-Disposition",
                          "attachment; filename=\"wire_team_members.csv\""
                        )
                     ]
                    "CSV of team members"
                    CSV
           )

type TeamMemberDeleteResultResponseType =
  '[ RespondEmpty 202 "Team member scheduled for deletion",
     RespondEmpty 200 ""
   ]

data TeamMemberDeleteResult
  = TeamMemberDeleteAccepted
  | TeamMemberDeleteCompleted
  deriving (Generic)
  deriving (AsUnion TeamMemberDeleteResultResponseType) via GenericAsUnion TeamMemberDeleteResultResponseType TeamMemberDeleteResult

instance GSOP.Generic TeamMemberDeleteResult

-- This is a work-around for the fact that we sometimes want to send larger lists of user ids
-- in the filter query than fits the url length limit.  For details, see
-- https://github.com/zinfra/backend-issues/issues/1248
type PostOtrDescriptionUnqualified =
  "This endpoint ensures that the list of clients is correct and only sends the message if the list is correct.\n\
  \To override this, the endpoint accepts two query params:\n\
  \- `ignore_missing`: Can be 'true' 'false' or a comma separated list of user IDs.\n\
  \  - When 'true' all missing clients are ignored.\n\
  \  - When 'false' all missing clients are reported.\n\
  \  - When comma separated list of user-ids, only clients for listed users are ignored.\n\
  \- `report_missing`: Can be 'true' 'false' or a comma separated list of user IDs.\n\
  \  - When 'true' all missing clients are reported.\n\
  \  - When 'false' all missing clients are ignored.\n\
  \  - When comma separated list of user-ids, only clients for listed users are reported.\n\
  \\n\
  \Apart from these, the request body also accepts `report_missing` which can only be a list of user ids and behaves the same way as the query parameter.\n\
  \\n\
  \All three of these should be considered mutually exclusive. The server however does not error if more than one is specified, it reads them in this order of precedence:\n\
  \- `report_missing` in the request body has highest precedence.\n\
  \- `ignore_missing` in the query param is the next.\n\
  \- `report_missing` in the query param has the lowest precedence.\n\
  \\n\
  \This endpoint can lead to OtrMessageAdd event being sent to the recipients.\n\
  \\n\
  \**NOTE:** The protobuf definitions of the request body can be found at https://github.com/wireapp/generic-message-proto/blob/master/proto/otr.proto."

type PostOtrDescription =
  "This endpoint ensures that the list of clients is correct and only sends the message if the list is correct.\n\
  \To override this, the endpoint accepts `client_mismatch_strategy` in the body. It can have these values:\n\
  \- `report_all`: When set, the message is not sent if any clients are missing. The missing clients are reported in the response.\n\
  \- `ignore_all`: When set, no checks about missing clients are carried out.\n\
  \- `report_only`: Takes a list of qualified UserIDs. If any clients of the listed users are missing, the message is not sent. The missing clients are reported in the response.\n\
  \- `ignore_only`: Takes a list of qualified UserIDs. If any clients of the non-listed users are missing, the message is not sent. The missing clients are reported in the response.\n\
  \\n\
  \The sending of messages in a federated conversation could theoretically fail partially. \
  \To make this case unlikely, the backend first gets a list of clients from all the involved backends and then tries to send a message. \
  \So, if any backend is down, the message is not propagated to anyone. \
  \But the actual message fan out to multiple backends could still fail partially. This type of failure is reported as a 201, \
  \the clients for which the message sending failed are part of the response body.\n\
  \\n\
  \This endpoint can lead to OtrMessageAdd event being sent to the recipients.\n\
  \\n\
  \**NOTE:** The protobuf definitions of the request body can be found at https://github.com/wireapp/generic-message-proto/blob/master/proto/otr.proto."

swaggerDoc :: Swagger.Swagger
swaggerDoc = toSwagger (Proxy @ServantAPI)
