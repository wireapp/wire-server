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

module Wire.API.Routes.Public.Galley.Conversation where

import Data.Code qualified as Code
import Data.CommaSeparatedList
import Data.Id
import Data.Misc
import Data.Range
import Data.SOP (I (..), NS (..))
import Imports hiding (head)
import Servant
import Servant.OpenApi.Internal.Orphans ()
import Wire.API.Conversation
import Wire.API.Conversation.Code
import Wire.API.Conversation.Protocol
import Wire.API.Conversation.Role
import Wire.API.Conversation.Typing
import Wire.API.Deprecated
import Wire.API.Error
import Wire.API.Error.Galley
import Wire.API.Event.Conversation
import Wire.API.MLS.GroupInfo
import Wire.API.MLS.Keys
import Wire.API.MLS.Servant
import Wire.API.MLS.SubConversation
import Wire.API.OAuth
import Wire.API.Routes.MultiVerb
import Wire.API.Routes.Named
import Wire.API.Routes.Public
import Wire.API.Routes.Public.Util
import Wire.API.Routes.QualifiedCapture
import Wire.API.Routes.Version
import Wire.API.Routes.Versioned
import Wire.API.Team.Feature

-- | A type similar to 'ResponseForExistedCreated' introduced to allow for a failure
-- to add remote members while creating a conversation or due to involved
-- backends forming an incomplete graph.
data CreateGroupConversationResponse
  = GroupConversationExisted Conversation
  | GroupConversationCreated CreateGroupConversation

instance
  ( ResponseType r1 ~ Conversation,
    ResponseType r2 ~ CreateGroupConversation
  ) =>
  AsUnion '[r1, r2] CreateGroupConversationResponse
  where
  toUnion (GroupConversationExisted x) = Z (I x)
  toUnion (GroupConversationCreated x) = S (Z (I x))

  fromUnion (Z (I x)) = GroupConversationExisted x
  fromUnion (S (Z (I x))) = GroupConversationCreated x
  fromUnion (S (S x)) = case x of {}

type ConversationHeaders = '[DescHeader "Location" "Conversation ID" ConvId]

type family ConversationResponse r

type instance ConversationResponse Conversation = ResponseForExistedCreated Conversation

type instance ConversationResponse CreateGroupConversation = CreateGroupConversationResponse

type ConversationVerb v r =
  MultiVerb
    'POST
    '[JSON]
    '[ WithHeaders
         ConversationHeaders
         Conversation
         (VersionedRespond v 200 "Conversation existed" Conversation),
       WithHeaders
         ConversationHeaders
         r
         (VersionedRespond v 201 "Conversation created" r)
     ]
    (ConversationResponse r)

type CreateConversationCodeVerb =
  MultiVerb
    'POST
    '[JSON]
    '[ Respond 200 "Conversation code already exists." ConversationCodeInfo,
       Respond 201 "Conversation code created." Event
     ]
    AddCodeResult

type ConvUpdateResponses = UpdateResponses "Conversation unchanged" "Conversation updated" Event

type ConvJoinResponses = UpdateResponses "Conversation unchanged" "Conversation joined" Event

type RemoveFromConversationVerb =
  MultiVerb
    'DELETE
    '[JSON]
    '[ RespondEmpty 204 "No change",
       Respond 200 "Member removed" Event
     ]
    (Maybe Event)

type ConversationAPI =
  Named
    "get-unqualified-conversation"
    ( Summary "Get a conversation by ID"
        :> Until 'V3
        :> CanThrow 'ConvNotFound
        :> CanThrow 'ConvAccessDenied
        :> ZLocalUser
        :> "conversations"
        :> Capture "cnv" ConvId
        :> MultiVerb1 'GET '[JSON] (VersionedRespond 'V2 200 "Conversation" Conversation)
    )
    :<|> Named
           "get-unqualified-conversation-legalhold-alias"
           -- This alias exists, so that it can be uniquely selected in zauth.acl
           ( Summary "Get a conversation by ID (Legalhold alias)"
               :> Until 'V2
               :> CanThrow 'ConvNotFound
               :> CanThrow 'ConvAccessDenied
               :> ZLocalUser
               :> "legalhold"
               :> "conversations"
               :> Capture "cnv" ConvId
               :> MultiVerb1 'GET '[JSON] (VersionedRespond 'V2 200 "Conversation" Conversation)
           )
    :<|> Named
           "get-conversation@v2"
           ( Summary "Get a conversation by ID"
               :> Until 'V3
               :> CanThrow 'ConvNotFound
               :> CanThrow 'ConvAccessDenied
               :> ZLocalUser
               :> "conversations"
               :> QualifiedCapture "cnv" ConvId
               :> MultiVerb1 'GET '[JSON] (VersionedRespond 'V2 200 "Conversation" Conversation)
           )
    :<|> Named
           "get-conversation@v5"
           ( Summary "Get a conversation by ID"
               :> From 'V3
               :> Until 'V6
               :> CanThrow 'ConvNotFound
               :> CanThrow 'ConvAccessDenied
               :> ZLocalUser
               :> "conversations"
               :> QualifiedCapture "cnv" ConvId
               :> MultiVerb1 'GET '[JSON] (VersionedRespond 'V5 200 "Conversation" Conversation)
           )
    :<|> Named
           "get-conversation"
           ( Summary "Get a conversation by ID"
               :> From 'V6
               :> CanThrow 'ConvNotFound
               :> CanThrow 'ConvAccessDenied
               :> ZLocalUser
               :> "conversations"
               :> QualifiedCapture "cnv" ConvId
               :> Get '[JSON] Conversation
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
           "get-group-info"
           ( Summary "Get MLS group information"
               :> From 'V5
               :> CanThrow 'ConvNotFound
               :> CanThrow 'MLSMissingGroupInfo
               :> CanThrow 'MLSNotEnabled
               :> ZLocalUser
               :> "conversations"
               :> QualifiedCapture "cnv" ConvId
               :> "groupinfo"
               :> MultiVerb1
                    'GET
                    '[MLS]
                    ( Respond
                        200
                        "The group information"
                        GroupInfoData
                    )
           )
    :<|> Named
           "list-conversation-ids-unqualified"
           ( Summary "[deprecated] Get all local conversation IDs."
               -- FUTUREWORK: add bounds to swagger schema for Range
               :> Until 'V3
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
           "list-conversation-ids-v2"
           ( Summary "Get all conversation IDs."
               :> Until 'V3
               :> Description PaginationDocs
               :> ZLocalUser
               :> "conversations"
               :> "list-ids"
               :> ReqBody '[Servant.JSON] GetPaginatedConversationIds
               :> Post '[Servant.JSON] ConvIdsPage
           )
    :<|> Named
           "list-conversation-ids"
           ( Summary "Get all conversation IDs."
               :> From 'V3
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
               :> Until 'V3
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
               :> MultiVerb1
                    'GET
                    '[JSON]
                    ( VersionedRespond
                        'V2
                        200
                        "List of local conversations"
                        (ConversationList Conversation)
                    )
           )
    :<|> Named
           "list-conversations@v1"
           ( Summary "Get conversation metadata for a list of conversation ids"
               :> Until 'V2
               :> ZLocalUser
               :> "conversations"
               :> "list"
               :> "v2"
               :> ReqBody '[JSON] ListConversations
               :> Post '[JSON] ConversationsResponse
           )
    :<|> Named
           "list-conversations@v2"
           ( Summary "Get conversation metadata for a list of conversation ids"
               :> From 'V2
               :> Until 'V3
               :> ZLocalUser
               :> "conversations"
               :> "list"
               :> ReqBody '[JSON] ListConversations
               :> MultiVerb1
                    'POST
                    '[JSON]
                    ( VersionedRespond
                        'V2
                        200
                        "Conversation page"
                        ConversationsResponse
                    )
           )
    :<|> Named
           "list-conversations@v5"
           ( Summary "Get conversation metadata for a list of conversation ids"
               :> From 'V3
               :> Until 'V6
               :> ZLocalUser
               :> "conversations"
               :> "list"
               :> ReqBody '[JSON] ListConversations
               :> MultiVerb1
                    'POST
                    '[JSON]
                    ( VersionedRespond
                        'V5
                        200
                        "Conversation page"
                        ConversationsResponse
                    )
           )
    :<|> Named
           "list-conversations"
           ( Summary "Get conversation metadata for a list of conversation ids"
               :> From 'V6
               :> ZLocalUser
               :> "conversations"
               :> "list"
               :> ReqBody '[JSON] ListConversations
               :> Post '[JSON] ConversationsResponse
           )
    -- This endpoint can lead to the following events being sent:
    -- - ConvCreate event to members
    :<|> Named
           "get-conversation-by-reusable-code"
           ( Summary "Get limited conversation information by key/code pair"
               :> CanThrow 'CodeNotFound
               :> CanThrow 'InvalidConversationPassword
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
           "create-group-conversation@v2"
           ( Summary "Create a new conversation"
               :> DescriptionOAuthScope 'WriteConversations
               :> Until 'V3
               :> CanThrow 'ConvAccessDenied
               :> CanThrow 'MLSNonEmptyMemberList
               :> CanThrow 'MLSNotEnabled
               :> CanThrow 'NotConnected
               :> CanThrow 'NotATeamMember
               :> CanThrow OperationDenied
               :> CanThrow 'MissingLegalholdConsent
               :> CanThrow UnreachableBackendsLegacy
               :> CanThrow 'NotAnMlsConversation
               :> CanThrow 'ChannelsNotEnabled
               :> Description "This returns 201 when a new conversation is created, and 200 when the conversation already existed"
               :> ZLocalUser
               :> ZOptConn
               :> "conversations"
               :> VersionedReqBody 'V2 '[Servant.JSON] NewConv
               :> ConversationVerb 'V2 Conversation
           )
    :<|> Named
           "create-group-conversation@v3"
           ( Summary "Create a new conversation"
               :> DescriptionOAuthScope 'WriteConversations
               :> From 'V3
               :> Until 'V4
               :> CanThrow 'ConvAccessDenied
               :> CanThrow 'MLSNonEmptyMemberList
               :> CanThrow 'MLSNotEnabled
               :> CanThrow 'NotConnected
               :> CanThrow 'NotATeamMember
               :> CanThrow OperationDenied
               :> CanThrow 'MissingLegalholdConsent
               :> CanThrow UnreachableBackendsLegacy
               :> CanThrow 'NotAnMlsConversation
               :> CanThrow 'ChannelsNotEnabled
               :> Description "This returns 201 when a new conversation is created, and 200 when the conversation already existed"
               :> ZLocalUser
               :> ZOptConn
               :> "conversations"
               :> ReqBody '[Servant.JSON] NewConv
               :> ConversationVerb 'V3 Conversation
           )
    :<|> Named
           "create-group-conversation@v5"
           ( Summary "Create a new conversation"
               :> From 'V4
               :> Until 'V6
               :> CanThrow 'ConvAccessDenied
               :> CanThrow 'MLSNonEmptyMemberList
               :> CanThrow 'MLSNotEnabled
               :> CanThrow 'NotConnected
               :> CanThrow 'NotATeamMember
               :> CanThrow OperationDenied
               :> CanThrow 'MissingLegalholdConsent
               :> CanThrow NonFederatingBackends
               :> CanThrow UnreachableBackends
               :> CanThrow 'NotAnMlsConversation
               :> CanThrow 'ChannelsNotEnabled
               :> Description "This returns 201 when a new conversation is created, and 200 when the conversation already existed"
               :> ZLocalUser
               :> ZOptConn
               :> "conversations"
               :> ReqBody '[Servant.JSON] NewConv
               :> ConversationVerb 'V5 CreateGroupConversation
           )
    :<|> Named
           "create-group-conversation"
           ( Summary "Create a new conversation"
               :> From 'V6
               :> CanThrow 'ConvAccessDenied
               :> CanThrow 'MLSNonEmptyMemberList
               :> CanThrow 'MLSNotEnabled
               :> CanThrow 'NotConnected
               :> CanThrow 'NotATeamMember
               :> CanThrow OperationDenied
               :> CanThrow 'MissingLegalholdConsent
               :> CanThrow NonFederatingBackends
               :> CanThrow UnreachableBackends
               :> CanThrow 'NotAnMlsConversation
               :> CanThrow 'ChannelsNotEnabled
               :> Description "This returns 201 when a new conversation is created, and 200 when the conversation already existed"
               :> ZLocalUser
               :> ZOptConn
               :> "conversations"
               :> ReqBody '[Servant.JSON] NewConv
               :> ConversationVerb 'V6 CreateGroupConversation
           )
    :<|> Named
           "create-self-conversation@v2"
           ( Summary "Create a self-conversation"
               :> Until 'V3
               :> ZLocalUser
               :> "conversations"
               :> "self"
               :> ConversationVerb 'V2 Conversation
           )
    :<|> Named
           "create-self-conversation@v5"
           ( Summary "Create a self-conversation"
               :> From 'V3
               :> Until 'V6
               :> ZLocalUser
               :> "conversations"
               :> "self"
               :> ConversationVerb 'V5 Conversation
           )
    :<|> Named
           "create-self-conversation"
           ( Summary "Create a self-conversation"
               :> From 'V6
               :> ZLocalUser
               :> "conversations"
               :> "self"
               :> ConversationVerb 'V6 Conversation
           )
    :<|> Named
           "get-mls-self-conversation@v5"
           ( Summary "Get the user's MLS self-conversation"
               :> From 'V5
               :> Until 'V6
               :> ZLocalUser
               :> "conversations"
               :> "mls-self"
               :> CanThrow 'MLSNotEnabled
               :> MultiVerb1
                    'GET
                    '[JSON]
                    ( VersionedRespond
                        'V5
                        200
                        "The MLS self-conversation"
                        Conversation
                    )
           )
    :<|> Named
           "get-mls-self-conversation"
           ( Summary "Get the user's MLS self-conversation"
               :> From 'V6
               :> ZLocalUser
               :> "conversations"
               :> "mls-self"
               :> CanThrow 'MLSNotEnabled
               :> MultiVerb1
                    'GET
                    '[JSON]
                    ( Respond
                        200
                        "The MLS self-conversation"
                        Conversation
                    )
           )
    :<|> Named
           "get-subconversation"
           ( Summary "Get information about an MLS subconversation"
               :> From 'V5
               :> CanThrow 'ConvNotFound
               :> CanThrow 'ConvAccessDenied
               :> CanThrow 'MLSSubConvUnsupportedConvType
               :> ZLocalUser
               :> "conversations"
               :> QualifiedCapture "cnv" ConvId
               :> "subconversations"
               :> Capture "subconv" SubConvId
               :> MultiVerb1
                    'GET
                    '[JSON]
                    ( Respond
                        200
                        "Subconversation"
                        PublicSubConversation
                    )
           )
    :<|> Named
           "leave-subconversation"
           ( Summary "Leave an MLS subconversation"
               :> From 'V5
               :> CanThrow 'ConvNotFound
               :> CanThrow 'ConvAccessDenied
               :> CanThrow 'MLSProtocolErrorTag
               :> CanThrow 'MLSStaleMessage
               :> CanThrow 'MLSNotEnabled
               :> ZLocalUser
               :> ZClient
               :> "conversations"
               :> QualifiedCapture "cnv" ConvId
               :> "subconversations"
               :> Capture "subconv" SubConvId
               :> "self"
               :> MultiVerb1
                    'DELETE
                    '[JSON]
                    (RespondEmpty 200 "OK")
           )
    :<|> Named
           "delete-subconversation"
           ( Summary "Delete an MLS subconversation"
               :> From 'V5
               :> CanThrow 'ConvAccessDenied
               :> CanThrow 'ConvNotFound
               :> CanThrow 'MLSNotEnabled
               :> CanThrow 'MLSStaleMessage
               :> ZLocalUser
               :> "conversations"
               :> QualifiedCapture "cnv" ConvId
               :> "subconversations"
               :> Capture "subconv" SubConvId
               :> ReqBody '[JSON] DeleteSubConversationRequest
               :> MultiVerb1
                    'DELETE
                    '[JSON]
                    (Respond 200 "Deletion successful" ())
           )
    :<|> Named
           "get-subconversation-group-info"
           ( Summary "Get MLS group information of subconversation"
               :> From 'V5
               :> CanThrow 'ConvNotFound
               :> CanThrow 'MLSMissingGroupInfo
               :> CanThrow 'MLSNotEnabled
               :> ZLocalUser
               :> "conversations"
               :> QualifiedCapture "cnv" ConvId
               :> "subconversations"
               :> Capture "subconv" SubConvId
               :> "groupinfo"
               :> MultiVerb1
                    'GET
                    '[MLS]
                    ( Respond
                        200
                        "The group information"
                        GroupInfoData
                    )
           )
    -- This endpoint can lead to the following events being sent:
    -- - ConvCreate event to members
    -- TODO: add note: "On 201, the conversation ID is the `Location` header"
    :<|> Named
           "create-one-to-one-conversation@v2"
           ( Summary "Create a 1:1 conversation"
               :> Until 'V3
               :> CanThrow 'ConvAccessDenied
               :> CanThrow 'InvalidOperation
               :> CanThrow 'NoBindingTeamMembers
               :> CanThrow 'NonBindingTeam
               :> CanThrow 'NotATeamMember
               :> CanThrow 'NotConnected
               :> CanThrow OperationDenied
               :> CanThrow 'TeamNotFound
               :> CanThrow 'MissingLegalholdConsent
               :> CanThrow UnreachableBackendsLegacy
               :> ZLocalUser
               :> ZConn
               :> "conversations"
               :> "one2one"
               :> ReqBody '[JSON] NewOne2OneConv
               :> ConversationVerb 'V2 Conversation
           )
    :<|> Named
           "create-one-to-one-conversation@v6"
           ( Summary "Create a 1:1 conversation"
               :> From 'V3
               :> Until 'V7
               :> CanThrow 'ConvAccessDenied
               :> CanThrow 'InvalidOperation
               :> CanThrow 'NoBindingTeamMembers
               :> CanThrow 'NonBindingTeam
               :> CanThrow 'NotATeamMember
               :> CanThrow 'NotConnected
               :> CanThrow OperationDenied
               :> CanThrow 'TeamNotFound
               :> CanThrow 'MissingLegalholdConsent
               :> CanThrow UnreachableBackendsLegacy
               :> ZLocalUser
               :> ZConn
               :> "conversations"
               :> "one2one"
               :> ReqBody '[JSON] NewOne2OneConv
               :> ConversationVerb 'V3 Conversation
           )
    :<|> Named
           "create-one-to-one-conversation"
           ( Summary "Create a 1:1 conversation"
               :> From 'V7
               :> CanThrow 'ConvAccessDenied
               :> CanThrow 'InvalidOperation
               :> CanThrow 'NoBindingTeamMembers
               :> CanThrow 'NonBindingTeam
               :> CanThrow 'NotATeamMember
               :> CanThrow 'NotConnected
               :> CanThrow OperationDenied
               :> CanThrow 'TeamNotFound
               :> CanThrow 'MissingLegalholdConsent
               :> CanThrow UnreachableBackendsLegacy
               :> ZLocalUser
               :> ZConn
               :> "one2one-conversations"
               :> ReqBody '[JSON] NewOne2OneConv
               :> ConversationVerb 'V3 Conversation
           )
    :<|> Named
           "get-one-to-one-mls-conversation@v5"
           ( Summary "Get an MLS 1:1 conversation"
               :> From 'V5
               :> Until 'V6
               :> ZLocalUser
               :> CanThrow 'MLSNotEnabled
               :> CanThrow 'NotConnected
               :> CanThrow 'MLSFederatedOne2OneNotSupported
               :> "conversations"
               :> "one2one"
               :> QualifiedCapture "usr" UserId
               :> MultiVerb1 'GET '[JSON] (VersionedRespond 'V5 200 "MLS 1-1 conversation" Conversation)
           )
    :<|> Named
           "get-one-to-one-mls-conversation@v6"
           ( Summary "Get an MLS 1:1 conversation"
               :> From 'V6
               :> Until 'V7
               :> ZLocalUser
               :> CanThrow 'MLSNotEnabled
               :> CanThrow 'NotConnected
               :> "conversations"
               :> "one2one"
               :> QualifiedCapture "usr" UserId
               :> MultiVerb1 'GET '[JSON] (Respond 200 "MLS 1-1 conversation" (MLSOne2OneConversation MLSPublicKey))
           )
    :<|> Named
           "get-one-to-one-mls-conversation"
           ( Summary "Get an MLS 1:1 conversation"
               :> From 'V7
               :> ZLocalUser
               :> CanThrow 'MLSNotEnabled
               :> CanThrow 'NotConnected
               :> "one2one-conversations"
               :> QualifiedCapture "usr" UserId
               :> QueryParam "format" MLSPublicKeyFormat
               :> MultiVerb1 'GET '[JSON] (Respond 200 "MLS 1-1 conversation" (MLSOne2OneConversation SomeKey))
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
               :> CanThrow NonFederatingBackends
               :> CanThrow UnreachableBackends
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
               :> CanThrow NonFederatingBackends
               :> CanThrow UnreachableBackends
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
               :> CanThrow NonFederatingBackends
               :> CanThrow UnreachableBackends
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
           ( Summary "Join a conversation by its ID (if link access enabled) (deprecated)"
               :> Until 'V5
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
           ( Summary "Join a conversation using a reusable code"
               :> Description
                    "If the guest links team feature is disabled, this will fail with 409 GuestLinksDisabled.\
                    \Note that this is currently inconsistent (for backwards compatibility reasons) with `POST /conversations/code-check` which responds with 404 CodeNotFound if guest links are disabled."
               :> CanThrow 'CodeNotFound
               :> CanThrow 'InvalidConversationPassword
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
               :> ReqBody '[Servant.JSON] JoinConversationByCode
               :> MultiVerb 'POST '[Servant.JSON] ConvJoinResponses (UpdateResult Event)
           )
    :<|> Named
           "code-check"
           ( Summary "Check validity of a conversation code."
               :> Description
                    "If the guest links team feature is disabled, this will fail with 404 CodeNotFound.\
                    \Note that this is currently inconsistent (for backwards compatibility reasons) with `POST /conversations/join` which responds with 409 GuestLinksDisabled if guest links are disabled."
               :> CanThrow 'CodeNotFound
               :> CanThrow 'ConvNotFound
               :> CanThrow 'InvalidConversationPassword
               :> Header' [Required, Strict] "X-Forwarded-For" IpAddr
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
           "create-conversation-code-unqualified@v3"
           ( Summary "Create or recreate a conversation code"
               :> Until 'V4
               :> DescriptionOAuthScope 'WriteConversationsCode
               :> CanThrow 'ConvAccessDenied
               :> CanThrow 'ConvNotFound
               :> CanThrow 'GuestLinksDisabled
               :> CanThrow 'CreateConversationCodeConflict
               :> ZUser
               :> ZHostOpt
               :> ZOptConn
               :> "conversations"
               :> Capture' '[Description "Conversation ID"] "cnv" ConvId
               :> "code"
               :> CreateConversationCodeVerb
           )
    -- this endpoint can lead to the following events being sent:
    -- - ConvCodeUpdate event to members, if code didn't exist before
    :<|> Named
           "create-conversation-code-unqualified"
           ( Summary "Create or recreate a conversation code"
               :> From 'V4
               :> DescriptionOAuthScope 'WriteConversationsCode
               :> CanThrow 'ConvAccessDenied
               :> CanThrow 'ConvNotFound
               :> CanThrow 'GuestLinksDisabled
               :> CanThrow 'CreateConversationCodeConflict
               :> ZUser
               :> ZHostOpt
               :> ZOptConn
               :> "conversations"
               :> Capture' '[Description "Conversation ID"] "cnv" ConvId
               :> "code"
               :> ReqBody '[JSON] CreateConversationCodeRequest
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
               :> Get '[Servant.JSON] (LockableFeature GuestLinksConfig)
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
               :> ZHostOpt
               :> ZLocalUser
               :> "conversations"
               :> Capture' '[Description "Conversation ID"] "cnv" ConvId
               :> "code"
               :> MultiVerb
                    'GET
                    '[JSON]
                    '[Respond 200 "Conversation Code" ConversationCodeInfo]
                    ConversationCodeInfo
           )
    -- This endpoint can lead to the following events being sent:
    -- - Typing event to members
    :<|> Named
           "member-typing-unqualified"
           ( Summary "Sending typing notifications"
               :> Until 'V3
               :> CanThrow 'ConvNotFound
               :> ZLocalUser
               :> ZConn
               :> "conversations"
               :> Capture' '[Description "Conversation ID"] "cnv" ConvId
               :> "typing"
               :> ReqBody '[JSON] TypingStatus
               :> MultiVerb 'POST '[JSON] '[RespondEmpty 200 "Notification sent"] ()
           )
    :<|> Named
           "member-typing-qualified"
           ( Summary "Sending typing notifications"
               :> CanThrow 'ConvNotFound
               :> ZLocalUser
               :> ZConn
               :> "conversations"
               :> QualifiedCapture' '[Description "Conversation ID"] "cnv" ConvId
               :> "typing"
               :> ReqBody '[JSON] TypingStatus
               :> MultiVerb 'POST '[JSON] '[RespondEmpty 200 "Notification sent"] ()
           )
    -- This endpoint can lead to the following events being sent:
    -- - MemberLeave event to members
    :<|> Named
           "remove-member-unqualified"
           ( Summary "Remove a member from a conversation (deprecated)"
               :> Until 'V2
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
               :> Until V7
               :> Deprecated
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
               :> Deprecated
               :> Until 'V8
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
               :> Deprecated
               :> Until 'V8
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
               :> Deprecated
               :> Until 'V8
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
               :> Deprecated
               :> Until 'V8
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
               :> Until 'V3
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
               :> VersionedReqBody 'V2 '[JSON] ConversationAccessData
               :> MultiVerb
                    'PUT
                    '[JSON]
                    (UpdateResponses "Access unchanged" "Access updated" Event)
                    (UpdateResult Event)
           )
    :<|> Named
           "update-conversation-access@v2"
           ( Summary "Update access modes for a conversation"
               :> Until 'V3
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
               :> VersionedReqBody 'V2 '[JSON] ConversationAccessData
               :> MultiVerb
                    'PUT
                    '[JSON]
                    (UpdateResponses "Access unchanged" "Access updated" Event)
                    (UpdateResult Event)
           )
    :<|> Named
           "update-conversation-access"
           ( Summary "Update access modes for a conversation"
               :> From 'V3
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
               :> Description "Use `/conversations/:domain/:conv` instead and get the self member from `response.members.self`."
               :> Deprecated
               :> Until 'V8
               :> ZLocalUser
               :> "conversations"
               :> Capture' '[Description "Conversation ID"] "cnv" ConvId
               :> "self"
               :> Get '[JSON] (Maybe Member)
           )
    :<|> Named
           "update-conversation-self-unqualified"
           ( Summary "Update self membership properties (deprecated)"
               :> Deprecated
               :> Until 'V8
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
    :<|> Named
           "update-conversation-protocol"
           ( Summary "Update the protocol of the conversation"
               :> From 'V5
               :> Description "**Note**: Only proteus->mixed upgrade is supported."
               :> CanThrow 'ConvNotFound
               :> CanThrow 'ConvInvalidProtocolTransition
               :> CanThrow ('ActionDenied 'LeaveConversation)
               :> CanThrow 'InvalidOperation
               :> CanThrow 'MLSMigrationCriteriaNotSatisfied
               :> CanThrow 'NotATeamMember
               :> CanThrow OperationDenied
               :> CanThrow 'TeamNotFound
               :> ZLocalUser
               :> ZConn
               :> "conversations"
               :> QualifiedCapture' '[Description "Conversation ID"] "cnv" ConvId
               :> "protocol"
               :> ReqBody '[JSON] ProtocolUpdate
               :> MultiVerb 'PUT '[Servant.JSON] ConvUpdateResponses (UpdateResult Event)
           )
