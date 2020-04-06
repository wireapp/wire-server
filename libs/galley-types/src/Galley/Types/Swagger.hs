{-# LANGUAGE OverloadedStrings #-}

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

module Galley.Types.Swagger where

import Data.Aeson (encode)
import Data.String.Conversions (cs)
import Data.Swagger.Build.Api as Swagger
import Galley.Types (Access)
import Imports
import qualified Wire.Swagger as Swagger

galleyModels :: [Model]
galleyModels =
  [ connect,
    connectEvent,
    conversation,
    conversations,
    conversationIds,
    conversationMembers,
    conversationUpdateName,
    conversationAccessUpdate,
    conversationReceiptModeUpdate,
    conversationMessageTimerUpdate,
    conversationCode,
    conversationNameUpdateEvent,
    conversationRole,
    conversationRolesList,
    errorObj,
    event,
    invite,
    member,
    memberEvent,
    memberUpdate,
    memberUpdateData,
    memberUpdateEvent,
    members,
    newConversation,
    otherMember,
    typing,
    typingEvent,
    otrMessage,
    newOtrMessage,
    otrRecipients,
    otrClientMap,
    userClients,
    clientMismatch,
    serviceRef,
    teamInfo,
    legalHoldTeamConfig,
    ssoTeamConfig,
    customBackend
  ]

event :: Model
event = defineModel "Event" $ do
  description "Event data"
  property "type" eventType $
    description "Event type"
  property "conversation" bytes' $
    description "Conversation ID"
  property "from" bytes' $
    description "User ID"
  property "time" dateTime' $
    description "Date and time this event occurred"
  children
    "type"
    [ memberEvent,
      connectEvent,
      conversationNameUpdateEvent,
      memberUpdateEvent,
      typingEvent,
      otrMessageEvent,
      conversationAccessUpdateEvent,
      conversationReceiptModeUpdateEvent,
      conversationMessageTimerUpdateEvent,
      conversationCodeUpdateEvent,
      conversationCodeDeleteEvent
    ]

eventType :: DataType
eventType =
  string $
    enum
      [ "conversation.member-join",
        "conversation.member-leave",
        "conversation.member-update",
        "conversation.rename",
        "conversation.access-update",
        "conversation.receipt-mode-update",
        "conversation.message-timer-update",
        "conversation.code-update",
        "conversation.code-delete",
        "conversation.create",
        "conversation.delete",
        "conversation.connect-request",
        "conversation.typing",
        "conversation.otr-message-add"
      ]

otrMessageEvent :: Model
otrMessageEvent = defineModel "OtrMessage" $ do
  description "off-the-record message event"
  property "data" (ref otrMessage) $ description "OTR message"

memberEvent :: Model
memberEvent = defineModel "MemberEvent" $ do
  description "member event"
  property "data" (ref members) $ description "members data"

connectEvent :: Model
connectEvent = defineModel "ConnectEvent" $ do
  description "connect event"
  property "data" (ref connect) $ description "connect data"

conversationNameUpdateEvent :: Model
conversationNameUpdateEvent = defineModel "ConversationNameUpdateEvent" $ do
  description "conversation update event"
  property "data" (ref conversationUpdateName) $ description "conversation name"

conversationRole :: Model
conversationRole = defineModel "ConversationRole" $ do
  description "Conversation role"
  property "conversation_role" string' $
    description
      "role name, between 2 and 128 chars, 'wire_' prefix \
      \is reserved for roles designed by Wire (i.e., no \
      \custom roles can have the same prefix)"
  property "actions" (array conversationRoleAction) $
    description "The set of actions allowed for this role"

conversationRoleAction :: DataType
conversationRoleAction =
  string $
    enum
      [ "add_conversation_member",
        "remove_conversation_member",
        "modify_conversation_name",
        "modify_conversation_message_timer",
        "modify_conversation_receipt_mode",
        "modify_conversation_access",
        "modify_other_conversation_member",
        "leave_conversation",
        "delete_conversation"
      ]

conversationRolesList :: Model
conversationRolesList = defineModel "ConversationRolesList" $ do
  description "list of roles allowed in the given conversation"
  property "conversation_roles" (unique $ array (ref conversationRole)) $
    description "the array of conversation roles"

conversationAccessUpdateEvent :: Model
conversationAccessUpdateEvent = defineModel "ConversationAccessUpdateEvent" $ do
  description "conversation access update event"
  property "data" (ref conversationAccessUpdate) $ description "conversation access data"

conversationReceiptModeUpdateEvent :: Model
conversationReceiptModeUpdateEvent = defineModel "ConversationReceiptModeUpdateEvent" $ do
  description "conversation receipt mode update event"
  property "data" (ref conversationReceiptModeUpdate) $ description "conversation receipt mode data"

conversationMessageTimerUpdateEvent :: Model
conversationMessageTimerUpdateEvent = defineModel "ConversationMessageTimerUpdateEvent" $ do
  description "conversation message timer update event"
  property "data" (ref conversationMessageTimerUpdate) $ description "conversation message timer data"

conversationCodeUpdateEvent :: Model
conversationCodeUpdateEvent = defineModel "ConversationCodeUpdateEvent" $ do
  description "conversation code update event"
  property "data" (ref conversationCode) $ description "conversation code data"

conversationCodeDeleteEvent :: Model
conversationCodeDeleteEvent =
  defineModel "ConversationCodeDeleteEvent" $
    description "conversation code delete event"

memberUpdateEvent :: Model
memberUpdateEvent = defineModel "MemberUpdateEvent" $ do
  description "member update event"
  property "data" (ref memberUpdateData) $ description "member data"

typingEvent :: Model
typingEvent = defineModel "TypingEvent" $ do
  description "typing event"
  property "data" (ref typing) $ description "typing data"

conversation :: Model
conversation = defineModel "Conversation" $ do
  description "A conversation object as returned from the server"
  property "id" bytes' $
    description "Conversation ID"
  property "type" conversationType $
    description "The conversation type of this object (0 = regular, 1 = self, 2 = 1:1, 3 = connect)"
  property "creator" bytes' $
    description "The creator's user ID."
  -- TODO: property "access"
  -- property "access_role"
  property "name" string' $ do
    description "The conversation name (can be null)"
  property "members" (ref conversationMembers) $
    description "The current set of conversation members"
  -- property "team"
  property "message_timer" (int64 (Swagger.min 0)) $ do
    description "Per-conversation message timer (can be null)"

conversationType :: DataType
conversationType = int32 $ enum [0, 1, 2, 3]

otrMessage :: Model
otrMessage = defineModel "OtrMessage" $ do
  description "Encrypted message of a conversation"
  property "sender" bytes' $
    description "The sender's client ID"
  property "recipient" bytes' $
    description "The recipient's client ID"
  property "text" bytes' $
    description "The ciphertext for the recipient (Base64 in JSON)"
  property "data" bytes' $ do
    description
      "Extra (symmetric) data (i.e. ciphertext, Base64 in JSON) \
      \that is common with all other recipients."
    optional

priority :: DataType
priority =
  string $
    enum
      [ "low",
        "high"
      ]

newOtrMessage :: Model
newOtrMessage = defineModel "NewOtrMessage" $ do
  description "OTR message per recipient"
  property "sender" bytes' $
    description "The sender's client ID"
  property "recipients" (ref otrRecipients) $
    description "Per-recipient data (i.e. ciphertext)."
  property "native_push" bool' $ do
    description "Whether to issue a native push to offline clients."
    optional
  property "transient" bool' $ do
    description "Whether to put this message into the notification queue."
    optional
  property "native_priority" priority $ do
    description "The native push priority (default 'high')."
    optional
  property "data" bytes' $ do
    description
      "Extra (symmetric) data (i.e. ciphertext) that is replicated \
      \for each recipient."
    optional

otrRecipients :: Model
otrRecipients = defineModel "OtrRecipients" $ do
  description "Recipients of OTR content."
  property "" (ref otrClientMap) $
    description "Mapping of user IDs to 'OtrClientMap's."

otrClientMap :: Model
otrClientMap = defineModel "OtrClientMap" $ do
  description "Map of client IDs to OTR content."
  property "" bytes' $
    description "Mapping from client IDs to OTR content (Base64 in JSON)."

clientMismatch :: Model
clientMismatch = defineModel "ClientMismatch" $ do
  description "Map of missing, redundant or deleted clients."
  property "time" dateTime' $
    description "Server timestamp (date and time)"
  property "missing" (ref userClients) $
    description "Map of missing clients per user."
  property "redundant" (ref userClients) $
    description "Map of redundant clients per user."
  property "deleted" (ref userClients) $
    description "Map of deleted clients per user."

userClients :: Model
userClients =
  defineModel "UserClients"
    $ property "" (unique $ array bytes')
    $ description "Map of user IDs to sets of client IDs ({ UserId: [ClientId] })."

members :: Model
members =
  defineModel "Members"
    $ property "users" (unique $ array bytes')
    $ description "List of user IDs"

conversationUpdateName :: Model
conversationUpdateName = defineModel "ConversationUpdateName" $ do
  description "Contains conversation name to update"
  property "name" string' $
    description "The new conversation name"

conversationAccessUpdate :: Model
conversationAccessUpdate = defineModel "ConversationAccessUpdate" $ do
  description "Contains conversation properties to update"
  property "access" (unique $ array access) $
    description "List of conversation access modes."
  property "access_role" (bytes') $
    description "Conversation access role: private|team|activated|non_activated"

access :: DataType
access = string . enum $ cs . encode <$> [(minBound :: Access) ..]

conversationReceiptModeUpdate :: Model
conversationReceiptModeUpdate = defineModel "conversationReceiptModeUpdate" $ do
  description
    "Contains conversation receipt mode to update to. Receipt mode tells \
    \clients whether certain types of receipts should be sent in the given \
    \conversation or not. How this value is interpreted is up to clients."
  property "receipt_mode" int32' $
    description "Receipt mode: int32"

conversationMessageTimerUpdate :: Model
conversationMessageTimerUpdate = defineModel "ConversationMessageTimerUpdate" $ do
  description "Contains conversation properties to update"
  property "message_timer" int64' $
    description "Conversation message timer (in milliseconds); can be null"

conversationCode :: Model
conversationCode = defineModel "ConversationCode" $ do
  description "Contains conversation properties to update"
  property "key" string' $
    description "Stable conversation identifier"
  property "code" string' $
    description "Conversation code (random)"
  property "uri" string' $ do
    description "Full URI (containing key/code) to join a conversation"
    optional

conversationMembers :: Model
conversationMembers = defineModel "ConversationMembers" $ do
  description "Object representing users of a conversation."
  property "self" (ref member) $
    description "The user ID of the requestor"
  property "others" (unique (array (ref otherMember))) $
    description "All other current users of this conversation"

member :: Model
member = defineModel "Member" $ do
  property "id" bytes' $
    description "User ID"
  property "otr_muted" bool' $ do
    description "Whether the conversation is muted"
    optional
  property "otr_muted_ref" bytes' $ do
    description "A reference point for (un)muting"
    optional
  property "otr_archived" bool' $ do
    description "Whether the conversation is archived"
    optional
  property "otr_archived_ref" bytes' $ do
    description "A reference point for (un)archiving"
    optional
  property "hidden" bool' $ do
    description "Whether the conversation is hidden"
    optional
  property "hidden_ref" bytes' $ do
    description "A reference point for (un)hiding"
    optional
  property "service" (ref serviceRef) $ do
    description "The reference to the owning service, if the member is a 'bot'."
    optional

otherMember :: Model
otherMember = defineModel "OtherMember" $ do
  property "id" bytes' $
    description "User ID"
  property "service" (ref serviceRef) $ do
    description "The reference to the owning service, if the member is a 'bot'."
    optional

newConversation :: Model
newConversation = defineModel "NewConversation" $ do
  description "JSON object to create a new conversation"
  property "users" (unique $ array bytes') $
    description "List of user IDs (excluding the requestor) to be part of this conversation"
  property "name" string' $ do
    description "The conversation name"
    optional
  property "team" (ref teamInfo) $ do
    description "Team information of this conversation"
    optional
  -- TODO: property "access"
  -- property "access_role"
  property "message_timer" (int64 (Swagger.min 0)) $ do
    description "Per-conversation message timer"
    optional
  property "receipt_mode" (int32 (Swagger.min 0)) $ do
    description "Conversation receipt mode"
    optional

teamInfo :: Model
teamInfo = defineModel "TeamInfo" $ do
  description "Team information"
  property "teamid" bytes' $
    description "Team ID"
  property "managed" bool' $
    description "Is this a managed team conversation?"

conversationIds :: Model
conversationIds = defineModel "ConversationIds" $ do
  description "Object holding a list of conversation IDs"
  property "conversations" (unique $ array string') end
  property "has_more" bool' $
    description "Indicator that the server has more IDs than returned"

conversations :: Model
conversations = defineModel "Conversations" $ do
  description "Object holding a list of conversations"
  property "conversations" (unique $ array (ref conversation)) end
  property "has_more" bool' $
    description "Indicator that the server has more conversations than returned"

invite :: Model
invite = defineModel "Invite" $ do
  description "Add users to a conversation"
  property "users" (unique $ array bytes') $
    description "List of user IDs to add to a conversation"

memberUpdate :: Model
memberUpdate = defineModel "MemberUpdate" $ do
  description "Update user properties relative to a conversation"
  property "otr_muted" bool' $ do
    description "Whether to notify on conversation updates"
    optional
  property "otr_muted_ref" bytes' $ do
    description "A reference point for (un)muting"
    optional
  property "otr_archived" bool' $ do
    description "Whether to notify on conversation updates"
    optional
  property "otr_archived_ref" bytes' $ do
    description "A reference point for (un)archiving"
    optional
  property "hidden" bool' $ do
    description "Whether the conversation is hidden"
    optional
  property "hidden_ref" bytes' $ do
    description "A reference point for (un)hiding"
    optional
  property "conversation_role" string' $ do
    description "Name of the conversation role to update to"
    optional

otherMemberUpdate :: Model
otherMemberUpdate = defineModel "otherMemberUpdate" $ do
  description "Update user properties of other members relative to a conversation"
  property "conversation_role" string' $ do
    description "Name of the conversation role updated to"
    optional

memberUpdateData :: Model
memberUpdateData = defineModel "MemberUpdateData" $ do
  description "Event data on member updates"
  property "target" bytes' $ do
    description "Target ID of the user that the action was performed on"
    optional
  property "otr_muted" bool' $ do
    description "Whether to notify on conversation updates"
    optional
  property "otr_muted_ref" bytes' $ do
    description "A reference point for (un)muting"
    optional
  property "otr_archived" bool' $ do
    description "Whether to notify on conversation updates"
    optional
  property "otr_archived_ref" bytes' $ do
    description "A reference point for (un)archiving"
    optional
  property "hidden" bool' $ do
    description "Whether the conversation is hidden"
    optional
  property "hidden_ref" bytes' $ do
    description "A reference point for (un)hiding"
    optional
  property "conversation_role" string' $ do
    description "Name of the conversation role to update to"
    optional

otherMemberUpdateData :: Model
otherMemberUpdateData = defineModel "OtherMemberUpdateData" $ do
  description "Event data on other member updates"
  property "target" bytes' $ do
    description "Target ID of the user that the action was performed on"
    optional
  property "conversation_role" string' $ do
    description "Name of the conversation role to update to"
    optional

typing :: Model
typing = defineModel "Typing" $ do
  description "Data to describe typing info"
  property "status" typingStatus $ description "typing status"

typingStatus :: DataType
typingStatus =
  string $
    enum
      [ "started",
        "stopped"
      ]

connect :: Model
connect = defineModel "Connect" $ do
  description "user to user connection request"
  property "recipient" bytes' $
    description "The user ID to connect to"
  property "message" string' $
    description "Initial message to send to user"
  property "name" string' $
    description "Name of requestor"
  property "email" string' $ do
    description "E-Mail of requestor"
    optional

serviceRef :: Model
serviceRef = defineModel "ServiceRef" $ do
  description "Service Reference"
  property "id" bytes' $
    description "Service ID"
  property "provider" bytes' $
    description "Provider ID"

errorObj :: Model
errorObj = Swagger.errorModel

legalHoldTeamConfig :: Model
legalHoldTeamConfig = defineModel "LegalHoldTeamConfig" $ do
  description "Configuration of LegalHold feature for team"
  property "status" featureStatus $ description "status"

ssoTeamConfig :: Model
ssoTeamConfig = defineModel "SSOTeamConfig" $ do
  description "Configuration of SSO feature for team"
  property "status" featureStatus $ description "status"

featureStatus :: DataType
featureStatus =
  string $
    enum
      [ "enabled",
        "disabled"
      ]

customBackend :: Model
customBackend = defineModel "CustomBackend" $ do
  description "Description of a custom backend"
  property "config_json_url" string' $
    description "the location of the custom backend's config.json file"
  property "webapp_welcome_url" string' $
    description "the location of the custom webapp"
