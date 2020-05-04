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

import qualified Data.Swagger.Build.Api as Doc
import Imports
import Wire.API.Conversation (modelConversationAccessUpdate, modelConversationMessageTimerUpdate, modelConversationReceiptModeUpdate, modelConversationUpdateName)
import Wire.API.Conversation.Code (modelConversationCode)
import Wire.API.Conversation.Typing (modelTyping)
import qualified Wire.Swagger as Swagger

-- TODO(wire-api): check if all models are used
galleyModels :: [Doc.Model]
galleyModels =
  [ modelServiceRef,
    modelNewOtrMessage,
    modelOtrRecipients,
    modelOtrClientMap,
    modelClientMismatch,
    modelUserClients,
    modelUserIdList,
    modelCustomBackend,
    --
    modelEvent,
    modelMemberUpdateEvent,
    modelMemberUpdateData,
    modelConnectEvent,
    modelConnect,
    modelConversationNameUpdateEvent,
    modelMemberEvent,
    modelMembers,
    modelTypingEvent,
    -- FUTUREWORK:
    -- The following events are used as children/subTypes of 'event',
    -- but have not been added to the list of models here.
    -- We should probably add them, but I don't want to do it as part
    -- of the current refactoring I'm doing.
    -- otrMessageEvent
    -- conversationAccessUpdateEvent,
    -- conversationReceiptModeUpdateEvent,
    -- conversationMessageTimerUpdateEvent,
    -- conversationCodeUpdateEvent,
    -- conversationCodeDeleteEvent
    modelOtrMessage -- used in otrMessageEvent
  ]

modelEvent :: Doc.Model
modelEvent = Doc.defineModel "Event" $ do
  Doc.description "Event data"
  Doc.property "type" typeEventType $
    Doc.description "Event type"
  Doc.property "conversation" Doc.bytes' $
    Doc.description "Conversation ID"
  Doc.property "from" Doc.bytes' $
    Doc.description "User ID"
  Doc.property "time" Doc.dateTime' $
    Doc.description "Date and time this event occurred"
  -- This doesn't really seem to work in swagger-ui.
  -- The children/subTypes are not displayed.
  Doc.children
    "type"
    [ modelMemberEvent,
      modelConnectEvent,
      modelMemberUpdateEvent,
      modelTypingEvent,
      modelOtrMessageEvent,
      modelConversationNameUpdateEvent,
      modelConversationAccessUpdateEvent,
      modelConversationReceiptModeUpdateEvent,
      modelConversationMessageTimerUpdateEvent,
      modelConversationCodeUpdateEvent,
      modelConversationCodeDeleteEvent
    ]

typeEventType :: Doc.DataType
typeEventType =
  Doc.string $
    Doc.enum
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

modelOtrMessageEvent :: Doc.Model
modelOtrMessageEvent = Doc.defineModel "OtrMessage" $ do
  Doc.description "off-the-record message event"
  Doc.property "data" (Doc.ref modelOtrMessage) $ Doc.description "OTR message"

modelMemberEvent :: Doc.Model
modelMemberEvent = Doc.defineModel "MemberEvent" $ do
  Doc.description "member event"
  Doc.property "data" (Doc.ref modelMembers) $ Doc.description "members data"

modelConnectEvent :: Doc.Model
modelConnectEvent = Doc.defineModel "ConnectEvent" $ do
  Doc.description "connect event"
  Doc.property "data" (Doc.ref modelConnect) $ Doc.description "connect data"

modelConversationNameUpdateEvent :: Doc.Model
modelConversationNameUpdateEvent = Doc.defineModel "ConversationNameUpdateEvent" $ do
  Doc.description "conversation update event"
  Doc.property "data" (Doc.ref modelConversationUpdateName) $ Doc.description "conversation name"

modelConversationAccessUpdateEvent :: Doc.Model
modelConversationAccessUpdateEvent = Doc.defineModel "ConversationAccessUpdateEvent" $ do
  Doc.description "conversation access update event"
  Doc.property "data" (Doc.ref modelConversationAccessUpdate) $ Doc.description "conversation access data"

modelConversationReceiptModeUpdateEvent :: Doc.Model
modelConversationReceiptModeUpdateEvent = Doc.defineModel "ConversationReceiptModeUpdateEvent" $ do
  Doc.description "conversation receipt mode update event"
  Doc.property "data" (Doc.ref modelConversationReceiptModeUpdate) $ Doc.description "conversation receipt mode data"

modelConversationMessageTimerUpdateEvent :: Doc.Model
modelConversationMessageTimerUpdateEvent = Doc.defineModel "ConversationMessageTimerUpdateEvent" $ do
  Doc.description "conversation message timer update event"
  Doc.property "data" (Doc.ref modelConversationMessageTimerUpdate) $ Doc.description "conversation message timer data"

modelConversationCodeUpdateEvent :: Doc.Model
modelConversationCodeUpdateEvent = Doc.defineModel "ConversationCodeUpdateEvent" $ do
  Doc.description "conversation code update event"
  Doc.property "data" (Doc.ref modelConversationCode) $ Doc.description "conversation code data"

modelConversationCodeDeleteEvent :: Doc.Model
modelConversationCodeDeleteEvent =
  Doc.defineModel "ConversationCodeDeleteEvent" $
    Doc.description "conversation code delete event"

modelMemberUpdateEvent :: Doc.Model
modelMemberUpdateEvent = Doc.defineModel "MemberUpdateEvent" $ do
  Doc.description "member update event"
  Doc.property "data" (Doc.ref modelMemberUpdateData) $ Doc.description "member data"

modelTypingEvent :: Doc.Model
modelTypingEvent = Doc.defineModel "TypingEvent" $ do
  Doc.description "typing event"
  Doc.property "data" (Doc.ref modelTyping) $ Doc.description "typing data"

modelOtrMessage :: Doc.Model
modelOtrMessage = Doc.defineModel "OtrMessage" $ do
  Doc.description "Encrypted message of a conversation"
  Doc.property "sender" Doc.bytes' $
    Doc.description "The sender's client ID"
  Doc.property "recipient" Doc.bytes' $
    Doc.description "The recipient's client ID"
  Doc.property "text" Doc.bytes' $
    Doc.description "The ciphertext for the recipient (Base64 in JSON)"
  Doc.property "data" Doc.bytes' $ do
    Doc.description
      "Extra (symmetric) data (i.e. ciphertext, Base64 in JSON) \
      \that is common with all other recipients."
    Doc.optional

typePriority :: Doc.DataType
typePriority =
  Doc.string $
    Doc.enum
      [ "low",
        "high"
      ]

modelNewOtrMessage :: Doc.Model
modelNewOtrMessage = Doc.defineModel "NewOtrMessage" $ do
  Doc.description "OTR message per recipient"
  Doc.property "sender" Doc.bytes' $
    Doc.description "The sender's client ID"
  Doc.property "recipients" (Doc.ref modelOtrRecipients) $
    Doc.description "Per-recipient data (i.e. ciphertext)."
  Doc.property "native_push" Doc.bool' $ do
    Doc.description "Whether to issue a native push to offline clients."
    Doc.optional
  Doc.property "transient" Doc.bool' $ do
    Doc.description "Whether to put this message into the notification queue."
    Doc.optional
  Doc.property "native_priority" typePriority $ do
    Doc.description "The native push priority (default 'high')."
    Doc.optional
  Doc.property "data" Doc.bytes' $ do
    Doc.description
      "Extra (symmetric) data (i.e. ciphertext) that is replicated \
      \for each recipient."
    Doc.optional
  Doc.property "report_missing" (Doc.unique $ Doc.array Doc.bytes') $ do
    Doc.description "List of user IDs"
    Doc.optional

modelOtrRecipients :: Doc.Model
modelOtrRecipients = Doc.defineModel "OtrRecipients" $ do
  Doc.description "Recipients of OTR content."
  Doc.property "" (Doc.ref modelOtrClientMap) $
    Doc.description "Mapping of user IDs to 'OtrClientMap's."

modelOtrClientMap :: Doc.Model
modelOtrClientMap = Doc.defineModel "OtrClientMap" $ do
  Doc.description "Map of client IDs to OTR content."
  Doc.property "" Doc.bytes' $
    Doc.description "Mapping from client IDs to OTR content (Base64 in JSON)."

modelClientMismatch :: Doc.Model
modelClientMismatch = Doc.defineModel "ClientMismatch" $ do
  Doc.description "Map of missing, redundant or deleted clients."
  Doc.property "time" Doc.dateTime' $
    Doc.description "Server timestamp (date and time)"
  Doc.property "missing" (Doc.ref modelUserClients) $
    Doc.description "Map of missing clients per user."
  Doc.property "redundant" (Doc.ref modelUserClients) $
    Doc.description "Map of redundant clients per user."
  Doc.property "deleted" (Doc.ref modelUserClients) $
    Doc.description "Map of deleted clients per user."

modelUserClients :: Doc.Model
modelUserClients =
  Doc.defineModel "UserClients"
    $ Doc.property "" (Doc.unique $ Doc.array Doc.bytes')
    $ Doc.description "Map of user IDs to sets of client IDs ({ UserId: [ClientId] })."

modelUserIdList :: Doc.Model
modelUserIdList = Doc.defineModel "UserIdList" $ do
  Doc.description "list of user ids"
  Doc.property "user_ids" (Doc.unique $ Doc.array Doc.bytes') $
    Doc.description "the array of team conversations"

modelMembers :: Doc.Model
modelMembers =
  Doc.defineModel "Members"
    $ Doc.property "users" (Doc.unique $ Doc.array Doc.bytes')
    $ Doc.description "List of user IDs"

modelMemberUpdateData :: Doc.Model
modelMemberUpdateData = Doc.defineModel "MemberUpdateData" $ do
  Doc.description "Event data on member updates"
  Doc.property "target" Doc.bytes' $ do
    Doc.description "Target ID of the user that the action was performed on"
    Doc.optional
  Doc.property "otr_muted" Doc.bool' $ do
    Doc.description "Whether to notify on conversation updates"
    Doc.optional
  Doc.property "otr_muted_ref" Doc.bytes' $ do
    Doc.description "A reference point for (un)muting"
    Doc.optional
  Doc.property "otr_archived" Doc.bool' $ do
    Doc.description "Whether to notify on conversation updates"
    Doc.optional
  Doc.property "otr_archived_ref" Doc.bytes' $ do
    Doc.description "A reference point for (un)archiving"
    Doc.optional
  Doc.property "hidden" Doc.bool' $ do
    Doc.description "Whether the conversation is hidden"
    Doc.optional
  Doc.property "hidden_ref" Doc.bytes' $ do
    Doc.description "A reference point for (un)hiding"
    Doc.optional
  Doc.property "conversation_role" Doc.string' $ do
    Doc.description "Name of the conversation role to update to"
    Doc.optional

modelOtherMemberUpdateData :: Doc.Model
modelOtherMemberUpdateData = Doc.defineModel "OtherMemberUpdateData" $ do
  Doc.description "Event data on other member updates"
  Doc.property "target" Doc.bytes' $ do
    Doc.description "Target ID of the user that the action was performed on"
    Doc.optional
  Doc.property "conversation_role" Doc.string' $ do
    Doc.description "Name of the conversation role to update to"
    Doc.optional

modelConnect :: Doc.Model
modelConnect = Doc.defineModel "Connect" $ do
  Doc.description "user to user connection request"
  Doc.property "recipient" Doc.bytes' $
    Doc.description "The user ID to connect to"
  Doc.property "message" Doc.string' $
    Doc.description "Initial message to send to user"
  Doc.property "name" Doc.string' $
    Doc.description "Name of requestor"
  Doc.property "email" Doc.string' $ do
    Doc.description "E-Mail of requestor"
    Doc.optional

modelServiceRef :: Doc.Model
modelServiceRef = Doc.defineModel "ServiceRef" $ do
  Doc.description "Service Reference"
  Doc.property "id" Doc.bytes' $
    Doc.description "Service ID"
  Doc.property "provider" Doc.bytes' $
    Doc.description "Provider ID"

modelErrorObj :: Doc.Model
modelErrorObj = Swagger.errorModel

modelCustomBackend :: Doc.Model
modelCustomBackend = Doc.defineModel "CustomBackend" $ do
  Doc.description "Description of a custom backend"
  Doc.property "config_json_url" Doc.string' $
    Doc.description "the location of the custom backend's config.json file"
  Doc.property "webapp_welcome_url" Doc.string' $
    Doc.description "the location of the custom webapp"
