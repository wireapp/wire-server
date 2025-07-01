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

module Galley.API.Public.Conversation where

import Galley.API.Create
import Galley.API.MLS.GroupInfo
import Galley.API.MLS.SubConversation
import Galley.API.MLS.Types
import Galley.API.Query
import Galley.API.Update
import Galley.App
import Imports
import Wire.API.Routes.API
import Wire.API.Routes.Public.Galley.Conversation

conversationAPI :: API ConversationAPI GalleyEffects
conversationAPI =
  mkNamedAPI @"get-unqualified-conversation" getUnqualifiedConversation
    <@> mkNamedAPI @"get-unqualified-conversation-legalhold-alias" getUnqualifiedConversation
    <@> mkNamedAPI @"get-conversation@v2" getConversation
    <@> mkNamedAPI @"get-conversation@v5" getConversation
    <@> mkNamedAPI @"get-conversation" getConversation
    <@> mkNamedAPI @"get-conversation-roles" getConversationRoles
    <@> mkNamedAPI @"get-group-info" getGroupInfo
    <@> mkNamedAPI @"list-conversation-ids-unqualified" conversationIdsPageFromUnqualified
    <@> mkNamedAPI @"list-conversation-ids-v2" (conversationIdsPageFromV2 DoNotListGlobalSelf)
    <@> mkNamedAPI @"list-conversation-ids" conversationIdsPageFrom
    <@> mkNamedAPI @"get-conversations" getConversations
    <@> mkNamedAPI @"list-conversations@v1" listConversations
    <@> mkNamedAPI @"list-conversations@v2" listConversations
    <@> mkNamedAPI @"list-conversations@v5" listConversations
    <@> mkNamedAPI @"list-conversations" listConversations
    <@> mkNamedAPI @"get-conversation-by-reusable-code" getConversationByReusableCode
    <@> mkNamedAPI @"create-group-conversation@v2" createGroupConversationUpToV3
    <@> mkNamedAPI @"create-group-conversation@v3" createGroupConversationUpToV3
    <@> mkNamedAPI @"create-group-conversation@v5" createGroupConversationV8
    <@> mkNamedAPI @"create-group-conversation@v9" createGroupConversationV8
    <@> mkNamedAPI @"create-group-conversation" createGroupConversation
    <@> mkNamedAPI @"create-self-conversation@v2" createProteusSelfConversation
    <@> mkNamedAPI @"create-self-conversation@v5" createProteusSelfConversation
    <@> mkNamedAPI @"create-self-conversation" createProteusSelfConversation
    <@> mkNamedAPI @"get-mls-self-conversation@v5" getMLSSelfConversationWithError
    <@> mkNamedAPI @"get-mls-self-conversation" getMLSSelfConversationWithError
    <@> mkNamedAPI @"get-subconversation" getSubConversation
    <@> mkNamedAPI @"leave-subconversation" leaveSubConversation
    <@> mkNamedAPI @"delete-subconversation" deleteSubConversation
    <@> mkNamedAPI @"get-subconversation-group-info" getSubConversationGroupInfo
    <@> mkNamedAPI @"create-one-to-one-conversation@v2" createOne2OneConversation
    <@> mkNamedAPI @"create-one-to-one-conversation@v6" createOne2OneConversation
    <@> mkNamedAPI @"create-one-to-one-conversation" createOne2OneConversation
    <@> mkNamedAPI @"get-one-to-one-mls-conversation@v5" getMLSOne2OneConversationV5
    <@> mkNamedAPI @"get-one-to-one-mls-conversation@v6" getMLSOne2OneConversationV6
    <@> mkNamedAPI @"get-one-to-one-mls-conversation" getMLSOne2OneConversation
    <@> mkNamedAPI @"add-members-to-conversation-unqualified" addMembersUnqualified
    <@> mkNamedAPI @"add-members-to-conversation-unqualified2" addMembersUnqualifiedV2
    <@> mkNamedAPI @"add-members-to-conversation" addMembers
    <@> mkNamedAPI @"join-conversation-by-id-unqualified" joinConversationById
    <@> mkNamedAPI @"join-conversation-by-code-unqualified" joinConversationByReusableCode
    <@> mkNamedAPI @"code-check" checkReusableCode
    <@> mkNamedAPI @"create-conversation-code-unqualified@v3" (addCodeUnqualified Nothing)
    <@> mkNamedAPI @"create-conversation-code-unqualified" addCodeUnqualifiedWithReqBody
    <@> mkNamedAPI @"get-conversation-guest-links-status" getConversationGuestLinksStatus
    <@> mkNamedAPI @"remove-code-unqualified" rmCodeUnqualified
    <@> mkNamedAPI @"get-code" getCode
    <@> mkNamedAPI @"member-typing-unqualified" memberTypingUnqualified
    <@> mkNamedAPI @"member-typing-qualified" memberTyping
    <@> mkNamedAPI @"remove-member-unqualified" removeMemberUnqualified
    <@> mkNamedAPI @"remove-member" removeMemberQualified
    <@> mkNamedAPI @"update-other-member-unqualified" updateOtherMemberUnqualified
    <@> mkNamedAPI @"update-other-member" updateOtherMember
    <@> mkNamedAPI @"update-conversation-name-deprecated" updateUnqualifiedConversationName
    <@> mkNamedAPI @"update-conversation-name-unqualified" updateUnqualifiedConversationName
    <@> mkNamedAPI @"update-conversation-name" updateConversationName
    <@> mkNamedAPI @"update-conversation-message-timer-unqualified" updateConversationMessageTimerUnqualified
    <@> mkNamedAPI @"update-conversation-message-timer" updateConversationMessageTimer
    <@> mkNamedAPI @"update-conversation-receipt-mode-unqualified" updateConversationReceiptModeUnqualified
    <@> mkNamedAPI @"update-conversation-receipt-mode" updateConversationReceiptMode
    <@> mkNamedAPI @"update-conversation-access-unqualified" updateConversationAccessUnqualified
    <@> mkNamedAPI @"update-conversation-access@v2" updateConversationAccess
    <@> mkNamedAPI @"update-conversation-access" updateConversationAccess
    <@> mkNamedAPI @"get-conversation-self-unqualified" getLocalSelf
    <@> mkNamedAPI @"update-conversation-self-unqualified" updateUnqualifiedSelfMember
    <@> mkNamedAPI @"get-conversation-self" getSelfMember
    <@> mkNamedAPI @"update-conversation-self" updateSelfMember
    <@> mkNamedAPI @"update-conversation-protocol" updateConversationProtocolWithLocalUser
    <@> mkNamedAPI @"update-channel-add-permission" updateChannelAddPermission
