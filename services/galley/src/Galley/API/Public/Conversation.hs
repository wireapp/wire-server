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
import Galley.API.MLS.Types
import Galley.API.Query
import Galley.API.Update
import Galley.App
import Galley.Cassandra.TeamFeatures
import Wire.API.Routes.API
import Wire.API.Routes.Public.Galley.Conversation

conversationAPI :: API ConversationAPI GalleyEffects
conversationAPI =
  mkNamedAPI @"get-unqualified-conversation" getUnqualifiedConversation
    <@> mkNamedAPI @"get-unqualified-conversation-legalhold-alias" getUnqualifiedConversation
    <@> mkNamedAPI @"get-conversation" getConversation
    <@> mkNamedAPI @"get-conversation-roles" getConversationRoles
    <@> mkNamedAPI @"get-group-info" getGroupInfo
    <@> mkNamedAPI @"list-conversation-ids-unqualified" conversationIdsPageFromUnqualified
    <@> mkNamedAPI @"list-conversation-ids-v2" (conversationIdsPageFromV2 DoNotListGlobalSelf)
    <@> mkNamedAPI @"list-conversation-ids" conversationIdsPageFrom
    <@> mkNamedAPI @"get-conversations" getConversations
    <@> mkNamedAPI @"list-conversations-v1" listConversations
    <@> mkNamedAPI @"list-conversations" listConversations
    <@> mkNamedAPI @"get-conversation-by-reusable-code" (getConversationByReusableCode @Cassandra)
    <@> mkNamedAPI @"create-group-conversation" createGroupConversation
    <@> mkNamedAPI @"create-self-conversation" createProteusSelfConversation
    <@> mkNamedAPI @"get-mls-self-conversation" getMLSSelfConversation
    <@> mkNamedAPI @"create-one-to-one-conversation" createOne2OneConversation
    <@> mkNamedAPI @"add-members-to-conversation-unqualified" addMembersUnqualified
    <@> mkNamedAPI @"add-members-to-conversation-unqualified2" addMembersUnqualifiedV2
    <@> mkNamedAPI @"add-members-to-conversation" addMembers
    <@> mkNamedAPI @"join-conversation-by-id-unqualified" (joinConversationById @Cassandra)
    <@> mkNamedAPI @"join-conversation-by-code-unqualified" (joinConversationByReusableCode @Cassandra)
    <@> mkNamedAPI @"code-check" (checkReusableCode @Cassandra)
    <@> mkNamedAPI @"create-conversation-code-unqualified" (addCodeUnqualified @Cassandra)
    <@> mkNamedAPI @"get-conversation-guest-links-status" (getConversationGuestLinksStatus @Cassandra)
    <@> mkNamedAPI @"remove-code-unqualified" rmCodeUnqualified
    <@> mkNamedAPI @"get-code" (getCode @Cassandra)
    <@> mkNamedAPI @"member-typing-unqualified" isTypingUnqualified
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
    <@> mkNamedAPI @"update-conversation-access" updateConversationAccess
    <@> mkNamedAPI @"get-conversation-self-unqualified" getLocalSelf
    <@> mkNamedAPI @"update-conversation-self-unqualified" updateUnqualifiedSelfMember
    <@> mkNamedAPI @"update-conversation-self" updateSelfMember
