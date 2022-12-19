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
import Galley.Cassandra.TeamFeatures
import Wire.API.Federation.API
import Wire.API.Routes.API
import Wire.API.Routes.Public.Galley.Conversation

conversationAPI :: API ConversationAPI GalleyEffects
conversationAPI =
  mkNamedAPI @"get-unqualified-conversation" getUnqualifiedConversation
    <@> mkNamedAPI @"get-unqualified-conversation-legalhold-alias" getUnqualifiedConversation
    <@> mkNamedAPI @"get-conversation@v2" (callsFed getConversation)
    <@> mkNamedAPI @"get-conversation" (callsFed getConversation)
    <@> mkNamedAPI @"get-conversation-roles" getConversationRoles
    <@> mkNamedAPI @"get-group-info" (callsFed getGroupInfo)
    <@> mkNamedAPI @"list-conversation-ids-unqualified" conversationIdsPageFromUnqualified
    <@> mkNamedAPI @"list-conversation-ids-v2" (conversationIdsPageFromV2 DoNotListGlobalSelf)
    <@> mkNamedAPI @"list-conversation-ids" conversationIdsPageFrom
    <@> mkNamedAPI @"get-conversations" getConversations
    <@> mkNamedAPI @"list-conversations@v1" (callsFed listConversations)
    <@> mkNamedAPI @"list-conversations@v2" (callsFed listConversations)
    <@> mkNamedAPI @"list-conversations" (callsFed listConversations)
    <@> mkNamedAPI @"get-conversation-by-reusable-code" (getConversationByReusableCode @Cassandra)
    <@> mkNamedAPI @"create-group-conversation@v2" (callsFed createGroupConversation)
    <@> mkNamedAPI @"create-group-conversation" (callsFed createGroupConversation)
    <@> mkNamedAPI @"create-self-conversation@v2" createProteusSelfConversation
    <@> mkNamedAPI @"create-self-conversation" createProteusSelfConversation
    <@> mkNamedAPI @"get-mls-self-conversation" getMLSSelfConversationWithError
    <@> mkNamedAPI @"get-conversation-clients" getMLSClientListForConv
    <@> mkNamedAPI @"get-subconversation" (callsFed getSubConversation)
    <@> mkNamedAPI @"leave-subconversation" (callsFed leaveSubConversation)
    <@> mkNamedAPI @"delete-subconversation" (callsFed deleteSubConversation)
    <@> mkNamedAPI @"get-subconversation-group-info" (callsFed getSubConversationGroupInfo)
    <@> mkNamedAPI @"create-one-to-one-conversation@v2" (callsFed createOne2OneConversation)
    <@> mkNamedAPI @"create-one-to-one-conversation" (callsFed createOne2OneConversation)
    <@> mkNamedAPI @"add-members-to-conversation-unqualified" (callsFed addMembersUnqualified)
    <@> mkNamedAPI @"add-members-to-conversation-unqualified2" (callsFed addMembersUnqualifiedV2)
    <@> mkNamedAPI @"add-members-to-conversation" (callsFed addMembers)
    <@> mkNamedAPI @"join-conversation-by-id-unqualified" (callsFed (joinConversationById @Cassandra))
    <@> mkNamedAPI @"join-conversation-by-code-unqualified" (callsFed (joinConversationByReusableCode @Cassandra))
    <@> mkNamedAPI @"code-check" (checkReusableCode @Cassandra)
    <@> mkNamedAPI @"create-conversation-code-unqualified" (addCodeUnqualified @Cassandra)
    <@> mkNamedAPI @"get-conversation-guest-links-status" (getConversationGuestLinksStatus @Cassandra)
    <@> mkNamedAPI @"remove-code-unqualified" rmCodeUnqualified
    <@> mkNamedAPI @"get-code" (getCode @Cassandra)
    <@> mkNamedAPI @"member-typing-unqualified" isTypingUnqualified
    <@> mkNamedAPI @"member-typing-qualified" (callsFed isTypingQualified)
    <@> mkNamedAPI @"remove-member-unqualified" (callsFed removeMemberUnqualified)
    <@> mkNamedAPI @"remove-member" (callsFed removeMemberQualified)
    <@> mkNamedAPI @"update-other-member-unqualified" (callsFed updateOtherMemberUnqualified)
    <@> mkNamedAPI @"update-other-member" (callsFed updateOtherMember)
    <@> mkNamedAPI @"update-conversation-name-deprecated" (callsFed updateUnqualifiedConversationName)
    <@> mkNamedAPI @"update-conversation-name-unqualified" (callsFed updateUnqualifiedConversationName)
    <@> mkNamedAPI @"update-conversation-name" (callsFed updateConversationName)
    <@> mkNamedAPI @"update-conversation-message-timer-unqualified" (callsFed updateConversationMessageTimerUnqualified)
    <@> mkNamedAPI @"update-conversation-message-timer" (callsFed updateConversationMessageTimer)
    <@> mkNamedAPI @"update-conversation-receipt-mode-unqualified" (callsFed updateConversationReceiptModeUnqualified)
    <@> mkNamedAPI @"update-conversation-receipt-mode" (callsFed updateConversationReceiptMode)
    <@> mkNamedAPI @"update-conversation-access-unqualified" (callsFed updateConversationAccessUnqualified)
    <@> mkNamedAPI @"update-conversation-access@v2" (callsFed updateConversationAccess)
    <@> mkNamedAPI @"update-conversation-access" (callsFed updateConversationAccess)
    <@> mkNamedAPI @"get-conversation-self-unqualified" getLocalSelf
    <@> mkNamedAPI @"update-conversation-self-unqualified" updateUnqualifiedSelfMember
    <@> mkNamedAPI @"update-conversation-self" updateSelfMember
