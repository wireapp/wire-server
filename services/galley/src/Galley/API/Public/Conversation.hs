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

import Data.Qualified
import Galley.App
import Imports
import Wire.API.Conversation
import Wire.API.Routes.API
import Wire.API.Routes.Public.Galley.Conversation
import Wire.ConversationStore.MLS.Types
import Wire.ConversationSubsystem

conversationAPI :: API ConversationAPI GalleyEffects
conversationAPI =
  mkNamedAPI @"get-unqualified-conversation" getUnqualifiedOwnConversation
    <@> mkNamedAPI @"get-unqualified-conversation-legalhold-alias" getUnqualifiedOwnConversation
    <@> mkNamedAPI @"get-conversation@v2" getOwnConversation
    <@> mkNamedAPI @"get-conversation@v5" getOwnConversation
    <@> mkNamedAPI @"get-conversation@v9" getOwnConversation
    <@> mkNamedAPI @"get-conversation" getConversation
    <@> mkNamedAPI @"get-conversation-roles" getConversationRoles
    <@> mkNamedAPI @"get-group-info" getGroupInfo
    <@> mkNamedAPI @"list-conversation-ids-unqualified" conversationIdsPageFromUnqualified
    <@> mkNamedAPI @"list-conversation-ids-v2" (conversationIdsPaginated DoNotListGlobalSelf)
    <@> mkNamedAPI @"list-conversation-ids" conversationIdsPageFrom
    <@> mkNamedAPI @"get-conversations" getPaginatedConversations
    <@> mkNamedAPI @"list-conversations@v1" listConversations
    <@> mkNamedAPI @"list-conversations@v2" listConversations
    <@> mkNamedAPI @"list-conversations@v5" listConversations
    <@> mkNamedAPI @"list-conversations" listConversations
    <@> mkNamedAPI @"get-conversation-by-reusable-code" getConversationByReusableCode
    <@> mkNamedAPI @"create-group-conversation@v2" createLegacyGroupConversation
    <@> mkNamedAPI @"create-group-conversation@v3" createLegacyGroupConversation
    <@> mkNamedAPI @"create-group-conversation@v5" createGroupOwnConversation
    <@> mkNamedAPI @"create-group-conversation@v9" createGroupOwnConversation
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
    <@> mkNamedAPI @"get-one-to-one-mls-conversation@v5" getMLSOne2OneOwnConversation
    <@> mkNamedAPI @"get-one-to-one-mls-conversation@v6" getMLSOne2OneMLSConversation
    <@> mkNamedAPI @"get-one-to-one-mls-conversation" getMLSOne2OneConversation
    <@> mkNamedAPI @"add-members-to-conversation-unqualified" (\lusr con cnv invite -> addMembers lusr con (tUntagged (qualifyAs lusr cnv)) (InviteQualified (fmap (tUntagged . qualifyAs lusr) (invUsers invite)) (invRoleName invite)))
    <@> mkNamedAPI @"add-members-to-conversation-unqualified2" addQualifiedMembersUnqualified
    <@> mkNamedAPI @"add-members-to-conversation" addMembers
    <@> mkNamedAPI @"replace-members-in-conversation" replaceMembers
    <@> mkNamedAPI @"join-conversation-by-id-unqualified" joinConversationById
    <@> mkNamedAPI @"join-conversation-by-code-unqualified" joinConversationByReusableCode
    <@> mkNamedAPI @"code-check" checkReusableCode
    <@> mkNamedAPI @"create-conversation-code-unqualified@v3" (addCodeUnqualified Nothing)
    <@> mkNamedAPI @"create-conversation-code-unqualified" (\uid zhost conn conv req -> addCodeUnqualified (Just req) uid zhost conn conv)
    <@> mkNamedAPI @"get-conversation-guest-links-status" getConversationGuestLinksStatus
    <@> mkNamedAPI @"remove-code-unqualified" rmCodeUnqualified
    <@> mkNamedAPI @"get-code" getCode
    <@> mkNamedAPI @"member-typing-unqualified" (\lusr con cnv status -> memberTyping lusr con (tUntagged (qualifyAs lusr cnv)) status)
    <@> mkNamedAPI @"member-typing-qualified" memberTyping
    <@> mkNamedAPI @"remove-member-unqualified" (\lusr con cnv victim -> removeMemberQualified lusr con (tUntagged (qualifyAs lusr cnv)) (tUntagged (qualifyAs lusr victim)))
    <@> mkNamedAPI @"remove-member" removeMemberQualified
    <@> mkNamedAPI @"update-other-member-unqualified" (\lusr con cnv victim update -> updateOtherMember lusr con (tUntagged (qualifyAs lusr cnv)) (tUntagged (qualifyAs lusr victim)) update)
    <@> mkNamedAPI @"update-other-member" updateOtherMember
    <@> mkNamedAPI @"update-conversation-name-deprecated" (\lusr con cnv rename -> updateConversationName lusr con (tUntagged (qualifyAs lusr cnv)) rename)
    <@> mkNamedAPI @"update-conversation-name-unqualified" (\lusr con cnv rename -> updateConversationName lusr con (tUntagged (qualifyAs lusr cnv)) rename)
    <@> mkNamedAPI @"update-conversation-name" updateConversationName
    <@> mkNamedAPI @"update-conversation-message-timer-unqualified" (\lusr con cnv update -> updateConversationMessageTimer lusr con (tUntagged (qualifyAs lusr cnv)) update)
    <@> mkNamedAPI @"update-conversation-message-timer" updateConversationMessageTimer
    <@> mkNamedAPI @"update-conversation-receipt-mode-unqualified" (\lusr con cnv update -> updateConversationReceiptMode lusr con (tUntagged (qualifyAs lusr cnv)) update)
    <@> mkNamedAPI @"update-conversation-receipt-mode" updateConversationReceiptMode
    <@> mkNamedAPI @"update-conversation-access-unqualified" (\lusr con cnv update -> updateConversationAccess lusr con (tUntagged (qualifyAs lusr cnv)) update)
    <@> mkNamedAPI @"update-conversation-access@v2" updateConversationAccess
    <@> mkNamedAPI @"update-conversation-access" updateConversationAccess
    <@> mkNamedAPI @"update-conversation-history" updateConversationHistory
    <@> mkNamedAPI @"get-conversation-self-unqualified" getLocalSelf
    <@> mkNamedAPI @"update-conversation-self-unqualified" (\lusr con cnv update -> updateSelfMember lusr con (tUntagged (qualifyAs lusr cnv)) update)
    <@> mkNamedAPI @"get-conversation-self" getSelfMember
    <@> mkNamedAPI @"update-conversation-self" updateSelfMember
    <@> mkNamedAPI @"update-conversation-protocol" updateConversationProtocolWithLocalUser
    <@> mkNamedAPI @"update-channel-add-permission" updateChannelAddPermission
