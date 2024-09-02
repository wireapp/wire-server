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
import Wire.API.Federation.API
import Wire.API.Routes.API
import Wire.API.Routes.Public.Galley.Conversation

conversationAPI :: API ConversationAPI GalleyEffects
conversationAPI =
  mkNamedAPI @"get-unqualified-conversation" getUnqualifiedConversation
    <@> mkNamedAPI @"get-unqualified-conversation-legalhold-alias" getUnqualifiedConversation
    <@> mkNamedAPI @"get-conversation@v2" (callsFed (exposeAnnotations getConversation))
    <@> mkNamedAPI @"get-conversation@v5" (callsFed (exposeAnnotations getConversation))
    <@> mkNamedAPI @"get-conversation" (callsFed (exposeAnnotations getConversation))
    <@> mkNamedAPI @"get-conversation-roles" getConversationRoles
    <@> mkNamedAPI @"get-group-info" (callsFed (exposeAnnotations getGroupInfo))
    <@> mkNamedAPI @"list-conversation-ids-unqualified" conversationIdsPageFromUnqualified
    <@> mkNamedAPI @"list-conversation-ids-v2" (conversationIdsPageFromV2 DoNotListGlobalSelf)
    <@> mkNamedAPI @"list-conversation-ids" conversationIdsPageFrom
    <@> mkNamedAPI @"get-conversations" getConversations
    <@> mkNamedAPI @"list-conversations@v1" (callsFed (exposeAnnotations listConversations))
    <@> mkNamedAPI @"list-conversations@v2" (callsFed (exposeAnnotations listConversations))
    <@> mkNamedAPI @"list-conversations@v5" (callsFed (exposeAnnotations listConversations))
    <@> mkNamedAPI @"list-conversations" (callsFed (exposeAnnotations listConversations))
    <@> mkNamedAPI @"get-conversation-by-reusable-code" getConversationByReusableCode
    <@> mkNamedAPI @"create-group-conversation@v2" (callsFed (exposeAnnotations createGroupConversationUpToV3))
    <@> mkNamedAPI @"create-group-conversation@v3" (callsFed (exposeAnnotations createGroupConversationUpToV3))
    <@> mkNamedAPI @"create-group-conversation@v5" (callsFed (exposeAnnotations createGroupConversation))
    <@> mkNamedAPI @"create-group-conversation" (callsFed (exposeAnnotations createGroupConversation))
    <@> mkNamedAPI @"create-self-conversation@v2" createProteusSelfConversation
    <@> mkNamedAPI @"create-self-conversation@v5" createProteusSelfConversation
    <@> mkNamedAPI @"create-self-conversation" createProteusSelfConversation
    <@> mkNamedAPI @"get-mls-self-conversation@v5" getMLSSelfConversationWithError
    <@> mkNamedAPI @"get-mls-self-conversation" getMLSSelfConversationWithError
    <@> mkNamedAPI @"get-subconversation" (callsFed getSubConversation)
    <@> mkNamedAPI @"leave-subconversation" (callsFed leaveSubConversation)
    <@> mkNamedAPI @"delete-subconversation" (callsFed deleteSubConversation)
    <@> mkNamedAPI @"get-subconversation-group-info" (callsFed getSubConversationGroupInfo)
    <@> mkNamedAPI @"create-one-to-one-conversation@v2" (callsFed createOne2OneConversation)
    <@> mkNamedAPI @"create-one-to-one-conversation" (callsFed createOne2OneConversation)
    <@> mkNamedAPI @"get-one-to-one-mls-conversation@v5" getMLSOne2OneConversationV5
    <@> mkNamedAPI @"get-one-to-one-mls-conversation" getMLSOne2OneConversation
    <@> mkNamedAPI @"add-members-to-conversation-unqualified" (callsFed addMembersUnqualified)
    <@> mkNamedAPI @"add-members-to-conversation-unqualified2" (callsFed addMembersUnqualifiedV2)
    <@> mkNamedAPI @"add-members-to-conversation" (callsFed addMembers)
    <@> mkNamedAPI @"join-conversation-by-id-unqualified" (callsFed joinConversationById)
    <@> mkNamedAPI @"join-conversation-by-code-unqualified" (callsFed joinConversationByReusableCode)
    <@> mkNamedAPI @"code-check" checkReusableCode
    <@> mkNamedAPI @"create-conversation-code-unqualified@v3" (addCodeUnqualified Nothing)
    <@> mkNamedAPI @"create-conversation-code-unqualified" addCodeUnqualifiedWithReqBody
    <@> mkNamedAPI @"get-conversation-guest-links-status" getConversationGuestLinksStatus
    <@> mkNamedAPI @"remove-code-unqualified" rmCodeUnqualified
    <@> mkNamedAPI @"get-code" getCode
    <@> mkNamedAPI @"member-typing-unqualified" (callsFed (exposeAnnotations memberTypingUnqualified))
    <@> mkNamedAPI @"member-typing-qualified" (callsFed (exposeAnnotations memberTyping))
    <@> mkNamedAPI @"remove-member-unqualified" (callsFed (exposeAnnotations removeMemberUnqualified))
    <@> mkNamedAPI @"remove-member" (callsFed (exposeAnnotations removeMemberQualified))
    <@> mkNamedAPI @"update-other-member-unqualified" (callsFed (exposeAnnotations updateOtherMemberUnqualified))
    <@> mkNamedAPI @"update-other-member" (callsFed (exposeAnnotations updateOtherMember))
    <@> mkNamedAPI @"update-conversation-name-deprecated" (callsFed (exposeAnnotations updateUnqualifiedConversationName))
    <@> mkNamedAPI @"update-conversation-name-unqualified" (callsFed (exposeAnnotations updateUnqualifiedConversationName))
    <@> mkNamedAPI @"update-conversation-name" (callsFed (exposeAnnotations updateConversationName))
    <@> mkNamedAPI @"update-conversation-message-timer-unqualified" (callsFed (exposeAnnotations updateConversationMessageTimerUnqualified))
    <@> mkNamedAPI @"update-conversation-message-timer" (callsFed (exposeAnnotations updateConversationMessageTimer))
    <@> mkNamedAPI @"update-conversation-receipt-mode-unqualified" (callsFed (exposeAnnotations updateConversationReceiptModeUnqualified))
    <@> mkNamedAPI @"update-conversation-receipt-mode" (callsFed (exposeAnnotations updateConversationReceiptMode))
    <@> mkNamedAPI @"update-conversation-access-unqualified" (callsFed (exposeAnnotations updateConversationAccessUnqualified))
    <@> mkNamedAPI @"update-conversation-access@v2" (callsFed (exposeAnnotations updateConversationAccess))
    <@> mkNamedAPI @"update-conversation-access" (callsFed (exposeAnnotations updateConversationAccess))
    <@> mkNamedAPI @"get-conversation-self-unqualified" getLocalSelf
    <@> mkNamedAPI @"update-conversation-self-unqualified" updateUnqualifiedSelfMember
    <@> mkNamedAPI @"update-conversation-self" updateSelfMember
    <@> mkNamedAPI @"update-conversation-protocol" updateConversationProtocolWithLocalUser
