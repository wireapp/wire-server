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
import Polysemy
import Wire.API.Conversation hiding (Member)
import Wire.API.Error
import Wire.API.Error.Galley
import Wire.API.Routes.API
import Wire.API.Routes.Public.Galley.Conversation
import Wire.ConversationStore.MLS.Types
import Wire.ConversationSubsystem

conversationAPI :: API ConversationAPI GalleyEffects
conversationAPI =
  mkNamedAPI @"get-unqualified-conversation" (\lusr cnv -> legacyOwnConversation =<< getUnqualifiedOwnConversation lusr cnv)
    <@> mkNamedAPI @"get-unqualified-conversation-legalhold-alias" (\lusr cnv -> legacyOwnConversation =<< getUnqualifiedOwnConversation lusr cnv)
    <@> mkNamedAPI @"get-conversation@v2" (\lusr cnv -> legacyOwnConversation =<< getOwnConversation lusr cnv)
    <@> mkNamedAPI @"get-conversation@v5" (\lusr cnv -> legacyOwnConversation =<< getOwnConversation lusr cnv)
    <@> mkNamedAPI @"get-conversation@v9" (\lusr cnv -> legacyOwnConversation =<< getOwnConversation lusr cnv)
    <@> mkNamedAPI @"get-conversation@v15" (\lusr cnv -> legacyConversation =<< getConversation lusr cnv)
    <@> mkNamedAPI @"get-conversation" getConversation
    <@> mkNamedAPI @"get-conversation-roles" getConversationRoles
    <@> mkNamedAPI @"get-group-info" getGroupInfo
    <@> mkNamedAPI @"list-conversation-ids-unqualified" conversationIdsPageFromUnqualified
    <@> mkNamedAPI @"list-conversation-ids-v2" (conversationIdsPaginated DoNotListGlobalSelf)
    <@> mkNamedAPI @"list-conversation-ids" conversationIdsPageFrom
    <@> mkNamedAPI @"get-conversations" (\lusr mids mstart msize -> (\cl -> ConversationList (map toLegacyOwnConversation (filter (not . isMeetingConversation) cl.convList)) cl.convHasMore) <$> getPaginatedConversations lusr mids mstart msize)
    <@> mkNamedAPI @"list-conversations@v1" (\lusr req -> toLegacyConversationsResponse <$> listConversations lusr req)
    <@> mkNamedAPI @"list-conversations@v2" (\lusr req -> toLegacyConversationsResponse <$> listConversations lusr req)
    <@> mkNamedAPI @"list-conversations@v5" (\lusr req -> toLegacyConversationsResponse <$> listConversations lusr req)
    <@> mkNamedAPI @"list-conversations@v15" (\lusr req -> toLegacyConversationsResponse <$> listConversations lusr req)
    <@> mkNamedAPI @"list-conversations" listConversations
    <@> mkNamedAPI @"get-conversation-by-reusable-code" getConversationByReusableCode
    <@> mkNamedAPI @"create-group-conversation@v2" (\lusr conn nc -> toLegacyOwnConversation <$$> createLegacyGroupConversation lusr conn nc)
    <@> mkNamedAPI @"create-group-conversation@v3" (\lusr conn nc -> toLegacyOwnConversation <$$> createLegacyGroupConversation lusr conn nc)
    <@> mkNamedAPI @"create-group-conversation@v5" (\lusr conn nc -> toLegacyCGRV9 <$> createGroupOwnConversation lusr conn nc)
    <@> mkNamedAPI @"create-group-conversation@v9" (\lusr conn nc -> toLegacyCGRV9 <$> createGroupOwnConversation lusr conn nc)
    <@> mkNamedAPI @"create-group-conversation@v15" (\lusr conn nc -> toLegacyCreateGroupConversation <$> createGroupConversation lusr conn nc)
    <@> mkNamedAPI @"create-group-conversation" createGroupConversation
    <@> mkNamedAPI @"create-self-conversation@v2" (\lusr -> toLegacyOwnConversation <$$> createProteusSelfConversation lusr)
    <@> mkNamedAPI @"create-self-conversation@v5" (\lusr -> toLegacyOwnConversation <$$> createProteusSelfConversation lusr)
    <@> mkNamedAPI @"create-self-conversation@v15" (\lusr -> toLegacyOwnConversation <$$> createProteusSelfConversation lusr)
    <@> mkNamedAPI @"create-self-conversation" createProteusSelfConversation
    <@> mkNamedAPI @"get-mls-self-conversation@v5" (fmap toLegacyOwnConversation . getMLSSelfConversationWithError)
    <@> mkNamedAPI @"get-mls-self-conversation@v15" (fmap toLegacyOwnConversation . getMLSSelfConversationWithError)
    <@> mkNamedAPI @"get-mls-self-conversation" getMLSSelfConversationWithError
    <@> mkNamedAPI @"get-subconversation" getSubConversation
    <@> mkNamedAPI @"leave-subconversation" leaveSubConversation
    <@> mkNamedAPI @"delete-subconversation" deleteSubConversation
    <@> mkNamedAPI @"get-subconversation-group-info" getSubConversationGroupInfo
    <@> mkNamedAPI @"create-one-to-one-conversation@v2" (\lusr conn req -> toLegacyOwnConversation <$$> createOne2OneConversation lusr conn req)
    <@> mkNamedAPI @"create-one-to-one-conversation@v6" (\lusr conn req -> toLegacyOwnConversation <$$> createOne2OneConversation lusr conn req)
    <@> mkNamedAPI @"create-one-to-one-conversation@v15" (\lusr conn req -> toLegacyOwnConversation <$$> createOne2OneConversation lusr conn req)
    <@> mkNamedAPI @"create-one-to-one-conversation" createOne2OneConversation
    <@> mkNamedAPI @"get-one-to-one-mls-conversation@v5" (\lusr usr -> toLegacyOwnConversation <$> getMLSOne2OneOwnConversation lusr usr)
    <@> mkNamedAPI @"get-one-to-one-mls-conversation@v6" (\lusr usr -> toLegacyMLSOne2OneConversation <$> getMLSOne2OneMLSConversation lusr usr)
    <@> mkNamedAPI @"get-one-to-one-mls-conversation@v15" (\lusr usr fmt -> toLegacyMLSOne2OneConversation <$> getMLSOne2OneConversation lusr usr fmt)
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
    <@> mkNamedAPI @"remove-member-unqualified" (\lusr con cnv victim -> removeMemberQualified RemoveMemberLegacyResponse lusr (Just con) (tUntagged (qualifyAs lusr cnv)) (tUntagged (qualifyAs lusr victim)))
    <@> mkNamedAPI @"remove-member@v15" (\lusr con -> removeMemberQualified RemoveMemberLegacyResponse lusr (Just con))
    <@> mkNamedAPI @"remove-member" (\lusr con -> removeMemberQualified RemoveMemberEligibleMembersResponse lusr (Just con))
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

toLegacyCGRV9 ::
  CreateGroupConversationResponseV9 GroupConvType ->
  CreateGroupConversationResponseV9 GroupConvTypeLegacy
toLegacyCGRV9 = \case
  GroupConversationExistedV9 conv -> GroupConversationExistedV9 (toLegacyOwnConversation conv)
  GroupConversationCreatedV9 cgoc -> GroupConversationCreatedV9 (toLegacyCreateGroupOwnConversation cgoc)

-- | Convert an own-conversation to the legacy (< V16) wire type, hiding meeting
-- conversations entirely (they have no legacy representation): a meeting yields
-- 'ConvNotFound' (404) rather than leaking @group_conv_type: null@. (WPB-26626)
legacyOwnConversation ::
  (Member (ErrorS 'ConvNotFound) r) =>
  OwnConversation GroupConvType ->
  Sem r (OwnConversation GroupConvTypeLegacy)
legacyOwnConversation conv = do
  when (isMeetingConversation conv) $ throwS @'ConvNotFound
  pure (toLegacyOwnConversation conv)

-- | As 'legacyOwnConversation', for the full 'Conversation' view (V10-V15 routes).
legacyConversation ::
  (Member (ErrorS 'ConvNotFound) r) =>
  Conversation GroupConvType ->
  Sem r (Conversation GroupConvTypeLegacy)
legacyConversation conv = do
  when (conv.metadata.cnvmGroupConvType == Just MeetingConversation) $ throwS @'ConvNotFound
  pure (toLegacyConversation conv)
