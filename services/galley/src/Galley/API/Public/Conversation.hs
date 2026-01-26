{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeApplications #-}

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

import Data.Default
import Data.Id
import Data.Json.Util (ToJSONObject (toJSONObject))
import Data.Qualified
import Galley.API.MLS.GroupInfo
import Galley.API.MLS.SubConversation
import Galley.API.Query
import Galley.API.Update
import Galley.App
import Galley.Mapping
import Galley.Types.Error
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.TinyLog qualified as P
import Wire.API.Conversation (CreateGroupConversation (..), CreateGroupOwnConversation (..), NewConv, NewOne2OneConv)
import Wire.API.Conversation qualified as Public
import Wire.API.Error.Galley (UnreachableBackends)
import Wire.API.Event.Conversation
import Wire.API.Federation.Client (FederatorClient)
import Wire.API.Federation.Error
import Wire.API.Push.V2 qualified as PushV2
import Wire.API.Routes.API
import Wire.API.Routes.Public.Galley.Conversation
import Wire.API.Routes.Public.Util (ResponseForExistedCreated (..))
import Wire.BackendNotificationQueueAccess (BackendNotificationQueueAccess)
import Wire.ConversationStore (ConversationStore)
import Wire.ConversationStore.MLS.Types
import Wire.ConversationSubsystem qualified as ConversationSubsystem
import Wire.ConversationSubsystem.Interpreter qualified as Interpreter
import Wire.FederationAPIAccess (FederationAPIAccess)
import Wire.FederationAPIAccess qualified as E
import Wire.NotificationSubsystem (NotificationSubsystem)
import Wire.NotificationSubsystem qualified as NS
import Wire.Sem.Now (Now)
import Wire.Sem.Now qualified as Now
import Wire.StoredConversation (StoredConversation, localMemberToOther, remoteMemberToOther)
import Wire.StoredConversation qualified as Data

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
    <@> mkNamedAPI @"get-one-to-one-mls-conversation@v5" getMLSOne2OneConversationV5
    <@> mkNamedAPI @"get-one-to-one-mls-conversation@v6" getMLSOne2OneConversationV6
    <@> mkNamedAPI @"get-one-to-one-mls-conversation" getMLSOne2OneConversation
    <@> mkNamedAPI @"add-members-to-conversation-unqualified" addMembersUnqualified
    <@> mkNamedAPI @"add-members-to-conversation-unqualified2" addMembersUnqualifiedV2
    <@> mkNamedAPI @"add-members-to-conversation" addMembers
    <@> mkNamedAPI @"replace-members-in-conversation" replaceMembers
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
    <@> mkNamedAPI @"update-conversation-history" updateConversationHistory
    <@> mkNamedAPI @"get-conversation-self-unqualified" getLocalSelf
    <@> mkNamedAPI @"update-conversation-self-unqualified" updateUnqualifiedSelfMember
    <@> mkNamedAPI @"get-conversation-self" getSelfMember
    <@> mkNamedAPI @"update-conversation-self" updateSelfMember
    <@> mkNamedAPI @"update-conversation-protocol" updateConversationProtocolWithLocalUser
    <@> mkNamedAPI @"update-channel-add-permission" updateChannelAddPermission

----------------------------------------------------------------------------
-- API Handlers

createGroupConversationUpToV3 ::
  ( Member ConversationSubsystem.ConversationSubsystem r,
    Member (Error InternalError) r,
    Member (Error UnreachableBackends) r,
    Member P.TinyLog r,
    Member Now r,
    Member ConversationStore r,
    Member (Error FederationError) r,
    Member (FederationAPIAccess FederatorClient) r,
    Member NotificationSubsystem r,
    Member BackendNotificationQueueAccess r
  ) =>
  Local UserId ->
  Maybe ConnId ->
  NewConv ->
  Sem r (ConversationResponse Public.OwnConversation)
createGroupConversationUpToV3 lusr conn newConv = do
  dbConv <- ConversationSubsystem.createGroupConversation lusr conn newConv
  conversationCreated lusr dbConv

createGroupOwnConversation ::
  ( Member ConversationSubsystem.ConversationSubsystem r,
    Member (Error InternalError) r,
    Member P.TinyLog r
  ) =>
  Local UserId ->
  Maybe ConnId ->
  NewConv ->
  Sem r CreateGroupConversationResponseV9
createGroupOwnConversation lusr conn newConv = do
  dbConv <- ConversationSubsystem.createGroupConversation lusr conn newConv
  conv <- conversationViewV9 lusr dbConv
  pure . GroupConversationCreatedV9 $ CreateGroupOwnConversation conv mempty

createGroupConversation ::
  (Member ConversationSubsystem.ConversationSubsystem r) =>
  Local UserId ->
  Maybe ConnId ->
  NewConv ->
  Sem r CreateGroupConversation
createGroupConversation lusr conn newConv = do
  dbConv <- ConversationSubsystem.createGroupConversation lusr conn newConv
  pure $
    CreateGroupConversation
      { conversation = conversationView (qualifyAs lusr ()) (Just lusr) dbConv,
        failedToAdd = mempty
      }

createProteusSelfConversation ::
  ( Member ConversationSubsystem.ConversationSubsystem r,
    Member (Error InternalError) r,
    Member (Error UnreachableBackends) r,
    Member P.TinyLog r,
    Member Now r,
    Member ConversationStore r,
    Member (Error FederationError) r,
    Member (FederationAPIAccess FederatorClient) r,
    Member NotificationSubsystem r,
    Member BackendNotificationQueueAccess r
  ) =>
  Local UserId ->
  Sem r (ConversationResponse Public.OwnConversation)
createProteusSelfConversation lusr = do
  (c, created) <- ConversationSubsystem.createProteusSelfConversation lusr
  if created
    then conversationCreated lusr c
    else conversationExisted lusr c

createOne2OneConversation ::
  ( Member ConversationSubsystem.ConversationSubsystem r,
    Member (Error InternalError) r,
    Member (Error UnreachableBackends) r,
    Member P.TinyLog r,
    Member Now r,
    Member ConversationStore r,
    Member (Error FederationError) r,
    Member (FederationAPIAccess FederatorClient) r,
    Member NotificationSubsystem r,
    Member BackendNotificationQueueAccess r
  ) =>
  Local UserId ->
  ConnId ->
  NewOne2OneConv ->
  Sem r (ConversationResponse Public.OwnConversation)
createOne2OneConversation lusr zcon j = do
  (c, created) <- ConversationSubsystem.createOne2OneConversation lusr zcon j
  if created
    then conversationCreated lusr c
    else conversationExisted lusr c

----------------------------------------------------------------------------
-- Helpers

conversationCreated ::
  ( Member (Error InternalError) r,
    Member (Error UnreachableBackends) r,
    Member P.TinyLog r,
    Member Now r,
    Member ConversationStore r,
    Member (Error FederationError) r,
    Member (FederationAPIAccess FederatorClient) r,
    Member NotificationSubsystem r,
    Member BackendNotificationQueueAccess r
  ) =>
  Local UserId ->
  StoredConversation ->
  Sem r (ConversationResponse Public.OwnConversation)
conversationCreated lusr cnv = do
  unless (Data.convType cnv == Public.SelfConv) $ do
    notifyCreatedConversation lusr Nothing cnv def
  Created <$> conversationViewV9 lusr cnv

conversationExisted ::
  ( Member (Error InternalError) r,
    Member P.TinyLog r
  ) =>
  Local UserId ->
  StoredConversation ->
  Sem r (ConversationResponse Public.OwnConversation)
conversationExisted lusr cnv = Existed <$> conversationViewV9 lusr cnv

notifyCreatedConversation ::
  ( Member ConversationStore r,
    Member (Error FederationError) r,
    Member (Error UnreachableBackends) r,
    Member (Error InternalError) r,
    Member (FederationAPIAccess FederatorClient) r,
    Member NotificationSubsystem r,
    Member BackendNotificationQueueAccess r,
    Member Now r,
    Member P.TinyLog r
  ) =>
  Local UserId ->
  Maybe ConnId ->
  StoredConversation ->
  JoinType ->
  Sem r ()
notifyCreatedConversation lusr conn c joinType = do
  now <- Now.get
  Interpreter.registerRemoteConversationMemberships now lusr (qualifyAs lusr c) joinType
  unless (null c.remoteMembers) $
    unlessM E.isFederationConfigured $
      throw FederationNotConfigured

  NS.pushNotifications =<< mapM (toPush now) c.localMembers
  where
    route
      | Data.convType c == Public.RegularConv = PushV2.RouteAny
      | otherwise = PushV2.RouteDirect
    toPush t m = do
      let remoteOthers = remoteMemberToOther <$> c.remoteMembers
          localOthers = map (localMemberToOther (tDomain lusr)) $ c.localMembers
          lconv = qualifyAs lusr c.id_
      c' <- conversationViewWithCachedOthers remoteOthers localOthers c (qualifyAs lusr m.id_)
      let e = Event (tUntagged lconv) Nothing (EventFromUser (tUntagged lusr)) t Nothing (EdConversation c')
      pure $
        NS.Push
          { NS.origin = Just (tUnqualified lusr),
            NS.json = toJSONObject e,
            NS.recipients = [NS.userRecipient m.id_],
            NS.isCellsEvent = False,
            NS.route = route,
            NS.conn = conn,
            NS.transient = False,
            NS.nativePriority = Nothing,
            NS.apsData = Nothing
          }

createConnectConversation ::
  ( Member ConversationSubsystem.ConversationSubsystem r,
    Member (Error InternalError) r,
    Member P.TinyLog r
  ) =>
  Local UserId ->
  Maybe ConnId ->
  Connect ->
  Sem r (ConversationResponse Public.OwnConversation)
createConnectConversation lusr conn j = do
  c <- ConversationSubsystem.createConnectConversation lusr conn j
  conversationExisted lusr c
