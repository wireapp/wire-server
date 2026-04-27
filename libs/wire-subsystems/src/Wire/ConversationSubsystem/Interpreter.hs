{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2026 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.ConversationSubsystem.Interpreter
  ( interpretConversationSubsystem,
    GroupInfoCheckEnabled (..),
    IntraListing (..),
    ConversationSubsystemError (..),
  )
where

import Data.Qualified
import Imports
import Network.Wai.Utilities.JSONResponse (JSONResponse)
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.Resource (Resource)
import Polysemy.TinyLog (TinyLog)
import Wire.API.Conversation.Config
import Wire.API.Error
import Wire.API.Federation.Client (FederatorClient)
import Wire.API.MLS.Keys (MLSKeysByPurpose, MLSPrivateKeys)
import Wire.API.Team.FeatureFlags (FanoutLimit, FeatureFlags)
import Wire.BackendNotificationQueueAccess (BackendNotificationQueueAccess)
import Wire.BrigAPIAccess
import Wire.CodeStore (CodeStore)
import Wire.ConversationStore (ConversationStore)
import Wire.ConversationStore qualified as ConvStore
import Wire.ConversationSubsystem (ConversationSubsystem (..))
import Wire.ConversationSubsystem.Action.Notify qualified as ActionNotify
import Wire.ConversationSubsystem.Clients as Clients
import Wire.ConversationSubsystem.Create qualified as Create
import Wire.ConversationSubsystem.CreateInternal qualified as CreateInternal
import Wire.ConversationSubsystem.Errors
import Wire.ConversationSubsystem.Federation qualified as Federation
import Wire.ConversationSubsystem.Fetch qualified as Fetch
import Wire.ConversationSubsystem.MLS qualified as MLS
import Wire.ConversationSubsystem.MLS.Enabled qualified as MLSEnabled
import Wire.ConversationSubsystem.MLS.GroupInfo qualified as MLSGroupInfo
import Wire.ConversationSubsystem.MLS.GroupInfoCheck (GroupInfoCheckEnabled (..))
import Wire.ConversationSubsystem.MLS.Message qualified as MLSMessage
import Wire.ConversationSubsystem.MLS.Removal qualified as MLSRemoval
import Wire.ConversationSubsystem.MLS.Reset qualified as MLSReset
import Wire.ConversationSubsystem.MLS.SubConversation qualified as MLSSubConversation
import Wire.ConversationSubsystem.Message (IntraListing (..))
import Wire.ConversationSubsystem.Notify qualified as Notify
import Wire.ConversationSubsystem.One2One qualified as One2One
import Wire.ConversationSubsystem.Query qualified as Query
import Wire.ConversationSubsystem.Update qualified as Update
import Wire.ExternalAccess (ExternalAccess)
import Wire.FeaturesConfigSubsystem
import Wire.FederationAPIAccess (FederationAPIAccess)
import Wire.FederationSubsystem (FederationSubsystem)
import Wire.FireAndForget (FireAndForget)
import Wire.HashPassword (HashPassword)
import Wire.LegalHoldStore (LegalHoldStore)
import Wire.NotificationSubsystem as NS
import Wire.Options.Galley (GuestLinkTTLSeconds)
import Wire.ProposalStore (ProposalStore)
import Wire.RateLimit (RateLimit)
import Wire.Sem.Now (Now)
import Wire.Sem.Random (Random)
import Wire.TeamCollaboratorsSubsystem
import Wire.TeamStore (TeamStore)
import Wire.TeamSubsystem (TeamSubsystem)
import Wire.UserClientIndexStore (UserClientIndexStore)
import Wire.UserGroupStore (UserGroupStore)

interpretConversationSubsystem ::
  ( Member (Error ConversationSubsystemError) r,
    Member (Error JSONResponse) r,
    Member (Error DynError) r,
    Member UserGroupStore r,
    Member (Input (Maybe GuestLinkTTLSeconds)) r,
    Member HashPassword r,
    Member RateLimit r,
    Member CodeStore r,
    Member FireAndForget r,
    Member BackendNotificationQueueAccess r,
    Member NotificationSubsystem r,
    Member ExternalAccess r,
    Member Now r,
    Member ConversationStore r,
    Member (FederationAPIAccess FederatorClient) r,
    Member BrigAPIAccess r,
    Member FeaturesConfigSubsystem r,
    Member TeamCollaboratorsSubsystem r,
    Member Random r,
    Member TeamSubsystem r,
    Member (Input FeatureFlags) r,
    Member (Input IntraListing) r,
    Member (Input ConversationSubsystemConfig) r,
    Member (Input (Local ())) r,
    Member (Input (Maybe GroupInfoCheckEnabled)) r,
    Member ProposalStore r,
    Member LegalHoldStore r,
    Member TeamStore r,
    Member ConvStore.MLSCommitLockStore r,
    Member FederationSubsystem r,
    Member Resource r,
    Member (Input (Maybe (MLSKeysByPurpose MLSPrivateKeys))) r,
    Member UserClientIndexStore r,
    Member (Input FanoutLimit) r,
    Member TinyLog r
  ) =>
  InterpreterFor ConversationSubsystem r
interpretConversationSubsystem = interpret $ \case
  NotifyConversationAction tag quid notifyOrigDomain con lconv targetsLocal targetsRemote targetsBots action extraData ->
    mapErrors $ Notify.notifyConversationActionImpl tag quid notifyOrigDomain con lconv targetsLocal targetsRemote targetsBots action extraData
  InternalCreateGroupConversation lusr conn newConv ->
    mapErrors $ CreateInternal.createGroupConversationGeneric lusr conn newConv
  CreateLegacyGroupConversation lusr conn newConv ->
    mapErrors $ Create.createLegacyGroupConversation lusr conn newConv
  CreateGroupOwnConversation lusr conn newConv ->
    mapErrors $ Create.createGroupOwnConversation lusr conn newConv
  CreateGroupConversation lusr conn newConv ->
    mapErrors $ Create.createGroupConversation lusr conn newConv
  CreateProteusSelfConversation lusr ->
    mapErrors $ Create.createProteusSelfConversation lusr
  CreateOne2OneConversation lusr zcon j ->
    mapErrors $ Create.createOne2OneConversation lusr zcon j
  CreateConnectConversation lusr conn j ->
    mapErrors $ Create.createConnectConversation lusr conn j
  GetConversations convIds ->
    mapErrors $ ConvStore.getConversations convIds
  GetConversationIds lusr maxIds pagingState ->
    mapErrors $ Fetch.getConversationIdsImpl lusr maxIds pagingState
  InternalGetLocalMember cid uid ->
    mapErrors $ ConvStore.getLocalMember cid uid
  PostMLSCommitBundle loc qusr c ctype qConvOrSub conn oosCheck bundle ->
    mapErrors $ MLSMessage.postMLSCommitBundle loc qusr c ctype qConvOrSub conn oosCheck bundle
  PostMLSCommitBundleFromLocalUser v lusr c conn bundle ->
    mapErrors $ MLSMessage.postMLSCommitBundleFromLocalUser v lusr c conn bundle
  PostMLSMessage loc qusr c ctype qconvOrSub con oosCheck msg ->
    mapErrors $ MLSMessage.postMLSMessage loc qusr c ctype qconvOrSub con oosCheck msg
  PostMLSMessageFromLocalUser v lusr c conn smsg ->
    mapErrors $ MLSMessage.postMLSMessageFromLocalUser v lusr c conn smsg
  IsMLSEnabled ->
    mapErrors $ MLSEnabled.isMLSEnabled
  GetConversationsInternal luser mids mstart msize ->
    mapErrors $ Query.getConversationsInternal luser mids mstart msize
  RemoveMemberFromLocalConv lcnv lusr con victim ->
    mapErrors $ Update.removeMemberFromLocalConv lcnv lusr con victim
  FederationOnConversationCreated domain rc ->
    mapErrors $ Federation.onConversationCreated domain rc
  FederationGetConversations domain req ->
    mapErrors $ Federation.getConversations domain req
  FederationLeaveConversation domain lc ->
    mapErrors $ Federation.leaveConversation domain lc
  FederationSendMessage domain msr ->
    mapErrors $ Federation.sendMessage domain msr
  FederationUpdateConversation domain uc ->
    mapErrors $ Federation.updateConversation domain uc
  FederationMlsSendWelcome domain req ->
    mapErrors $ Federation.mlsSendWelcome domain req
  FederationSendMLSMessage domain msr ->
    mapErrors $ Federation.sendMLSMessage domain msr
  FederationSendMLSCommitBundle domain msr ->
    mapErrors $ Federation.sendMLSCommitBundle domain msr
  FederationQueryGroupInfo domain req ->
    mapErrors $ Federation.queryGroupInfo domain req
  FederationUpdateTypingIndicator domain req ->
    mapErrors $ Federation.updateTypingIndicator domain req
  FederationOnTypingIndicatorUpdated domain td ->
    mapErrors $ Federation.onTypingIndicatorUpdated domain td
  FederationGetSubConversationForRemoteUser domain req ->
    mapErrors $ Federation.getSubConversationForRemoteUser domain req
  FederationDeleteSubConversationForRemoteUser domain req ->
    mapErrors $ Federation.deleteSubConversationForRemoteUser domain req
  FederationLeaveSubConversation domain lscr ->
    mapErrors $ Federation.leaveSubConversation domain lscr
  FederationGetLegacyOne2OneConversation domain req ->
    mapErrors $ Federation.getLegacyOne2OneConversation domain req
  FederationGetOne2OneConversation domain req ->
    mapErrors $ Federation.getOne2OneConversation domain req
  FederationOnClientRemoved domain req ->
    mapErrors $ Federation.onClientRemoved domain req
  FederationOnMessageSent domain rm ->
    mapErrors $ Federation.onMessageSent domain rm
  FederationOnMLSMessageSent domain rmm ->
    mapErrors $ Federation.onMLSMessageSent domain rmm
  FederationOnConversationUpdated domain cu ->
    mapErrors $ Federation.onConversationUpdated domain cu
  FederationOnUserDeleted domain udcn ->
    mapErrors $ Federation.onUserDeleted domain udcn
  PostOtrMessageUnqualified lusr con cnv ignore report msg ->
    mapErrors $ Update.postOtrMessageUnqualified lusr con cnv ignore report msg
  PostOtrBroadcastUnqualified lusr con ignore report msg ->
    mapErrors $ Update.postOtrBroadcastUnqualified lusr con ignore report msg
  PostProteusMessage lusr con cnv msg ->
    mapErrors $ Update.postProteusMessage lusr con cnv msg
  PostProteusBroadcast lusr con msg ->
    mapErrors $ Update.postProteusBroadcast lusr con msg
  DeleteLocalConversation lusr con lcnv ->
    mapErrors $ Update.deleteLocalConversation lusr con lcnv
  GetMLSPublicKeys fmt ->
    mapErrors $ MLS.getMLSPublicKeys fmt
  ResetMLSConversation lusr reset ->
    mapErrors $ MLSReset.resetMLSConversation lusr reset
  GetSubConversation lusr cnv sub ->
    mapErrors $ MLSSubConversation.getSubConversation lusr cnv sub
  GetBotConversation bid cnv ->
    mapErrors $ Query.getBotConversation bid cnv
  GetUnqualifiedOwnConversation lusr cnv ->
    mapErrors $ Query.getUnqualifiedOwnConversation lusr cnv
  GetOwnConversation lusr qcnv ->
    mapErrors $ Query.getOwnConversation lusr qcnv
  GetConversation lusr qcnv ->
    mapErrors $ Query.getConversation lusr qcnv
  InternalGetConversation cnv ->
    mapErrors $ ConvStore.getConversation cnv
  GetConversationRoles lusr cnv ->
    mapErrors $ Query.getConversationRoles lusr cnv
  GetGroupInfo lusr qcnv ->
    mapErrors $ MLSGroupInfo.getGroupInfo lusr qcnv
  ConversationIdsPageFromUnqualified lusr mstart msize ->
    mapErrors $ Query.conversationIdsPageFromUnqualified lusr mstart msize
  ConversationIdsPaginated listGlobalSelf lself req ->
    mapErrors $ Query.conversationIdsPaginated listGlobalSelf lself req
  ConversationIdsPageFrom lusr req ->
    mapErrors $ Query.conversationIdsPageFrom lusr req
  ListConversations luser req ->
    mapErrors $ Query.listConversations luser req
  GetConversationByReusableCode lusr key value ->
    mapErrors $ Query.getConversationByReusableCode lusr key value
  GetMLSSelfConversationWithError lusr ->
    mapErrors $ Query.getMLSSelfConversationWithError lusr
  GetMLSOne2OneOwnConversation lself qother ->
    mapErrors $ Query.getMLSOne2OneOwnConversation lself qother
  GetMLSOne2OneMLSConversation lself qother ->
    mapErrors $ Query.getMLSOne2OneMLSConversation lself qother
  GetMLSOne2OneConversation lself qother fmt ->
    mapErrors $ Query.getMLSOne2OneConversation lself qother fmt
  GetLocalSelf lusr cnv ->
    mapErrors $ Query.getLocalSelf lusr cnv
  GetSelfMember lusr qcnv ->
    mapErrors $ Query.getSelfMember lusr qcnv
  GetConversationGuestLinksStatus uid cid ->
    mapErrors $ Query.getConversationGuestLinksStatus uid cid
  GetCode mcode lusr cnv ->
    mapErrors $ Update.getCode mcode lusr cnv
  AddQualifiedMembersUnqualified lusr con cnv invite ->
    mapErrors $ Update.addQualifiedMembersUnqualified lusr con cnv invite
  AddMembers lusr zcon qcnv invite ->
    mapErrors $ Update.addMembers lusr zcon qcnv invite
  ReplaceMembers lusr zcon qcnv invite ->
    mapErrors $ Update.replaceMembers lusr zcon qcnv invite
  JoinConversationById lusr con cnv ->
    mapErrors $ Update.joinConversationById lusr con cnv
  JoinConversationByReusableCode lusr con req ->
    mapErrors $ Update.joinConversationByReusableCode lusr con req
  CheckReusableCode addr code ->
    mapErrors $ Update.checkReusableCode addr code
  AddCodeUnqualified mReq usr mbZHost mZcon cnv ->
    mapErrors $ Update.addCodeUnqualified mReq usr mbZHost mZcon cnv
  RmCodeUnqualified lusr con cnv ->
    mapErrors $ Update.rmCodeUnqualified lusr con cnv
  MemberTyping lusr con qcnv status ->
    mapErrors $ Update.memberTyping lusr con qcnv status
  RemoveMemberQualified lusr con qcnv quid ->
    mapErrors $ Update.removeMemberQualified lusr con qcnv quid
  UpdateOtherMember lusr con qcnv quid update ->
    mapErrors $ Update.updateOtherMember lusr con qcnv quid update
  UpdateConversationName lusr zcon qcnv rename ->
    mapErrors $ Update.updateConversationName lusr zcon qcnv rename
  UpdateConversationMessageTimer lusr zcon qcnv update ->
    mapErrors $ Update.updateConversationMessageTimer lusr zcon qcnv update
  UpdateConversationReceiptMode lusr zcon qcnv update ->
    mapErrors $ Update.updateConversationReceiptMode lusr zcon qcnv update
  UpdateConversationAccess lusr zcon qcnv update ->
    mapErrors $ Update.updateConversationAccess lusr zcon qcnv update
  UpdateConversationHistory lusr zcon qcnv update ->
    mapErrors $ Update.updateConversationHistory lusr zcon qcnv update
  UpdateSelfMember lusr zcon qcnv update ->
    mapErrors $ Update.updateSelfMember lusr zcon qcnv update
  UpdateConversationProtocolWithLocalUser lusr conn qcnv update ->
    mapErrors $ Update.updateConversationProtocolWithLocalUser lusr conn qcnv update
  UpdateChannelAddPermission lusr conn qcnv update ->
    mapErrors $ Update.updateChannelAddPermission lusr conn qcnv update
  PostBotMessageUnqualified bid cnv ignore report msg ->
    mapErrors $ Update.postBotMessageUnqualified bid cnv ignore report msg
  DeleteSubConversation lusr qcnv sub reset ->
    mapErrors $ MLSSubConversation.deleteSubConversation lusr qcnv sub reset
  GetSubConversationGroupInfo lusr qcnv sub ->
    mapErrors $ MLSSubConversation.getSubConversationGroupInfo lusr qcnv sub
  LeaveSubConversation lusr cli qcnv sub ->
    mapErrors $ MLSSubConversation.leaveSubConversation lusr cli qcnv sub
  SendConversationActionNotifications tag quid notifyOrigDomain con lconv targets action extraData ->
    mapErrors $ ActionNotify.sendConversationActionNotifications tag quid notifyOrigDomain con lconv targets action extraData
  GetPaginatedConversations lusr mids mstart msize ->
    mapErrors $ Query.getConversations lusr mids mstart msize
  SearchChannels lusr tid searchString sortOrder pageSize lastName lastId discoverable ->
    mapErrors $ Query.searchChannels lusr tid searchString sortOrder pageSize lastName lastId discoverable
  InternalGetMember qcnv usr ->
    mapErrors $ Query.internalGetMember qcnv usr
  GetConversationMeta cnv ->
    mapErrors $ Query.getConversationMeta cnv
  GetMLSOne2OneConversationInternal lself qother ->
    mapErrors $ Query.getMLSOne2OneConversationInternal lself qother
  IsMLSOne2OneEstablished lself qother ->
    mapErrors $ Query.isMLSOne2OneEstablished lself qother
  GetLocalConversationInternal cid ->
    mapErrors $ Query.getLocalConversationInternal cid
  RemoveClient uid cid ->
    mapErrors $ Clients.rmClient uid cid
  AddBot lusr zcon b ->
    mapErrors $ Update.addBot lusr zcon b
  RmBot lusr zcon b ->
    mapErrors $ Update.rmBot lusr zcon b
  UpdateCellsState cnv state ->
    mapErrors $ Update.updateCellsState cnv state
  RemoveUser lc includeMain qusr ->
    mapErrors $ MLSRemoval.removeUser lc includeMain qusr
  InternalUpsertOne2OneConversation req ->
    mapErrors $ One2One.internalUpsertOne2OneConversation req
  AcceptConv lusr conn cnv ->
    mapErrors $ Update.acceptConv lusr conn cnv
  BlockConv lusr qcnv ->
    mapErrors $ Update.blockConv lusr qcnv
  UnblockConv lusr conn qcnv ->
    mapErrors $ Update.unblockConv lusr conn qcnv
