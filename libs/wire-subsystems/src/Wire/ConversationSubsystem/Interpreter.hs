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
    Update.GuestLinkTTLSeconds (..),
  )
where

import Data.Proxy (Proxy (..))
import Data.Qualified
import Data.Singletons (fromSing)
import Galley.Types.Error (InternalError, InvalidInput (..))
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.Internal.Tactics (liftT)
import Polysemy.Resource (Resource)
import Polysemy.TinyLog (TinyLog)
import Wire.API.Conversation.Config
import Wire.API.Conversation.Role qualified as ConvRole
import Wire.API.Error
import Wire.API.Error.Galley
import Wire.API.Federation.Client (FederatorClient)
import Wire.API.Federation.Error
import Wire.API.MLS.Keys (MLSKeysByPurpose, MLSPrivateKeys)
import Wire.API.Team.Feature (LegalholdConfig)
import Wire.API.Team.FeatureFlags (FanoutLimit, FeatureDefaults, FeatureFlags)
import Wire.BackendNotificationQueueAccess (BackendNotificationQueueAccess)
import Wire.BrigAPIAccess
import Wire.CodeStore (CodeStore)
import Wire.ConversationStore (ConversationStore)
import Wire.ConversationStore qualified as ConvStore
import Wire.ConversationSubsystem
import Wire.ConversationSubsystem.Action.Notify qualified as ActionNotify
import Wire.ConversationSubsystem.Clients qualified as Clients
import Wire.ConversationSubsystem.Create qualified as Create
import Wire.ConversationSubsystem.CreateInternal qualified as CreateInternal
import Wire.ConversationSubsystem.Features qualified as Features
import Wire.ConversationSubsystem.Federation qualified as Federation
import Wire.ConversationSubsystem.Fetch qualified as Fetch
import Wire.ConversationSubsystem.Internal qualified as Internal
import Wire.ConversationSubsystem.Legalhold qualified as Legalhold
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
import Wire.ConversationSubsystem.Util qualified as Util
import Wire.ExternalAccess (ExternalAccess)
import Wire.FeaturesConfigSubsystem
import Wire.FeaturesConfigSubsystem.Types (ExposeInvitationURLsAllowlist)
import Wire.FederationAPIAccess (FederationAPIAccess)
import Wire.FederationSubsystem (FederationSubsystem)
import Wire.FireAndForget (FireAndForget)
import Wire.HashPassword (HashPassword)
import Wire.LegalHoldStore (LegalHoldStore)
import Wire.NotificationSubsystem as NS
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
  ( Member (Error FederationError) r,
    Member (Error UnreachableBackends) r,
    Member (Error InternalError) r,
    Member (Error InvalidInput) r,
    Member (ErrorS 'ConvAccessDenied) r,
    Member (ErrorS 'NotATeamMember) r,
    Member (ErrorS OperationDenied) r,
    Member (Error AuthenticationError) r,
    Member (Input (FeatureDefaults LegalholdConfig)) r,
    Member (ErrorS 'NotConnected) r,
    Member (ErrorS 'MLSNotEnabled) r,
    Member (ErrorS 'MLSNonEmptyMemberList) r,
    Member (ErrorS 'MissingLegalholdConsent) r,
    Member (ErrorS 'NonBindingTeam) r,
    Member (ErrorS 'NoBindingTeamMembers) r,
    Member (ErrorS 'TeamNotFound) r,
    Member (ErrorS 'InvalidOperation) r,
    Member (ErrorS 'ConvNotFound) r,
    Member (ErrorS 'ChannelsNotEnabled) r,
    Member (ErrorS 'NotAnMlsConversation) r,
    Member (ErrorS 'MLSLegalholdIncompatible) r,
    Member (ErrorS 'MLSIdentityMismatch) r,
    Member (ErrorS 'MLSUnsupportedMessage) r,
    Member (ErrorS 'MLSStaleMessage) r,
    Member (ErrorS 'MLSProposalNotFound) r,
    Member (ErrorS 'MLSCommitMissingReferences) r,
    Member (ErrorS 'MLSSelfRemovalNotAllowed) r,
    Member (ErrorS 'MLSClientSenderUserMismatch) r,
    Member (ErrorS 'MLSSubConvClientNotInParent) r,
    Member (ErrorS 'MLSInvalidLeafNodeSignature) r,
    Member (ErrorS 'MLSClientMismatch) r,
    Member (ErrorS 'MLSInvalidLeafNodeIndex) r,
    Member (ErrorS 'MLSUnsupportedProposal) r,
    Member (Error MLSProtocolError) r,
    Member (Error GroupInfoDiagnostics) r,
    Member (Error MLSOutOfSyncError) r,
    Member (Error MLSProposalFailure) r,
    Member (Error NonFederatingBackends) r,
    Member (ErrorS 'GroupIdVersionNotSupported) r,
    Member (ErrorS 'ConvMemberNotFound) r,
    Member (ErrorS 'HistoryNotSupported) r,
    Member (Error UnreachableBackendsLegacy) r,
    Member (ErrorS MLSGroupConversationMismatch) r,
    Member (ErrorS ('ActionDenied ConvRole.LeaveConversation)) r,
    Member (ErrorS ('ActionDenied ConvRole.RemoveConversationMember)) r,
    Member (ErrorS ('ActionDenied ConvRole.DeleteConversation)) r,
    Member (ErrorS 'BroadcastLimitExceeded) r,
    Member (ErrorS 'MLSFederatedResetNotSupported) r,
    Member (ErrorS 'MLSSubConvUnsupportedConvType) r,
    Member (ErrorS 'TeamMemberNotFound) r,
    Member (ErrorS 'AccessDenied) r,
    Member (ErrorS 'MLSMissingGroupInfo) r,
    Member (ErrorS 'CodeNotFound) r,
    Member (ErrorS 'InvalidConversationPassword) r,
    Member (ErrorS 'GuestLinksDisabled) r,
    Member (ErrorS 'MLSFederatedOne2OneNotSupported) r,
    Member (ErrorS 'TooManyMembers) r,
    Member (ErrorS 'CreateConversationCodeConflict) r,
    Member (ErrorS 'InvalidTarget) r,
    Member (ErrorS 'MLSReadReceiptsNotAllowed) r,
    Member (ErrorS 'InvalidTargetAccess) r,
    Member (ErrorS 'ConvInvalidProtocolTransition) r,
    Member (ErrorS 'MLSMigrationCriteriaNotSatisfied) r,
    Member (ErrorS ('ActionDenied ConvRole.AddConversationMember)) r,
    Member (ErrorS ('ActionDenied ConvRole.ModifyOtherConversationMember)) r,
    Member (ErrorS ('ActionDenied ConvRole.ModifyConversationName)) r,
    Member (ErrorS ('ActionDenied ConvRole.ModifyConversationMessageTimer)) r,
    Member (ErrorS ('ActionDenied ConvRole.ModifyConversationReceiptMode)) r,
    Member (ErrorS ('ActionDenied ConvRole.ModifyConversationAccess)) r,
    Member (ErrorS ('ActionDenied ConvRole.ModifyAddPermission)) r,
    Member UserGroupStore r,
    Member (Input (Maybe Update.GuestLinkTTLSeconds)) r,
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
    Member (Input ExposeInvitationURLsAllowlist) r,
    Member TeamStore r,
    Member ConvStore.MLSCommitLockStore r,
    Member FederationSubsystem r,
    Member Resource r,
    Member (Input (Maybe (MLSKeysByPurpose MLSPrivateKeys))) r,
    Member UserClientIndexStore r,
    Member (Input FanoutLimit) r,
    Member TinyLog r
  ) =>
  Sem (ConversationSubsystem : r) a ->
  Sem r a
interpretConversationSubsystem = interpretH $ \case
  NotifyConversationAction tag quid notifyOrigDomain con lconv targetsLocal targetsRemote targetsBots action extraData ->
    liftT $ Notify.notifyConversationActionImpl tag quid notifyOrigDomain con lconv targetsLocal targetsRemote targetsBots action extraData
  InternalCreateGroupConversation lusr conn newConv ->
    liftT $ CreateInternal.createGroupConversationGeneric lusr conn newConv
  CreateGroupConversationUpToV3 lusr conn newConv ->
    liftT $ Create.createGroupConversationUpToV3 lusr conn newConv
  CreateGroupOwnConversation lusr conn newConv ->
    liftT $ Create.createGroupOwnConversation lusr conn newConv
  CreateGroupConversation lusr conn newConv ->
    liftT $ Create.createGroupConversation lusr conn newConv
  CreateProteusSelfConversation lusr ->
    liftT $ Create.createProteusSelfConversation lusr
  CreateOne2OneConversation lusr zcon j ->
    liftT $ Create.createOne2OneConversation lusr zcon j
  CreateConnectConversation lusr conn j ->
    liftT $ Create.createConnectConversation lusr conn j
  GetConversations convIds ->
    liftT $ ConvStore.getConversations convIds
  GetConversationIds lusr maxIds pagingState ->
    liftT $ Fetch.getConversationIdsImpl lusr maxIds pagingState
  InternalGetClientIds uids ->
    liftT $ Internal.internalGetClientIds uids
  InternalGetLocalMember cid uid ->
    liftT $ ConvStore.getLocalMember cid uid
  PostMLSCommitBundle loc qusr c ctype qConvOrSub conn oosCheck bundle ->
    liftT $ MLSMessage.postMLSCommitBundle loc qusr c ctype qConvOrSub conn oosCheck bundle
  PostMLSCommitBundleFromLocalUser v lusr c conn bundle ->
    liftT $ MLSMessage.postMLSCommitBundleFromLocalUser v lusr c conn bundle
  PostMLSMessage loc qusr c ctype qconvOrSub con oosCheck msg ->
    liftT $ MLSMessage.postMLSMessage loc qusr c ctype qconvOrSub con oosCheck msg
  PostMLSMessageFromLocalUser v lusr c conn smsg ->
    liftT $ MLSMessage.postMLSMessageFromLocalUser v lusr c conn smsg
  IsMLSEnabled ->
    liftT $ MLSEnabled.isMLSEnabled
  IterateConversations luid pageSize handleConvs -> do
    handleConvsT <- bindT handleConvs
    ins <- getInitialStateT
    void $ raise $ interpretConversationSubsystem $ Query.iterateConversations luid pageSize $ handleConvsT . ($>) ins
    pureT ()
  RemoveMemberFromLocalConv lcnv lusr con victim ->
    liftT $ Update.removeMemberFromLocalConv lcnv lusr con victim
  FederationOnConversationCreated domain rc ->
    liftT $ Federation.onConversationCreated domain rc
  FederationGetConversationsV1 domain req ->
    liftT $ Federation.getConversationsV1 domain req
  FederationGetConversations domain req ->
    liftT $ Federation.getConversations domain req
  FederationLeaveConversation domain lc ->
    liftT $ Federation.leaveConversation domain lc
  FederationSendMessage domain msr ->
    liftT $ Federation.sendMessage domain msr
  FederationUpdateConversation domain uc ->
    liftT $ Federation.updateConversation domain uc
  FederationMlsSendWelcome domain req ->
    liftT $ Federation.mlsSendWelcome domain req
  FederationSendMLSMessage domain msr ->
    liftT $ Federation.sendMLSMessage domain msr
  FederationSendMLSCommitBundle domain msr ->
    liftT $ Federation.sendMLSCommitBundle domain msr
  FederationQueryGroupInfo domain req ->
    liftT $ Federation.queryGroupInfo domain req
  FederationUpdateTypingIndicator domain req ->
    liftT $ Federation.updateTypingIndicator domain req
  FederationOnTypingIndicatorUpdated domain td ->
    liftT $ Federation.onTypingIndicatorUpdated domain td
  FederationGetSubConversationForRemoteUser domain req ->
    liftT $ Federation.getSubConversationForRemoteUser domain req
  FederationDeleteSubConversationForRemoteUser domain req ->
    liftT $ Federation.deleteSubConversationForRemoteUser domain req
  FederationLeaveSubConversation domain lscr ->
    liftT $ Federation.leaveSubConversation domain lscr
  FederationGetOne2OneConversationV1 domain req ->
    liftT $ Federation.getOne2OneConversationV1 domain req
  FederationGetOne2OneConversation domain req ->
    liftT $ Federation.getOne2OneConversation domain req
  FederationOnClientRemoved domain req ->
    liftT $ Federation.onClientRemoved domain req
  FederationOnMessageSent domain rm ->
    liftT $ Federation.onMessageSent domain rm
  FederationOnMLSMessageSent domain rmm ->
    liftT $ Federation.onMLSMessageSent domain rmm
  FederationOnConversationUpdatedV0 domain cu ->
    liftT $ Federation.onConversationUpdatedV0 domain cu
  FederationOnConversationUpdated domain cu ->
    liftT $ Federation.onConversationUpdated domain cu
  FederationOnUserDeleted domain udcn ->
    liftT $ Federation.onUserDeleted domain udcn
  PostOtrMessageUnqualified lusr con cnv ignore report msg ->
    liftT $ Update.postOtrMessageUnqualified lusr con cnv ignore report msg
  PostOtrBroadcastUnqualified lusr con ignore report msg ->
    liftT $ Update.postOtrBroadcastUnqualified lusr con ignore report msg
  PostProteusMessage lusr con cnv msg ->
    liftT $ Update.postProteusMessage lusr con cnv msg
  PostProteusBroadcast lusr con msg ->
    liftT $ Update.postProteusBroadcast lusr con msg
  DeleteLocalConversation lusr con lcnv ->
    liftT $ Update.deleteLocalConversation lusr con lcnv
  GetMLSPublicKeys fmt ->
    liftT $ MLS.getMLSPublicKeys fmt
  ResetMLSConversation lusr reset ->
    liftT $ MLSReset.resetMLSConversation lusr reset
  GetSubConversation lusr cnv sub ->
    liftT $ MLSSubConversation.getSubConversation lusr cnv sub
  GetUserStatus lusr tid uid ->
    liftT $ Legalhold.getUserStatus lusr tid uid
  GuardSecondFactorDisabled uid cid ->
    liftT $ Features.guardSecondFactorDisabled uid cid
  GetBotConversation bid cnv ->
    liftT $ Query.getBotConversation bid cnv
  GetUnqualifiedOwnConversation lusr cnv ->
    liftT $ Query.getUnqualifiedOwnConversation lusr cnv
  GetOwnConversation lusr qcnv ->
    liftT $ Query.getOwnConversation lusr qcnv
  GetConversation lusr qcnv ->
    liftT $ Query.getConversation lusr qcnv
  GetConversationRoles lusr cnv ->
    liftT $ Query.getConversationRoles lusr cnv
  GetGroupInfo lusr qcnv ->
    liftT $ MLSGroupInfo.getGroupInfo lusr qcnv
  ConversationIdsPageFromUnqualified lusr mstart msize ->
    liftT $ Query.conversationIdsPageFromUnqualified lusr mstart msize
  ConversationIdsPageFromV2 listGlobalSelf lself req ->
    liftT $ Query.conversationIdsPageFromV2 listGlobalSelf lself req
  ConversationIdsPageFrom lusr req ->
    liftT $ Query.conversationIdsPageFrom lusr req
  ListConversations luser req ->
    liftT $ Query.listConversations luser req
  GetConversationByReusableCode lusr key value ->
    liftT $ Query.getConversationByReusableCode lusr key value
  GetMLSSelfConversationWithError lusr ->
    liftT $ Query.getMLSSelfConversationWithError lusr
  GetMLSOne2OneConversationV5 lself qother ->
    liftT $ Query.getMLSOne2OneConversationV5 lself qother
  GetMLSOne2OneConversationV6 lself qother ->
    liftT $ Query.getMLSOne2OneConversationV6 lself qother
  GetMLSOne2OneConversation lself qother fmt ->
    liftT $ Query.getMLSOne2OneConversation lself qother fmt
  GetLocalSelf lusr cnv ->
    liftT $ Query.getLocalSelf lusr cnv
  GetSelfMember lusr qcnv ->
    liftT $ Query.getSelfMember lusr qcnv
  GetConversationGuestLinksStatus uid cid ->
    liftT $ Query.getConversationGuestLinksStatus uid cid
  GetCode mcode lusr cnv ->
    liftT $ Update.getCode mcode lusr cnv
  AddMembersUnqualified lusr con cnv invite ->
    liftT $ Update.addMembersUnqualified lusr con cnv invite
  AddMembersUnqualifiedV2 lusr con cnv invite ->
    liftT $ Update.addMembersUnqualifiedV2 lusr con cnv invite
  AddMembers lusr zcon qcnv invite ->
    liftT $ Update.addMembers lusr zcon qcnv invite
  ReplaceMembers lusr zcon qcnv invite ->
    liftT $ Update.replaceMembers lusr zcon qcnv invite
  JoinConversationById lusr con cnv ->
    liftT $ Update.joinConversationById lusr con cnv
  JoinConversationByReusableCode lusr con req ->
    liftT $ Update.joinConversationByReusableCode lusr con req
  CheckReusableCode addr code ->
    liftT $ Update.checkReusableCode addr code
  AddCodeUnqualified mReq usr mbZHost mZcon cnv ->
    liftT $ Update.addCodeUnqualified mReq usr mbZHost mZcon cnv
  AddCodeUnqualifiedWithReqBody lusr mname mconn cnv req ->
    liftT $ Update.addCodeUnqualifiedWithReqBody lusr mname mconn cnv req
  RmCodeUnqualified lusr con cnv ->
    liftT $ Update.rmCodeUnqualified lusr con cnv
  MemberTypingUnqualified lusr con cnv status ->
    liftT $ Update.memberTypingUnqualified lusr con cnv status
  MemberTyping lusr con qcnv status ->
    liftT $ Update.memberTyping lusr con qcnv status
  RemoveMemberUnqualified lusr con cnv uid ->
    liftT $ Update.removeMemberUnqualified lusr con cnv uid
  RemoveMemberQualified lusr con qcnv quid ->
    liftT $ Update.removeMemberQualified lusr con qcnv quid
  UpdateOtherMemberUnqualified lusr con cnv uid update ->
    liftT $ Update.updateOtherMemberUnqualified lusr con cnv uid update
  UpdateOtherMember lusr con qcnv quid update ->
    liftT $ Update.updateOtherMember lusr con qcnv quid update
  UpdateUnqualifiedConversationName lusr con cnv rename ->
    liftT $ Update.updateUnqualifiedConversationName lusr con cnv rename
  UpdateConversationName lusr zcon qcnv rename ->
    liftT $ Update.updateConversationName lusr zcon qcnv rename
  UpdateConversationMessageTimerUnqualified lusr con cnv update ->
    liftT $ Update.updateConversationMessageTimerUnqualified lusr con cnv update
  UpdateConversationMessageTimer lusr zcon qcnv update ->
    liftT $ Update.updateConversationMessageTimer lusr zcon qcnv update
  UpdateConversationReceiptModeUnqualified lusr con cnv update ->
    liftT $ Update.updateConversationReceiptModeUnqualified lusr con cnv update
  UpdateConversationReceiptMode lusr zcon qcnv update ->
    liftT $ Update.updateConversationReceiptMode lusr zcon qcnv update
  UpdateConversationAccessUnqualified lusr con cnv update ->
    liftT $ Update.updateConversationAccessUnqualified lusr con cnv update
  UpdateConversationAccess lusr zcon qcnv update ->
    liftT $ Update.updateConversationAccess lusr zcon qcnv update
  UpdateConversationHistory lusr zcon qcnv update ->
    liftT $ Update.updateConversationHistory lusr zcon qcnv update
  UpdateUnqualifiedSelfMember lusr con cnv update ->
    liftT $ Update.updateUnqualifiedSelfMember lusr con cnv update
  UpdateSelfMember lusr zcon qcnv update ->
    liftT $ Update.updateSelfMember lusr zcon qcnv update
  UpdateConversationProtocolWithLocalUser lusr conn qcnv update ->
    liftT $ Update.updateConversationProtocolWithLocalUser lusr conn qcnv update
  UpdateChannelAddPermission lusr conn qcnv update ->
    liftT $ Update.updateChannelAddPermission lusr conn qcnv update
  PostBotMessageUnqualified bid cnv ignore report msg ->
    liftT $ Update.postBotMessageUnqualified bid cnv ignore report msg
  DeleteSubConversation lusr qcnv sub reset ->
    liftT $ MLSSubConversation.deleteSubConversation lusr qcnv sub reset
  GetSubConversationGroupInfo lusr qcnv sub ->
    liftT $ MLSSubConversation.getSubConversationGroupInfo lusr qcnv sub
  LeaveSubConversation lusr cli qcnv sub ->
    liftT $ MLSSubConversation.leaveSubConversation lusr cli qcnv sub
  SendConversationActionNotifications tag quid notifyOrigDomain con lconv targets action extraData ->
    liftT $ ActionNotify.sendConversationActionNotifications tag quid notifyOrigDomain con lconv targets action extraData
  GetPaginatedConversations lusr mids mstart msize ->
    liftT $ Query.getConversations lusr mids mstart msize
  SearchChannels lusr tid searchString sortOrder pageSize lastName lastId discoverable ->
    liftT $ Query.searchChannels lusr tid searchString sortOrder pageSize lastName lastId discoverable
  FeatureEnabledForTeam (Proxy :: Proxy cfg) tid ->
    liftT $ Features.featureEnabledForTeam @cfg tid
  GetAllTeamFeaturesForUser uid ->
    liftT $ Features.getAllTeamFeaturesForUser uid
  GetSingleFeatureForUser uid ->
    liftT $ Features.getSingleFeatureForUser uid
  PermissionCheck p mTeam ->
    liftT $ Util.permissionCheck p mTeam
  PermissionCheckSAbs (PermissionCheckArgs p mTeam) ->
    liftT $ Util.permissionCheck (fromSing p) mTeam
  EnsureReAuthorised u secret mbAction mbCode ->
    liftT $ Util.ensureReAuthorised u secret mbAction mbCode
  QualifyLocal a ->
    liftT $ Util.qualifyLocal a
  AssertOnTeam uid tid ->
    liftT $ Util.assertOnTeam uid tid
  CheckConsent teamsOfUsers other ->
    liftT $ Util.checkConsent teamsOfUsers other
  GetLHStatusForUsers uids ->
    liftT $ Util.getLHStatusForUsers uids
  EnsureConnectedToLocals u uids ->
    liftT $ Util.ensureConnectedToLocals u uids
  GetTeamMembersForFanout tid ->
    liftT $ Util.getTeamMembersForFanout tid
  AssertTeamExists tid ->
    liftT $ Util.assertTeamExists tid
  InternalGetMember qcnv usr ->
    liftT $ Query.internalGetMember qcnv usr
  GetConversationMeta cnv ->
    liftT $ Query.getConversationMeta cnv
  GetMLSOne2OneConversationInternal lself qother ->
    liftT $ Query.getMLSOne2OneConversationInternal lself qother
  IsMLSOne2OneEstablished lself qother ->
    liftT $ Query.isMLSOne2OneEstablished lself qother
  GetLocalConversationInternal cid ->
    liftT $ Query.getLocalConversationInternal cid
  RmClient usr cid ->
    liftT $ Clients.rmClient usr cid
  GetClients usr ->
    liftT $ Clients.getClients usr
  AddBot lusr zcon b ->
    liftT $ Update.addBot lusr zcon b
  RmBot lusr zcon b ->
    liftT $ Update.rmBot lusr zcon b
  GetFeatureInternal tid ->
    liftT $ Features.getFeatureInternal tid
  UpdateCellsState cnv state ->
    liftT $ Update.updateCellsState cnv state
  RemoveUser lc includeMain qusr ->
    liftT $ MLSRemoval.removeUser lc includeMain qusr
  InternalUpsertOne2OneConversation req ->
    liftT $ One2One.internalUpsertOne2OneConversation req
  AcceptConv lusr conn cnv ->
    liftT $ Update.acceptConv lusr conn cnv
  BlockConv lusr qcnv ->
    liftT $ Update.blockConv lusr qcnv
  UnblockConv lusr conn qcnv ->
    liftT $ Update.unblockConv lusr conn qcnv
