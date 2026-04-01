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
    ConversationSubsystemError (..),
  )
where

import Data.Qualified
import Data.Tagged
import Galley.Types.Error (InternalError, InvalidInput (..))
import Imports
import Network.Wai.Utilities.JSONResponse (JSONResponse)
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
import Wire.API.Routes.API (ServerEffect (interpretServerEffect))
import Wire.API.Team.Feature (LegalholdConfig)
import Wire.API.Team.FeatureFlags (FanoutLimit, FeatureDefaults, FeatureFlags)
import Wire.BackendNotificationQueueAccess (BackendNotificationQueueAccess)
import Wire.BrigAPIAccess
import Wire.CodeStore (CodeStore)
import Wire.ConversationStore (ConversationStore)
import Wire.ConversationStore qualified as ConvStore
import Wire.ConversationSubsystem
import Wire.ConversationSubsystem.Action.Notify qualified as ActionNotify
import Wire.ConversationSubsystem.Create qualified as Create
import Wire.ConversationSubsystem.CreateInternal qualified as CreateInternal
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
  ( Member (Error ConversationSubsystemError) r,
    Member (Error JSONResponse) r,
    Member (Error DynError) r,
    Member (Input (FeatureDefaults LegalholdConfig)) r,
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
    liftT $ mapErrors $ Notify.notifyConversationActionImpl tag quid notifyOrigDomain con lconv targetsLocal targetsRemote targetsBots action extraData
  InternalCreateGroupConversation lusr conn newConv ->
    liftT $ mapErrors $ CreateInternal.createGroupConversationGeneric lusr conn newConv
  CreateGroupConversationUpToV3 lusr conn newConv ->
    liftT $ mapErrors $ Create.createGroupConversationUpToV3 lusr conn newConv
  CreateGroupOwnConversation lusr conn newConv ->
    liftT $ mapErrors $ Create.createGroupOwnConversation lusr conn newConv
  CreateGroupConversation lusr conn newConv ->
    liftT $ mapErrors $ Create.createGroupConversation lusr conn newConv
  CreateProteusSelfConversation lusr ->
    liftT $ mapErrors $ Create.createProteusSelfConversation lusr
  CreateOne2OneConversation lusr zcon j ->
    liftT $ mapErrors $ Create.createOne2OneConversation lusr zcon j
  CreateConnectConversation lusr conn j ->
    liftT $ mapErrors $ Create.createConnectConversation lusr conn j
  GetConversations convIds ->
    liftT $ mapErrors $ ConvStore.getConversations convIds
  GetConversationIds lusr maxIds pagingState ->
    liftT $ mapErrors $ Fetch.getConversationIdsImpl lusr maxIds pagingState
  InternalGetLocalMember cid uid ->
    liftT $ mapErrors $ ConvStore.getLocalMember cid uid
  PostMLSCommitBundle loc qusr c ctype qConvOrSub conn oosCheck bundle ->
    liftT $ mapErrors $ MLSMessage.postMLSCommitBundle loc qusr c ctype qConvOrSub conn oosCheck bundle
  PostMLSCommitBundleFromLocalUser v lusr c conn bundle ->
    liftT $ mapErrors $ MLSMessage.postMLSCommitBundleFromLocalUser v lusr c conn bundle
  PostMLSMessage loc qusr c ctype qconvOrSub con oosCheck msg ->
    liftT $ mapErrors $ MLSMessage.postMLSMessage loc qusr c ctype qconvOrSub con oosCheck msg
  PostMLSMessageFromLocalUser v lusr c conn smsg ->
    liftT $ mapErrors $ MLSMessage.postMLSMessageFromLocalUser v lusr c conn smsg
  IsMLSEnabled ->
    liftT $ mapErrors $ MLSEnabled.isMLSEnabled
  IterateConversations luid pageSize handleConvs -> do
    handleConvsT <- bindT handleConvs
    ins <- getInitialStateT
    void $ raise $ interpretConversationSubsystem $ Query.iterateConversations luid pageSize $ handleConvsT . ($>) ins
    pureT ()
  RemoveMemberFromLocalConv lcnv lusr con victim ->
    liftT $ mapErrors $ Update.removeMemberFromLocalConv lcnv lusr con victim
  FederationOnConversationCreated domain rc ->
    liftT $ mapErrors $ Federation.onConversationCreated domain rc
  FederationGetConversationsV1 domain req ->
    liftT $ mapErrors $ Federation.getConversationsV1 domain req
  FederationGetConversations domain req ->
    liftT $ mapErrors $ Federation.getConversations domain req
  FederationLeaveConversation domain lc ->
    liftT $ mapErrors $ Federation.leaveConversation domain lc
  FederationSendMessage domain msr ->
    liftT $ mapErrors $ Federation.sendMessage domain msr
  FederationUpdateConversation domain uc ->
    liftT $ mapErrors $ Federation.updateConversation domain uc
  FederationMlsSendWelcome domain req ->
    liftT $ mapErrors $ Federation.mlsSendWelcome domain req
  FederationSendMLSMessage domain msr ->
    liftT $ mapErrors $ Federation.sendMLSMessage domain msr
  FederationSendMLSCommitBundle domain msr ->
    liftT $ mapErrors $ Federation.sendMLSCommitBundle domain msr
  FederationQueryGroupInfo domain req ->
    liftT $ mapErrors $ Federation.queryGroupInfo domain req
  FederationUpdateTypingIndicator domain req ->
    liftT $ mapErrors $ Federation.updateTypingIndicator domain req
  FederationOnTypingIndicatorUpdated domain td ->
    liftT $ mapErrors $ Federation.onTypingIndicatorUpdated domain td
  FederationGetSubConversationForRemoteUser domain req ->
    liftT $ mapErrors $ Federation.getSubConversationForRemoteUser domain req
  FederationDeleteSubConversationForRemoteUser domain req ->
    liftT $ mapErrors $ Federation.deleteSubConversationForRemoteUser domain req
  FederationLeaveSubConversation domain lscr ->
    liftT $ mapErrors $ Federation.leaveSubConversation domain lscr
  FederationGetOne2OneConversationV1 domain req ->
    liftT $ mapErrors $ Federation.getOne2OneConversationV1 domain req
  FederationGetOne2OneConversation domain req ->
    liftT $ mapErrors $ Federation.getOne2OneConversation domain req
  FederationOnClientRemoved domain req ->
    liftT $ mapErrors $ Federation.onClientRemoved domain req
  FederationOnMessageSent domain rm ->
    liftT $ mapErrors $ Federation.onMessageSent domain rm
  FederationOnMLSMessageSent domain rmm ->
    liftT $ mapErrors $ Federation.onMLSMessageSent domain rmm
  FederationOnConversationUpdatedV0 domain cu ->
    liftT $ mapErrors $ Federation.onConversationUpdatedV0 domain cu
  FederationOnConversationUpdated domain cu ->
    liftT $ mapErrors $ Federation.onConversationUpdated domain cu
  FederationOnUserDeleted domain udcn ->
    liftT $ mapErrors $ Federation.onUserDeleted domain udcn
  PostOtrMessageUnqualified lusr con cnv ignore report msg ->
    liftT $ mapErrors $ Update.postOtrMessageUnqualified lusr con cnv ignore report msg
  PostOtrBroadcastUnqualified lusr con ignore report msg ->
    liftT $ mapErrors $ Update.postOtrBroadcastUnqualified lusr con ignore report msg
  PostProteusMessage lusr con cnv msg ->
    liftT $ mapErrors $ Update.postProteusMessage lusr con cnv msg
  PostProteusBroadcast lusr con msg ->
    liftT $ mapErrors $ Update.postProteusBroadcast lusr con msg
  DeleteLocalConversation lusr con lcnv ->
    liftT $ mapErrors $ Update.deleteLocalConversation lusr con lcnv
  GetMLSPublicKeys fmt ->
    liftT $ mapErrors $ MLS.getMLSPublicKeys fmt
  ResetMLSConversation lusr reset ->
    liftT $ mapErrors $ MLSReset.resetMLSConversation lusr reset
  GetSubConversation lusr cnv sub ->
    liftT $ mapErrors $ MLSSubConversation.getSubConversation lusr cnv sub
  GetBotConversation bid cnv ->
    liftT $ mapErrors $ Query.getBotConversation bid cnv
  GetUnqualifiedOwnConversation lusr cnv ->
    liftT $ mapErrors $ Query.getUnqualifiedOwnConversation lusr cnv
  GetOwnConversation lusr qcnv ->
    liftT $ mapErrors $ Query.getOwnConversation lusr qcnv
  GetConversation lusr qcnv ->
    liftT $ mapErrors $ Query.getConversation lusr qcnv
  InternalGetConversation cnv ->
    liftT $ mapErrors $ ConvStore.getConversation cnv
  GetConversationRoles lusr cnv ->
    liftT $ mapErrors $ Query.getConversationRoles lusr cnv
  GetGroupInfo lusr qcnv ->
    liftT $ mapErrors $ MLSGroupInfo.getGroupInfo lusr qcnv
  ConversationIdsPageFromUnqualified lusr mstart msize ->
    liftT $ mapErrors $ Query.conversationIdsPageFromUnqualified lusr mstart msize
  ConversationIdsPageFromV2 listGlobalSelf lself req ->
    liftT $ mapErrors $ Query.conversationIdsPageFromV2 listGlobalSelf lself req
  ConversationIdsPageFrom lusr req ->
    liftT $ mapErrors $ Query.conversationIdsPageFrom lusr req
  ListConversations luser req ->
    liftT $ mapErrors $ Query.listConversations luser req
  GetConversationByReusableCode lusr key value ->
    liftT $ mapErrors $ Query.getConversationByReusableCode lusr key value
  GetMLSSelfConversationWithError lusr ->
    liftT $ mapErrors $ Query.getMLSSelfConversationWithError lusr
  GetMLSOne2OneConversationV5 lself qother ->
    liftT $ mapErrors $ Query.getMLSOne2OneConversationV5 lself qother
  GetMLSOne2OneConversationV6 lself qother ->
    liftT $ mapErrors $ Query.getMLSOne2OneConversationV6 lself qother
  GetMLSOne2OneConversation lself qother fmt ->
    liftT $ mapErrors $ Query.getMLSOne2OneConversation lself qother fmt
  GetLocalSelf lusr cnv ->
    liftT $ mapErrors $ Query.getLocalSelf lusr cnv
  GetSelfMember lusr qcnv ->
    liftT $ mapErrors $ Query.getSelfMember lusr qcnv
  GetConversationGuestLinksStatus uid cid ->
    liftT $ mapErrors $ Query.getConversationGuestLinksStatus uid cid
  GetCode mcode lusr cnv ->
    liftT $ mapErrors $ Update.getCode mcode lusr cnv
  AddMembersUnqualified lusr con cnv invite ->
    liftT $ mapErrors $ Update.addMembersUnqualified lusr con cnv invite
  AddMembersUnqualifiedV2 lusr con cnv invite ->
    liftT $ mapErrors $ Update.addMembersUnqualifiedV2 lusr con cnv invite
  AddMembers lusr zcon qcnv invite ->
    liftT $ mapErrors $ Update.addMembers lusr zcon qcnv invite
  ReplaceMembers lusr zcon qcnv invite ->
    liftT $ mapErrors $ Update.replaceMembers lusr zcon qcnv invite
  JoinConversationById lusr con cnv ->
    liftT $ mapErrors $ Update.joinConversationById lusr con cnv
  JoinConversationByReusableCode lusr con req ->
    liftT $ mapErrors $ Update.joinConversationByReusableCode lusr con req
  CheckReusableCode addr code ->
    liftT $ mapErrors $ Update.checkReusableCode addr code
  AddCodeUnqualified mReq usr mbZHost mZcon cnv ->
    liftT $ mapErrors $ Update.addCodeUnqualified mReq usr mbZHost mZcon cnv
  AddCodeUnqualifiedWithReqBody lusr mname mconn cnv req ->
    liftT $ mapErrors $ Update.addCodeUnqualifiedWithReqBody lusr mname mconn cnv req
  RmCodeUnqualified lusr con cnv ->
    liftT $ mapErrors $ Update.rmCodeUnqualified lusr con cnv
  MemberTypingUnqualified lusr con cnv status ->
    liftT $ mapErrors $ Update.memberTypingUnqualified lusr con cnv status
  MemberTyping lusr con qcnv status ->
    liftT $ mapErrors $ Update.memberTyping lusr con qcnv status
  RemoveMemberUnqualified lusr con cnv uid ->
    liftT $ mapErrors $ Update.removeMemberUnqualified lusr con cnv uid
  RemoveMemberQualified lusr con qcnv quid ->
    liftT $ mapErrors $ Update.removeMemberQualified lusr con qcnv quid
  UpdateOtherMemberUnqualified lusr con cnv uid update ->
    liftT $ mapErrors $ Update.updateOtherMemberUnqualified lusr con cnv uid update
  UpdateOtherMember lusr con qcnv quid update ->
    liftT $ mapErrors $ Update.updateOtherMember lusr con qcnv quid update
  UpdateUnqualifiedConversationName lusr con cnv rename ->
    liftT $ mapErrors $ Update.updateUnqualifiedConversationName lusr con cnv rename
  UpdateConversationName lusr zcon qcnv rename ->
    liftT $ mapErrors $ Update.updateConversationName lusr zcon qcnv rename
  UpdateConversationMessageTimerUnqualified lusr con cnv update ->
    liftT $ mapErrors $ Update.updateConversationMessageTimerUnqualified lusr con cnv update
  UpdateConversationMessageTimer lusr zcon qcnv update ->
    liftT $ mapErrors $ Update.updateConversationMessageTimer lusr zcon qcnv update
  UpdateConversationReceiptModeUnqualified lusr con cnv update ->
    liftT $ mapErrors $ Update.updateConversationReceiptModeUnqualified lusr con cnv update
  UpdateConversationReceiptMode lusr zcon qcnv update ->
    liftT $ mapErrors $ Update.updateConversationReceiptMode lusr zcon qcnv update
  UpdateConversationAccessUnqualified lusr con cnv update ->
    liftT $ mapErrors $ Update.updateConversationAccessUnqualified lusr con cnv update
  UpdateConversationAccess lusr zcon qcnv update ->
    liftT $ mapErrors $ Update.updateConversationAccess lusr zcon qcnv update
  UpdateConversationHistory lusr zcon qcnv update ->
    liftT $ mapErrors $ Update.updateConversationHistory lusr zcon qcnv update
  UpdateUnqualifiedSelfMember lusr con cnv update ->
    liftT $ mapErrors $ Update.updateUnqualifiedSelfMember lusr con cnv update
  UpdateSelfMember lusr zcon qcnv update ->
    liftT $ mapErrors $ Update.updateSelfMember lusr zcon qcnv update
  UpdateConversationProtocolWithLocalUser lusr conn qcnv update ->
    liftT $ mapErrors $ Update.updateConversationProtocolWithLocalUser lusr conn qcnv update
  UpdateChannelAddPermission lusr conn qcnv update ->
    liftT $ mapErrors $ Update.updateChannelAddPermission lusr conn qcnv update
  PostBotMessageUnqualified bid cnv ignore report msg ->
    liftT $ mapErrors $ Update.postBotMessageUnqualified bid cnv ignore report msg
  DeleteSubConversation lusr qcnv sub reset ->
    liftT $ mapErrors $ MLSSubConversation.deleteSubConversation lusr qcnv sub reset
  GetSubConversationGroupInfo lusr qcnv sub ->
    liftT $ mapErrors $ MLSSubConversation.getSubConversationGroupInfo lusr qcnv sub
  LeaveSubConversation lusr cli qcnv sub ->
    liftT $ mapErrors $ MLSSubConversation.leaveSubConversation lusr cli qcnv sub
  SendConversationActionNotifications tag quid notifyOrigDomain con lconv targets action extraData ->
    liftT $ mapErrors $ ActionNotify.sendConversationActionNotifications tag quid notifyOrigDomain con lconv targets action extraData
  GetPaginatedConversations lusr mids mstart msize ->
    liftT $ mapErrors $ Query.getConversations lusr mids mstart msize
  SearchChannels lusr tid searchString sortOrder pageSize lastName lastId discoverable ->
    liftT $ mapErrors $ Query.searchChannels lusr tid searchString sortOrder pageSize lastName lastId discoverable
  InternalGetMember qcnv usr ->
    liftT $ mapErrors $ Query.internalGetMember qcnv usr
  GetConversationMeta cnv ->
    liftT $ mapErrors $ Query.getConversationMeta cnv
  GetMLSOne2OneConversationInternal lself qother ->
    liftT $ mapErrors $ Query.getMLSOne2OneConversationInternal lself qother
  IsMLSOne2OneEstablished lself qother ->
    liftT $ mapErrors $ Query.isMLSOne2OneEstablished lself qother
  GetLocalConversationInternal cid ->
    liftT $ mapErrors $ Query.getLocalConversationInternal cid
  RemoveClient lc qusr c ->
    liftT $ mapErrors $ MLSRemoval.removeClient lc qusr c
  AddBot lusr zcon b ->
    liftT $ mapErrors $ Update.addBot lusr zcon b
  RmBot lusr zcon b ->
    liftT $ mapErrors $ Update.rmBot lusr zcon b
  UpdateCellsState cnv state ->
    liftT $ mapErrors $ Update.updateCellsState cnv state
  RemoveUser lc includeMain qusr ->
    liftT $ mapErrors $ MLSRemoval.removeUser lc includeMain qusr
  InternalUpsertOne2OneConversation req ->
    liftT $ mapErrors $ One2One.internalUpsertOne2OneConversation req
  AcceptConv lusr conn cnv ->
    liftT $ mapErrors $ Update.acceptConv lusr conn cnv
  BlockConv lusr qcnv ->
    liftT $ mapErrors $ Update.blockConv lusr qcnv
  UnblockConv lusr conn qcnv ->
    liftT $ mapErrors $ Update.unblockConv lusr conn qcnv

data ConversationSubsystemError
  = ConversationSubsystemErrorConvAccessDenied
  | ConversationSubsystemErrorNotATeamMember
  | ConversationSubsystemErrorperationDenied
  | ConversationSubsystemErrorNotConnected
  | ConversationSubsystemErrorMLSNotEnabled
  | ConversationSubsystemErrorMLSNonEmptyMemberList
  | ConversationSubsystemErrorMissingLegalholdConsent
  | ConversationSubsystemErrorNonBindingTeam
  | ConversationSubsystemErrorNoBindingTeamMembers
  | ConversationSubsystemErrorTeamNotFound
  | ConversationSubsystemErrorInvalidOperation
  | ConversationSubsystemErrorConvNotFound
  | ConversationSubsystemErrorChannelsNotEnabled
  | ConversationSubsystemErrorNotAnMlsConversation
  | ConversationSubsystemErrorMLSLegalholdIncompatible
  | ConversationSubsystemErrorMLSIdentityMismatch
  | ConversationSubsystemErrorMLSUnsupportedMessage
  | ConversationSubsystemErrorMLSStaleMessage
  | ConversationSubsystemErrorMLSProposalNotFound
  | ConversationSubsystemErrorMLSCommitMissingReferences
  | ConversationSubsystemErrorMLSSelfRemovalNotAllowed
  | ConversationSubsystemErrorMLSClientSenderUserMismatch
  | ConversationSubsystemErrorMLSSubConvClientNotInParent
  | ConversationSubsystemErrorMLSInvalidLeafNodeSignature
  | ConversationSubsystemErrorMLSClientMismatch
  | ConversationSubsystemErrorMLSInvalidLeafNodeIndex
  | ConversationSubsystemErrorMLSUnsupportedProposal
  | ConversationSubsystemErrorGroupIdVersionNotSupported
  | ConversationSubsystemErrorConvMemberNotFound
  | ConversationSubsystemErrorHistoryNotSupported
  | ConversationSubsystemErrorLSGroupConversationMismatch
  | ConversationSubsystemErrorActionDeniedLeaveConversation
  | ConversationSubsystemErrorActionDeniedRemoveConversationMember
  | ConversationSubsystemErrorActionDeniedDeleteConversation
  | ConversationSubsystemErrorBroadcastLimitExceeded
  | ConversationSubsystemErrorMLSFederatedResetNotSupported
  | ConversationSubsystemErrorMLSSubConvUnsupportedConvType
  | ConversationSubsystemErrorTeamMemberNotFound
  | ConversationSubsystemErrorAccessDenied
  | ConversationSubsystemErrorMLSMissingGroupInfo
  | ConversationSubsystemErrorCodeNotFound
  | ConversationSubsystemErrorInvalidConversationPassword
  | ConversationSubsystemErrorGuestLinksDisabled
  | ConversationSubsystemErrorMLSFederatedOne2OneNotSupported
  | ConversationSubsystemErrorTooManyMembers
  | ConversationSubsystemErrorCreateConversationCodeConflict
  | ConversationSubsystemErrorInvalidTarget
  | ConversationSubsystemErrorMLSReadReceiptsNotAllowed
  | ConversationSubsystemErrorInvalidTargetAccess
  | ConversationSubsystemErrorConvInvalidProtocolTransition
  | ConversationSubsystemErrorMLSMigrationCriteriaNotSatisfied
  | ConversationSubsystemErrorActionDeniedAddConversationMember
  | ConversationSubsystemErrorActionDeniedModifyOtherConversationMember
  | ConversationSubsystemErrorActionDeniedModifyConversationName
  | ConversationSubsystemErrorActionDeniedModifyConversationMessageTimer
  | ConversationSubsystemErrorActionDeniedModifyConversationReceiptMode
  | ConversationSubsystemErrorActionDeniedModifyConversationAccess
  | ConversationSubsystemErrorActionDeniedModifyAddPermission
  | ConversationSubsystemErrorFederationError FederationError
  | ConversationSubsystemErrorUnreachableBackends UnreachableBackends
  | ConversationSubsystemErrorInternalError InternalError
  | ConversationSubsystemErrorInvalidInput InvalidInput
  | ConversationSubsystemErrorMLSProtocolError MLSProtocolError
  | ConversationSubsystemErrorGroupInfoDiagnostics GroupInfoDiagnostics
  | ConversationSubsystemErrorMLSOutOfSyncError MLSOutOfSyncError
  | ConversationSubsystemErrorNonFederatingBackends NonFederatingBackends
  | ConversationSubsystemErrorUnreachableBackendsLegacy UnreachableBackendsLegacy

instance APIError ConversationSubsystemError where
  toResponse =
    \case
      ConversationSubsystemErrorConvAccessDenied -> toResponse $ Tagged @'ConvAccessDenied ()
      ConversationSubsystemErrorNotATeamMember -> toResponse $ Tagged @'NotATeamMember ()
      ConversationSubsystemErrorperationDenied -> toResponse $ Tagged @OperationDenied ()
      ConversationSubsystemErrorNotConnected -> toResponse $ Tagged @'NotConnected ()
      ConversationSubsystemErrorMLSNotEnabled -> toResponse $ Tagged @'MLSNotEnabled ()
      ConversationSubsystemErrorMLSNonEmptyMemberList -> toResponse $ Tagged @'MLSNonEmptyMemberList ()
      ConversationSubsystemErrorMissingLegalholdConsent -> toResponse $ Tagged @'MissingLegalholdConsent ()
      ConversationSubsystemErrorNonBindingTeam -> toResponse $ Tagged @'NonBindingTeam ()
      ConversationSubsystemErrorNoBindingTeamMembers -> toResponse $ Tagged @'NoBindingTeamMembers ()
      ConversationSubsystemErrorTeamNotFound -> toResponse $ Tagged @'TeamNotFound ()
      ConversationSubsystemErrorInvalidOperation -> toResponse $ Tagged @'InvalidOperation ()
      ConversationSubsystemErrorConvNotFound -> toResponse $ Tagged @'ConvNotFound ()
      ConversationSubsystemErrorChannelsNotEnabled -> toResponse $ Tagged @'ChannelsNotEnabled ()
      ConversationSubsystemErrorNotAnMlsConversation -> toResponse $ Tagged @'NotAnMlsConversation ()
      ConversationSubsystemErrorMLSLegalholdIncompatible -> toResponse $ Tagged @'MLSLegalholdIncompatible ()
      ConversationSubsystemErrorMLSIdentityMismatch -> toResponse $ Tagged @'MLSIdentityMismatch ()
      ConversationSubsystemErrorMLSUnsupportedMessage -> toResponse $ Tagged @'MLSUnsupportedMessage ()
      ConversationSubsystemErrorMLSStaleMessage -> toResponse $ Tagged @'MLSStaleMessage ()
      ConversationSubsystemErrorMLSProposalNotFound -> toResponse $ Tagged @'MLSProposalNotFound ()
      ConversationSubsystemErrorMLSCommitMissingReferences -> toResponse $ Tagged @'MLSCommitMissingReferences ()
      ConversationSubsystemErrorMLSSelfRemovalNotAllowed -> toResponse $ Tagged @'MLSSelfRemovalNotAllowed ()
      ConversationSubsystemErrorMLSClientSenderUserMismatch -> toResponse $ Tagged @'MLSClientSenderUserMismatch ()
      ConversationSubsystemErrorMLSSubConvClientNotInParent -> toResponse $ Tagged @'MLSSubConvClientNotInParent ()
      ConversationSubsystemErrorMLSInvalidLeafNodeSignature -> toResponse $ Tagged @'MLSInvalidLeafNodeSignature ()
      ConversationSubsystemErrorMLSClientMismatch -> toResponse $ Tagged @'MLSClientMismatch ()
      ConversationSubsystemErrorMLSInvalidLeafNodeIndex -> toResponse $ Tagged @'MLSInvalidLeafNodeIndex ()
      ConversationSubsystemErrorMLSUnsupportedProposal -> toResponse $ Tagged @'MLSUnsupportedProposal ()
      ConversationSubsystemErrorGroupIdVersionNotSupported -> toResponse $ Tagged @'GroupIdVersionNotSupported ()
      ConversationSubsystemErrorConvMemberNotFound -> toResponse $ Tagged @'ConvMemberNotFound ()
      ConversationSubsystemErrorHistoryNotSupported -> toResponse $ Tagged @'HistoryNotSupported ()
      ConversationSubsystemErrorLSGroupConversationMismatch -> toResponse $ Tagged @MLSGroupConversationMismatch ()
      ConversationSubsystemErrorActionDeniedLeaveConversation -> toResponse $ Tagged @('ActionDenied ConvRole.LeaveConversation) ()
      ConversationSubsystemErrorActionDeniedRemoveConversationMember -> toResponse $ Tagged @('ActionDenied ConvRole.RemoveConversationMember) ()
      ConversationSubsystemErrorActionDeniedDeleteConversation -> toResponse $ Tagged @('ActionDenied ConvRole.DeleteConversation) ()
      ConversationSubsystemErrorBroadcastLimitExceeded -> toResponse $ Tagged @'BroadcastLimitExceeded ()
      ConversationSubsystemErrorMLSFederatedResetNotSupported -> toResponse $ Tagged @'MLSFederatedResetNotSupported ()
      ConversationSubsystemErrorMLSSubConvUnsupportedConvType -> toResponse $ Tagged @'MLSSubConvUnsupportedConvType ()
      ConversationSubsystemErrorTeamMemberNotFound -> toResponse $ Tagged @'TeamMemberNotFound ()
      ConversationSubsystemErrorAccessDenied -> toResponse $ Tagged @'AccessDenied ()
      ConversationSubsystemErrorMLSMissingGroupInfo -> toResponse $ Tagged @'MLSMissingGroupInfo ()
      ConversationSubsystemErrorCodeNotFound -> toResponse $ Tagged @'CodeNotFound ()
      ConversationSubsystemErrorInvalidConversationPassword -> toResponse $ Tagged @'InvalidConversationPassword ()
      ConversationSubsystemErrorGuestLinksDisabled -> toResponse $ Tagged @'GuestLinksDisabled ()
      ConversationSubsystemErrorMLSFederatedOne2OneNotSupported -> toResponse $ Tagged @'MLSFederatedOne2OneNotSupported ()
      ConversationSubsystemErrorTooManyMembers -> toResponse $ Tagged @'TooManyMembers ()
      ConversationSubsystemErrorCreateConversationCodeConflict -> toResponse $ Tagged @'CreateConversationCodeConflict ()
      ConversationSubsystemErrorInvalidTarget -> toResponse $ Tagged @'InvalidTarget ()
      ConversationSubsystemErrorMLSReadReceiptsNotAllowed -> toResponse $ Tagged @'MLSReadReceiptsNotAllowed ()
      ConversationSubsystemErrorInvalidTargetAccess -> toResponse $ Tagged @'InvalidTargetAccess ()
      ConversationSubsystemErrorConvInvalidProtocolTransition -> toResponse $ Tagged @'ConvInvalidProtocolTransition ()
      ConversationSubsystemErrorMLSMigrationCriteriaNotSatisfied -> toResponse $ Tagged @'MLSMigrationCriteriaNotSatisfied ()
      ConversationSubsystemErrorActionDeniedAddConversationMember -> toResponse $ Tagged @('ActionDenied ConvRole.AddConversationMember) ()
      ConversationSubsystemErrorActionDeniedModifyOtherConversationMember -> toResponse $ Tagged @('ActionDenied ConvRole.ModifyOtherConversationMember) ()
      ConversationSubsystemErrorActionDeniedModifyConversationName -> toResponse $ Tagged @('ActionDenied ConvRole.ModifyConversationName) ()
      ConversationSubsystemErrorActionDeniedModifyConversationMessageTimer -> toResponse $ Tagged @('ActionDenied ConvRole.ModifyConversationMessageTimer) ()
      ConversationSubsystemErrorActionDeniedModifyConversationReceiptMode -> toResponse $ Tagged @('ActionDenied ConvRole.ModifyConversationReceiptMode) ()
      ConversationSubsystemErrorActionDeniedModifyConversationAccess -> toResponse $ Tagged @('ActionDenied ConvRole.ModifyConversationAccess) ()
      ConversationSubsystemErrorActionDeniedModifyAddPermission -> toResponse $ Tagged @('ActionDenied ConvRole.ModifyAddPermission) ()
      ConversationSubsystemErrorFederationError x -> toResponse x
      ConversationSubsystemErrorUnreachableBackends x -> toResponse x
      ConversationSubsystemErrorInternalError x -> toResponse x
      ConversationSubsystemErrorInvalidInput x -> toResponse x
      ConversationSubsystemErrorMLSProtocolError x -> toResponse $ (dynError @(MapError 'MLSProtocolErrorTag)) {eMessage = unTagged x}
      ConversationSubsystemErrorGroupInfoDiagnostics x -> toResponse x
      ConversationSubsystemErrorMLSOutOfSyncError x -> toResponse x
      ConversationSubsystemErrorNonFederatingBackends x -> toResponse x
      ConversationSubsystemErrorUnreachableBackendsLegacy x -> toResponse x

type ConversationSubsystemErrorEffects =
  '[ ErrorS 'ConvAccessDenied,
     ErrorS 'NotATeamMember,
     ErrorS OperationDenied,
     ErrorS 'NotConnected,
     ErrorS 'MLSNotEnabled,
     ErrorS 'MLSNonEmptyMemberList,
     ErrorS 'MissingLegalholdConsent,
     ErrorS 'NonBindingTeam,
     ErrorS 'NoBindingTeamMembers,
     ErrorS 'TeamNotFound,
     ErrorS 'InvalidOperation,
     ErrorS 'ConvNotFound,
     ErrorS 'ChannelsNotEnabled,
     ErrorS 'NotAnMlsConversation,
     ErrorS 'MLSLegalholdIncompatible,
     ErrorS 'MLSIdentityMismatch,
     ErrorS 'MLSUnsupportedMessage,
     ErrorS 'MLSStaleMessage,
     ErrorS 'MLSProposalNotFound,
     ErrorS 'MLSCommitMissingReferences,
     ErrorS 'MLSSelfRemovalNotAllowed,
     ErrorS 'MLSClientSenderUserMismatch,
     ErrorS 'MLSSubConvClientNotInParent,
     ErrorS 'MLSInvalidLeafNodeSignature,
     ErrorS 'MLSClientMismatch,
     ErrorS 'MLSInvalidLeafNodeIndex,
     ErrorS 'MLSUnsupportedProposal,
     ErrorS 'GroupIdVersionNotSupported,
     ErrorS 'ConvMemberNotFound,
     ErrorS 'HistoryNotSupported,
     ErrorS MLSGroupConversationMismatch,
     ErrorS ('ActionDenied ConvRole.LeaveConversation),
     ErrorS ('ActionDenied ConvRole.RemoveConversationMember),
     ErrorS ('ActionDenied ConvRole.DeleteConversation),
     ErrorS 'BroadcastLimitExceeded,
     ErrorS 'MLSFederatedResetNotSupported,
     ErrorS 'MLSSubConvUnsupportedConvType,
     ErrorS 'TeamMemberNotFound,
     ErrorS 'AccessDenied,
     ErrorS 'MLSMissingGroupInfo,
     ErrorS 'CodeNotFound,
     ErrorS 'InvalidConversationPassword,
     ErrorS 'GuestLinksDisabled,
     ErrorS 'MLSFederatedOne2OneNotSupported,
     ErrorS 'TooManyMembers,
     ErrorS 'CreateConversationCodeConflict,
     ErrorS 'InvalidTarget,
     ErrorS 'MLSReadReceiptsNotAllowed,
     ErrorS 'InvalidTargetAccess,
     ErrorS 'ConvInvalidProtocolTransition,
     ErrorS 'MLSMigrationCriteriaNotSatisfied,
     ErrorS ('ActionDenied ConvRole.AddConversationMember),
     ErrorS ('ActionDenied ConvRole.ModifyOtherConversationMember),
     ErrorS ('ActionDenied ConvRole.ModifyConversationName),
     ErrorS ('ActionDenied ConvRole.ModifyConversationMessageTimer),
     ErrorS ('ActionDenied ConvRole.ModifyConversationReceiptMode),
     ErrorS ('ActionDenied ConvRole.ModifyConversationAccess),
     ErrorS ('ActionDenied ConvRole.ModifyAddPermission),
     Error FederationError,
     Error UnreachableBackends,
     Error InternalError,
     Error InvalidInput,
     Error AuthenticationError,
     Error MLSProtocolError,
     Error GroupInfoDiagnostics,
     Error MLSOutOfSyncError,
     Error MLSProposalFailure,
     Error NonFederatingBackends,
     Error UnreachableBackendsLegacy
   ]

mapErrors ::
  ( Member (Error ConversationSubsystemError) r,
    Member (Error JSONResponse) r,
    Member (Error DynError) r
  ) =>
  InterpretersFor ConversationSubsystemErrorEffects r
mapErrors =
  mapError (ConversationSubsystemErrorUnreachableBackendsLegacy)
    . mapError (ConversationSubsystemErrorNonFederatingBackends)
    . interpretServerEffect
    . mapError (ConversationSubsystemErrorMLSOutOfSyncError)
    . mapError (ConversationSubsystemErrorGroupInfoDiagnostics)
    . mapError (ConversationSubsystemErrorMLSProtocolError)
    . interpretServerEffect
    . mapError (ConversationSubsystemErrorInvalidInput)
    . mapError (ConversationSubsystemErrorInternalError)
    . mapError (ConversationSubsystemErrorUnreachableBackends)
    . mapError (ConversationSubsystemErrorFederationError)
    . mapError (const ConversationSubsystemErrorActionDeniedModifyAddPermission)
    . mapError (const ConversationSubsystemErrorActionDeniedModifyConversationAccess)
    . mapError (const ConversationSubsystemErrorActionDeniedModifyConversationReceiptMode)
    . mapError (const ConversationSubsystemErrorActionDeniedModifyConversationMessageTimer)
    . mapError (const ConversationSubsystemErrorActionDeniedModifyConversationName)
    . mapError (const ConversationSubsystemErrorActionDeniedModifyOtherConversationMember)
    . mapError (const ConversationSubsystemErrorActionDeniedAddConversationMember)
    . mapError (const ConversationSubsystemErrorMLSMigrationCriteriaNotSatisfied)
    . mapError (const ConversationSubsystemErrorConvInvalidProtocolTransition)
    . mapError (const ConversationSubsystemErrorInvalidTargetAccess)
    . mapError (const ConversationSubsystemErrorMLSReadReceiptsNotAllowed)
    . mapError (const ConversationSubsystemErrorInvalidTarget)
    . mapError (const ConversationSubsystemErrorCreateConversationCodeConflict)
    . mapError (const ConversationSubsystemErrorTooManyMembers)
    . mapError (const ConversationSubsystemErrorMLSFederatedOne2OneNotSupported)
    . mapError (const ConversationSubsystemErrorGuestLinksDisabled)
    . mapError (const ConversationSubsystemErrorInvalidConversationPassword)
    . mapError (const ConversationSubsystemErrorCodeNotFound)
    . mapError (const ConversationSubsystemErrorMLSMissingGroupInfo)
    . mapError (const ConversationSubsystemErrorAccessDenied)
    . mapError (const ConversationSubsystemErrorTeamMemberNotFound)
    . mapError (const ConversationSubsystemErrorMLSSubConvUnsupportedConvType)
    . mapError (const ConversationSubsystemErrorMLSFederatedResetNotSupported)
    . mapError (const ConversationSubsystemErrorBroadcastLimitExceeded)
    . mapError (const ConversationSubsystemErrorActionDeniedDeleteConversation)
    . mapError (const ConversationSubsystemErrorActionDeniedRemoveConversationMember)
    . mapError (const ConversationSubsystemErrorActionDeniedLeaveConversation)
    . mapError (const ConversationSubsystemErrorLSGroupConversationMismatch)
    . mapError (const ConversationSubsystemErrorHistoryNotSupported)
    . mapError (const ConversationSubsystemErrorConvMemberNotFound)
    . mapError (const ConversationSubsystemErrorGroupIdVersionNotSupported)
    . mapError (const ConversationSubsystemErrorMLSUnsupportedProposal)
    . mapError (const ConversationSubsystemErrorMLSInvalidLeafNodeIndex)
    . mapError (const ConversationSubsystemErrorMLSClientMismatch)
    . mapError (const ConversationSubsystemErrorMLSInvalidLeafNodeSignature)
    . mapError (const ConversationSubsystemErrorMLSSubConvClientNotInParent)
    . mapError (const ConversationSubsystemErrorMLSClientSenderUserMismatch)
    . mapError (const ConversationSubsystemErrorMLSSelfRemovalNotAllowed)
    . mapError (const ConversationSubsystemErrorMLSCommitMissingReferences)
    . mapError (const ConversationSubsystemErrorMLSProposalNotFound)
    . mapError (const ConversationSubsystemErrorMLSStaleMessage)
    . mapError (const ConversationSubsystemErrorMLSUnsupportedMessage)
    . mapError (const ConversationSubsystemErrorMLSIdentityMismatch)
    . mapError (const ConversationSubsystemErrorMLSLegalholdIncompatible)
    . mapError (const ConversationSubsystemErrorNotAnMlsConversation)
    . mapError (const ConversationSubsystemErrorChannelsNotEnabled)
    . mapError (const ConversationSubsystemErrorConvNotFound)
    . mapError (const ConversationSubsystemErrorInvalidOperation)
    . mapError (const ConversationSubsystemErrorTeamNotFound)
    . mapError (const ConversationSubsystemErrorNoBindingTeamMembers)
    . mapError (const ConversationSubsystemErrorNonBindingTeam)
    . mapError (const ConversationSubsystemErrorMissingLegalholdConsent)
    . mapError (const ConversationSubsystemErrorMLSNonEmptyMemberList)
    . mapError (const ConversationSubsystemErrorMLSNotEnabled)
    . mapError (const ConversationSubsystemErrorNotConnected)
    . mapError (const ConversationSubsystemErrorperationDenied)
    . mapError (const ConversationSubsystemErrorNotATeamMember)
    . mapError (const ConversationSubsystemErrorConvAccessDenied)
