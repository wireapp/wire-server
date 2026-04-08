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
    ConversationSubsystemError (..),
  )
where

import Data.Tagged
import Galley.Types.Error (InternalError, InvalidInput (..))
import Imports
import Network.Wai.Utilities.JSONResponse (JSONResponse)
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Wire.API.Conversation.Config
import Wire.API.Conversation.Role qualified as ConvRole
import Wire.API.Error
import Wire.API.Error.Galley
import Wire.API.Federation.Client (FederatorClient)
import Wire.API.Federation.Error
import Wire.API.Routes.API (ServerEffect (interpretServerEffect))
import Wire.BackendNotificationQueueAccess (BackendNotificationQueueAccess)
import Wire.BrigAPIAccess
import Wire.ConversationStore (ConversationStore)
import Wire.ConversationStore qualified as ConvStore
import Wire.ConversationSubsystem
import Wire.ConversationSubsystem.CreateInternal qualified as CreateInternal
import Wire.ConversationSubsystem.Fetch qualified as Fetch
import Wire.ConversationSubsystem.Internal qualified as Internal
import Wire.ConversationSubsystem.Notify qualified as Notify
import Wire.ExternalAccess (ExternalAccess)
import Wire.FeaturesConfigSubsystem
import Wire.FederationAPIAccess (FederationAPIAccess)
import Wire.LegalHoldStore (LegalHoldStore)
import Wire.NotificationSubsystem as NS
import Wire.Sem.Now (Now)
import Wire.Sem.Random (Random)
import Wire.TeamCollaboratorsSubsystem
import Wire.TeamStore (TeamStore)
import Wire.TeamSubsystem (TeamSubsystem)
import Wire.UserClientIndexStore (UserClientIndexStore)

interpretConversationSubsystem ::
  ( Member (Error ConversationSubsystemError) r,
    Member (Error JSONResponse) r,
    Member (Error DynError) r,
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
    Member (Input ConversationSubsystemConfig) r,
    Member LegalHoldStore r,
    Member TeamStore r,
    Member UserClientIndexStore r
  ) =>
  Sem (ConversationSubsystem : r) a ->
  Sem r a
interpretConversationSubsystem = interpret $ \case
  NotifyConversationAction tag quid notifyOrigDomain con lconv targetsLocal targetsRemote targetsBots action extraData ->
    mapErrors $ Notify.notifyConversationActionImpl tag quid notifyOrigDomain con lconv targetsLocal targetsRemote targetsBots action extraData
  CreateGroupConversation lusr conn newConv ->
    mapErrors $ CreateInternal.createGroupConversationGeneric lusr conn newConv
  CreateOne2OneConversation lusr conn newOne2One ->
    mapErrors $ CreateInternal.createOne2OneConversationLogic lusr conn newOne2One
  CreateProteusSelfConversation lusr ->
    mapErrors $ CreateInternal.createProteusSelfConversationLogic lusr
  CreateConnectConversation lusr conn j ->
    mapErrors $ CreateInternal.createConnectConversationLogic lusr conn j
  GetConversations convIds ->
    mapErrors $ ConvStore.getConversations convIds
  GetConversationIds lusr maxIds pagingState ->
    mapErrors $ Fetch.getConversationIdsImpl lusr maxIds pagingState
  InternalGetClientIds uids ->
    mapErrors $ Internal.internalGetClientIdsImpl uids
  InternalGetLocalMember cid uid ->
    mapErrors $ ConvStore.getLocalMember cid uid

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
