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

module Wire.ConversationSubsystem.Errors
  ( catchCSE,
    cseToGalleyError,
    galleyErrorToCSE,
    mapErrors,
    ConversationSubsystemError (..),
  )
where

import Data.Tagged
import Galley.Types.Error (InternalError, InvalidInput (..))
import Imports
import Network.Wai.Utilities.JSONResponse (JSONResponse)
import Polysemy
import Polysemy.Error
import Wire.API.Conversation.Role qualified as ConvRole
import Wire.API.Error
import Wire.API.Error.Galley
import Wire.API.Federation.Error
import Wire.API.Routes.API (ServerEffect (interpretServerEffect))

-- | Catch a 'ConversationSubsystemError' thrown by @act@ and transform the
-- constructors matched by @f@ into the target error; all other constructors
-- are rethrown unchanged.
catchCSE ::
  ( Member (Error ConversationSubsystemError) r,
    Member (Error e') r
  ) =>
  (ConversationSubsystemError -> Maybe e') ->
  Sem r a ->
  Sem r a
catchCSE f act =
  catch @ConversationSubsystemError act (\e -> maybe (throw e) throw (f e))

-- | Map the 'ConversationSubsystemError' constructors that can still arise
-- from conversation actions to their 'GalleyError' equivalents at the
-- federation boundary; all other constructors are rethrown unchanged.
cseToGalleyError :: ConversationSubsystemError -> Maybe GalleyError
cseToGalleyError = \case
  ConversationSubsystemErrorConvAccessDenied -> Just ConvAccessDenied
  ConversationSubsystemErrorNotATeamMember -> Just NotATeamMember
  ConversationSubsystemErrorperationDenied -> Just (MissingPermission Nothing)
  ConversationSubsystemErrorNotConnected -> Just NotConnected
  ConversationSubsystemErrorMLSNotEnabled -> Just MLSNotEnabled
  ConversationSubsystemErrorMLSNonEmptyMemberList -> Just MLSNonEmptyMemberList
  ConversationSubsystemErrorMissingLegalholdConsent -> Just MissingLegalholdConsent
  ConversationSubsystemErrorNonBindingTeam -> Just NonBindingTeam
  ConversationSubsystemErrorNoBindingTeamMembers -> Just NoBindingTeamMembers
  ConversationSubsystemErrorTeamNotFound -> Just TeamNotFound
  ConversationSubsystemErrorInvalidOperation -> Just InvalidOperation
  ConversationSubsystemErrorConvNotFound -> Just ConvNotFound
  ConversationSubsystemErrorChannelsNotEnabled -> Just ChannelsNotEnabled
  ConversationSubsystemErrorNotAnMlsConversation -> Just NotAnMlsConversation
  ConversationSubsystemErrorMLSLegalholdIncompatible -> Just MLSLegalholdIncompatible
  ConversationSubsystemErrorMLSIdentityMismatch -> Just MLSIdentityMismatch
  ConversationSubsystemErrorMLSUnsupportedMessage -> Just MLSUnsupportedMessage
  ConversationSubsystemErrorMLSStaleMessage -> Just MLSStaleMessage
  ConversationSubsystemErrorMLSProposalNotFound -> Just MLSProposalNotFound
  ConversationSubsystemErrorMLSCommitMissingReferences -> Just MLSCommitMissingReferences
  ConversationSubsystemErrorMLSSelfRemovalNotAllowed -> Just MLSSelfRemovalNotAllowed
  ConversationSubsystemErrorMLSClientSenderUserMismatch -> Just MLSClientSenderUserMismatch
  ConversationSubsystemErrorMLSSubConvClientNotInParent -> Just MLSSubConvClientNotInParent
  ConversationSubsystemErrorMLSInvalidLeafNodeSignature -> Just MLSInvalidLeafNodeSignature
  ConversationSubsystemErrorMLSClientMismatch -> Just MLSClientMismatch
  ConversationSubsystemErrorMLSInvalidLeafNodeIndex -> Just MLSInvalidLeafNodeIndex
  ConversationSubsystemErrorMLSUnsupportedProposal -> Just MLSUnsupportedProposal
  ConversationSubsystemErrorGroupIdVersionNotSupported -> Just GroupIdVersionNotSupported
  ConversationSubsystemErrorConvMemberNotFound -> Just ConvMemberNotFound
  ConversationSubsystemErrorHistoryNotSupported -> Just HistoryNotSupported
  ConversationSubsystemErrorLSGroupConversationMismatch -> Just MLSGroupConversationMismatch
  ConversationSubsystemErrorActionDeniedLeaveConversation -> Just (ActionDenied ConvRole.LeaveConversation)
  ConversationSubsystemErrorActionDeniedRemoveConversationMember -> Just (ActionDenied ConvRole.RemoveConversationMember)
  ConversationSubsystemErrorActionDeniedDeleteConversation -> Just (ActionDenied ConvRole.DeleteConversation)
  ConversationSubsystemErrorBroadcastLimitExceeded -> Just BroadcastLimitExceeded
  ConversationSubsystemErrorMLSFederatedResetNotSupported -> Just MLSFederatedResetNotSupported
  ConversationSubsystemErrorMLSSubConvUnsupportedConvType -> Just MLSSubConvUnsupportedConvType
  ConversationSubsystemErrorTeamMemberNotFound -> Just TeamMemberNotFound
  ConversationSubsystemErrorAccessDenied -> Just AccessDenied
  ConversationSubsystemErrorMLSMissingGroupInfo -> Just MLSMissingGroupInfo
  ConversationSubsystemErrorCodeNotFound -> Just CodeNotFound
  ConversationSubsystemErrorInvalidConversationPassword -> Just InvalidConversationPassword
  ConversationSubsystemErrorGuestLinksDisabled -> Just GuestLinksDisabled
  ConversationSubsystemErrorMLSFederatedOne2OneNotSupported -> Just MLSFederatedOne2OneNotSupported
  ConversationSubsystemErrorTooManyMembers -> Just TooManyMembers
  ConversationSubsystemErrorCreateConversationCodeConflict -> Just CreateConversationCodeConflict
  ConversationSubsystemErrorInvalidTarget -> Just InvalidTarget
  ConversationSubsystemErrorMLSReadReceiptsNotAllowed -> Just MLSReadReceiptsNotAllowed
  ConversationSubsystemErrorInvalidTargetAccess -> Just InvalidTargetAccess
  ConversationSubsystemErrorConvInvalidProtocolTransition -> Just ConvInvalidProtocolTransition
  ConversationSubsystemErrorMLSMigrationCriteriaNotSatisfied -> Just MLSMigrationCriteriaNotSatisfied
  ConversationSubsystemErrorActionDeniedAddConversationMember -> Just (ActionDenied ConvRole.AddConversationMember)
  ConversationSubsystemErrorActionDeniedModifyOtherConversationMember -> Just (ActionDenied ConvRole.ModifyOtherConversationMember)
  ConversationSubsystemErrorActionDeniedModifyConversationName -> Just (ActionDenied ConvRole.ModifyConversationName)
  ConversationSubsystemErrorActionDeniedModifyConversationMessageTimer -> Just (ActionDenied ConvRole.ModifyConversationMessageTimer)
  ConversationSubsystemErrorActionDeniedModifyConversationReceiptMode -> Just (ActionDenied ConvRole.ModifyConversationReceiptMode)
  ConversationSubsystemErrorActionDeniedModifyConversationAccess -> Just (ActionDenied ConvRole.ModifyConversationAccess)
  ConversationSubsystemErrorActionDeniedModifyAddPermission -> Just (ActionDenied ConvRole.ModifyAddPermission)
  ConversationSubsystemErrorMLSHistoryClientConflict -> Just MLSHistoryClientConflict
  ConversationSubsystemErrorMLSHistoryClientDuplication -> Just MLSHistoryClientDuplication
  _ -> Nothing

-- | Inverse of 'cseToGalleyError' for the conversation-action error set:
-- rethrows fed Galley errors into the local 'ConversationSubsystemError' row.
galleyErrorToCSE :: GalleyError -> Maybe ConversationSubsystemError
galleyErrorToCSE = \case
  ConvAccessDenied -> Just ConversationSubsystemErrorConvAccessDenied
  NotATeamMember -> Just ConversationSubsystemErrorNotATeamMember
  MissingPermission Nothing -> Just ConversationSubsystemErrorperationDenied
  NotConnected -> Just ConversationSubsystemErrorNotConnected
  MLSNotEnabled -> Just ConversationSubsystemErrorMLSNotEnabled
  MLSNonEmptyMemberList -> Just ConversationSubsystemErrorMLSNonEmptyMemberList
  MissingLegalholdConsent -> Just ConversationSubsystemErrorMissingLegalholdConsent
  NonBindingTeam -> Just ConversationSubsystemErrorNonBindingTeam
  NoBindingTeamMembers -> Just ConversationSubsystemErrorNoBindingTeamMembers
  TeamNotFound -> Just ConversationSubsystemErrorTeamNotFound
  InvalidOperation -> Just ConversationSubsystemErrorInvalidOperation
  ConvNotFound -> Just ConversationSubsystemErrorConvNotFound
  ChannelsNotEnabled -> Just ConversationSubsystemErrorChannelsNotEnabled
  NotAnMlsConversation -> Just ConversationSubsystemErrorNotAnMlsConversation
  MLSLegalholdIncompatible -> Just ConversationSubsystemErrorMLSLegalholdIncompatible
  MLSIdentityMismatch -> Just ConversationSubsystemErrorMLSIdentityMismatch
  MLSUnsupportedMessage -> Just ConversationSubsystemErrorMLSUnsupportedMessage
  MLSStaleMessage -> Just ConversationSubsystemErrorMLSStaleMessage
  MLSProposalNotFound -> Just ConversationSubsystemErrorMLSProposalNotFound
  MLSCommitMissingReferences -> Just ConversationSubsystemErrorMLSCommitMissingReferences
  MLSSelfRemovalNotAllowed -> Just ConversationSubsystemErrorMLSSelfRemovalNotAllowed
  MLSClientSenderUserMismatch -> Just ConversationSubsystemErrorMLSClientSenderUserMismatch
  MLSSubConvClientNotInParent -> Just ConversationSubsystemErrorMLSSubConvClientNotInParent
  MLSInvalidLeafNodeSignature -> Just ConversationSubsystemErrorMLSInvalidLeafNodeSignature
  MLSClientMismatch -> Just ConversationSubsystemErrorMLSClientMismatch
  MLSInvalidLeafNodeIndex -> Just ConversationSubsystemErrorMLSInvalidLeafNodeIndex
  MLSUnsupportedProposal -> Just ConversationSubsystemErrorMLSUnsupportedProposal
  GroupIdVersionNotSupported -> Just ConversationSubsystemErrorGroupIdVersionNotSupported
  ConvMemberNotFound -> Just ConversationSubsystemErrorConvMemberNotFound
  HistoryNotSupported -> Just ConversationSubsystemErrorHistoryNotSupported
  MLSGroupConversationMismatch -> Just ConversationSubsystemErrorLSGroupConversationMismatch
  ActionDenied ConvRole.LeaveConversation -> Just ConversationSubsystemErrorActionDeniedLeaveConversation
  ActionDenied ConvRole.RemoveConversationMember -> Just ConversationSubsystemErrorActionDeniedRemoveConversationMember
  ActionDenied ConvRole.DeleteConversation -> Just ConversationSubsystemErrorActionDeniedDeleteConversation
  BroadcastLimitExceeded -> Just ConversationSubsystemErrorBroadcastLimitExceeded
  MLSFederatedResetNotSupported -> Just ConversationSubsystemErrorMLSFederatedResetNotSupported
  MLSSubConvUnsupportedConvType -> Just ConversationSubsystemErrorMLSSubConvUnsupportedConvType
  TeamMemberNotFound -> Just ConversationSubsystemErrorTeamMemberNotFound
  AccessDenied -> Just ConversationSubsystemErrorAccessDenied
  MLSMissingGroupInfo -> Just ConversationSubsystemErrorMLSMissingGroupInfo
  CodeNotFound -> Just ConversationSubsystemErrorCodeNotFound
  InvalidConversationPassword -> Just ConversationSubsystemErrorInvalidConversationPassword
  GuestLinksDisabled -> Just ConversationSubsystemErrorGuestLinksDisabled
  MLSFederatedOne2OneNotSupported -> Just ConversationSubsystemErrorMLSFederatedOne2OneNotSupported
  TooManyMembers -> Just ConversationSubsystemErrorTooManyMembers
  CreateConversationCodeConflict -> Just ConversationSubsystemErrorCreateConversationCodeConflict
  InvalidTarget -> Just ConversationSubsystemErrorInvalidTarget
  MLSReadReceiptsNotAllowed -> Just ConversationSubsystemErrorMLSReadReceiptsNotAllowed
  InvalidTargetAccess -> Just ConversationSubsystemErrorInvalidTargetAccess
  ConvInvalidProtocolTransition -> Just ConversationSubsystemErrorConvInvalidProtocolTransition
  MLSMigrationCriteriaNotSatisfied -> Just ConversationSubsystemErrorMLSMigrationCriteriaNotSatisfied
  ActionDenied ConvRole.AddConversationMember -> Just ConversationSubsystemErrorActionDeniedAddConversationMember
  ActionDenied ConvRole.ModifyOtherConversationMember -> Just ConversationSubsystemErrorActionDeniedModifyOtherConversationMember
  ActionDenied ConvRole.ModifyConversationName -> Just ConversationSubsystemErrorActionDeniedModifyConversationName
  ActionDenied ConvRole.ModifyConversationMessageTimer -> Just ConversationSubsystemErrorActionDeniedModifyConversationMessageTimer
  ActionDenied ConvRole.ModifyConversationReceiptMode -> Just ConversationSubsystemErrorActionDeniedModifyConversationReceiptMode
  ActionDenied ConvRole.ModifyConversationAccess -> Just ConversationSubsystemErrorActionDeniedModifyConversationAccess
  ActionDenied ConvRole.ModifyAddPermission -> Just ConversationSubsystemErrorActionDeniedModifyAddPermission
  MLSHistoryClientConflict -> Just ConversationSubsystemErrorMLSHistoryClientConflict
  MLSHistoryClientDuplication -> Just ConversationSubsystemErrorMLSHistoryClientDuplication
  _ -> Nothing

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
  | ConversationSubsystemErrorAdminlessConversation AdminlessConversation
  | ConversationSubsystemErrorNonFederatingBackends NonFederatingBackends
  | ConversationSubsystemErrorUnreachableBackendsLegacy UnreachableBackendsLegacy
  | ConversationSubsystemErrorMLSHistoryClientConflict
  | ConversationSubsystemErrorMLSHistoryClientDuplication

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
      ConversationSubsystemErrorAdminlessConversation x -> toResponse x
      ConversationSubsystemErrorNonFederatingBackends x -> toResponse x
      ConversationSubsystemErrorUnreachableBackendsLegacy x -> toResponse x
      ConversationSubsystemErrorMLSHistoryClientConflict -> toResponse $ Tagged @'MLSHistoryClientConflict ()
      ConversationSubsystemErrorMLSHistoryClientDuplication -> toResponse $ Tagged @'MLSHistoryClientDuplication ()

type ConversationSubsystemErrorEffects =
  '[ ErrorS 'ConvAccessDenied,
     ErrorS 'NotATeamMember,
     ErrorS OperationDenied,
     ErrorS 'MissingLegalholdConsent,
     ErrorS 'NonBindingTeam,
     ErrorS 'TeamNotFound,
     ErrorS 'InvalidOperation,
     ErrorS 'ConvNotFound,
     ErrorS 'ConvMemberNotFound,
     ErrorS ('ActionDenied ConvRole.LeaveConversation),
     ErrorS ('ActionDenied ConvRole.RemoveConversationMember),
     ErrorS ('ActionDenied ConvRole.DeleteConversation),
     ErrorS 'MLSSubConvUnsupportedConvType,
     ErrorS 'TeamMemberNotFound,
     ErrorS 'AccessDenied,
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
    . mapError (const ConversationSubsystemErrorAccessDenied)
    . mapError (const ConversationSubsystemErrorTeamMemberNotFound)
    . mapError (const ConversationSubsystemErrorMLSSubConvUnsupportedConvType)
    . mapError (const ConversationSubsystemErrorActionDeniedDeleteConversation)
    . mapError (const ConversationSubsystemErrorActionDeniedRemoveConversationMember)
    . mapError (const ConversationSubsystemErrorActionDeniedLeaveConversation)
    . mapError (const ConversationSubsystemErrorConvMemberNotFound)
    . mapError (const ConversationSubsystemErrorConvNotFound)
    . mapError (const ConversationSubsystemErrorInvalidOperation)
    . mapError (const ConversationSubsystemErrorTeamNotFound)
    . mapError (const ConversationSubsystemErrorNonBindingTeam)
    . mapError (const ConversationSubsystemErrorMissingLegalholdConsent)
    . mapError (const ConversationSubsystemErrorperationDenied)
    . mapError (const ConversationSubsystemErrorNotATeamMember)
    . mapError (const ConversationSubsystemErrorConvAccessDenied)
