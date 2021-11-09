-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
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

module Galley.API.Error where

import Data.Domain (Domain, domainText)
import Data.Id
import Data.Proxy
import Data.String.Conversions (cs)
import Data.Text.Lazy as LT (pack)
import qualified Data.Text.Lazy as LT
import GHC.TypeLits
import Galley.Types.Teams (hardTruncationLimit)
import Imports
import Network.HTTP.Types.Status
import Network.Wai.Utilities.Error
import Polysemy
import qualified Polysemy.Error as P
import Polysemy.Internal (Append)
import Servant.API.Status (KnownStatus (..))
import Wire.API.Conversation (ConvType (..))
import Wire.API.Conversation.Role (Action)
import Wire.API.ErrorDescription
import Wire.API.Federation.Client
import Wire.API.Federation.Error

----------------------------------------------------------------------------
-- Fine-grained API error types

class APIError e where
  toWai :: e -> Error

data InternalError
  = BadConvState ConvId
  | BadMemberState
  | NoPrekeyForUser
  | CannotCreateManagedConv
  | DeleteQueueFull
  | InternalErrorWithDescription LText

instance APIError InternalError where
  toWai (BadConvState convId) = badConvState convId
  toWai BadMemberState = mkError status500 "bad-state" "Bad internal member state."
  toWai NoPrekeyForUser = internalError
  toWai CannotCreateManagedConv = internalError
  toWai DeleteQueueFull = deleteQueueFull
  toWai (InternalErrorWithDescription t) = internalErrorWithDescription t

data ActionError
  = InvalidAction
  | InvalidTargetAccess
  | InvalidTargetUserOp
  | ActionDenied Action
  | AccessDenied
  | InvalidOp ConvType
  | OperationDenied String
  | NotConnected
  | NoAddToManaged
  | BroadcastLimitExceeded
  | InvalidTeamStatusUpdate
  | InvalidPermissions

instance APIError ActionError where
  toWai InvalidAction = invalidActions
  toWai InvalidTargetAccess = errorDescriptionTypeToWai @InvalidTargetAccess
  toWai (ActionDenied action) = errorDescriptionToWai (actionDenied action)
  toWai AccessDenied = accessDenied
  toWai (InvalidOp RegularConv) = invalidOp "invalid operation"
  toWai (InvalidOp SelfConv) = invalidSelfOp
  toWai (InvalidOp One2OneConv) = invalidOne2OneOp
  toWai (InvalidOp ConnectConv) = invalidConnectOp
  toWai (OperationDenied p) = errorDescriptionToWai $ operationDeniedSpecialized p
  toWai NotConnected = errorDescriptionTypeToWai @NotConnected
  toWai InvalidTargetUserOp = invalidTargetUserOp
  toWai NoAddToManaged = noAddToManaged
  toWai BroadcastLimitExceeded = broadcastLimitExceeded
  toWai InvalidTeamStatusUpdate = invalidTeamStatusUpdate
  toWai InvalidPermissions = invalidPermissions

data CustomBackendError = CustomBackendNotFound Domain

instance APIError CustomBackendError where
  toWai (CustomBackendNotFound d) = customBackendNotFound d

data InvalidInput
  = CustomRolesNotSupported
  | InvalidRange LText
  | InvalidUUID4
  | BulkGetMemberLimitExceeded
  | InvalidPayload LText

instance APIError InvalidInput where
  toWai CustomRolesNotSupported = badRequest "Custom roles not supported"
  toWai (InvalidRange t) = invalidRange t
  toWai InvalidUUID4 = invalidUUID4
  toWai BulkGetMemberLimitExceeded = bulkGetMemberLimitExceeded
  toWai (InvalidPayload t) = invalidPayload t

data AuthenticationError
  = ReAuthFailed

instance APIError AuthenticationError where
  toWai ReAuthFailed = reAuthFailed

data ConversationError
  = ConvAccessDenied
  | ConvNotFound
  | TooManyMembers
  | ConvMemberNotFound
  | NoBindingTeamMembers
  | NoManagedTeamConv

instance APIError ConversationError where
  toWai ConvAccessDenied = errorDescriptionTypeToWai @ConvAccessDenied
  toWai ConvNotFound = errorDescriptionTypeToWai @ConvNotFound
  toWai TooManyMembers = tooManyMembers
  toWai ConvMemberNotFound = errorDescriptionTypeToWai @ConvMemberNotFound
  toWai NoBindingTeamMembers = noBindingTeamMembers
  toWai NoManagedTeamConv = noManagedTeamConv

data TeamError
  = NoBindingTeam
  | NoAddToBinding
  | NotABindingTeamMember
  | NotAOneMemberTeam
  | TeamNotFound
  | TeamMemberNotFound
  | TeamSearchVisibilityNotEnabled
  | UserBindingExists
  | TooManyTeamMembers
  | CannotEnableLegalHoldServiceLargeTeam

instance APIError TeamError where
  toWai NoBindingTeam = noBindingTeam
  toWai NoAddToBinding = noAddToBinding
  toWai NotABindingTeamMember = nonBindingTeam
  toWai NotAOneMemberTeam = notAOneMemberTeam
  toWai TeamNotFound = teamNotFound
  toWai TeamMemberNotFound = teamMemberNotFound
  toWai TeamSearchVisibilityNotEnabled = teamSearchVisibilityNotEnabled
  toWai UserBindingExists = userBindingExists
  toWai TooManyTeamMembers = tooManyTeamMembers
  toWai CannotEnableLegalHoldServiceLargeTeam = cannotEnableLegalHoldServiceLargeTeam

data TeamFeatureError
  = AppLockinactivityTimeoutTooLow
  | LegalHoldFeatureFlagNotEnabled
  | LegalHoldWhitelistedOnly
  | DisableSsoNotImplemented
  | PaymentStatusLocked

instance APIError TeamFeatureError where
  toWai AppLockinactivityTimeoutTooLow = inactivityTimeoutTooLow
  toWai LegalHoldFeatureFlagNotEnabled = legalHoldFeatureFlagNotEnabled
  toWai LegalHoldWhitelistedOnly = legalHoldWhitelistedOnly
  toWai DisableSsoNotImplemented = disableSsoNotImplemented
  toWai PaymentStatusLocked = setTeamFeatureConfigPaymentStatusLocked

data TeamNotificationError
  = InvalidTeamNotificationId

instance APIError TeamNotificationError where
  toWai InvalidTeamNotificationId = invalidTeamNotificationId

instance APIError FederationError where
  toWai = federationErrorToWai

data LegalHoldError
  = MissingLegalholdConsent
  | NoUserLegalHoldConsent
  | LegalHoldNotEnabled
  | LegalHoldDisableUnimplemented
  | LegalHoldServiceInvalidKey
  | LegalHoldServiceBadResponse
  | UserLegalHoldAlreadyEnabled
  | LegalHoldServiceNotRegistered
  | LegalHoldCouldNotBlockConnections
  | UserLegalHoldIllegalOperation
  | TooManyTeamMembersOnTeamWithLegalhold
  | NoLegalHoldDeviceAllocated
  | UserLegalHoldNotPending

instance APIError LegalHoldError where
  toWai MissingLegalholdConsent = errorDescriptionTypeToWai @MissingLegalholdConsent
  toWai NoUserLegalHoldConsent = userLegalHoldNoConsent
  toWai LegalHoldNotEnabled = legalHoldNotEnabled
  toWai LegalHoldDisableUnimplemented = legalHoldDisableUnimplemented
  toWai LegalHoldServiceInvalidKey = legalHoldServiceInvalidKey
  toWai LegalHoldServiceBadResponse = legalHoldServiceBadResponse
  toWai UserLegalHoldAlreadyEnabled = userLegalHoldAlreadyEnabled
  toWai LegalHoldServiceNotRegistered = legalHoldServiceNotRegistered
  toWai LegalHoldCouldNotBlockConnections = legalHoldCouldNotBlockConnections
  toWai UserLegalHoldIllegalOperation = userLegalHoldIllegalOperation
  toWai TooManyTeamMembersOnTeamWithLegalhold = tooManyTeamMembersOnTeamWithLegalhold
  toWai NoLegalHoldDeviceAllocated = noLegalHoldDeviceAllocated
  toWai UserLegalHoldNotPending = userLegalHoldNotPending

data CodeError = CodeNotFound

instance APIError CodeError where
  toWai CodeNotFound = errorDescriptionTypeToWai @CodeNotFound

data ClientError = UnknownClient

instance APIError ClientError where
  toWai UnknownClient = errorDescriptionTypeToWai @UnknownClient

throwED ::
  forall e code label desc r a.
  ( e ~ ErrorDescription code label desc,
    KnownSymbol desc,
    Member (P.Error e) r
  ) =>
  Sem r a
throwED = P.throw @e mkErrorDescription

noteED ::
  forall e code label desc r a.
  ( e ~ ErrorDescription code label desc,
    KnownSymbol desc,
    Member (P.Error e) r
  ) =>
  Maybe a ->
  Sem r a
noteED = P.note (mkErrorDescription :: e)

type AllErrorEffects =
  '[ P.Error ActionError,
     P.Error AuthenticationError,
     P.Error ClientError,
     P.Error CodeError,
     P.Error ConversationError,
     P.Error CustomBackendError,
     P.Error FederationError,
     P.Error InternalError,
     P.Error InvalidInput,
     P.Error LegalHoldError,
     P.Error TeamError,
     P.Error TeamFeatureError,
     P.Error TeamNotificationError,
     P.Error NotATeamMember
   ]

mapAllErrors :: Member (P.Error Error) r => Sem (Append AllErrorEffects r) a -> Sem r a
mapAllErrors =
  P.mapError errorDescriptionToWai
    . P.mapError toWai
    . P.mapError toWai
    . P.mapError toWai
    . P.mapError toWai
    . P.mapError toWai
    . P.mapError toWai
    . P.mapError toWai
    . P.mapError toWai
    . P.mapError toWai
    . P.mapError toWai
    . P.mapError toWai
    . P.mapError toWai
    . P.mapError toWai

----------------------------------------------------------------------------
-- Error description integration

errorDescriptionToWai ::
  forall (code :: Nat) (lbl :: Symbol) (desc :: Symbol).
  (KnownStatus code, KnownSymbol lbl) =>
  ErrorDescription code lbl desc ->
  Error
errorDescriptionToWai (ErrorDescription msg) =
  mkError (statusVal (Proxy @code)) (LT.pack (symbolVal (Proxy @lbl))) (LT.fromStrict msg)

errorDescriptionTypeToWai ::
  forall e (code :: Nat) (lbl :: Symbol) (desc :: Symbol).
  ( KnownStatus code,
    KnownSymbol lbl,
    KnownSymbol desc,
    e ~ ErrorDescription code lbl desc
  ) =>
  Error
errorDescriptionTypeToWai = errorDescriptionToWai (mkErrorDescription :: e)

----------------------------------------------------------------------------
-- Other errors

internalError :: Error
internalError = internalErrorWithDescription "internal error"

internalErrorWithDescription :: LText -> Error
internalErrorWithDescription = mkError status500 "internal-error"

invalidSelfOp :: Error
invalidSelfOp = invalidOp "invalid operation for self conversation"

invalidOne2OneOp :: Error
invalidOne2OneOp = invalidOp "invalid operation for 1:1 conversations"

invalidConnectOp :: Error
invalidConnectOp = invalidOp "invalid operation for connect conversation"

invalidAccessOp :: Error
invalidAccessOp = invalidOp "invalid operation for conversation without 'code' access"

invalidManagedConvOp :: Error
invalidManagedConvOp = invalidOp "invalid operation for managed conversation"

invalidTargetUserOp :: Error
invalidTargetUserOp = invalidOp "invalid target user for the given operation"

invalidOp :: LText -> Error
invalidOp = mkError status403 "invalid-op"

invalidPayload :: LText -> Error
invalidPayload = mkError status400 "invalid-payload"

badRequest :: LText -> Error
badRequest = mkError status400 "bad-request"

unknownRemoteUser :: Error
unknownRemoteUser = mkError status400 "unknown-remote-user" "Remote user(s) not found"

tooManyMembers :: Error
tooManyMembers = mkError status403 "too-many-members" "Maximum number of members per conversation reached"

accessDenied :: Error
accessDenied = mkError status403 "access-denied" "You do not have permission to access this resource"

reAuthFailed :: Error
reAuthFailed = mkError status403 "access-denied" "This operation requires reauthentication"

invalidUUID4 :: Error
invalidUUID4 = mkError status400 "client-error" "Invalid UUID v4 format"

invalidRange :: LText -> Error
invalidRange = mkError status400 "client-error"

noBindingTeam :: Error
noBindingTeam = mkError status403 "no-binding-team" "Operation allowed only on binding teams."

notAOneMemberTeam :: Error
notAOneMemberTeam = mkError status403 "not-one-member-team" "Can only delete teams with a single member."

badConvState :: ConvId -> Error
badConvState cid =
  mkError status500 "bad-state" $
    "Connect conversation with more than 2 members: "
      <> LT.pack (show cid)

bulkGetMemberLimitExceeded :: Error
bulkGetMemberLimitExceeded =
  mkError
    status400
    "too-many-uids"
    ("Can only process " <> cs (show @Int hardTruncationLimit) <> " user ids per request.")

broadcastLimitExceeded :: Error
broadcastLimitExceeded =
  mkError
    status400
    "too-many-users-to-broadcast"
    ("Too many users to fan out the broadcast event to.")

noAddToManaged :: Error
noAddToManaged = mkError status403 "no-add-to-managed" "Adding users/bots directly to managed conversation is not allowed."

teamNotFound :: Error
teamNotFound = mkError status404 "no-team" "team not found"

invalidPermissions :: Error
invalidPermissions = mkError status403 "invalid-permissions" "The specified permissions are invalid."

invalidActions :: Error
invalidActions = mkError status403 "invalid-actions" "The specified actions are invalid."

tooManyTeamMembers :: Error
tooManyTeamMembers = mkError status403 "too-many-team-members" "Maximum number of members per team reached"

tooManyTeamMembersOnTeamWithLegalhold :: Error
tooManyTeamMembersOnTeamWithLegalhold = mkError status403 "too-many-members-for-legalhold" "cannot add more members to team when legalhold service is enabled."

teamMemberNotFound :: Error
teamMemberNotFound = mkError status404 "no-team-member" "team member not found"

noManagedTeamConv :: Error
noManagedTeamConv = mkError status400 "no-managed-team-conv" "Managed team conversations have been deprecated."

userBindingExists :: Error
userBindingExists = mkError status403 "binding-exists" "User already bound to a different team."

noAddToBinding :: Error
noAddToBinding = mkError status403 "binding-team" "Cannot add users to binding teams, invite only."

deleteQueueFull :: Error
deleteQueueFull = mkError status503 "queue-full" "The delete queue is full. No further delete requests can be processed at the moment."

nonBindingTeam :: Error
nonBindingTeam = mkError status404 "non-binding-team" "not member of a binding team"

noBindingTeamMembers :: Error
noBindingTeamMembers = mkError status403 "non-binding-team-members" "Both users must be members of the same binding team."

invalidTeamStatusUpdate :: Error
invalidTeamStatusUpdate = mkError status403 "invalid-team-status-update" "Cannot use this endpoint to update the team to the given status."

cannotEnableLegalHoldServiceLargeTeam :: Error
cannotEnableLegalHoldServiceLargeTeam = mkError status403 "too-large-team-for-legalhold" "cannot enable legalhold on large teams.  (reason: for removing LH from team, we need to iterate over all members, which is only supported for teams with less than 2k members.)"

legalHoldServiceInvalidKey :: Error
legalHoldServiceInvalidKey = mkError status400 "legalhold-invalid-key" "legal hold service pubkey is invalid"

legalHoldServiceUnavailable :: Error
legalHoldServiceUnavailable = mkError status412 "legalhold-unavailable" "legal hold service does not respond or tls handshake could not be completed (did you pin the wrong public key?)"

legalHoldServiceNotRegistered :: Error
legalHoldServiceNotRegistered = mkError status400 "legalhold-not-registered" "legal hold service has not been registered for this team"

legalHoldServiceBadResponse :: Error
legalHoldServiceBadResponse = mkError status400 "legalhold-status-bad" "legal hold service: invalid response"

legalHoldWhitelistedOnly :: Error
legalHoldWhitelistedOnly = mkError status403 "legalhold-whitelisted-only" "legal hold is enabled for teams via server config and cannot be changed here"

legalHoldFeatureFlagNotEnabled :: Error
legalHoldFeatureFlagNotEnabled = mkError status403 "legalhold-not-enabled" "legal hold is not enabled for this wire instance"

legalHoldNotEnabled :: Error
legalHoldNotEnabled = mkError status403 "legalhold-not-enabled" "legal hold is not enabled for this team"

legalHoldDisableUnimplemented :: Error
legalHoldDisableUnimplemented = mkError status403 "legalhold-disable-unimplemented" "legal hold cannot be disabled for whitelisted teams"

userLegalHoldAlreadyEnabled :: Error
userLegalHoldAlreadyEnabled = mkError status409 "legalhold-already-enabled" "legal hold is already enabled for this user"

userLegalHoldNoConsent :: Error
userLegalHoldNoConsent = mkError status409 "legalhold-no-consent" "user has not given consent to using legal hold"

userLegalHoldIllegalOperation :: Error
userLegalHoldIllegalOperation = mkError status500 "legalhold-illegal-op" "internal server error: inconsistent change of user's legalhold state"

userLegalHoldNotPending :: Error
userLegalHoldNotPending = mkError status412 "legalhold-not-pending" "legal hold cannot be approved without being in a pending state"

noLegalHoldDeviceAllocated :: Error
noLegalHoldDeviceAllocated = mkError status404 "legalhold-no-device-allocated" "no legal hold device is registered for this user. POST /teams/:tid/legalhold/:uid/ to start the flow."

legalHoldCouldNotBlockConnections :: Error
legalHoldCouldNotBlockConnections = mkError status500 "legalhold-internal" "legal hold service: could not block connections when resolving policy conflicts."

setTeamFeatureConfigPaymentStatusLocked :: Error
setTeamFeatureConfigPaymentStatusLocked = mkError status409 "payment-status-locked" "feature config cannot be updated when the payment status is locked"

disableSsoNotImplemented :: Error
disableSsoNotImplemented =
  mkError
    status403
    "not-implemented"
    "The SSO feature flag is disabled by default.  It can only be enabled once for any team, never disabled.\n\
    \\n\
    \Rationale: there are two services in the backend that need to keep their status in sync: galley (teams),\n\
    \and spar (SSO).  Galley keeps track of team features.  If galley creates an idp, the feature flag is\n\
    \checked.  For authentication, spar avoids this expensive check and assumes that the idp can only have\n\
    \been created if the SSO is enabled.  This assumption does not hold any more if the switch is turned off\n\
    \again, so we do not support this.\n\
    \\n\
    \It is definitely feasible to change this.  If you have a use case, please contact customer support, or\n\
    \open an issue on https://github.com/wireapp/wire-server."

teamSearchVisibilityNotEnabled :: Error
teamSearchVisibilityNotEnabled = mkError status403 "team-search-visibility-not-enabled" "custom search is not available for this team"

customBackendNotFound :: Domain -> Error
customBackendNotFound domain =
  mkError
    status404
    "custom-backend-not-found"
    ("custom backend not found for domain: " <> cs (domainText domain))

invalidTeamNotificationId :: Error
invalidTeamNotificationId = mkError status400 "invalid-notification-id" "Could not parse notification id (must be UUIDv1)."

inactivityTimeoutTooLow :: Error
inactivityTimeoutTooLow = mkError status400 "inactivity-timeout-too-low" "applock inactivity timeout must be at least 30 seconds"
