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

import Control.Monad.Catch (MonadThrow (..))
import Data.Domain (Domain, domainText)
import Data.Proxy
import Data.String.Conversions (cs)
import Data.Text.Lazy as LT (pack)
import qualified Data.Text.Lazy as LT
import GHC.TypeLits
import Galley.Types.Teams (hardTruncationLimit)
import Imports
import Network.HTTP.Types.Status
import Network.Wai.Utilities.Error
import Servant.API.Status (KnownStatus (..))
import Wire.API.ErrorDescription

errorDescriptionToWai ::
  forall (code :: Nat) (lbl :: Symbol) (desc :: Symbol).
  (KnownStatus code, KnownSymbol lbl) =>
  ErrorDescription code lbl desc ->
  Error
errorDescriptionToWai (ErrorDescription msg) =
  mkError (statusVal (Proxy @code)) (LT.pack (symbolVal (Proxy @lbl))) (LT.fromStrict msg)

throwErrorDescription ::
  (KnownStatus code, KnownSymbol lbl, MonadThrow m) =>
  ErrorDescription code lbl desc ->
  m a
throwErrorDescription = throwM . errorDescriptionToWai

internalError :: Error
internalError = internalErrorWithDescription "internal error"

internalErrorWithDescription :: LText -> Error
internalErrorWithDescription = mkError status500 "internal-error"

convMemberNotFound :: Error
convMemberNotFound = mkError status404 "no-conversation-member" "conversation member not found"

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

invalidTargetAccess :: Error
invalidTargetAccess = invalidOp "invalid target access"

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
