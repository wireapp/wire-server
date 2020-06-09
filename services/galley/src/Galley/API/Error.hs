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
import Data.Id (Id, Mapped, Remote, idToText)
import Data.IdMapping (IdMapping (idMappingGlobal, idMappingLocal))
import Data.List.NonEmpty (NonEmpty)
import Data.Qualified (Qualified, renderQualifiedId)
import Data.String.Conversions (cs)
import Data.Text.Lazy as LT (pack)
import qualified Data.Text.Lazy as LT
import Galley.Types.Conversations.Roles (Action)
import Galley.Types.Teams (IsPerm, hardTruncationLimit)
import Imports
import Network.HTTP.Types.Status
import Network.Wai.Utilities.Error
import Type.Reflection (Typeable, typeRep)

internalError :: Error
internalError = internalErrorWithDescription "internal error"

internalErrorWithDescription :: LText -> Error
internalErrorWithDescription = Error status500 "internal-error"

convNotFound :: Error
convNotFound = Error status404 "no-conversation" "conversation not found"

convMemberNotFound :: Error
convMemberNotFound = Error status404 "no-conversation-member" "conversation member not found"

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
invalidOp = Error status403 "invalid-op"

invalidPayload :: LText -> Error
invalidPayload = Error status400 "invalid-payload"

badRequest :: LText -> Error
badRequest = Error status400 "bad-request"

notConnected :: Error
notConnected = Error status403 "not-connected" "Users are not connected"

tooManyMembers :: Error
tooManyMembers = Error status403 "too-many-members" "Maximum number of members per conversation reached"

convAccessDenied :: Error
convAccessDenied = Error status403 "access-denied" "Conversation access denied"

accessDenied :: Error
accessDenied = Error status403 "access-denied" "You do not have permission to access this resource"

reAuthFailed :: Error
reAuthFailed = Error status403 "access-denied" "This operation requires reauthentication"

invalidUUID4 :: Error
invalidUUID4 = Error status400 "client-error" "Invalid UUID v4 format"

unknownClient :: Error
unknownClient = Error status403 "unknown-client" "Sending client not known"

invalidRange :: LText -> Error
invalidRange = Error status400 "client-error"

operationDenied :: (IsPerm perm, Show perm) => perm -> Error
operationDenied p =
  Error
    status403
    "operation-denied"
    ("Insufficient permissions (missing " <> (pack $ show p) <> ")")

actionDenied :: Action -> Error
actionDenied a =
  Error
    status403
    "action-denied"
    ("Insufficient authorization (missing " <> (pack $ show a) <> ")")

noBindingTeam :: Error
noBindingTeam = Error status403 "no-binding-team" "Operation allowed only on binding teams."

notAOneMemberTeam :: Error
notAOneMemberTeam = Error status403 "not-one-member-team" "Can only delete teams with a single member."

notATeamMember :: Error
notATeamMember = Error status403 "no-team-member" "Requesting user is not a team member."

bulkGetMemberLimitExceeded :: Error
bulkGetMemberLimitExceeded =
  Error
    status400
    "too-many-uids"
    ("Can only process " <> cs (show @Int hardTruncationLimit) <> " user ids per request.")

broadcastLimitExceeded :: Error
broadcastLimitExceeded =
  Error
    status400
    "too-many-users-to-broadcast"
    ("Too many users to fan out the broadcast event to.")

noAddToManaged :: Error
noAddToManaged = Error status403 "no-add-to-managed" "Adding users/bots directly to managed conversation is not allowed."

teamNotFound :: Error
teamNotFound = Error status404 "no-team" "team not found"

invalidPermissions :: Error
invalidPermissions = Error status403 "invalid-permissions" "The specified permissions are invalid."

invalidActions :: Error
invalidActions = Error status403 "invalid-actions" "The specified actions are invalid."

tooManyTeamMembers :: Error
tooManyTeamMembers = Error status403 "too-many-team-members" "Maximum number of members per team reached"

tooManyTeamMembersOnTeamWithLegalhold :: Error
tooManyTeamMembersOnTeamWithLegalhold = Error status403 "too-many-members-for-legalhold" "cannot add more members to team legalhold service is enabled."

teamMemberNotFound :: Error
teamMemberNotFound = Error status404 "no-team-member" "team member not found"

noManagedTeamConv :: Error
noManagedTeamConv = Error status400 "no-managed-team-conv" "Managed team conversations have been deprecated."

userBindingExists :: Error
userBindingExists = Error status403 "binding-exists" "User already bound to a different team."

noAddToBinding :: Error
noAddToBinding = Error status403 "binding-team" "Cannot add users to binding teams, invite only."

deleteQueueFull :: Error
deleteQueueFull = Error status503 "queue-full" "The delete queue is full. No further delete requests can be processed at the moment."

nonBindingTeam :: Error
nonBindingTeam = Error status404 "non-binding-team" "not member of a binding team"

noBindingTeamMembers :: Error
noBindingTeamMembers = Error status403 "non-binding-team-members" "Both users must be members of the same binding team."

invalidTeamStatusUpdate :: Error
invalidTeamStatusUpdate = Error status403 "invalid-team-status-update" "Cannot use this endpoint to update the team to the given status."

codeNotFound :: Error
codeNotFound = Error status404 "no-conversation-code" "conversation code not found"

cannotEnableLegalHoldServiceLargeTeam :: Error
cannotEnableLegalHoldServiceLargeTeam = Error status403 "too-large-team-for-legalhold" "cannot enable legalhold on large teams."

legalHoldServiceInvalidKey :: Error
legalHoldServiceInvalidKey = Error status400 "legalhold-invalid-key" "legal hold service pubkey is invalid"

legalHoldServiceUnavailable :: Error
legalHoldServiceUnavailable = Error status412 "legalhold-unavailable" "legal hold service does not respond or tls handshake could not be completed (did you pin the wrong public key?)"

legalHoldServiceNotRegistered :: Error
legalHoldServiceNotRegistered = Error status400 "legalhold-not-registered" "legal hold service has not been registered for this team"

legalHoldServiceBadResponse :: Error
legalHoldServiceBadResponse = Error status400 "legalhold-status-bad" "legal hold service: invalid response"

legalHoldFeatureFlagNotEnabled :: Error
legalHoldFeatureFlagNotEnabled = Error status403 "legalhold-not-enabled" "legal hold is not enabled for this wire instance"

legalHoldNotEnabled :: Error
legalHoldNotEnabled = Error status403 "legalhold-not-enabled" "legal hold is not enabled for this team"

userLegalHoldAlreadyEnabled :: Error
userLegalHoldAlreadyEnabled = Error status409 "legalhold-already-enabled" "legal hold is already enabled for this user"

userLegalHoldNotPending :: Error
userLegalHoldNotPending = Error status412 "legalhold-not-pending" "legal hold cannot be approved without being in a pending state"

noLegalHoldDeviceAllocated :: Error
noLegalHoldDeviceAllocated = Error status404 "legalhold-no-device-allocated" "no legal hold device is registered for this user. POST /teams/:tid/legalhold/:uid/ to start the flow."

disableSsoNotImplemented :: Error
disableSsoNotImplemented =
  Error
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
teamSearchVisibilityNotEnabled = Error status403 "team-search-visibility-not-enabled" "custom search is not available for this team"

customBackendNotFound :: Domain -> Error
customBackendNotFound domain =
  Error
    status404
    "custom-backend-not-found"
    ("custom backend not found for domain: " <> cs (domainText domain))

invalidTeamNotificationId :: Error
invalidTeamNotificationId = Error status400 "invalid-notification-id" "Could not parse notification id (must be UUIDv1)."

--------------------------------------------------------------------------------
-- Federation

federationNotImplemented' ::
  forall a. Typeable a => NonEmpty (Maybe (Id (Mapped a)), Qualified (Id (Remote a))) -> Error
federationNotImplemented' qualifiedIds =
  Error
    status403
    "federation-not-implemented"
    ("Federation is not implemented, but global qualified IDs (" <> idType <> ") found: " <> rendered)
  where
    idType = cs (show (typeRep @a))
    rendered = LT.intercalate ", " . toList . fmap (LT.fromStrict . renderMapping) $ qualifiedIds
    renderMapping (mMapped, qualified) = case mMapped of
      Nothing ->
        renderQualifiedId qualified
      Just mapped ->
        idToText mapped <> " -> " <> renderQualifiedId qualified

federationNotImplemented :: forall a. Typeable a => NonEmpty (IdMapping a) -> Error
federationNotImplemented =
  federationNotImplemented' . fmap (\i -> (Just (idMappingLocal i), idMappingGlobal i))
