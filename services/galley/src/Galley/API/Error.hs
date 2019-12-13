module Galley.API.Error where

import Imports
import Data.Text.Lazy as LT (pack)
import Galley.Types.Teams (IsPerm)
import Galley.Types.Conversations.Roles (Action)
import Network.HTTP.Types.Status
import Network.Wai.Utilities.Error

internalError :: Error
internalError = Error status500 "internal-error" "internal error"

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
operationDenied p = Error
    status403
    "operation-denied"
    ("Insufficient permissions (missing " <> (pack $ show p) <> ")")

actionDenied :: Action -> Error
actionDenied a = Error
    status403
    "action-denied"
    ("Insufficient authorization (missing " <> (pack $ show a) <> ")")

noTeamMember :: Error
noTeamMember = Error status403 "no-team-member" "Requesting user is not a team member."

noOtherOwner :: Error
noOtherOwner = Error status403 "no-other-owner" "You are trying to remove or downgrade\
                            \ an owner. Promote another team member before proceeding."

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
disableSsoNotImplemented = Error status403 "not-implemented"
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
