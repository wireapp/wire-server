{-# LANGUAGE OverloadedStrings #-}

module Galley.API.Error where

import Data.Monoid
import Data.Text.Lazy
import Galley.Types.Teams (Perm)
import Network.HTTP.Types.Status
import Network.Wai.Utilities.Error

convNotFound :: Error
convNotFound = Error status404 "no-conversation" "conversation not found"

invalidSelfOp :: Error
invalidSelfOp = invalidOp "invalid operation for self conversation"

invalidOne2OneOp :: Error
invalidOne2OneOp = invalidOp "invalid operation for 1:1 conversations"

invalidConnectOp :: Error
invalidConnectOp = invalidOp "invalid operation for connect conversation"

invalidOp :: Text -> Error
invalidOp = Error status403 "invalid-op"

invalidPayload :: Text -> Error
invalidPayload = Error status400 "invalid-payload"

notConnected :: Error
notConnected = Error status403 "not-connected" "Users are not connected"

tooManyMembers :: Error
tooManyMembers = Error status403 "too-many-members" "Maximum number of members per conversation reached"

accessDenied :: Error
accessDenied = Error status403 "access-denied" "Conversation access denied"

invalidUUID4 :: Error
invalidUUID4 = Error status400 "client-error" "Invalid UUID v4 format"

unknownClient :: Error
unknownClient = Error status403 "unknown-client" "Sending client not known"

operationDenied :: Perm -> Error
operationDenied p = Error
    status403
    "operation-denied"
    ("Insufficient permissions (missing " <> (pack $ show p) <> ")")

noTeamMember :: Error
noTeamMember = Error status403 "no-team-member" "Requesting user is not a team member."

noAddToManaged :: Error
noAddToManaged = Error status403 "no-add-to-managed" "Adding users directly to managed conversation is not allowed."

teamNotFound :: Error
teamNotFound = Error status404 "no-team" "team not found"

noAddToBound :: Error
noAddToBound = Error status403 "no-add-to-bound" "Cannot add users to bound teams, invite only."

noBotsInTeamConvs :: Error
noBotsInTeamConvs = Error status403 "bots-not-allowed" "Adding bots to team conversations is not allowed."

invalidPermissions :: Error
invalidPermissions = Error status403 "invalid-permissions" "The specified permissions are invalid."

tooManyTeamMembers :: Error
tooManyTeamMembers = Error status403 "too-many-team-members" "Maximum number of members per team reached"

teamMemberNotFound :: Error
teamMemberNotFound = Error status404 "no-team-member" "team member not found"

noTeamConv :: Error
noTeamConv = Error status400 "no-team-conv" "Team conversations are not allowed in this context."

userBindingExists :: Error
userBindingExists = Error status403 "binding-exists" "The user is already bound to a different team."

deleteQueueFull :: Error
deleteQueueFull = Error status503 "queue-full" "The delete queue is full. No further delete requests can be processed at the moment."
