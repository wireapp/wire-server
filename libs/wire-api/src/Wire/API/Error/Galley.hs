-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.API.Error.Galley
  ( GalleyError (..),
    OperationDenied,
    AuthenticationError (..),
    TeamFeatureError (..),
  )
where

import Data.Singletons.Prelude (Show_)
import GHC.TypeLits
import Imports
import Polysemy
import Polysemy.Error
import Wire.API.Conversation.Role
import Wire.API.Error
import Wire.API.Routes.API
import Wire.API.Team.Permission

data GalleyError
  = InvalidAction
  | InvalidTargetAccess
  | TeamNotFound
  | TeamMemberNotFound
  | NotATeamMember
  | NonBindingTeam
  | BroadcastLimitExceeded
  | UserBindingExists
  | NoAddToBinding
  | TooManyTeamMembers
  | -- FUTUREWORK: possibly make MissingPermission take a list of Perm
    MissingPermission (Maybe Perm)
  | ActionDenied Action
  | NotConnected
  | InvalidOperation
  | InvalidTarget
  | ConvNotFound
  | ConvAccessDenied
  | MLSNonEmptyMemberList
  | NoBindingTeamMembers
  | NoBindingTeam
  | NotAOneMemberTeam
  | TooManyMembers
  | ConvMemberNotFound
  | GuestLinksDisabled
  | CodeNotFound
  | InvalidPermissions
  | InvalidTeamStatusUpdate
  | AccessDenied
  | UnknownWelcomeRecipient
  | CustomBackendNotFound
  | DeleteQueueFull
  | TeamSearchVisibilityNotEnabled
  | CannotEnableLegalHoldServiceLargeTeam

instance KnownError (MapError e) => IsSwaggerError (e :: GalleyError) where
  addToSwagger = addStaticErrorToSwagger @(MapError e)

-- | Convenience synonym for an operation denied error with an unspecified permission
type OperationDenied = 'MissingPermission 'Nothing

type instance ErrorEffect (e :: GalleyError) = ErrorS e

type instance MapError 'InvalidAction = 'StaticError 400 "invalid-actions" "The specified actions are invalid"

type instance MapError 'InvalidTargetAccess = 'StaticError 403 "invalid-op" "Invalid target access"

type instance MapError 'TeamNotFound = 'StaticError 404 "no-team" "Team not found"

type instance MapError 'NonBindingTeam = 'StaticError 404 "non-binding-team" "Not a member of a binding team"

type instance MapError 'BroadcastLimitExceeded = 'StaticError 400 "too-many-users-to-broadcast" "Too many users to fan out the broadcast event to"

type instance MapError 'TeamMemberNotFound = 'StaticError 404 "no-team-member" "Team member not found"

type instance MapError 'NotATeamMember = 'StaticError 403 "no-team-member" "Requesting user is not a team member"

type instance MapError 'UserBindingExists = 'StaticError 403 "binding-exists" "User already bound to a different team"

type instance MapError 'NoAddToBinding = 'StaticError 403 "binding-team" "Cannot add users to binding teams, invite only"

type instance MapError 'TooManyTeamMembers = 'StaticError 403 "too-many-team-members" "Maximum number of members per team reached"

type instance MapError ('MissingPermission mperm) = 'StaticError 403 "operation-denied" (MissingPermissionMessage mperm)

type instance MapError ('ActionDenied action) = 'StaticError 403 "action-denied" ("Insufficient authorization (missing " `AppendSymbol` ActionName action `AppendSymbol` ")")

type instance MapError 'NotConnected = 'StaticError 403 "not-connected" "Users are not connected"

type instance MapError 'InvalidOperation = 'StaticError 403 "invalid-op" "Invalid operation"

type instance MapError 'InvalidTarget = 'StaticError 403 "invalid-op" "Invalid target"

type instance MapError 'ConvNotFound = 'StaticError 404 "no-conversation" "Conversation not found"

type instance MapError 'ConvAccessDenied = 'StaticError 403 "access-denied" "Conversation access denied"

type instance MapError 'MLSNonEmptyMemberList = 'StaticError 400 "non-empty-member-list" "Attempting to add group members outside MLS"

type instance MapError 'NoBindingTeamMembers = 'StaticError 403 "non-binding-team-members" "Both users must be members of the same binding team"

type instance MapError 'NoBindingTeam = 'StaticError 403 "no-binding-team" "Operation allowed only on binding teams"

type instance MapError 'NotAOneMemberTeam = 'StaticError 403 "not-one-member-team" "Can only delete teams with a single member"

type instance MapError 'TooManyMembers = 'StaticError 403 "too-many-members" "Maximum number of members per conversation reached"

type instance MapError 'ConvMemberNotFound = 'StaticError 404 "no-conversation-member" "Conversation member not found"

type instance MapError 'GuestLinksDisabled = 'StaticError 409 "guest-links-disabled" "The guest link feature is disabled and all guest links have been revoked"

type instance MapError 'CodeNotFound = 'StaticError 404 "no-conversation-code" "Conversation code not found"

type instance MapError 'InvalidPermissions = 'StaticError 403 "invalid-permissions" "The specified permissions are invalid"

type instance MapError 'InvalidTeamStatusUpdate = 'StaticError 403 "invalid-team-status-update" "Cannot use this endpoint to update the team to the given status."

type instance MapError 'AccessDenied = 'StaticError 403 "access-denied" "You do not have permission to access this resource"

type instance MapError 'UnknownWelcomeRecipient = 'StaticError 400 "mls-unknown-welcome-recipient" "One of the key packages of a welcome message could not be mapped to a known client"

type instance MapError 'CustomBackendNotFound = 'StaticError 404 "custom-backend-not-found" "Custom backend not found"

type instance MapError 'DeleteQueueFull = 'StaticError 503 "queue-full" "The delete queue is full; no further delete requests can be processed at the moment"

type instance MapError 'TeamSearchVisibilityNotEnabled = 'StaticError 403 "team-search-visibility-not-enabled" "Custom search is not available for this team"

type instance MapError 'CannotEnableLegalHoldServiceLargeTeam = 'StaticError 403 "too-large-team-for-legalhold" "Cannot enable legalhold on large teams (reason: for removing LH from team, we need to iterate over all members, which is only supported for teams with less than 2k members)"

-- We need this to document possible (operation denied) errors in the servant routes.
type family MissingPermissionMessage (a :: Maybe Perm) :: Symbol where
  MissingPermissionMessage ('Just p) = "Insufficient permissions (missing " `AppendSymbol` Show_ p `AppendSymbol` ")"
  MissingPermissionMessage 'Nothing = "Insufficient permissions"

--------------------------------------------------------------------------------
-- Authentication errors

data AuthenticationError
  = ReAuthFailed
  | VerificationCodeAuthFailed
  | VerificationCodeRequired

type instance MapError 'ReAuthFailed = 'StaticError 403 "access-denied" "This operation requires reauthentication"

type instance MapError 'VerificationCodeAuthFailed = 'StaticError 403 "code-authentication-failed" "Code authentication failed"

type instance MapError 'VerificationCodeRequired = 'StaticError 403 "code-authentication-required" "Verification code required"

instance IsSwaggerError AuthenticationError where
  addToSwagger =
    addStaticErrorToSwagger @(MapError 'ReAuthFailed)
      . addStaticErrorToSwagger @(MapError 'VerificationCodeAuthFailed)
      . addStaticErrorToSwagger @(MapError 'VerificationCodeRequired)

type instance ErrorEffect AuthenticationError = Error AuthenticationError

authenticationErrorToDyn :: AuthenticationError -> DynError
authenticationErrorToDyn ReAuthFailed = dynError @(MapError 'ReAuthFailed)
authenticationErrorToDyn VerificationCodeAuthFailed = dynError @(MapError 'VerificationCodeAuthFailed)
authenticationErrorToDyn VerificationCodeRequired = dynError @(MapError 'VerificationCodeRequired)

instance Member (Error DynError) r => ServerEffect (Error AuthenticationError) r where
  interpretServerEffect = mapError authenticationErrorToDyn

--------------------------------------------------------------------------------
-- Team feature errors

data TeamFeatureError
  = AppLockInactivityTimeoutTooLow
  | LegalHoldFeatureFlagNotEnabled
  | LegalHoldWhitelistedOnly
  | DisableSsoNotImplemented
  | FeatureLocked

instance IsSwaggerError TeamFeatureError where
  -- Do not display in Swagger
  addToSwagger = id

type instance MapError 'AppLockInactivityTimeoutTooLow = 'StaticError 400 "inactivity-timeout-too-low" "Applock inactivity timeout must be at least 30 seconds"

type instance MapError 'LegalHoldFeatureFlagNotEnabled = 'StaticError 403 "legalhold-not-enabled" "Legal hold is not enabled for this wire instance"

type instance MapError 'LegalHoldWhitelistedOnly = 'StaticError 403 "legalhold-whitelisted-only" "Legal hold is enabled for teams via server config and cannot be changed here"

type instance
  MapError 'DisableSsoNotImplemented =
    'StaticError
      403
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

type instance MapError 'FeatureLocked = 'StaticError 409 "feature-locked" "Feature config cannot be updated (e.g. because it is configured to be locked, or because you need to upgrade your plan)"

type instance ErrorEffect TeamFeatureError = Error TeamFeatureError

instance Member (Error DynError) r => ServerEffect (Error TeamFeatureError) r where
  interpretServerEffect = mapError $ \case
    AppLockInactivityTimeoutTooLow -> dynError @(MapError 'AppLockInactivityTimeoutTooLow)
    LegalHoldFeatureFlagNotEnabled -> dynError @(MapError 'LegalHoldFeatureFlagNotEnabled)
    LegalHoldWhitelistedOnly -> dynError @(MapError 'LegalHoldWhitelistedOnly)
    DisableSsoNotImplemented -> dynError @(MapError 'DisableSsoNotImplemented)
    FeatureLocked -> dynError @(MapError 'FeatureLocked)
