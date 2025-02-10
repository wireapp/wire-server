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

module Wire.API.Error.Brig where

import Data.Data
import Wire.API.Error

data BrigError
  = UserNotFound
  | InvalidUser
  | InvalidCode
  | BadCredentials
  | MissingAuth
  | DeleteCodePending
  | OwnerDeletingSelf
  | TooManyClients
  | MalformedPrekeys
  | CodeAuthenticationFailed
  | CodeAuthenticationRequired
  | MissingLegalholdConsent
  | MissingLegalholdConsentOldClients
  | ConnectionLimitReached
  | UnknownClient
  | ClientNotFound
  | NotConnected
  | InvalidTransition
  | NoIdentity
  | NoUser
  | HandleExists
  | InvalidHandle
  | HandleNotFound
  | UserCreationRestricted
  | AllowlistError
  | InvalidInvitationCode
  | MissingIdentity
  | BlacklistedEmail
  | InvalidEmail
  | InvalidActivationCodeWrongUser
  | InvalidActivationCodeWrongCode
  | TooManyTeamMembers
  | MLSNotEnabled
  | MLSIdentityMismatch
  | MLSProtocolError
  | MLSDuplicatePublicKey
  | InvalidPhone
  | PasswordExists
  | AccountSuspended
  | AccountEphemeral
  | AccountPending
  | UserKeyExists
  | EmailExists
  | NameManagedByScim
  | HandleManagedByScim
  | LocaleManagedByScim
  | EmailManagedByScim
  | LastIdentity
  | NoPassword
  | ChangePasswordMustDiffer
  | TooManyTeamInvitations
  | CannotJoinMultipleTeams
  | InsufficientTeamPermissions
  | KeyPackageDecodingError
  | InvalidKeyPackageRef
  | CustomerExtensionBlockedDomain
  | PasswordResetInProgress
  | InvalidPasswordResetKey
  | InvalidPasswordResetCode
  | ResetPasswordMustDiffer
  | NoEmail
  | NotificationNotFound
  | PendingInvitationNotFound
  | ConflictingInvitations
  | AccessDenied
  | InvalidConversation
  | TooManyConversationMembers
  | ServiceDisabled
  | InvalidBot
  | InvalidServiceKey
  | ServiceNotFound
  | VerificationCodeThrottled
  | InvalidProvider
  | ProviderNotFound
  | TeamsNotFederating
  | PasswordIsStale
  | TooManyProperties
  | PropertyKeyTooLarge
  | PropertyValueTooLarge
  | UserAlreadyInATeam
  | MLSServicesNotAllowed
  | NotificationQueueConnectionError
  | DomainVerificationErrorNotFound
  | DomainVerificationInvalidDomain
  | DomainVerificationDomainVerificationFailed
  | DomainVerificationOperationForbidden
  | DomainVerificationAuthFailure
  | DomainVerificationPaymentRequired
  | DomainVerificationNotEnabled
  | DomainVerificationChallengeNotFound

instance (Typeable (MapError e), KnownError (MapError e)) => IsSwaggerError (e :: BrigError) where
  addToOpenApi = addStaticErrorToSwagger @(MapError e)

type instance MapError 'ServiceNotFound = 'StaticError 404 "not-found" "Service not found."

type instance MapError 'InvalidServiceKey = 'StaticError 400 "invalid-service-key" "Invalid service key."

type instance MapError 'ProviderNotFound = 'StaticError 404 "not-found" "Provider not found."

type instance MapError 'InvalidProvider = 'StaticError 403 "invalid-provider" "The provider does not exist."

type instance MapError 'VerificationCodeThrottled = 'StaticError 429 "too-many-requests" "Too many request to generate a verification code."

type instance MapError 'ServiceDisabled = 'StaticError 403 "service-disabled" "The desired service is currently disabled."

type instance MapError 'InvalidBot = 'StaticError 403 "invalid-bot" "The targeted user is not a bot."

type instance MapError 'UserNotFound = 'StaticError 404 "not-found" "User not found"

type instance MapError 'InvalidConversation = 'StaticError 403 "invalid-conversation" "The operation is not allowed in this conversation."

type instance MapError 'TooManyConversationMembers = 'StaticError 403 "too-many-members" "Maximum number of members per conversation reached."

type instance MapError 'AccessDenied = 'StaticError 403 "access-denied" "Access denied."

type instance MapError 'InvalidUser = 'StaticError 400 "invalid-user" "Invalid user"

type instance MapError 'InvalidCode = 'StaticError 403 "invalid-code" "Invalid verification code"

type instance MapError 'MissingAuth = 'StaticError 403 "missing-auth" "Re-authentication via password required"

type instance MapError 'BadCredentials = 'StaticError 403 "invalid-credentials" "Authentication failed"

type instance MapError 'DeleteCodePending = 'StaticError 403 "pending-delete" "A verification code for account deletion is still pending"

type instance MapError 'OwnerDeletingSelf = 'StaticError 403 "no-self-delete-for-team-owner" "Team owners are not allowed to delete themselves; ask a fellow owner"

type instance MapError 'TooManyClients = 'StaticError 403 "too-many-clients" "Too many clients"

type instance MapError 'MalformedPrekeys = 'StaticError 400 "bad-request" "Malformed prekeys uploaded"

type instance MapError 'CodeAuthenticationFailed = 'StaticError 403 "code-authentication-failed" "Code authentication failed"

type instance MapError 'CodeAuthenticationRequired = 'StaticError 403 "code-authentication-required" "Code authentication is required"

type instance
  MapError 'MissingLegalholdConsentOldClients =
    'StaticError
      403
      "missing-legalhold-consent-old-clients"
      "Failed to connect to a user or to invite a user to a group because somebody is under legalhold and somebody else has old clients that do not support legalhold's UI requirements"

type instance
  MapError 'MissingLegalholdConsent =
    'StaticError
      403
      "missing-legalhold-consent"
      "Failed to connect to a user or to invite a user to a group because somebody is under legalhold and somebody else has not granted consent"

type instance MapError 'ConnectionLimitReached = 'StaticError 403 "connection-limit" "Too many sent/accepted connections"

type instance MapError 'UnknownClient = 'StaticError 403 "unknown-client" "Unknown Client"

type instance MapError 'ClientNotFound = 'StaticError 404 "client-not-found" "Client not found"

type instance MapError 'NotConnected = 'StaticError 403 "not-connected" "Users are not connected"

type instance MapError 'InvalidTransition = 'StaticError 403 "bad-conn-update" "Invalid status transition"

type instance MapError 'NoIdentity = 'StaticError 403 "no-identity" "The user has no verified email"

type instance MapError 'HandleExists = 'StaticError 409 "handle-exists" "The given handle is already taken"

type instance MapError 'InvalidHandle = 'StaticError 400 "invalid-handle" "The given handle is invalid (less than 2 or more than 256 characters; chars not in \"a-z0-9_.-\"; or on the blocklist)"

type instance MapError 'HandleNotFound = 'StaticError 404 "not-found" "Handle not found"

type instance MapError 'MLSDuplicatePublicKey = 'StaticError 400 "mls-duplicate-public-key" "MLS public key for the given signature scheme already exists"

type instance MapError 'AllowlistError = 'StaticError 403 "unauthorized" "Unauthorized e-mail address"

type instance MapError 'InvalidInvitationCode = 'StaticError 400 "invalid-invitation-code" "Invalid invitation code."

type instance MapError 'MissingIdentity = 'StaticError 403 "missing-identity" "Using an invitation code requires registering the given email."

type instance
  MapError 'BlacklistedEmail =
    'StaticError
      403
      "blacklisted-email"
      "The given e-mail address has been blacklisted due to a permanent bounce \
      \or a complaint."

type instance MapError 'InvalidEmail = 'StaticError 400 "invalid-email" "Invalid e-mail address."

type instance MapError 'InvalidActivationCodeWrongUser = 'StaticError 404 "invalid-code" "User does not exist"

type instance MapError 'InvalidActivationCodeWrongCode = 'StaticError 404 "invalid-code" "Invalid activation code"

type instance MapError 'TooManyTeamMembers = 'StaticError 403 "too-many-team-members" "Too many members in this team."

type instance
  MapError 'MLSNotEnabled =
    'StaticError
      400
      "mls-not-enabled"
      "MLS is not configured on this backend. See docs.wire.com for instructions on how to enable it"

type instance
  MapError 'MLSIdentityMismatch =
    'StaticError
      403
      "mls-identity-mismatch"
      "Key package credential does not match qualified client ID"

-- | docs/reference/user/registration.md {#RefRestrictRegistration}.
type instance MapError 'UserCreationRestricted = 'StaticError 403 "user-creation-restricted" "This instance does not allow creation of personal users or teams."

type instance MapError 'MLSProtocolError = 'StaticError 400 "mls-protocol-error" "MLS protocol error"

type instance MapError 'InvalidPhone = 'StaticError 400 "invalid-phone" "Invalid mobile phone number"

type instance
  MapError 'PasswordExists =
    'StaticError
      403
      "password-exists"
      "The operation is not permitted because the user has a password set"

type instance MapError 'AccountSuspended = 'StaticError 403 "suspended" "Account suspended"

type instance MapError 'AccountEphemeral = 'StaticError 403 "ephemeral" "Account ephemeral"

type instance MapError 'AccountPending = 'StaticError 403 "pending-activation" "Account pending activation"

type instance MapError 'UserKeyExists = 'StaticError 409 "key-exists" "The given e-mail address is in use."

type instance MapError 'EmailExists = 'StaticError 409 "email-exists" "The given e-mail address is in use."

type instance MapError 'NameManagedByScim = 'StaticError 403 "managed-by-scim" "Updating name is not allowed, because it is managed by SCIM, or E2EId is enabled"

type instance MapError 'HandleManagedByScim = 'StaticError 403 "managed-by-scim" "Updating handle is not allowed, because it is managed by SCIM, or E2EId is enabled"

type instance MapError 'LocaleManagedByScim = 'StaticError 403 "managed-by-scim" "Updating locale is not allowed, because it is managed by SCIM, or E2EId is enabled"

type instance MapError 'EmailManagedByScim = 'StaticError 403 "managed-by-scim" "Updating email is not allowed, because it is managed by SCIM, or E2EId is enabled"

type instance MapError 'LastIdentity = 'StaticError 403 "last-identity" "The last user identity cannot be removed."

type instance MapError 'NoPassword = 'StaticError 403 "no-password" "The user has no password."

type instance MapError 'ChangePasswordMustDiffer = 'StaticError 409 "password-must-differ" "For password change, new and old password must be different."

type instance MapError 'TooManyTeamInvitations = 'StaticError 403 "too-many-team-invitations" "Too many team invitations for this team"

type instance MapError 'CannotJoinMultipleTeams = 'StaticError 403 "cannot-join-multiple-teams" "Cannot accept invitations from multiple teams"

type instance MapError 'InsufficientTeamPermissions = 'StaticError 403 "insufficient-permissions" "Insufficient team permissions"

type instance MapError 'KeyPackageDecodingError = 'StaticError 409 "decoding-error" "Key package could not be TLS-decoded"

type instance MapError 'InvalidKeyPackageRef = 'StaticError 409 "invalid-reference" "Key package's reference does not match its data"

type instance
  MapError 'CustomerExtensionBlockedDomain =
    'StaticError
      451
      "domain-blocked-for-registration"
      "[Customer extension] the email domain example.com \
      \that you are attempting to register a user with has been \
      \blocked for creating wire users.  Please contact your IT department."

type instance MapError 'PasswordResetInProgress = 'StaticError 409 "code-exists" "A password reset is already in progress."

type instance MapError 'InvalidPasswordResetKey = 'StaticError 400 "invalid-key" "Invalid email or mobile number for password reset."

type instance MapError 'InvalidPasswordResetCode = 'StaticError 400 "invalid-code" "Invalid password reset code."

type instance MapError 'ResetPasswordMustDiffer = 'StaticError 409 "password-must-differ" "For password reset, new and old password must be different."

type instance MapError 'NoEmail = 'StaticError 403 "no-email" "This operation requires the user to have a verified email address."

type instance MapError 'NotificationNotFound = 'StaticError 404 "not-found" "Notification not found."

type instance MapError 'PendingInvitationNotFound = 'StaticError 404 "not-found" "No pending invitations exists."

type instance MapError 'ConflictingInvitations = 'StaticError 409 "conflicting-invitations" "Multiple conflicting invitations to different teams exists."

type instance MapError 'TeamsNotFederating = 'StaticError 403 "team-not-federating" "The target user is owned by a federated backend, but is not in an allow-listed team"

type instance MapError 'PasswordIsStale = 'StaticError 403 "password-is-stale" "The password is too old, please update your password."

type instance MapError 'TooManyProperties = 'StaticError 403 "too-many-properties" "Too many properties"

type instance MapError 'PropertyKeyTooLarge = 'StaticError 403 "property-key-too-large" "The property key is too large."

type instance MapError 'PropertyValueTooLarge = 'StaticError 403 "property-value-too-large" "The property value is too large"

type instance MapError 'UserAlreadyInATeam = 'StaticError 403 "user-already-in-a-team" "Switching teams is not allowed"

type instance MapError 'MLSServicesNotAllowed = 'StaticError 409 "mls-services-not-allowed" "Services not allowed in MLS"

type instance MapError 'NotificationQueueConnectionError = 'StaticError 500 "internal-server-error" "Internal server error"

type instance MapError 'DomainVerificationErrorNotFound = 'StaticError 404 "not-found" "Not Found"

type instance MapError 'DomainVerificationInvalidDomain = 'StaticError 400 "invalid-domain" "Invalid domain"

type instance MapError 'DomainVerificationDomainVerificationFailed = 'StaticError 403 "domain-verification-failed" "Domain verification failed"

type instance MapError 'DomainVerificationOperationForbidden = 'StaticError 403 "operation-forbidden-for-domain-registration-state" "Invalid domain registration state update"

type instance MapError 'DomainVerificationAuthFailure = 'StaticError 401 "domain-registration-update-auth-failure" "Domain registration updated auth failure"

type instance MapError 'DomainVerificationPaymentRequired = 'StaticError 402 "domain-registration-update-payment-required" "Domain registration updated payment required"

type instance MapError 'DomainVerificationNotEnabled = 'StaticError 503 "enterprise-service-not-enabled" "Enterprise service not enabled"

type instance MapError 'DomainVerificationChallengeNotFound = 'StaticError 404 "challenge-not-found" "Challenge not found"
