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

import Wire.API.Error
  ( IsSwaggerError (..),
    KnownError,
    MapError,
    StaticError (StaticError),
    addStaticErrorToSwagger,
  )

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
  | ConnectionLimitReached
  | UnknownClient
  | ClientNotFound
  | NotConnected
  | InvalidTransition
  | NoIdentity
  | HandleExists
  | InvalidHandle
  | HandleNotFound
  | UserCreationRestricted
  | BlacklistedPhone
  | WhitelistError
  | InvalidInvitationCode
  | MissingIdentity
  | BlacklistedEmail
  | InvalidEmail
  | InvalidActivationCodeWrongUser
  | InvalidActivationCodeWrongCode
  | TooManyTeamMembers
  | MLSIdentityMismatch
  | MLSProtocolError
  | MLSDuplicatePublicKey
  | InvalidPhone
  | UserKeyExists
  | NameManagedByScim
  | HandleManagedByScim
  | LastIdentity
  | NoPassword
  | ChangePasswordMustDiffer
  | PasswordAuthenticationFailed
  | TooManyTeamInvitations
  | InsufficientTeamPermissions
  | KeyPackageDecodingError
  | InvalidKeyPackageRef
  | CustomerExtensionBlockedDomain
  | PasswordResetInProgress
  | InvalidPasswordResetKey
  | InvalidPasswordResetCode
  | ResetPasswordMustDiffer

instance KnownError (MapError e) => IsSwaggerError (e :: BrigError) where
  addToSwagger = addStaticErrorToSwagger @(MapError e)

type instance MapError 'UserNotFound = 'StaticError 404 "not-found" "User not found"

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

type instance MapError 'NoIdentity = 'StaticError 403 "no-identity" "The user has no verified identity (email or phone number)"

type instance MapError 'HandleExists = 'StaticError 409 "handle-exists" "The given handle is already taken"

type instance MapError 'InvalidHandle = 'StaticError 400 "invalid-handle" "The given handle is invalid"

type instance MapError 'HandleNotFound = 'StaticError 404 "not-found" "Handle not found"

type instance MapError 'MLSDuplicatePublicKey = 'StaticError 400 "mls-duplicate-public-key" "MLS public key for the given signature scheme already exists"

type instance MapError 'BlacklistedPhone = 'StaticError 403 "blacklisted-phone" "The given phone number has been blacklisted due to suspected abuse or a complaint"

type instance MapError 'WhitelistError = 'StaticError 403 "unauthorized" "Unauthorized e-mail address or phone number."

type instance MapError 'InvalidInvitationCode = 'StaticError 400 "invalid-invitation-code" "Invalid invitation code."

type instance MapError 'MissingIdentity = 'StaticError 403 "missing-identity" "Using an invitation code requires registering the given email and/or phone."

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
  MapError 'MLSIdentityMismatch =
    'StaticError
      403
      "mls-identity-mismatch"
      "Key package credential does not match qualified client ID"

-- | docs/reference/user/registration.md {#RefRestrictRegistration}.
type instance MapError 'UserCreationRestricted = 'StaticError 403 "user-creation-restricted" "This instance does not allow creation of personal users or teams."

type instance MapError 'MLSProtocolError = 'StaticError 400 "mls-protocol-error" "MLS protocol error"

type instance MapError 'InvalidPhone = 'StaticError 400 "invalid-phone" "Invalid mobile phone number"

type instance MapError 'UserKeyExists = 'StaticError 409 "key-exists" "The given e-mail address or phone number is in use."

type instance MapError 'NameManagedByScim = 'StaticError 403 "managed-by-scim" "Updating name is not allowed, because it is managed by SCIM"

type instance MapError 'HandleManagedByScim = 'StaticError 403 "managed-by-scim" "Updating handle is not allowed, because it is managed by SCIM"

type instance MapError 'LastIdentity = 'StaticError 403 "last-identity" "The last user identity (email or phone number) cannot be removed."

type instance MapError 'NoPassword = 'StaticError 403 "no-password" "The user has no password."

type instance MapError 'ChangePasswordMustDiffer = 'StaticError 409 "password-must-differ" "For password change, new and old password must be different."

type instance MapError 'PasswordAuthenticationFailed = 'StaticError 403 "password-authentication-failed" "Password authentication failed."

type instance MapError 'TooManyTeamInvitations = 'StaticError 403 "too-many-team-invitations" "Too many team invitations for this team"

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

type instance MapError 'InvalidPasswordResetKey = 'StaticError 400 "invalid-key" "Invalid email or mobile number for password reset."

type instance MapError 'InvalidPasswordResetCode = 'StaticError 400 "invalid-code" "Invalid password reset code."

type instance MapError 'ResetPasswordMustDiffer = 'StaticError 409 "password-must-differ" "For password reset, new and old password must be different."

type instance MapError 'PasswordResetInProgress = 'StaticError 409 "code-exists" "A password reset is already in progress."
