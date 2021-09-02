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

module Brig.API.Error where

import Brig.API.Types
import Brig.Options (DomainsBlockedForRegistration)
import Brig.Phone (PhoneException (..))
import Brig.Types (DeletionCodeTimeout (..))
import Brig.Types.Common (PhoneBudgetTimeout (..))
import Control.Monad.Error.Class hiding (Error)
import Data.Aeson
import Data.ByteString.Conversion
import Data.Domain (Domain)
import qualified Data.HashMap.Strict as HashMap
import Data.Proxy
import Data.String.Conversions (cs)
import qualified Data.Text.Lazy as LT
import qualified Data.ZAuth.Validation as ZAuth
import GHC.TypeLits
import Imports
import Network.HTTP.Types.Header
import Network.HTTP.Types.Status
import qualified Network.Wai.Utilities.Error as Wai
import Servant.API.Status
import Wire.API.ErrorDescription
import Wire.API.Federation.Client (FederationError (..))
import Wire.API.Federation.Error

errorDescriptionToWai ::
  forall (code :: Nat) (lbl :: Symbol) (desc :: Symbol).
  (KnownStatus code, KnownSymbol lbl) =>
  ErrorDescription code lbl desc ->
  Wai.Error
errorDescriptionToWai (ErrorDescription msg) =
  Wai.mkError
    (statusVal (Proxy @code))
    (LT.pack (symbolVal (Proxy @lbl)))
    (LT.fromStrict msg)

data Error where
  StdError :: !Wai.Error -> Error
  RichError :: ToJSON a => !Wai.Error -> !a -> [Header] -> Error

errorLabel :: Error -> LText
errorLabel (StdError e) = Wai.label e
errorLabel (RichError e _ _) = Wai.label e

errorStatus :: Error -> Status
errorStatus (StdError e) = Wai.code e
errorStatus (RichError e _ _) = Wai.code e

throwStd :: MonadError Error m => Wai.Error -> m a
throwStd = throwError . StdError

throwRich :: (MonadError Error m, ToJSON x) => Wai.Error -> x -> [Header] -> m a
throwRich e x h = throwError (RichError e x h)

throwErrorDescription ::
  (KnownStatus code, KnownSymbol lbl, MonadError Error m) =>
  ErrorDescription code lbl desc ->
  m a
throwErrorDescription = throwStd . errorDescriptionToWai

instance ToJSON Error where
  toJSON (StdError e) = toJSON e
  toJSON (RichError e x _) = case (toJSON e, toJSON x) of
    (Object o1, Object o2) -> Object (HashMap.union o1 o2)
    (j, _) -> j

-- Error Mapping ----------------------------------------------------------

connError :: ConnectionError -> Error
connError TooManyConnections {} = StdError (errorDescriptionToWai connectionLimitReached)
connError InvalidTransition {} = StdError invalidTransition
connError NotConnected {} = StdError (errorDescriptionToWai notConnected)
connError InvalidUser {} = StdError (errorDescriptionToWai invalidUser)
connError ConnectNoIdentity {} = StdError (errorDescriptionToWai (noIdentity 0))
connError (ConnectBlacklistedUserKey k) = StdError $ foldKey (const blacklistedEmail) (const blacklistedPhone) k
connError (ConnectInvalidEmail _ _) = StdError invalidEmail
connError ConnectInvalidPhone {} = StdError invalidPhone
connError ConnectSameBindingTeamUsers = StdError sameBindingTeamUsers
connError ConnectMissingLegalholdConsent = StdError (errorDescriptionToWai missingLegalholdConsent)

actError :: ActivationError -> Error
actError (UserKeyExists _) = StdError userKeyExists
actError (InvalidActivationCode e) = StdError (invalidActivationCode e)
actError (InvalidActivationEmail _ _) = StdError invalidEmail
actError (InvalidActivationPhone _) = StdError invalidPhone

pwResetError :: PasswordResetError -> Error
pwResetError InvalidPasswordResetKey = StdError invalidPwResetKey
pwResetError InvalidPasswordResetCode = StdError invalidPwResetCode
pwResetError (PasswordResetInProgress Nothing) = StdError duplicatePwResetCode
pwResetError (PasswordResetInProgress (Just t)) =
  RichError
    duplicatePwResetCode
    ()
    [("Retry-After", toByteString' t)]
pwResetError ResetPasswordMustDiffer = StdError resetPasswordMustDiffer

newUserError :: CreateUserError -> Error
newUserError InvalidInvitationCode = StdError invalidInvitationCode
newUserError MissingIdentity = StdError missingIdentity
newUserError (InvalidEmail _ _) = StdError invalidEmail
newUserError (InvalidPhone _) = StdError invalidPhone
newUserError (DuplicateUserKey _) = StdError userKeyExists
newUserError (EmailActivationError e) = actError e
newUserError (PhoneActivationError e) = actError e
newUserError (BlacklistedUserKey k) = StdError $ foldKey (const blacklistedEmail) (const blacklistedPhone) k
newUserError TooManyTeamMembers = StdError tooManyTeamMembers
newUserError UserCreationRestricted = StdError userCreationRestricted
newUserError (ExternalPreconditionFailed e) = StdError e

sendLoginCodeError :: SendLoginCodeError -> Error
sendLoginCodeError (SendLoginInvalidPhone _) = StdError invalidPhone
sendLoginCodeError SendLoginPasswordExists = StdError passwordExists

sendActCodeError :: SendActivationCodeError -> Error
sendActCodeError (InvalidRecipient k) = StdError $ foldKey (const invalidEmail) (const invalidPhone) k
sendActCodeError (UserKeyInUse _) = StdError userKeyExists
sendActCodeError (ActivationBlacklistedUserKey k) = StdError $ foldKey (const blacklistedEmail) (const blacklistedPhone) k

changeEmailError :: ChangeEmailError -> Error
changeEmailError (InvalidNewEmail _ _) = StdError invalidEmail
changeEmailError (EmailExists _) = StdError userKeyExists
changeEmailError (ChangeBlacklistedEmail _) = StdError blacklistedEmail
changeEmailError EmailManagedByScim = StdError $ propertyManagedByScim "email"

changePhoneError :: ChangePhoneError -> Error
changePhoneError (InvalidNewPhone _) = StdError invalidPhone
changePhoneError (PhoneExists _) = StdError userKeyExists

changePwError :: ChangePasswordError -> Error
changePwError InvalidCurrentPassword = StdError badCredentials
changePwError ChangePasswordNoIdentity = StdError (errorDescriptionToWai (noIdentity 1))
changePwError ChangePasswordMustDiffer = StdError changePasswordMustDiffer

changeHandleError :: ChangeHandleError -> Error
changeHandleError ChangeHandleNoIdentity = StdError (errorDescriptionToWai (noIdentity 2))
changeHandleError ChangeHandleExists = StdError handleExists
changeHandleError ChangeHandleInvalid = StdError invalidHandle
changeHandleError ChangeHandleManagedByScim = StdError $ propertyManagedByScim "handle"

legalHoldLoginError :: LegalHoldLoginError -> Error
legalHoldLoginError LegalHoldLoginNoBindingTeam = StdError noBindingTeam
legalHoldLoginError LegalHoldLoginLegalHoldNotEnabled = StdError legalHoldNotEnabled
legalHoldLoginError (LegalHoldLoginError e) = loginError e
legalHoldLoginError (LegalHoldReAuthError e) = reauthError e

loginError :: LoginError -> Error
loginError LoginFailed = StdError badCredentials
loginError LoginSuspended = StdError accountSuspended
loginError LoginEphemeral = StdError accountEphemeral
loginError LoginPendingActivation = StdError accountPending
loginError (LoginThrottled wait) =
  RichError
    loginsTooFrequent
    ()
    [("Retry-After", toByteString' (retryAfterSeconds wait))]
loginError (LoginBlocked wait) =
  RichError
    tooManyFailedLogins
    ()
    [("Retry-After", toByteString' (retryAfterSeconds wait))]

authError :: AuthError -> Error
authError AuthInvalidUser = StdError badCredentials
authError AuthInvalidCredentials = StdError badCredentials
authError AuthSuspended = StdError accountSuspended
authError AuthEphemeral = StdError accountEphemeral
authError AuthPendingInvitation = StdError accountPending

reauthError :: ReAuthError -> Error
reauthError ReAuthMissingPassword = StdError (errorDescriptionToWai missingAuthError)
reauthError (ReAuthError e) = authError e

zauthError :: ZAuth.Failure -> Error
zauthError ZAuth.Expired = StdError authTokenExpired
zauthError ZAuth.Falsified = StdError authTokenInvalid
zauthError ZAuth.Invalid = StdError authTokenInvalid
zauthError ZAuth.Unsupported = StdError authTokenUnsupported

clientError :: ClientError -> Error
clientError ClientNotFound = StdError (errorDescriptionToWai clientNotFound)
clientError (ClientDataError e) = clientDataError e
clientError (ClientUserNotFound _) = StdError (errorDescriptionToWai invalidUser)
clientError ClientLegalHoldCannotBeRemoved = StdError can'tDeleteLegalHoldClient
clientError ClientLegalHoldCannotBeAdded = StdError can'tAddLegalHoldClient
clientError (ClientFederationError e) = fedError e
clientError ClientCapabilitiesCannotBeRemoved = StdError clientCapabilitiesCannotBeRemoved
clientError ClientMissingLegalholdConsent = StdError (errorDescriptionToWai missingLegalholdConsent)

fedError :: FederationError -> Error
fedError = StdError . federationErrorToWai

idtError :: RemoveIdentityError -> Error
idtError LastIdentity = StdError lastIdentity
idtError NoPassword = StdError noPassword
idtError NoIdentity = StdError (errorDescriptionToWai (noIdentity 3))

propDataError :: PropertiesDataError -> Error
propDataError TooManyProperties = StdError tooManyProperties

clientDataError :: ClientDataError -> Error
clientDataError TooManyClients = StdError (errorDescriptionToWai tooManyClients)
clientDataError (ClientReAuthError e) = reauthError e
clientDataError ClientMissingAuth = StdError (errorDescriptionToWai missingAuthError)
clientDataError MalformedPrekeys = StdError (errorDescriptionToWai malformedPrekeys)

deleteUserError :: DeleteUserError -> Error
deleteUserError DeleteUserInvalid = StdError (errorDescriptionToWai invalidUser)
deleteUserError DeleteUserInvalidCode = StdError invalidCode
deleteUserError DeleteUserInvalidPassword = StdError badCredentials
deleteUserError DeleteUserMissingPassword = StdError (errorDescriptionToWai missingAuthError)
deleteUserError (DeleteUserPendingCode t) = RichError deletionCodePending (DeletionCodeTimeout t) []
deleteUserError DeleteUserOwnerDeletingSelf = StdError ownerDeletingSelf

accountStatusError :: AccountStatusError -> Error
accountStatusError InvalidAccountStatus = StdError invalidAccountStatus

phoneError :: PhoneException -> Error
phoneError PhoneNumberUnreachable = StdError invalidPhone
phoneError PhoneNumberBarred = StdError blacklistedPhone
phoneError (PhoneBudgetExhausted t) = RichError phoneBudgetExhausted (PhoneBudgetTimeout t) []

updateProfileError :: UpdateProfileError -> Error
updateProfileError DisplayNameManagedByScim = StdError (propertyManagedByScim "name")
updateProfileError (ProfileNotFound _) = StdError (errorDescriptionToWai userNotFound)

-- WAI Errors -----------------------------------------------------------------

tooManyProperties :: Wai.Error
tooManyProperties = Wai.mkError status403 "too-many-properties" "Too many properties"

propertyKeyTooLarge :: Wai.Error
propertyKeyTooLarge = Wai.mkError status403 "property-key-too-large" "The property key is too large."

propertyValueTooLarge :: Wai.Error
propertyValueTooLarge = Wai.mkError status403 "property-value-too-large" "The property value is too large"

clientCapabilitiesCannotBeRemoved :: Wai.Error
clientCapabilitiesCannotBeRemoved = Wai.mkError status409 "client-capabilities-cannot-be-removed" "You can only add capabilities to a client, not remove them."

invalidTransition :: Wai.Error
invalidTransition = Wai.mkError status403 "bad-conn-update" "Invalid status transition."

noEmail :: Wai.Error
noEmail = Wai.mkError status403 "no-email" "This operation requires the user to have a verified email address."

lastIdentity :: Wai.Error
lastIdentity = Wai.mkError status403 "last-identity" "The last user identity (email or phone number) cannot be removed."

noPassword :: Wai.Error
noPassword = Wai.mkError status403 "no-password" "The user has no password."

invalidEmail :: Wai.Error
invalidEmail = Wai.mkError status400 "invalid-email" "Invalid e-mail address."

invalidPwResetKey :: Wai.Error
invalidPwResetKey = Wai.mkError status400 "invalid-key" "Invalid email or mobile number for password reset."

resetPasswordMustDiffer :: Wai.Error
resetPasswordMustDiffer = Wai.mkError status409 "password-must-differ" "For password reset, new and old password must be different."

changePasswordMustDiffer :: Wai.Error
changePasswordMustDiffer = Wai.mkError status409 "password-must-differ" "For password change, new and old password must be different."

invalidPhone :: Wai.Error
invalidPhone = Wai.mkError status400 "invalid-phone" "Invalid mobile phone number."

invalidInvitationCode :: Wai.Error
invalidInvitationCode = Wai.mkError status400 "invalid-invitation-code" "Invalid invitation code."

missingIdentity :: Wai.Error
missingIdentity = Wai.mkError status403 "missing-identity" "Using an invitation code requires registering the given email and/or phone."

invalidPwResetCode :: Wai.Error
invalidPwResetCode = Wai.mkError status400 "invalid-code" "Invalid password reset code."

duplicatePwResetCode :: Wai.Error
duplicatePwResetCode = Wai.mkError status409 "code-exists" "A password reset is already in progress."

userKeyExists :: Wai.Error
userKeyExists = Wai.mkError status409 "key-exists" "The given e-mail address or phone number is in use."

emailExists :: Wai.Error
emailExists = Wai.mkError status409 "email-exists" "The given e-mail address is in use."

phoneExists :: Wai.Error
phoneExists = Wai.mkError status409 "phone-exists" "The given phone number is in use."

handleExists :: Wai.Error
handleExists = Wai.mkError status409 "handle-exists" "The given handle is already taken."

invalidHandle :: Wai.Error
invalidHandle = Wai.mkError status400 "invalid-handle" "The given handle is invalid."

badRequest :: LText -> Wai.Error
badRequest = Wai.mkError status400 "bad-request"

loginCodePending :: Wai.Error
loginCodePending = Wai.mkError status403 "pending-login" "A login code is still pending."

loginCodeNotFound :: Wai.Error
loginCodeNotFound = Wai.mkError status404 "no-pending-login" "No login code was found."

accountPending :: Wai.Error
accountPending = Wai.mkError status403 "pending-activation" "Account pending activation."

accountSuspended :: Wai.Error
accountSuspended = Wai.mkError status403 "suspended" "Account suspended."

accountEphemeral :: Wai.Error
accountEphemeral = Wai.mkError status403 "ephemeral" "Account is ephemeral."

badCredentials :: Wai.Error
badCredentials = Wai.mkError status403 "invalid-credentials" "Authentication failed."

newPasswordMustDiffer :: Wai.Error
newPasswordMustDiffer = Wai.mkError status409 "password-must-differ" "For provider password change or reset, new and old password must be different."

notFound :: LText -> Wai.Error
notFound = Wai.mkError status404 "not-found"

invalidCode :: Wai.Error
invalidCode = Wai.mkError status403 "invalid-code" "Invalid verification code"

invalidAccountStatus :: Wai.Error
invalidAccountStatus = Wai.mkError status400 "invalid-status" "The specified account status cannot be set."

activationKeyNotFound :: Wai.Error
activationKeyNotFound = notFound "Activation key not found."

invalidActivationCode :: LText -> Wai.Error
invalidActivationCode = Wai.mkError status404 "invalid-code"

activationCodeNotFound :: Wai.Error
activationCodeNotFound = invalidActivationCode "Activation key/code not found or invalid."

deletionCodePending :: Wai.Error
deletionCodePending = Wai.mkError status403 "pending-delete" "A verification code for account deletion is still pending."

whitelistError :: Wai.Error
whitelistError = Wai.mkError status403 "unauthorized" "Unauthorized e-mail address or phone number."

blacklistedEmail :: Wai.Error
blacklistedEmail =
  Wai.mkError
    status403
    "blacklisted-email"
    "The given e-mail address has been blacklisted due to a permanent bounce \
    \or a complaint."

blacklistedPhone :: Wai.Error
blacklistedPhone =
  Wai.mkError
    status403
    "blacklisted-phone"
    "The given phone number has been blacklisted due to suspected abuse \
    \or a complaint."

passwordExists :: Wai.Error
passwordExists =
  Wai.mkError
    status403
    "password-exists"
    "The operation is not permitted because the user has a password set."

phoneBudgetExhausted :: Wai.Error
phoneBudgetExhausted =
  Wai.mkError
    status403
    "phone-budget-exhausted"
    "The SMS or voice call budget for the given phone number has been \
    \exhausted. Please try again later. Repeated exhaustion of the SMS or \
    \voice call budget is considered abuse of the API and may result in \
    \permanent blacklisting of the phone number."

authMissingCookie :: Wai.Error
authMissingCookie = Wai.mkError status403 "invalid-credentials" "Missing cookie"

authInvalidCookie :: Wai.Error
authInvalidCookie = Wai.mkError status403 "invalid-credentials" "Invalid cookie"

authMissingToken :: Wai.Error
authMissingToken = Wai.mkError status403 "invalid-credentials" "Missing token"

authMissingCookieAndToken :: Wai.Error
authMissingCookieAndToken = Wai.mkError status403 "invalid-credentials" "Missing cookie and token"

invalidUserToken :: Wai.Error
invalidUserToken = Wai.mkError status403 "invalid-credentials" "Invalid user token"

invalidAccessToken :: Wai.Error
invalidAccessToken = Wai.mkError status403 "invalid-credentials" "Invalid access token"

missingAccessToken :: Wai.Error
missingAccessToken = Wai.mkError status403 "invalid-credentials" "Missing access token"

authTokenMismatch :: Wai.Error
authTokenMismatch = Wai.mkError status403 "invalid-credentials" "Token mismatch"

authTokenExpired :: Wai.Error
authTokenExpired = Wai.mkError status403 "invalid-credentials" "Token expired"

authTokenInvalid :: Wai.Error
authTokenInvalid = Wai.mkError status403 "invalid-credentials" "Invalid token"

authTokenUnsupported :: Wai.Error
authTokenUnsupported = Wai.mkError status403 "invalid-credentials" "Unsupported token operation for this token type"

incorrectPermissions :: Wai.Error
incorrectPermissions = Wai.mkError status403 "invalid-permissions" "Copy permissions must be a subset of self permissions"

-- | User's relation to the team is not what we expect it to be. Examples:
--
-- * Requested action requires the user to be a team member, but the user doesn't belong to
--   the team.
--
-- * Requested action requires the user to be a team owner.
--
-- * Requested action can't be performed if the user is the only team owner left in the team.
insufficientTeamPermissions :: Wai.Error
insufficientTeamPermissions = Wai.mkError status403 "insufficient-permissions" "Insufficient team permissions"

noBindingTeam :: Wai.Error
noBindingTeam = Wai.mkError status403 "no-binding-team" "Operation allowed only on binding teams"

propertyManagedByScim :: LText -> Wai.Error
propertyManagedByScim prop = Wai.mkError status403 "managed-by-scim" $ "Updating \"" <> prop <> "\" is not allowed, because it is managed by SCIM"

sameBindingTeamUsers :: Wai.Error
sameBindingTeamUsers = Wai.mkError status403 "same-binding-team-users" "Operation not allowed to binding team users."

ownerDeletingSelf :: Wai.Error
ownerDeletingSelf =
  Wai.mkError
    status403
    "no-self-delete-for-team-owner"
    "Team owners are not allowed to delete themselves.  Ask a fellow owner."

tooManyTeamInvitations :: Wai.Error
tooManyTeamInvitations = Wai.mkError status403 "too-many-team-invitations" "Too many team invitations for this team."

tooManyTeamMembers :: Wai.Error
tooManyTeamMembers = Wai.mkError status403 "too-many-team-members" "Too many members in this team."

-- | docs/reference/user/registration.md {#RefRestrictRegistration}.
userCreationRestricted :: Wai.Error
userCreationRestricted = Wai.mkError status403 "user-creation-restricted" "This instance does not allow creation of personal users or teams."

-- | In contrast to 'tooManyFailedLogins', this is about too many *successful* logins.
loginsTooFrequent :: Wai.Error
loginsTooFrequent = Wai.mkError status429 "client-error" "Logins too frequent"

tooManyFailedLogins :: Wai.Error
tooManyFailedLogins = Wai.mkError status403 "client-error" "Too many failed logins"

tooLargeRichInfo :: Wai.Error
tooLargeRichInfo = Wai.mkError status413 "too-large-rich-info" "Rich info has exceeded the limit"

internalServerError :: Wai.Error
internalServerError = Wai.mkError status500 "internal-server-error" "Internal Server Error"

invalidRange :: LText -> Wai.Error
invalidRange = Wai.mkError status400 "client-error"

--- Legalhold
can'tDeleteLegalHoldClient :: Wai.Error
can'tDeleteLegalHoldClient =
  Wai.mkError
    status400
    "client-error"
    "LegalHold clients cannot be deleted. LegalHold must be disabled on this user by an admin"

can'tAddLegalHoldClient :: Wai.Error
can'tAddLegalHoldClient =
  Wai.mkError
    status400
    "client-error"
    "LegalHold clients cannot be added manually. LegalHold must be enabled on this user by an admin"

legalHoldNotEnabled :: Wai.Error
legalHoldNotEnabled = Wai.mkError status403 "legalhold-not-enabled" "LegalHold must be enabled and configured on the team first"

-- (the tautological constraint in the type signature is added so that once we remove the
-- feature, ghc will guide us here.)
customerExtensionBlockedDomain :: (DomainsBlockedForRegistration ~ DomainsBlockedForRegistration) => Domain -> Wai.Error
customerExtensionBlockedDomain domain = Wai.mkError (mkStatus 451 "Unavailable For Legal Reasons") "domain-blocked-for-registration" msg
  where
    msg =
      "[Customer extension] the email domain " <> cs (show domain)
        <> " that you are attempting to register a user with has been \
           \blocked for creating wire users.  Please contact your IT department."
