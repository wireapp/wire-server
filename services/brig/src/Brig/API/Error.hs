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

module Brig.API.Error where

import Brig.API.Types
import Control.Monad.Error.Class
import Data.ByteString.Conversion
import Data.Domain (Domain)
import Data.Jwt.Tools (DPoPTokenGenerationError (..))
import Data.Text.Lazy as LT
import Data.ZAuth.Validation qualified as ZAuth
import Imports
import Network.HTTP.Types.Status
import Network.Wai.Utilities.Error qualified as Wai
import Wire.API.Error
import Wire.API.Error.Brig qualified as E
import Wire.API.Federation.Error
import Wire.API.User
import Wire.Error

throwStd :: (MonadError HttpError m) => Wai.Error -> m a
throwStd = throwError . StdError

-- Error Mapping ----------------------------------------------------------

connError :: ConnectionError -> HttpError
connError TooManyConnections {} = StdError (errorToWai @'E.ConnectionLimitReached)
connError InvalidTransition {} = StdError (errorToWai @'E.InvalidTransition)
connError NotConnected {} = StdError (errorToWai @'E.NotConnected)
connError InvalidUser {} = StdError (errorToWai @'E.InvalidUser)
connError ConnectNoIdentity {} = StdError (errorToWai @'E.NoIdentity)
connError (ConnectBlacklistedUserKey _) = StdError blacklistedEmail
connError (ConnectInvalidEmail _ _) = StdError (errorToWai @'E.InvalidEmail)
connError ConnectInvalidPhone {} = StdError (errorToWai @'E.InvalidPhone)
connError ConnectSameBindingTeamUsers = StdError sameBindingTeamUsers
connError ConnectMissingLegalholdConsentOldClients = StdError (errorToWai @'E.MissingLegalholdConsentOldClients)
connError ConnectMissingLegalholdConsent = StdError (errorToWai @'E.MissingLegalholdConsent)
connError (ConnectFederationError e) = fedError e
connError ConnectTeamFederationError = StdError (errorToWai @'E.TeamsNotFederating)

actError :: ActivationError -> HttpError
actError (UserKeyExists _) = StdError (errorToWai @'E.UserKeyExists)
actError InvalidActivationCodeWrongUser = StdError (errorToWai @'E.InvalidActivationCodeWrongUser)
actError InvalidActivationCodeWrongCode = StdError (errorToWai @'E.InvalidActivationCodeWrongCode)
actError (InvalidActivationEmail _ _) = StdError (errorToWai @'E.InvalidEmail)
actError (InvalidActivationPhone _) = StdError (errorToWai @'E.InvalidPhone)

pwResetError :: PasswordResetError -> HttpError
pwResetError InvalidPasswordResetKey = StdError (errorToWai @'E.InvalidPasswordResetKey)
pwResetError InvalidPasswordResetCode = StdError (errorToWai @'E.InvalidPasswordResetCode)
pwResetError (PasswordResetInProgress Nothing) = StdError (errorToWai @'E.PasswordResetInProgress)
pwResetError (PasswordResetInProgress (Just t)) =
  RichError
    (errorToWai @'E.PasswordResetInProgress)
    ()
    [("Retry-After", toByteString' t)]
pwResetError ResetPasswordMustDiffer = StdError (errorToWai @'E.ResetPasswordMustDiffer)

sendActCodeError :: SendActivationCodeError -> HttpError
sendActCodeError (InvalidRecipient _) = StdError $ errorToWai @'E.InvalidEmail
sendActCodeError (UserKeyInUse _) = StdError (errorToWai @'E.UserKeyExists)
sendActCodeError (ActivationBlacklistedUserKey _) = StdError blacklistedEmail

changeEmailError :: ChangeEmailError -> HttpError
changeEmailError (InvalidNewEmail _ _) = StdError (errorToWai @'E.InvalidEmail)
changeEmailError (EmailExists _) = StdError (errorToWai @'E.UserKeyExists)
changeEmailError (ChangeBlacklistedEmail _) = StdError blacklistedEmail
changeEmailError EmailManagedByScim = StdError $ propertyManagedByScim "email"

legalHoldLoginError :: LegalHoldLoginError -> HttpError
legalHoldLoginError LegalHoldLoginNoBindingTeam = StdError noBindingTeam
legalHoldLoginError LegalHoldLoginLegalHoldNotEnabled = StdError legalHoldNotEnabled
legalHoldLoginError (LegalHoldLoginError e) = loginError e
legalHoldLoginError (LegalHoldReAuthError e) = reauthError e

loginError :: LoginError -> HttpError
loginError LoginFailed = StdError (errorToWai @'E.BadCredentials)
loginError LoginSuspended = StdError (errorToWai @'E.AccountSuspended)
loginError LoginEphemeral = StdError (errorToWai @'E.AccountEphemeral)
loginError LoginPendingActivation = StdError (errorToWai @'E.AccountPending)
loginError LoginPasswordUpdateRequired = StdError (errorToWai @'E.PasswordIsStale)
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
loginError LoginCodeRequired = StdError (errorToWai @'E.CodeAuthenticationRequired)
loginError LoginCodeInvalid = StdError (errorToWai @'E.CodeAuthenticationFailed)

authError :: AuthError -> HttpError
authError AuthInvalidUser = StdError (errorToWai @'E.BadCredentials)
authError AuthInvalidCredentials = StdError (errorToWai @'E.BadCredentials)
authError AuthSuspended = StdError (errorToWai @'E.AccountSuspended)
authError AuthEphemeral = StdError (errorToWai @'E.AccountEphemeral)
authError AuthPendingInvitation = StdError (errorToWai @'E.AccountPending)

reauthError :: ReAuthError -> HttpError
reauthError ReAuthMissingPassword = StdError (errorToWai @'E.MissingAuth)
reauthError (ReAuthError e) = authError e
reauthError ReAuthCodeVerificationRequired = StdError verificationCodeRequired
reauthError ReAuthCodeVerificationNoPendingCode = StdError verificationCodeNoPendingCode
reauthError ReAuthCodeVerificationNoEmail = StdError verificationCodeNoEmail

zauthError :: ZAuth.Failure -> HttpError
zauthError ZAuth.Expired = StdError authTokenExpired
zauthError ZAuth.Falsified = StdError authTokenInvalid
zauthError ZAuth.Invalid = StdError authTokenInvalid
zauthError ZAuth.Unsupported = StdError authTokenUnsupported

clientError :: ClientError -> HttpError
clientError ClientNotFound = StdError (errorToWai @'E.ClientNotFound)
clientError (ClientDataError e) = clientDataError e
clientError (ClientUserNotFound _) = StdError (errorToWai @'E.InvalidUser)
clientError ClientLegalHoldCannotBeRemoved = StdError can'tDeleteLegalHoldClient
clientError ClientLegalHoldCannotBeAdded = StdError can'tAddLegalHoldClient
clientError (ClientFederationError e) = fedError e
clientError ClientCapabilitiesCannotBeRemoved = StdError clientCapabilitiesCannotBeRemoved
clientError ClientMissingLegalholdConsentOldClients = StdError (errorToWai @'E.MissingLegalholdConsentOldClients)
clientError ClientMissingLegalholdConsent = StdError (errorToWai @'E.MissingLegalholdConsent)
clientError ClientCodeAuthenticationFailed = StdError verificationCodeAuthFailed
clientError ClientCodeAuthenticationRequired = StdError verificationCodeRequired

-- Note that UnknownError, FfiError, and ImplementationError semantically should rather be 500s than 400s.
-- However, the errors returned from the FFI are not always correct,
-- and we don't want to bombard our environments with 500 log messages, so we treat them as 400s, for now.
certEnrollmentError :: CertEnrollmentError -> HttpError
certEnrollmentError (RustError NoError) = StdError $ Wai.mkError status400 "internal-error" "The server experienced an internal error during DPoP token generation. Unexpected NoError."
certEnrollmentError (RustError UnknownError) = StdError $ Wai.mkError status400 "internal-error" "The server experienced an internal error during DPoP token generation. Unknown error."
certEnrollmentError (RustError FfiError) = StdError $ Wai.mkError status400 "internal-error" "The server experienced an internal error during DPoP token generation"
certEnrollmentError (RustError ImplementationError) = StdError $ Wai.mkError status400 "internal-error" "The server experienced an internal error during DPoP token generation. Unexpected ImplementationError."
certEnrollmentError (RustError DpopSyntaxError) = StdError $ Wai.mkError status400 "client-token-parse-error" "The client JWT DPoP could not be parsed"
certEnrollmentError (RustError DpopTypError) = StdError $ Wai.mkError status400 "client-token-type-error" "The client JWT DPoP 'typ' must be 'dpop+jwt'"
certEnrollmentError (RustError DpopUnsupportedAlgorithmError) = StdError $ Wai.mkError status400 "client-token-unsupported-alg" "DPoP signature algorithm (alg) in JWT header is not a supported algorithm (ES256, ES384, Ed25519)"
certEnrollmentError (RustError DpopInvalidSignatureError) = StdError $ Wai.mkError status400 "client-token-bad-signature" "DPoP signature does not correspond to the public key (jwk) in the JWT header"
certEnrollmentError (RustError ClientIdMismatchError) = StdError $ Wai.mkError status400 "client-token-bad-client-id" "The client id does not correspond to the (sub) claim expressed as URI"
certEnrollmentError (RustError BackendNonceMismatchError) = StdError $ Wai.mkError status400 "client-token-bad-nonce" "The backend nonce does not correspond to the (nonce) claim in DPoP token (base64url encoded)"
certEnrollmentError (RustError HtuMismatchError) = StdError $ Wai.mkError status400 "client-token-bad-uri" "The request uri does not correspond to the (htu) claim in DPoP token"
certEnrollmentError (RustError HtmMismatchError) = StdError $ Wai.mkError status400 "client-token-bad-method" "The request method does not correspond to the (htm) claim in DPoP token"
certEnrollmentError (RustError MissingJtiError) = StdError $ Wai.mkError status400 "client-token-jti-missing" "(jti) claim is absent in DPoP token"
certEnrollmentError (RustError MissingChallengeError) = StdError $ Wai.mkError status400 "client-token-chal-missing" "(chal) claim is absent in DPoP token"
certEnrollmentError (RustError MissingIatError) = StdError $ Wai.mkError status400 "client-token-iat-missing" "(iat) claim is absent in DPoP token"
certEnrollmentError (RustError IatError) = StdError $ Wai.mkError status400 "client-token-bad-iat" "(iat) claim in DPoP token is not earlier of now (with max_skew_secs leeway)"
certEnrollmentError (RustError MissingExpError) = StdError $ Wai.mkError status400 "client-token-exp-missing" "(exp) claim is absent in DPoP token"
certEnrollmentError (RustError ExpMismatchError) = StdError $ Wai.mkError status400 "client-token-exp-too-large" "(exp) claim in DPoP token is larger than supplied [max_expiration]"
certEnrollmentError (RustError Expired) = StdError $ Wai.mkError status400 "client-token-exp-too-small" "(exp) claim in DPoP token is sooner than now (with [max_skew_secs] leeway)"
certEnrollmentError (RustError InvalidUserId) = StdError $ Wai.mkError status400 "invalid-user-id" "userId supplied across the FFI is invalid"
certEnrollmentError (RustError NotYetValid) = StdError $ Wai.mkError status400 "not-yet-valid" "Client DPoP token 'nbf' claim is in the future"
certEnrollmentError (RustError JwtSimpleError) = StdError $ Wai.mkError status400 "jwt-simple-error" "Bubbling up errors"
certEnrollmentError (RustError RandError) = StdError $ Wai.mkError status400 "rand-error" "Bubbling up errors"
certEnrollmentError (RustError Sec1Error) = StdError $ Wai.mkError status400 "sec1-error" "Bubbling up errors"
certEnrollmentError (RustError UrlParseError) = StdError $ Wai.mkError status400 "url-parse-error" "Bubbling up errors"
certEnrollmentError (RustError UuidError) = StdError $ Wai.mkError status400 "uuid-error" "Bubbling up errors"
certEnrollmentError (RustError Utf8Error) = StdError $ Wai.mkError status400 "utf8-error" "Bubbling up errors"
certEnrollmentError (RustError Base64DecodeError) = StdError $ Wai.mkError status400 "base64-decode-error" "Bubbling up errors"
certEnrollmentError (RustError JsonError) = StdError $ Wai.mkError status400 "json-error" "Bubbling up errors"
certEnrollmentError (RustError InvalidJsonPath) = StdError $ Wai.mkError status400 "invalid-json-path" "Bubbling up errors"
certEnrollmentError (RustError JsonPathError) = StdError $ Wai.mkError status400 "json-path-error" "Bubbling up errors"
certEnrollmentError (RustError InvalidJwkThumbprint) = StdError $ Wai.mkError status400 "invalid-jwk-thumbprint" "Bubbling up errors"
certEnrollmentError (RustError MissingDpopHeader) = StdError $ Wai.mkError status400 "missing-dpop-header" "Bubbling up errors"
certEnrollmentError (RustError MissingIssuer) = StdError $ Wai.mkError status400 "missing-issuer" "Bubbling up errors"
certEnrollmentError (RustError DpopChallengeMismatch) = StdError $ Wai.mkError status400 "dpop-challenge-mismatch" "Bubbling up errors"
certEnrollmentError (RustError DpopHtuMismatch) = StdError $ Wai.mkError status400 "dpop-htu-mismatch" "Bubbling up errors"
certEnrollmentError (RustError DpopHtmMismatch) = StdError $ Wai.mkError status400 "dpop-htm-mismatch" "Bubbling up errors"
certEnrollmentError (RustError InvalidBackendKeys) = StdError $ Wai.mkError status400 "invalid-backend-keys" "Bubbling up errors"
certEnrollmentError (RustError InvalidClientId) = StdError $ Wai.mkError status400 "invalid-client-id" "Bubbling up errors"
certEnrollmentError (RustError UnsupportedApiVersion) = StdError $ Wai.mkError status400 "unsupported-api-version" "Bubbling up errors"
certEnrollmentError (RustError UnsupportedScope) = StdError $ Wai.mkError status400 "unsupported-scope" "Bubbling up errors"
certEnrollmentError (RustError DpopHandleMismatch) = StdError $ Wai.mkError status400 "dpop-handle-mismatch" "Bubbling up errors"
certEnrollmentError (RustError DpopTeamMismatch) = StdError $ Wai.mkError status400 "dpop-team-mismatch" "Bubbling up errors"
certEnrollmentError (RustError DpopDisplayNameMismatch) = StdError $ Wai.mkError status400 "dpop-display-name-mismatch" "Bubbling up errors"
certEnrollmentError NonceNotFound = StdError $ Wai.mkError status400 "client-token-bad-nonce" "The client sent an unacceptable anti-replay nonce"
certEnrollmentError MisconfiguredRequestUrl = StdError $ Wai.mkError status500 "misconfigured-request-url" "The request url cannot be derived from optSettings.setFederationDomain in brig.yaml"
certEnrollmentError KeyBundleError = StdError $ Wai.mkError status404 "no-server-key-bundle" "The key bundle required for the certificate enrollment process could not be found"
certEnrollmentError ClientIdSyntaxError = StdError $ Wai.mkError status400 "client-token-id-parse-error" "The client id could not be parsed"
certEnrollmentError NotATeamUser = StdError $ Wai.mkError status400 "not-a-team-user" "The user is not a team user"
certEnrollmentError MissingHandle = StdError $ Wai.mkError status400 "missing-handle" "The user has no handle"
certEnrollmentError MissingName = StdError $ Wai.mkError status400 "missing-name" "The user has no name"

fedError :: FederationError -> HttpError
fedError = StdError . federationErrorToWai

clientDataError :: ClientDataError -> HttpError
clientDataError TooManyClients = StdError (errorToWai @'E.TooManyClients)
clientDataError (ClientReAuthError e) = reauthError e
clientDataError ClientMissingAuth = StdError (errorToWai @'E.MissingAuth)
clientDataError MalformedPrekeys = StdError (errorToWai @'E.MalformedPrekeys)
clientDataError MLSPublicKeyDuplicate = StdError (errorToWai @'E.MLSDuplicatePublicKey)
clientDataError KeyPackageDecodingError = StdError (errorToWai @'E.KeyPackageDecodingError)
clientDataError InvalidKeyPackageRef = StdError (errorToWai @'E.InvalidKeyPackageRef)
clientDataError MLSNotEnabled = StdError (errorToWai @'E.MLSNotEnabled)

deleteUserError :: DeleteUserError -> HttpError
deleteUserError DeleteUserInvalid = StdError (errorToWai @'E.InvalidUser)
deleteUserError DeleteUserInvalidCode = StdError (errorToWai @'E.InvalidCode)
deleteUserError DeleteUserInvalidPassword = StdError (errorToWai @'E.BadCredentials)
deleteUserError DeleteUserMissingPassword = StdError (errorToWai @'E.MissingAuth)
deleteUserError (DeleteUserPendingCode t) = RichError deletionCodePending (DeletionCodeTimeout t) []
deleteUserError DeleteUserOwnerDeletingSelf = StdError (errorToWai @'E.OwnerDeletingSelf)
deleteUserError (DeleteUserVerificationCodeThrottled t) =
  verificationCodeThrottledError (VerificationCodeThrottled t)

accountStatusError :: AccountStatusError -> HttpError
accountStatusError InvalidAccountStatus = StdError invalidAccountStatus
accountStatusError AccountNotFound = StdError (notFound "Account not found")

verificationCodeThrottledError :: VerificationCodeThrottledError -> HttpError
verificationCodeThrottledError (VerificationCodeThrottled t) =
  RichError
    (dynErrorToWai $ dynError @(MapError 'E.VerificationCodeThrottled))
    ()
    [("Retry-After", toByteString' (retryAfterSeconds t))]

-- WAI Errors -----------------------------------------------------------------

clientCapabilitiesCannotBeRemoved :: Wai.Error
clientCapabilitiesCannotBeRemoved = Wai.mkError status409 "client-capabilities-cannot-be-removed" "You can only add capabilities to a client, not remove them."

-- One of two cases:
-- (1) the email is in use by any other account or invitation;
-- (2) (when posting an invitation) the email is in use by a member of another team (and we can't steal away those, invitee has to be personal user).
emailExists :: Wai.Error
emailExists = Wai.mkError status409 "email-exists" "The given e-mail address is in use."

badRequest :: LText -> Wai.Error
badRequest = Wai.mkError status400 "bad-request"

loginCodeNotFound :: Wai.Error
loginCodeNotFound = Wai.mkError status404 "no-pending-login" "No login code was found."

notFound :: LText -> Wai.Error
notFound = Wai.mkError status404 "not-found"

invalidAccountStatus :: Wai.Error
invalidAccountStatus = Wai.mkError status400 "invalid-status" "The specified account status cannot be set."

activationKeyNotFound :: Wai.Error
activationKeyNotFound = notFound "Activation key not found."

deletionCodePending :: Wai.Error
deletionCodePending = Wai.mkError status403 "pending-delete" "A verification code for account deletion is still pending."

allowlistError :: Wai.Error
allowlistError = Wai.mkError status403 "unauthorized" "Unauthorized e-mail address"

blacklistedEmail :: Wai.Error
blacklistedEmail =
  Wai.mkError
    status403
    "blacklisted-email"
    "The given e-mail address has been blacklisted due to a permanent bounce \
    \or a complaint."

authMissingCookie :: Wai.Error
authMissingCookie = Wai.mkError status403 "invalid-credentials" "Missing cookie"

authMissingToken :: Wai.Error
authMissingToken = Wai.mkError status403 "invalid-credentials" "Missing token"

authMissingCookieAndToken :: Wai.Error
authMissingCookieAndToken = Wai.mkError status403 "invalid-credentials" "Missing cookie and token"

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

-- | User's relation to the team is not what we expect it to be. Examples:
--
-- * Requested action requires the user to be a team member, but the user doesn't belong to
--   the team.
--
-- * Requested action requires the user to be a team owner.
--
-- * Requested action can't be performed if the user is the only team owner left in the team.
insufficientTeamPermissions :: Wai.Error
insufficientTeamPermissions = errorToWai @'E.InsufficientTeamPermissions

noBindingTeam :: Wai.Error
noBindingTeam = Wai.mkError status403 "no-binding-team" "Operation allowed only on binding teams"

propertyManagedByScim :: LText -> Wai.Error
propertyManagedByScim prop = Wai.mkError status403 "managed-by-scim" $ "Updating \"" <> prop <> "\" is not allowed, because it is managed by SCIM"

sameBindingTeamUsers :: Wai.Error
sameBindingTeamUsers = Wai.mkError status403 "same-binding-team-users" "Operation not allowed to binding team users."

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
customerExtensionBlockedDomain :: Domain -> Wai.Error
customerExtensionBlockedDomain domain = Wai.mkError (mkStatus 451 "Unavailable For Legal Reasons") "domain-blocked-for-registration" msg
  where
    msg =
      "[Customer extension] the email domain "
        <> LT.pack (show domain)
        <> " that you are attempting to register a user with has been \
           \blocked for creating wire users.  Please contact your IT department."

verificationCodeRequired :: Wai.Error
verificationCodeRequired = Wai.mkError status403 "code-authentication-required" "Verification code required."

verificationCodeNoPendingCode :: Wai.Error
verificationCodeNoPendingCode = Wai.mkError status403 "code-authentication-failed" "Code authentication failed (no such code)."

verificationCodeNoEmail :: Wai.Error
verificationCodeNoEmail = Wai.mkError status403 "code-authentication-failed" "Code authentication failed (no such email)."

verificationCodeAuthFailed :: Wai.Error
verificationCodeAuthFailed = Wai.mkError status403 "code-authentication-failed" "Code authentication failed."
