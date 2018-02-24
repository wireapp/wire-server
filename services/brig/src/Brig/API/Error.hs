{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}

module Brig.API.Error where

import Control.Monad.Error.Class hiding (Error)
import Data.Aeson
import Data.ByteString.Conversion
import Data.Text.Lazy (Text)
import Network.HTTP.Types.Header
import Network.HTTP.Types.Status

import Brig.API.Types
import Brig.Phone (PhoneException (..))
import Brig.Types (DeletionCodeTimeout (..))
import Brig.Types.Common (PhoneBudgetTimeout (..))

import qualified Data.HashMap.Strict         as HashMap
import qualified Data.ZAuth.Validation       as ZAuth
import qualified Network.Wai.Utilities.Error as Wai

data Error where
    StdError  :: !Wai.Error -> Error
    RichError :: ToJSON a => !Wai.Error -> !a -> [Header] -> Error

waiError :: Error -> Wai.Error
waiError (StdError  e    ) = e
waiError (RichError e _ _) = e

throwStd :: MonadError Error m => Wai.Error -> m a
throwStd = throwError . StdError

throwRich :: (MonadError Error m, ToJSON x) => Wai.Error -> x -> [Header] -> m a
throwRich e x h = throwError (RichError e x h)

instance ToJSON Error where
    toJSON (StdError  e    ) = toJSON e
    toJSON (RichError e x _) = case (toJSON e, toJSON x) of
        (Object o1, Object o2) -> Object (HashMap.union o1 o2)
        (        j,         _) -> j

-- Error Mapping ----------------------------------------------------------

connError :: ConnectionError -> Error
connError TooManyConnections{}          = StdError connectionLimitReached
connError InvalidTransition{}           = StdError invalidTransition
connError NotConnected{}                = StdError notConnected
connError InvalidUser{}                 = StdError invalidUser
connError ConnectNoIdentity{}           = StdError noIdentity
connError (ConnectBlacklistedUserKey k) = StdError $ foldKey (const blacklistedEmail) (const blacklistedPhone) k
connError ConnectInvalidEmail{}         = StdError invalidEmail
connError ConnectInvalidPhone{}         = StdError invalidPhone
connError ConnectSameBindingTeamUsers   = StdError sameBindingTeamUsers

actError :: ActivationError -> Error
actError (UserKeyExists          _) = StdError userKeyExists
actError (InvalidActivationCode  e) = StdError (invalidActivationCode e)
actError (InvalidActivationEmail _) = StdError invalidEmail
actError (InvalidActivationPhone _) = StdError invalidPhone

pwResetError :: PasswordResetError -> Error
pwResetError InvalidPasswordResetKey            = StdError invalidPwResetKey
pwResetError InvalidPasswordResetCode           = StdError invalidPwResetCode
pwResetError (PasswordResetInProgress Nothing)  = StdError duplicatePwResetCode
pwResetError (PasswordResetInProgress (Just t)) = RichError duplicatePwResetCode ()
    [("Retry-After", toByteString' t)]

newUserError :: CreateUserError -> Error
newUserError InvalidInvitationCode    = StdError invalidInvitationCode
newUserError MissingIdentity          = StdError missingIdentity
newUserError (InvalidEmail _)         = StdError invalidEmail
newUserError (InvalidPhone _)         = StdError invalidPhone
newUserError (DuplicateUserKey _)     = StdError userKeyExists
newUserError (EmailActivationError e) = actError e
newUserError (PhoneActivationError e) = actError e
newUserError (BlacklistedUserKey k)   = StdError $ foldKey (const blacklistedEmail) (const blacklistedPhone) k
newUserError TooManyTeamMembers       = StdError tooManyTeamMembers

sendLoginCodeError :: SendLoginCodeError -> Error
sendLoginCodeError (SendLoginInvalidPhone _) = StdError invalidPhone
sendLoginCodeError SendLoginPasswordExists   = StdError passwordExists

sendActCodeError :: SendActivationCodeError -> Error
sendActCodeError (InvalidRecipient k)             = StdError $ foldKey (const invalidEmail) (const invalidPhone) k
sendActCodeError (UserKeyInUse     _)             = StdError userKeyExists
sendActCodeError (ActivationBlacklistedUserKey k) = StdError $ foldKey (const blacklistedEmail) (const blacklistedPhone) k

changeEmailError :: ChangeEmailError -> Error
changeEmailError (InvalidNewEmail        _) = StdError invalidEmail
changeEmailError (EmailExists            _) = StdError userKeyExists
changeEmailError (ChangeBlacklistedEmail _) = StdError blacklistedEmail

changePhoneError :: ChangePhoneError -> Error
changePhoneError (InvalidNewPhone _) = StdError invalidPhone
changePhoneError (PhoneExists     _) = StdError userKeyExists

changePwError :: ChangePasswordError -> Error
changePwError InvalidCurrentPassword   = StdError badCredentials
changePwError ChangePasswordNoIdentity = StdError noIdentity

changeHandleError :: ChangeHandleError -> Error
changeHandleError ChangeHandleNoIdentity  = StdError noIdentity
changeHandleError ChangeHandleExists      = StdError handleExists
changeHandleError ChangeHandleInvalid     = StdError invalidHandle

loginError :: LoginError -> Error
loginError LoginFailed            = StdError badCredentials
loginError LoginSuspended         = StdError accountSuspended
loginError LoginPendingActivation = StdError accountPending
loginError (LoginThrottled wait)  = RichError loginsTooFrequent ()
    [("Retry-After", toByteString' (retryAfterSeconds wait))]

authError :: AuthError -> Error
authError AuthInvalidUser        = StdError badCredentials
authError AuthInvalidCredentials = StdError badCredentials
authError AuthSuspended          = StdError accountSuspended

reauthError :: ReAuthError -> Error
reauthError ReAuthMissingPassword = StdError missingAuthError
reauthError (ReAuthError e)       = authError e

zauthError :: ZAuth.Failure -> Error
zauthError ZAuth.Expired   = StdError authTokenExpired
zauthError ZAuth.Falsified = StdError authTokenInvalid
zauthError ZAuth.Invalid   = StdError authTokenInvalid

clientError :: ClientError -> Error
clientError ClientNotFound         = StdError clientNotFound
clientError (ClientDataError e)    = clientDataError e
clientError (ClientUserNotFound _) = StdError invalidUser

idtError :: RemoveIdentityError -> Error
idtError LastIdentity = StdError lastIdentity
idtError NoPassword   = StdError noPassword
idtError NoIdentity   = StdError noIdentity

propDataError :: PropertiesDataError -> Error
propDataError TooManyProperties = StdError tooManyProperties

clientDataError :: ClientDataError -> Error
clientDataError TooManyClients        = StdError tooManyClients
clientDataError (ClientReAuthError e) = reauthError e
clientDataError ClientMissingAuth     = StdError missingAuthError
clientDataError MalformedPrekeys      = StdError malformedPrekeys

deleteUserError :: DeleteUserError -> Error
deleteUserError DeleteUserInvalid         = StdError invalidUser
deleteUserError DeleteUserInvalidCode     = StdError invalidCode
deleteUserError DeleteUserInvalidPassword = StdError badCredentials
deleteUserError DeleteUserMissingPassword = StdError missingAuthError
deleteUserError (DeleteUserPendingCode t) = RichError deletionCodePending (DeletionCodeTimeout t) []
deleteUserError DeleteUserOnlyOwner       = StdError noOtherOwner

accountStatusError :: AccountStatusError -> Error
accountStatusError InvalidAccountStatus = StdError invalidAccountStatus

phoneError :: PhoneException -> Error
phoneError PhoneNumberUnreachable   = StdError invalidPhone
phoneError PhoneNumberBarred        = StdError blacklistedPhone
phoneError (PhoneBudgetExhausted t) = RichError phoneBudgetExhausted (PhoneBudgetTimeout t) []

-- WAI Errors -----------------------------------------------------------------

tooManyProperties :: Wai.Error
tooManyProperties = Wai.Error status403 "too-many-properties" "Too many properties"

propertyKeyTooLarge :: Wai.Error
propertyKeyTooLarge = Wai.Error status403 "property-key-too-large" "The property key is too large."

propertyValueTooLarge :: Wai.Error
propertyValueTooLarge = Wai.Error status403 "property-value-too-large" "The property value is too large"

connectionLimitReached :: Wai.Error
connectionLimitReached = Wai.Error status403 "connection-limit" "Too many sent/accepted connections."

missingAuthError :: Wai.Error
missingAuthError = Wai.Error status403 "missing-auth" "Re-authentication via password required."

clientNotFound :: Wai.Error
clientNotFound = Wai.Error status404 "client-not-found" "Client not found"

tooManyClients :: Wai.Error
tooManyClients = Wai.Error status403 "too-many-clients" "Too many clients"

malformedPrekeys :: Wai.Error
malformedPrekeys = Wai.Error status400 "bad-request" "Malformed prekeys uploaded."

invalidUser :: Wai.Error
invalidUser = Wai.Error status400 "invalid-user" "Invalid user."

invalidTransition :: Wai.Error
invalidTransition = Wai.Error status403 "bad-conn-update" "Invalid status transition."

notConnected :: Wai.Error
notConnected = Wai.Error status403 "no-connection" "No connection exists between users."

noIdentity :: Wai.Error
noIdentity = Wai.Error status403 "no-identity" "The user has no verified identity (email or phone number)."

noEmail :: Wai.Error
noEmail = Wai.Error status403 "no-email" "This operation requires the user to have a verified email address."

lastIdentity :: Wai.Error
lastIdentity = Wai.Error status403 "last-identity" "The last user identity (email or phone number) cannot be removed."

noPassword :: Wai.Error
noPassword = Wai.Error status403 "no-password" "The user has no password."

invalidEmail :: Wai.Error
invalidEmail = Wai.Error status400 "invalid-email" "Invalid e-mail address."

invalidPwResetKey :: Wai.Error
invalidPwResetKey = Wai.Error status400 "invalid-key" "Invalid email or mobile number for password reset."

invalidPhone :: Wai.Error
invalidPhone = Wai.Error status400 "invalid-phone" "Invalid mobile phone number."

invalidInvitationCode :: Wai.Error
invalidInvitationCode = Wai.Error status400 "invalid-invitation-code" "Invalid invitation code."

missingIdentity :: Wai.Error
missingIdentity = Wai.Error status403 "missing-identity" "Using an invitation code requires registering the given email and/or phone."

invalidPwResetCode :: Wai.Error
invalidPwResetCode = Wai.Error status400 "invalid-code" "Invalid password reset code."

duplicatePwResetCode :: Wai.Error
duplicatePwResetCode = Wai.Error status409 "code-exists" "A password reset is already in progress."

userKeyExists :: Wai.Error
userKeyExists = Wai.Error status409 "key-exists" "The given e-mail address or phone number is in use."

emailExists :: Wai.Error
emailExists = Wai.Error status409 "email-exists" "The given e-mail address is in use."

handleExists :: Wai.Error
handleExists = Wai.Error status409 "handle-exists" "The given handle is already taken."

invalidHandle :: Wai.Error
invalidHandle = Wai.Error status400 "invalid-handle" "The given handle is invalid."

badRequest :: Text -> Wai.Error
badRequest = Wai.Error status400 "bad-request"

loginCodePending :: Wai.Error
loginCodePending = Wai.Error status403 "pending-login" "A login code is still pending."

loginCodeNotFound :: Wai.Error
loginCodeNotFound = Wai.Error status404 "no-pending-login" "No login code was found."

accountPending :: Wai.Error
accountPending = Wai.Error status403 "pending-activation" "Account pending activation."

accountSuspended :: Wai.Error
accountSuspended = Wai.Error status403 "suspended" "Account suspended."

badCredentials :: Wai.Error
badCredentials = Wai.Error status403 "invalid-credentials" "Authentication failed."

notFound :: Text -> Wai.Error
notFound = Wai.Error status404 "not-found"

userNotFound :: Wai.Error
userNotFound = notFound "User not found."

handleNotFound :: Wai.Error
handleNotFound = notFound "Handle not found."

invalidCode :: Wai.Error
invalidCode = Wai.Error status403 "invalid-code" "Invalid verification code"

invalidAccountStatus :: Wai.Error
invalidAccountStatus = Wai.Error status400 "invalid-status" "The specified account status cannot be set."

activationKeyNotFound :: Wai.Error
activationKeyNotFound = notFound "Activation key not found."

invalidActivationCode :: Text -> Wai.Error
invalidActivationCode = Wai.Error status404 "invalid-code"

activationCodeNotFound :: Wai.Error
activationCodeNotFound = invalidActivationCode "Activation key/code not found or invalid."

deletionCodePending :: Wai.Error
deletionCodePending = Wai.Error status403 "pending-delete" "A verification code for account deletion is still pending."

whitelistError :: Wai.Error
whitelistError = Wai.Error status403 "unauthorized" "Unauthorized e-mail address or phone number."

blacklistedEmail :: Wai.Error
blacklistedEmail = Wai.Error status403 "blacklisted-email"
    "The given e-mail address has been blacklisted due to a permanent bounce \
    \or a complaint."

blacklistedPhone :: Wai.Error
blacklistedPhone = Wai.Error status403 "blacklisted-phone"
    "The given phone number has been blacklisted due to suspected abuse \
    \or a complaint."

passwordExists :: Wai.Error
passwordExists = Wai.Error status403 "password-exists"
    "The operation is not permitted because the user has a password set."

phoneBudgetExhausted :: Wai.Error
phoneBudgetExhausted = Wai.Error status403 "phone-budget-exhausted"
    "The SMS or voice call budget for the given phone number has been \
    \exhausted. Please try again later. Repeated exhaustion of the SMS or \
    \voice call budget is considered abuse of the API and may result in \
    \permanent blacklisting of the phone number."

authMissingCookie :: Wai.Error
authMissingCookie = Wai.Error status403 "invalid-credentials" "Missing cookie"

authMissingToken :: Wai.Error
authMissingToken = Wai.Error status403 "invalid-credentials" "Missing token"

authMissingCookieAndToken :: Wai.Error
authMissingCookieAndToken = Wai.Error status403 "invalid-credentials" "Missing cookie and token"

invalidUserToken :: Wai.Error
invalidUserToken = Wai.Error status403 "invalid-credentials" "Invalid user token"

invalidAccessToken :: Wai.Error
invalidAccessToken = Wai.Error status403 "invalid-credentials" "Invalid access token"

authTokenMismatch :: Wai.Error
authTokenMismatch = Wai.Error status403 "invalid-credentials" "Token mismatch"

authTokenExpired :: Wai.Error
authTokenExpired = Wai.Error status403 "invalid-credentials" "Token expired"

authTokenInvalid :: Wai.Error
authTokenInvalid = Wai.Error status403 "invalid-credentials" "Invalid token"

incorrectPermissions :: Wai.Error
incorrectPermissions = Wai.Error status403 "invalid-permissions" "Copy permissions must be a subset of self permissions"

insufficientTeamPermissions :: Wai.Error
insufficientTeamPermissions = Wai.Error status403 "insufficient-permissions" "Insufficient team permissions"

noBindingTeam :: Wai.Error
noBindingTeam = Wai.Error status403 "no-binding-team" "Operation allowed only on binding teams"

sameBindingTeamUsers :: Wai.Error
sameBindingTeamUsers = Wai.Error status403 "same-binding-team-users" "Operation not allowed to binding team users."

noOtherOwner :: Wai.Error
noOtherOwner = Wai.Error status403 "no-other-owner" "You are trying to remove or downgrade\
                                \ an owner. Promote another team member before proceeding."

tooManyTeamInvitations :: Wai.Error
tooManyTeamInvitations = Wai.Error status403 "too-many-team-invitations" "Too many team invitations for this team."

tooManyTeamMembers :: Wai.Error
tooManyTeamMembers = Wai.Error status403 "too-many-team-members" "Too many members in this team."

loginsTooFrequent :: Wai.Error
loginsTooFrequent = Wai.Error status429 "client-error" "Logins too frequent"

internalServerError :: Wai.Error
internalServerError = Wai.Error status500 "internal-server-error" "Internal Server Error"

failedQueueEvent :: Wai.Error
failedQueueEvent = Wai.Error status500 "event-queue-failed" "Failed to queue the event, MD5 mismatch. Try again later"

invalidRange :: Text -> Wai.Error
invalidRange = Wai.Error status400 "client-error"
