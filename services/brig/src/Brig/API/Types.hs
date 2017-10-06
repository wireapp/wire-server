{-# LANGUAGE DeriveDataTypeable #-}

module Brig.API.Types
    ( module Brig.API.Types
    , Activation          (..)
    , ActivationError     (..)
    , ClientDataError     (..)
    , PropertiesDataError (..)
    , AuthError           (..)
    , ReAuthError         (..)
    , RetryAfter          (..)
    , foldKey
    ) where

import Brig.Data.Activation (ActivationError (..), Activation (..))
import Brig.Data.Client (ClientDataError (..))
import Brig.Data.User (AuthError (..), ReAuthError (..))
import Brig.Data.UserKey (UserKey, foldKey)
import Brig.Data.Properties (PropertiesDataError (..))
import Brig.Types
import Brig.Types.Code (Timeout)
import Brig.Types.Intra
import Brig.User.Auth.Cookie (RetryAfter (..))
import Control.Exception
import Data.Id
import Data.Typeable

-------------------------------------------------------------------------------
-- Successes

data CreateUserResult = CreateUserResult
    { createdAccount :: !UserAccount
        -- ^ The newly created user account.
    , createdEmailActivation :: !(Maybe Activation)
        -- ^ Activation data for the registered email address, if any.
    , createdPhoneActivation :: !(Maybe Activation)
        -- ^ Activation data for the registered phone number, if any.
    }

data ConnectionResult
    = ConnectionCreated !UserConnection
    | ConnectionExists  !UserConnection

data ActivationResult
    = ActivationSuccess !(Maybe UserIdentity) !Bool
        -- ^ The key/code was valid and successfully activated.
    | ActivationPass
        -- ^ The key/code was valid but already recently activated.

-------------------------------------------------------------------------------
-- Failures

data CreateUserError
    = InvalidInvitationCode
    | MissingIdentity
    | PhoneActivationError ActivationError
    | InvalidEmail Email
    | InvalidPhone Phone
    | DuplicateUserKey UserKey
    | BlacklistedUserKey UserKey
    | TooManyTeamMembers

data InvitationError
    = InviteeEmailExists UserId
    | InviteInvalidEmail Email
    | InviteBlacklistedEmail Email

data ConnectionError
    = TooManyConnections UserId
        -- ^ Max. # of 'Accepted' / 'Sent' connections reached
        -- when attempting to create or accept a connection.
    | InvalidTransition UserId Relation
        -- ^ An invalid connection status change.
    | InvalidUser UserId
        -- ^ The target user in an connection attempt is invalid, e.g. not activated.
    | NotConnected UserId UserId
        -- ^ An attempt at updating a non-existent connection.
    | ConnectNoIdentity
        -- ^ An attempt at creating a connection from an account with
        -- no verified user identity.
    | ConnectBlacklistedUserKey UserKey
        -- ^ An attempt at creating an invitation to a blacklisted user key.
    | ConnectInvalidEmail Email
        -- ^ An attempt at creating an invitation to an invalid email address.
    | ConnectInvalidPhone Phone
        -- ^ An attempt at creating an invitation to an invalid phone nbumber.

data PasswordResetError
    = PasswordResetInProgress
    | InvalidPasswordResetKey
    | InvalidPasswordResetCode

data LoginError
    = LoginFailed
    | LoginSuspended
    | LoginPendingActivation
    | LoginThrottled RetryAfter

data ChangePasswordError
    = InvalidCurrentPassword
    | ChangePasswordNoIdentity

data ChangePhoneError
    = PhoneExists !Phone
    | InvalidNewPhone !Phone

data ChangeEmailError
    = InvalidNewEmail !Email
    | EmailExists !Email
    | ChangeBlacklistedEmail !Email

data ChangeHandleError
    = ChangeHandleNoIdentity
    | ChangeHandleExists
    | ChangeHandleInvalid

data SendActivationCodeError
    = InvalidRecipient UserKey
    | UserKeyInUse UserKey
    | ActivationBlacklistedUserKey UserKey

data SendLoginCodeError
    = SendLoginInvalidPhone Phone
    | SendLoginPasswordExists

data ClientError
    = ClientNotFound
    | ClientDataError !ClientDataError
    | ClientUserNotFound !UserId

data RemoveIdentityError
    = LastIdentity
    | NoPassword
    | NoIdentity

data DeleteUserError
    = DeleteUserInvalid
    | DeleteUserInvalidCode
    | DeleteUserInvalidPassword
    | DeleteUserMissingPassword
    | DeleteUserPendingCode Timeout
    | DeleteUserOnlyOwner

data AccountStatusError
    = InvalidAccountStatus

-------------------------------------------------------------------------------
-- Exceptions

-- | A user name was unexpectedly not found for an existing user ID.
data UserNameNotFound = UserNameNotFound !UserId
    deriving (Typeable)

instance Exception UserNameNotFound

instance Show UserNameNotFound where
    show (UserNameNotFound uid) = "User name not found for user: " ++ show uid

data UserProfileNotFound = UserProfileNotFound !UserId
    deriving (Typeable)

instance Exception UserProfileNotFound

instance Show UserProfileNotFound where
    show (UserProfileNotFound uid) = "User profile not found for user: " ++ show uid
