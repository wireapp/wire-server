{-# LANGUAGE DeriveDataTypeable #-}

module Brig.API.Types
  ( module Brig.API.Types,
    Activation (..),
    ActivationError (..),
    ClientDataError (..),
    PropertiesDataError (..),
    AuthError (..),
    ReAuthError (..),
    LegalHoldLoginError (..),
    RetryAfter (..),
    foldKey,
  )
where

import Brig.Data.Activation (Activation (..), ActivationError (..))
import Brig.Data.Client (ClientDataError (..))
import Brig.Data.Properties (PropertiesDataError (..))
import Brig.Data.User (AuthError (..), ReAuthError (..))
import Brig.Data.UserKey (UserKey, foldKey)
import Brig.Types
import Brig.Types.Code (Timeout)
import Brig.Types.Intra
import Brig.User.Auth.Cookie (RetryAfter (..))
import Data.Id
import Imports

-------------------------------------------------------------------------------
-- Successes

data CreateUserResult
  = CreateUserResult
      { -- | The newly created user account.
        createdAccount :: !UserAccount,
        -- | Activation data for the registered email address, if any.
        createdEmailActivation :: !(Maybe Activation),
        -- | Activation data for the registered phone number, if any.
        createdPhoneActivation :: !(Maybe Activation),
        -- | Info of a team just created/joined
        createdUserTeam :: !(Maybe CreateUserTeam)
      }

data CreateUserTeam
  = CreateUserTeam
      { createdTeamId :: !TeamId,
        createdTeamName :: !Text
      }

data ConnectionResult
  = ConnectionCreated !UserConnection
  | ConnectionExists !UserConnection

data ActivationResult
  = -- | The key/code was valid and successfully activated.
    ActivationSuccess !(Maybe UserIdentity) !Bool
  | -- | The key/code was valid but already recently activated.
    ActivationPass

data ChangeEmailResult
  = -- | The request was successful, user needs to verify the new email address
    ChangeEmailNeedsActivation !(User, Activation, Email)
  | -- | The user asked to change the email address to the one already owned
    ChangeEmailIdempotent

-------------------------------------------------------------------------------
-- Failures

data CreateUserError
  = InvalidInvitationCode
  | MissingIdentity
  | EmailActivationError ActivationError
  | PhoneActivationError ActivationError
  | InvalidEmail Email String
  | InvalidPhone Phone
  | DuplicateUserKey UserKey
  | BlacklistedUserKey UserKey
  | TooManyTeamMembers

data InvitationError
  = InviteeEmailExists UserId
  | InviteInvalidEmail Email
  | InviteBlacklistedEmail Email

data ConnectionError
  = -- | Max. # of 'Accepted' / 'Sent' connections reached
    -- when attempting to create or accept a connection.
    TooManyConnections UserId
  | -- | An invalid connection status change.
    InvalidTransition UserId Relation
  | -- | The target user in an connection attempt is invalid, e.g. not activated.
    InvalidUser UserId
  | -- | An attempt at updating a non-existent connection.
    NotConnected UserId UserId
  | -- | An attempt at creating a connection from an account with
    -- no verified user identity.
    ConnectNoIdentity
  | -- | An attempt at creating an invitation to a blacklisted user key.
    ConnectBlacklistedUserKey UserKey
  | -- | An attempt at creating an invitation to an invalid email address.
    ConnectInvalidEmail Email String
  | -- | An attempt at creating an invitation to an invalid phone nbumber.
    ConnectInvalidPhone Phone
  | -- | An attempt at creating a connection with another user from the same binding team.
    ConnectSameBindingTeamUsers

data PasswordResetError
  = PasswordResetInProgress (Maybe Timeout)
  | InvalidPasswordResetKey
  | InvalidPasswordResetCode
  | ResetPasswordMustDiffer

data LegalHoldLoginError
  = LegalHoldLoginNoBindingTeam
  | LegalHoldLoginLegalHoldNotEnabled
  | LegalHoldLoginError LoginError
  | LegalHoldReAuthError ReAuthError

data LoginError
  = LoginFailed
  | LoginSuspended
  | LoginEphemeral
  | LoginPendingActivation
  | LoginThrottled RetryAfter
  | LoginBlocked RetryAfter

data ChangePasswordError
  = InvalidCurrentPassword
  | ChangePasswordNoIdentity
  | ChangePasswordMustDiffer

data ChangePhoneError
  = PhoneExists !Phone
  | InvalidNewPhone !Phone

data ChangeEmailError
  = InvalidNewEmail !Email !String
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
  | ClientLegalHoldCannotBeRemoved
  | ClientLegalHoldCannotBeAdded

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
