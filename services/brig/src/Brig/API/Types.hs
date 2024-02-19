{-# LANGUAGE DeriveDataTypeable #-}

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
    ListUsersById (..),
    foldKey,
  )
where

import Brig.Data.Activation (Activation (..), ActivationError (..))
import Brig.Data.Client (ClientDataError (..))
import Brig.Data.Properties (PropertiesDataError (..))
import Brig.Data.User (AuthError (..), ReAuthError (..))
import Brig.Data.UserKey (UserKey, foldKey)
import Brig.Types.Intra
import Data.Code
import Data.Id
import Data.Jwt.Tools (DPoPTokenGenerationError (..))
import Data.Qualified
import Data.RetryAfter
import Imports
import Network.Wai.Utilities.Error qualified as Wai
import Wire.API.Federation.Error
import Wire.API.User

-------------------------------------------------------------------------------
-- Successes

data CreateUserResult = CreateUserResult
  { -- | The newly created user account.
    createdAccount :: !UserAccount,
    -- | Activation data for the registered email address, if any.
    createdEmailActivation :: !(Maybe Activation),
    -- | Activation data for the registered phone number, if any.
    createdPhoneActivation :: !(Maybe Activation),
    -- | Info of a team just created/joined
    createdUserTeam :: !(Maybe CreateUserTeam)
  }
  deriving (Show)

data CreateUserTeam = CreateUserTeam
  { createdTeamId :: !TeamId,
    createdTeamName :: !Text
  }
  deriving (Show)

data ActivationResult
  = -- | The key/code was valid and successfully activated.
    ActivationSuccess !(Maybe UserIdentity) !Bool
  | -- | The key/code was valid but already recently activated.
    ActivationPass

-- | Outcome of the invariants check in 'Brig.API.User.changeEmail'.
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
  | UserCreationRestricted
  | -- | Some precondition on another Wire service failed. We propagate this error.
    ExternalPreconditionFailed Wai.Error

data InvitationError
  = InviteeEmailExists UserId
  | InviteInvalidEmail Email
  | InviteBlacklistedEmail Email

data ConnectionError
  = -- | Max. # of 'Accepted' / 'Sent' connections reached
    -- when attempting to create or accept a connection.
    TooManyConnections UserId
  | -- | An invalid connection status change.
    InvalidTransition UserId
  | -- | The target user in an connection attempt is invalid, e.g. not activated.
    InvalidUser (Qualified UserId)
  | -- | An attempt at updating a non-existent connection.
    NotConnected UserId (Qualified UserId)
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
  | -- | Something doesn't work because somebody has a LH device and somebody else has not granted consent.
    ConnectMissingLegalholdConsent
  | -- | Same as above, but because old clients that don't support LH are still in the game.
    ConnectMissingLegalholdConsentOldClients
  | -- | Remote connection creation or update failed because of a federation error
    ConnectFederationError FederationError
  | -- | The teams of the users that want to connect do not federate
    ConnectTeamFederationError

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
  | LoginCodeRequired
  | LoginCodeInvalid
  | LoginPasswordUpdateRequired

data VerificationCodeError
  = VerificationCodeRequired
  | VerificationCodeNoPendingCode
  | VerificationCodeNoEmail

data ChangeEmailError
  = InvalidNewEmail !Email !String
  | EmailExists !Email
  | ChangeBlacklistedEmail !Email
  | EmailManagedByScim

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
  | ClientFederationError FederationError
  | ClientCapabilitiesCannotBeRemoved
  | ClientMissingLegalholdConsentOldClients
  | ClientMissingLegalholdConsent
  | ClientCodeAuthenticationFailed
  | ClientCodeAuthenticationRequired

data DeleteUserError
  = DeleteUserInvalid
  | DeleteUserInvalidCode
  | DeleteUserInvalidPassword
  | DeleteUserMissingPassword
  | DeleteUserPendingCode Timeout
  | DeleteUserOwnerDeletingSelf
  | DeleteUserVerificationCodeThrottled RetryAfter

data AccountStatusError
  = InvalidAccountStatus
  | AccountNotFound

data VerificationCodeThrottledError
  = VerificationCodeThrottled RetryAfter

data CertEnrollmentError
  = NonceNotFound
  | RustError DPoPTokenGenerationError
  | KeyBundleError
  | MisconfiguredRequestUrl
  | ClientIdSyntaxError
  | NotATeamUser
  | MissingHandle
  | MissingName

-------------------------------------------------------------------------------
-- Exceptions

-- | A user name was unexpectedly not found for an existing user ID.
newtype UserDisplayNameNotFound = UserDisplayNameNotFound UserId
  deriving (Typeable)

instance Exception UserDisplayNameNotFound

instance Show UserDisplayNameNotFound where
  show (UserDisplayNameNotFound uid) = "User name not found for user: " ++ show uid

newtype UserProfileNotFound = UserProfileNotFound UserId
  deriving (Typeable)

instance Exception UserProfileNotFound

instance Show UserProfileNotFound where
  show (UserProfileNotFound uid) = "User profile not found for user: " ++ show uid
