{-# LANGUAGE DeriveDataTypeable #-}

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
import qualified Network.Wai.Utilities.Error as Wai
import qualified Wire.API.Federation.GRPC.Types as Proto

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

data CreateUserTeam = CreateUserTeam
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

-- | Outcome of the invariants check in 'Brig.API.User.changeEmail'.
data ChangeEmailResult
  = -- | The request was successful, user needs to verify the new email address
    ChangeEmailNeedsActivation !(User, Activation, Email)
  | -- | The user asked to change the email address to the one already owned
    ChangeEmailIdempotent

-- | Typed response of the @put /self/email@ end-point (returned in
-- 'Brig.API.User.changeSelfEmail'.
data ChangeEmailResponse
  = ChangeEmailResponseIdempotent
  | ChangeEmailResponseNeedsActivation

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

data UpdateProfileError
  = DisplayNameManagedByScim
  | ProfileNotFound UserId

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
    InvalidUser OpaqueUserId
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
  | EmailManagedByScim

data ChangeHandleError
  = ChangeHandleNoIdentity
  | ChangeHandleExists
  | ChangeHandleInvalid
  | ChangeHandleManagedByScim

data SendActivationCodeError
  = InvalidRecipient UserKey
  | UserKeyInUse UserKey
  | ActivationBlacklistedUserKey UserKey

data SendLoginCodeError
  = SendLoginInvalidPhone Phone
  | SendLoginPasswordExists

data FederationError
  = FederationRpcError Text
  | FederationInvalidResponseCode Word32
  | FederationInvalidResponseBody Text
  | FederationRemoteError Proto.OutwardError
  | FederationUnavailable Text
  | FederationNotImplemented
  | FederationNotConfigured

data ClientError
  = ClientNotFound
  | ClientDataError !ClientDataError
  | ClientUserNotFound !OpaqueUserId
  | ClientLegalHoldCannotBeRemoved
  | ClientLegalHoldCannotBeAdded
  | ClientFederationError FederationError

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
  | DeleteUserOwnerDeletingSelf

data AccountStatusError
  = InvalidAccountStatus

-------------------------------------------------------------------------------
-- Exceptions

-- | A user name was unexpectedly not found for an existing user ID.
data UserDisplayNameNotFound = UserDisplayNameNotFound !UserId
  deriving (Typeable)

instance Exception UserDisplayNameNotFound

instance Show UserDisplayNameNotFound where
  show (UserDisplayNameNotFound uid) = "User name not found for user: " ++ show uid

data UserProfileNotFound = UserProfileNotFound !UserId
  deriving (Typeable)

instance Exception UserProfileNotFound

instance Show UserProfileNotFound where
  show (UserProfileNotFound uid) = "User profile not found for user: " ++ show uid
