-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2024 Wire Swiss GmbH <opensource@wire.com>
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
module Wire.AuthenticationSubsystem.Error where

import Data.ZAuth.Validation qualified as ZAuth
import Imports
import Network.HTTP.Types.Status
import Network.Wai.Utilities.Error qualified as Wai
import Wire.API.Error
import Wire.API.Error.Brig qualified as E
import Wire.Error

-- | Authentication errors.
data AuthError
  = AuthInvalidUser
  | AuthInvalidCredentials
  | AuthSuspended
  | AuthEphemeral
  | AuthPendingInvitation
  deriving (Show, Eq)

instance Exception AuthError

-- | Re-authentication errors.
data ReAuthError
  = ReAuthError !AuthError
  | ReAuthMissingPassword
  | ReAuthCodeVerificationRequired
  | ReAuthCodeVerificationNoPendingCode
  | ReAuthCodeVerificationNoEmail
  deriving (Show, Eq)

instance Exception ReAuthError

data AuthenticationSubsystemError
  = AuthenticationSubsystemInvalidPasswordResetKey
  | AuthenticationSubsystemResetPasswordMustDiffer
  | AuthenticationSubsystemInvalidPasswordResetCode
  | AuthenticationSubsystemInvalidPhone
  | AuthenticationSubsystemAllowListError
  | AuthenticationSubsystemBadCredentials
  | AuthenticationSubsystemZAuthFailure ZAuth.Failure
  deriving (Eq, Show)

instance Exception AuthenticationSubsystemError

authenticationSubsystemErrorToHttpError :: AuthenticationSubsystemError -> HttpError
authenticationSubsystemErrorToHttpError =
  StdError . \case
    AuthenticationSubsystemInvalidPasswordResetKey -> errorToWai @E.InvalidPasswordResetKey
    AuthenticationSubsystemInvalidPasswordResetCode -> errorToWai @E.InvalidPasswordResetCode
    AuthenticationSubsystemResetPasswordMustDiffer -> errorToWai @E.ResetPasswordMustDiffer
    AuthenticationSubsystemInvalidPhone -> errorToWai @E.InvalidPhone
    AuthenticationSubsystemAllowListError -> errorToWai @E.AllowlistError
    AuthenticationSubsystemBadCredentials -> errorToWai @E.BadCredentials
    AuthenticationSubsystemZAuthFailure f -> zauthError f

zauthError :: ZAuth.Failure -> Wai.Error
zauthError ZAuth.Expired = authTokenExpired
zauthError ZAuth.Falsified = authTokenInvalid
zauthError ZAuth.Invalid = authTokenInvalid
zauthError ZAuth.Unsupported = authTokenUnsupported

authTokenExpired :: Wai.Error
authTokenExpired = Wai.mkError status403 "invalid-credentials" "Zauth token expired"

authTokenInvalid :: Wai.Error
authTokenInvalid = Wai.mkError status403 "invalid-credentials" "Invalid zauth token"

authTokenUnsupported :: Wai.Error
authTokenUnsupported = Wai.mkError status403 "invalid-credentials" "Unsupported token operation for this token type"
