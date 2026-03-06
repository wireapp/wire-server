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
{-# LANGUAGE TemplateHaskell #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.AuthenticationSubsystem where

import Data.Id
import Data.Misc
import Data.Qualified
import Data.RetryAfter
import Data.ZAuth.Token qualified as ZAuth
import Imports
import Polysemy
import Wire.API.User
import Wire.API.User.Auth
import Wire.API.User.Password (PasswordResetCode, PasswordResetIdentity)
import Wire.Arbitrary
import Wire.AuthenticationSubsystem.Error
import Wire.AuthenticationSubsystem.ZAuth
import Wire.HashPassword
import Wire.UserKeyStore

-- | Policy for handling existing cookies with the same label when issuing a new cookie.
-- The default policy is 'RevokeSameLabel': for a given account and label, only one active
-- cookie should remain. This keeps label semantics deterministic and prevents stale parallel
-- sessions under the same logical device label.
--
-- 'KeepSameLabel' is a narrowly scoped relaxation used during cookie renewal. In that case
-- the predecessor cookie may temporarily coexist with its successor for the same device:
-- it is linked via 'cookieSucc' and written with a TTL, so it will be garbage-collected.
-- This is accepted to bridge in-flight requests during rotation while preserving eventual
-- single-cookie-per-(account,label) behavior.
data SameLabelPolicy = RevokeSameLabel | KeepSameLabel
  deriving (Show, Eq, Ord, Generic)
  deriving (Arbitrary) via (GenericUniform SameLabelPolicy)

data AuthenticationSubsystem m a where
  -- Password Management
  CreatePasswordResetCode :: EmailKey -> AuthenticationSubsystem m ()
  ResetPassword :: PasswordResetIdentity -> PasswordResetCode -> PlainTextPassword8 -> AuthenticationSubsystem m ()
  -- Password Verification
  AuthenticateEither :: UserId -> PlainTextPassword6 -> AuthenticationSubsystem m (Either AuthError ())
  ReauthenticateEither :: UserId -> Maybe PlainTextPassword6 -> AuthenticationSubsystem m (Either ReAuthError ())
  VerifyUserPassword :: UserId -> PlainTextPassword6 -> AuthenticationSubsystem r (Bool, PasswordStatus)
  VerifyUserPasswordError :: Local UserId -> PlainTextPassword6 -> AuthenticationSubsystem m ()
  VerifyProviderPassword :: ProviderId -> PlainTextPassword6 -> AuthenticationSubsystem r (Bool, PasswordStatus)
  -- Cookie Management
  NewCookie ::
    (UserTokenLike t) =>
    UserId ->
    Maybe ClientId ->
    CookieType ->
    Maybe CookieLabel ->
    SameLabelPolicy ->
    AuthenticationSubsystem m (Cookie (ZAuth.Token t))
  NewCookieLimited ::
    (UserTokenLike t) =>
    UserId ->
    Maybe ClientId ->
    CookieType ->
    Maybe CookieLabel ->
    SameLabelPolicy ->
    AuthenticationSubsystem m (Either RetryAfter (Cookie (ZAuth.Token t)))
  RevokeCookies :: UserId -> [CookieId] -> [CookieLabel] -> AuthenticationSubsystem m ()
  -- For testing
  InternalLookupPasswordResetCode :: EmailKey -> AuthenticationSubsystem m (Maybe PasswordResetPair)

makeSem ''AuthenticationSubsystem
