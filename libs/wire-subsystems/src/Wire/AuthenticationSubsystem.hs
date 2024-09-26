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

module Wire.AuthenticationSubsystem where

import Data.Id
import Data.Misc
import Data.Qualified
import Imports
import Polysemy
import Wire.API.User
import Wire.API.User.Password (PasswordResetCode, PasswordResetIdentity)
import Wire.UserKeyStore

data AuthenticationSubsystem m a where
  VerifyPasswordE :: Local UserId -> PlainTextPassword6 -> AuthenticationSubsystem m ()
  CreatePasswordResetCode :: EmailKey -> AuthenticationSubsystem m ()
  ResetPassword :: PasswordResetIdentity -> PasswordResetCode -> PlainTextPassword8 -> AuthenticationSubsystem m ()
  VerifyPassword :: UserId -> PlainTextPassword6 -> AuthenticationSubsystem m Bool
  -- For testing
  InternalLookupPasswordResetCode :: EmailKey -> AuthenticationSubsystem m (Maybe PasswordResetPair)

makeSem ''AuthenticationSubsystem

verifyProviderPassword ::
  (Member AuthenticationSubsystem r) =>
  ProviderId ->
  PlainTextPassword6 ->
  Sem r Bool
verifyProviderPassword pid pwd =
  let uid = Id . toUUID $ pid
   in verifyPassword uid pwd
