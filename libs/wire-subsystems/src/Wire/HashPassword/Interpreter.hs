{-# LANGUAGE RecordWildCards #-}

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

module Wire.HashPassword.Interpreter where

import Data.ByteArray
import Data.Misc
import Data.Text.Encoding qualified as Text
import Imports
import Polysemy
import Util.Options
import Wire.API.Password
import Wire.API.Password.Argon2id
import Wire.API.Password.Scrypt
import Wire.HashPassword
import Wire.HashPassword.Argon2id
import Wire.HashPassword.Scrypt
import Wire.Sem.Random (Random)

runHashPassword ::
  forall r.
  (Member Random r) =>
  PasswordHashingOptions ->
  InterpreterFor HashPassword r
runHashPassword opts =
  interpret $
    \case
      HashPassword6 pw6 -> hashPasswordImpl opts pw6
      HashPassword8 pw8 -> hashPasswordImpl opts pw8
      VerifyPasswordWithStatus plain pwd -> pure $ verifyPasswordWithStatusImpl opts plain pwd

hashPasswordImpl :: (Member Random r) => PasswordHashingOptions -> PlainTextPassword' t -> Sem r Password
hashPasswordImpl opts = case opts of
  PasswordHashingArgon2id o -> mkSafePasswordArgon2id (argon2OptsFromHashingOpts o)
  PasswordHashingScrypt -> mkSafePasswordScrypt

verifyPasswordWithStatusImpl :: PasswordHashingOptions -> PlainTextPassword' t -> Password -> (Bool, PasswordStatus)
verifyPasswordWithStatusImpl hashingOpts (fromPlainTextPassword -> plain) hashed =
  case hashed of
    (Argon2Password Argon2HashedPassword {..}) ->
      let producedKey = hashPasswordWithOptions opts (Text.encodeUtf8 plain) salt
       in (hashedKey `constEq` producedKey, PasswordStatusOk)
    (ScryptPassword ScryptHashedPassword {..}) ->
      let producedKey = hashPasswordWithParams params (Text.encodeUtf8 plain) salt
          status = case hashingOpts of
            PasswordHashingScrypt -> PasswordStatusOk
            PasswordHashingArgon2id {} -> PasswordStatusNeedsUpdate
       in (hashedKey `constEq` producedKey, status)
