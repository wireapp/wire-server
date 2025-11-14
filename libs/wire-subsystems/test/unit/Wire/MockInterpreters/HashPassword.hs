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

module Wire.MockInterpreters.HashPassword (staticHashPasswordInterpreter, hashPassword) where

import Crypto.KDF.Argon2 as Argon2
import Data.Misc
import Data.Text.Encoding qualified as Text
import Imports
import Polysemy
import Util.Options
import Wire.API.Password as Password
import Wire.HashPassword
import Wire.HashPassword.Argon2id
import Wire.HashPassword.Interpreter

staticHashPasswordInterpreter :: InterpreterFor HashPassword r
staticHashPasswordInterpreter = interpret $ \case
  HashPassword6 password -> hashPassword password
  HashPassword8 password -> hashPassword password
  VerifyPasswordWithStatus plain hashed -> pure $ verifyPasswordWithStatusImpl PasswordHashingScrypt plain hashed

hashPassword :: (Monad m) => PlainTextPassword' t -> m Password
hashPassword password =
  pure . Argon2Password $
    hashPasswordArgon2idWithSalt
      fastArgon2IdOptions
      "9bytesalt"
      (Text.encodeUtf8 (fromPlainTextPassword password))

fastArgon2IdOptions :: Argon2.Options
fastArgon2IdOptions =
  let hashParallelism = 4
   in Argon2.Options
        { variant = Argon2.Argon2id,
          version = Argon2.Version13,
          iterations = 1,
          parallelism = hashParallelism,
          -- This needs to be min 8 * hashParallelism, otherewise we get an
          -- unsafe error
          memory = 8 * hashParallelism
        }
