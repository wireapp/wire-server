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

module Wire.HashPassword.Scrypt where

import Crypto.KDF.Scrypt (Parameters (..), generate)
import Data.ByteArray
import Data.Misc
import Data.Text.Encoding qualified as Text
import Imports
import Polysemy
import Wire.API.Password
import Wire.API.Password.Scrypt
import Wire.Sem.Random (Random)
import Wire.Sem.Random qualified as Random

mkSafePasswordScrypt :: (Member Random r) => PlainTextPassword' t -> Sem r Password
mkSafePasswordScrypt =
  fmap ScryptPassword . hashPasswordScrypt . Text.encodeUtf8 . fromPlainTextPassword

hashPasswordScrypt :: (Member Random r) => ByteString -> Sem r ScryptHashedPassword
hashPasswordScrypt password = do
  salt <- Random.bytes $ fromIntegral defaultScryptParams.saltLength
  let params = defaultScryptParams
  let hashedKey = hashPasswordWithParams params password salt
  pure $! ScryptHashedPassword {..}

defaultScryptParams :: ScryptParameters
defaultScryptParams =
  ScryptParameters
    { saltLength = 32,
      rounds = 14,
      blockSize = 8,
      parallelism = 1,
      outputLength = 64
    }

fromScrypt :: ScryptParameters -> Parameters
fromScrypt scryptParams =
  Parameters
    { n = 2 ^ scryptParams.rounds,
      r = fromIntegral scryptParams.blockSize,
      p = fromIntegral scryptParams.parallelism,
      outputLength = 64
    }

hashPasswordWithParams ::
  ( ByteArrayAccess password,
    ByteArrayAccess salt
  ) =>
  ScryptParameters ->
  password ->
  salt ->
  ByteString
hashPasswordWithParams parameters password salt =
  convert (generate (fromScrypt parameters) password salt :: Bytes)
