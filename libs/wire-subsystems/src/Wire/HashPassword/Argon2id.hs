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

module Wire.HashPassword.Argon2id where

import Crypto.Error
import Crypto.KDF.Argon2 qualified as Argon2
import Data.Misc
import Data.Text.Encoding qualified as Text
import Imports
import Polysemy
import Wire.API.Password
import Wire.API.Password.Argon2id
import Wire.Sem.Random (Random)
import Wire.Sem.Random qualified as Random

mkSafePasswordArgon2id :: (Member Random r) => Argon2.Options -> PlainTextPassword' t -> Sem r Password
mkSafePasswordArgon2id opts = fmap Argon2Password . hashPasswordArgon2id opts . Text.encodeUtf8 . fromPlainTextPassword

hashPasswordArgon2id :: (Member Random r) => Argon2.Options -> ByteString -> Sem r Argon2HashedPassword
hashPasswordArgon2id opts pwd = do
  salt <- Random.bytes 16
  pure $! hashPasswordArgon2idWithSalt opts salt pwd

hashPasswordArgon2idWithSalt :: Argon2.Options -> ByteString -> ByteString -> Argon2HashedPassword
hashPasswordArgon2idWithSalt opts salt pwd = do
  let hashedKey = hashPasswordWithOptions opts pwd salt
   in Argon2HashedPassword {..}

hashPasswordWithOptions :: Argon2.Options -> ByteString -> ByteString -> ByteString
hashPasswordWithOptions opts password salt = do
  let tagSize = 16
  case (Argon2.hash opts password salt tagSize) of
    -- CryptoFailed occurs when salt, output or input are too small/big.
    -- since we control those values ourselves, it should never have a runtime error
    CryptoFailed cErr -> error $ "Impossible error: " <> show cErr
    CryptoPassed hash -> hash
