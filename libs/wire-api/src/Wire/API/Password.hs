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

module Wire.API.Password
  ( Password,
    genPassword,
    mkSafePassword,
    verifyPassword,
  )
where

import Cassandra
import Crypto.Scrypt
import Data.ByteString.Base64 qualified as B64
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Misc
import Data.Text.Encoding qualified as Text
import Imports
import OpenSSL.Random (randBytes)

-- | A derived, stretched password that can be safely stored.
newtype Password = Password
  {fromPassword :: EncryptedPass}

instance Show Password where
  show _ = "<Password>"

instance Cql Password where
  ctype = Tagged BlobColumn

  fromCql (CqlBlob lbs) = pure . Password . EncryptedPass $ toStrict lbs
  fromCql _ = Left "password: expected blob"

  toCql = CqlBlob . fromStrict . getEncryptedPass . fromPassword

-- | Generate a strong, random plaintext password of length 16
-- containing only alphanumeric characters, '+' and '/'.
genPassword :: MonadIO m => m PlainTextPassword8
genPassword =
  liftIO . fmap (plainTextPassword8Unsafe . Text.decodeUtf8 . B64.encode) $
    randBytes 12

-- | Stretch a plaintext password so that it can be safely stored.
mkSafePassword :: MonadIO m => PlainTextPassword' t -> m Password
mkSafePassword = liftIO . fmap Password . encryptPassIO' . pass
  where
    pass = Pass . Text.encodeUtf8 . fromPlainTextPassword

-- | Verify a plaintext password from user input against a stretched
-- password from persistent storage.
verifyPassword :: PlainTextPassword' t -> Password -> Bool
verifyPassword plain opaque =
  let actual = Pass . Text.encodeUtf8 $ fromPlainTextPassword plain
      expected = fromPassword opaque
   in verifyPass' actual expected
