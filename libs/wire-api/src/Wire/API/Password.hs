{-# LANGUAGE RecordWildCards #-}
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
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Wire.API.Password
  ( Password,
    genPassword,
    mkSafePassword,
    verifyPassword,
    unsafeMkPassword,
  )
where

import Cassandra
import Crypto.KDF.Scrypt as Scrypt
import Crypto.Random
import Data.ByteArray hiding (length)
import Data.ByteString.Base64 qualified as B64
import Data.ByteString.Char8 qualified as C8
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Misc
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Imports
import OpenSSL.Random (randBytes)

-- | A derived, stretched password that can be safely stored.
newtype Password = Password
  {fromPassword :: Text}

instance Show Password where
  show _ = "<Password>"

instance Cql Password where
  ctype = Tagged BlobColumn

  fromCql (CqlBlob lbs) = pure . Password . Text.decodeUtf8 . toStrict $ lbs
  fromCql _ = Left "password: expected blob"

  toCql = CqlBlob . fromStrict . Text.encodeUtf8 . fromPassword

unsafeMkPassword :: Text -> Password
unsafeMkPassword = Password

-------------------------------------------------------------------------------

data ScryptParameters = ScryptParameters
  { -- | Bytes to randomly generate as a unique salt, default is __32__
    saltLength :: Word32,
    -- | log2(N) rounds to hash, default is __14__ (i.e. 2^14 rounds)
    rounds :: Word32,
    -- | Block size, default is __8__
    --
    -- Limits are min: @1@, and max: @blockSize * scryptParallelism < 2 ^ 30@
    blockSize :: Word32,
    -- | Parallelism factor, default is __1__
    --
    -- Limits are min: @0@, and max: @blockSize * scryptParallelism < 2 ^ 30@
    parallelism :: Word32,
    -- | Output key length in bytes, default is __64__
    outputLength :: Word32
  }
  deriving (Eq, Show)

defaultParams :: ScryptParameters
defaultParams =
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

-------------------------------------------------------------------------------

-- | Generate a strong, random plaintext password of length 16
-- containing only alphanumeric characters, '+' and '/'.
genPassword :: MonadIO m => m PlainTextPassword8
genPassword =
  liftIO . fmap (plainTextPassword8Unsafe . Text.decodeUtf8 . B64.encode) $
    randBytes 12

-- | Stretch a plaintext password so that it can be safely stored.
mkSafePassword :: MonadIO m => PlainTextPassword' t -> m Password
mkSafePassword = fmap Password . hashPassword . Text.encodeUtf8 . fromPlainTextPassword

-- | Verify a plaintext password from user input against a stretched
-- password from persistent storage.
verifyPassword :: PlainTextPassword' t -> Password -> Bool
verifyPassword plain opaque =
  let actual = fromPlainTextPassword plain
      expected = fromPassword opaque
   in checkPassword actual expected

hashPassword :: MonadIO m => ByteString -> m Text
hashPassword password = do
  salt <- newSalt $ fromIntegral defaultParams.saltLength
  let key = hashPasswordWithSalt password salt
  pure $
    Text.intercalate
      "|"
      [ "14",
        "8",
        "1",
        Text.decodeUtf8 . B64.encode $ salt,
        Text.decodeUtf8 . B64.encode $ key
      ]

hashPasswordWithSalt :: ByteString -> ByteString -> ByteString
hashPasswordWithSalt password salt = hashPasswordWithParams defaultParams password salt

hashPasswordWithParams ::
  ( ByteArrayAccess password,
    ByteArrayAccess salt
  ) =>
  ScryptParameters ->
  password ->
  salt ->
  ByteString
hashPasswordWithParams parameters password salt = convert (generate (fromScrypt parameters) password salt :: Bytes)

checkPassword :: Text -> Text -> Bool
checkPassword actual expected = fromMaybe False $ do
  (sparams, salt, hashedKey) <- parseScryptPasswordHashParams $ Text.encodeUtf8 expected
  let producedKey = hashPasswordWithParams sparams (Text.encodeUtf8 actual) salt
  pure $ hashedKey `constEq` producedKey

newSalt :: MonadIO m => Int -> m ByteString
newSalt i = liftIO $ getRandomBytes i
{-# INLINE newSalt #-}

parseScryptPasswordHashParams :: ByteString -> Maybe (ScryptParameters, ByteString, ByteString)
parseScryptPasswordHashParams passwordHash = do
  let paramList = Text.split (== '|') . Text.decodeUtf8 $ passwordHash
  guard $ length paramList == 5
  let [ scryptRoundsT,
        scryptBlockSizeT,
        scryptParallelismT,
        salt64,
        hashedKey64
        ] = paramList
  rounds <- readT scryptRoundsT
  blockSize <- readT scryptBlockSizeT
  parallelism <- readT scryptParallelismT
  salt <- from64 salt64
  hashedKey <- from64 hashedKey64
  let outputLength = fromIntegral $ C8.length hashedKey
      saltLength = fromIntegral $ C8.length salt
  pure
    ( ScryptParameters {..},
      salt,
      hashedKey
    )
