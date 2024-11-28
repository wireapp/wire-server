{-# LANGUAGE StrictData #-}

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
  ( Password (..),
    PasswordReqBody (..),

    -- * Misc
    genPassword,

    -- * Only for testing
    parsePassword,
  )
where

import Cassandra hiding (params)
import Data.Aeson qualified as A
import Data.ByteString.Base64 qualified as B64
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Misc
import Data.OpenApi qualified as S
import Data.Schema
import Data.Text.Encoding qualified as Text
import Imports
import OpenSSL.Random (randBytes)
import Wire.API.Password.Argon2id
import Wire.API.Password.Scrypt

-- | A derived, stretched password that can be safely stored.
data Password
  = Argon2Password Argon2HashedPassword
  | ScryptPassword ScryptHashedPassword

instance Show Password where
  show _ = "<Password>"

instance Cql Password where
  ctype = Tagged BlobColumn

  fromCql (CqlBlob lbs) = parsePassword . Text.decodeUtf8 . toStrict $ lbs
  fromCql _ = Left "password: expected blob"

  toCql pw = CqlBlob . fromStrict $ Text.encodeUtf8 encoded
    where
      encoded = case pw of
        Argon2Password argon2pw -> encodeArgon2HashedPassword argon2pw
        ScryptPassword scryptpw -> encodeScryptPassword scryptpw

-------------------------------------------------------------------------------

parsePassword :: Text -> Either String Password
parsePassword expected =
  case parseArgon2idPasswordHashOptions expected of
    Right hashedPassword -> Right $ Argon2Password hashedPassword
    Left argon2ParseError ->
      case parseScryptPasswordHashParams $ Text.encodeUtf8 expected of
        Right hashedPassword -> Right $ ScryptPassword hashedPassword
        Left scryptParseError ->
          Left $
            "Failed to parse Argon2 or Scrypt. Argon2 parse error: "
              <> argon2ParseError
              <> ", Scrypt parse error: "
              <> scryptParseError

--------------------------------------------------------------------------------
-- Type that can be used to pass a plaintext password as a request body

newtype PasswordReqBody = PasswordReqBody
  {fromPasswordReqBody :: Maybe PlainTextPassword6}
  deriving stock (Eq, Show)
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via Schema PasswordReqBody

instance ToSchema PasswordReqBody where
  schema =
    object "PasswordReqBody" $
      PasswordReqBody
        <$> fromPasswordReqBody .= maybe_ (optField "password" schema)

--------------------------------------------------------------------------------

-- | Generate a strong, random plaintext password of length 16
-- containing only alphanumeric characters, '+' and '/'.
genPassword :: (MonadIO m) => m PlainTextPassword8
genPassword =
  liftIO . fmap (plainTextPassword8Unsafe . Text.decodeUtf8 . B64.encode) $
    randBytes 12
