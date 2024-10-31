{-# LANGUAGE RecordWildCards #-}
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
    PasswordStatus (..),
    genPassword,
    mkSafePassword,
    verifyPassword,
    verifyPasswordWithStatus,
    PasswordReqBody (..),
    argon2OptsFromHashingOpts,

    -- * Only for testing
    hashPasswordArgon2idWithSalt,
    mkSafePasswordScrypt,
    parsePassword,
  )
where

import Cassandra hiding (params)
import Crypto.Error
import Crypto.KDF.Argon2 qualified as Argon2
import Crypto.KDF.Scrypt as Scrypt
import Crypto.Random
import Data.Aeson qualified as A
import Data.ByteArray hiding (length)
import Data.ByteString.Base64 qualified as B64
import Data.ByteString.Char8 qualified as C8
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Misc
import Data.OpenApi qualified as S
import Data.Schema
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Imports
import OpenSSL.Random (randBytes)
import Util.Options

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

data Argon2HashedPassword = Argon2HashedPassword
  { opts :: Argon2.Options,
    salt :: ByteString,
    hashedKey :: ByteString
  }

data ScryptHashedPassword = ScryptHashedPassword
  { params :: ScryptParameters,
    salt :: ByteString,
    hashedKey :: ByteString
  }

data PasswordStatus
  = PasswordStatusOk
  | PasswordStatusNeedsUpdate
  deriving (Show, Eq)

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

argon2OptsFromHashingOpts :: Argon2idOptions -> Argon2.Options
argon2OptsFromHashingOpts Argon2idOptions {..} =
  Argon2.Options
    { variant = Argon2.Argon2id,
      version = Argon2.Version13,
      iterations = iterations,
      memory = memory,
      parallelism = parallelism
    }

-------------------------------------------------------------------------------

-- | Generate a strong, random plaintext password of length 16
-- containing only alphanumeric characters, '+' and '/'.
genPassword :: (MonadIO m) => m PlainTextPassword8
genPassword =
  liftIO . fmap (plainTextPassword8Unsafe . Text.decodeUtf8 . B64.encode) $
    randBytes 12

mkSafePasswordScrypt :: (MonadIO m) => PlainTextPassword' t -> m Password
mkSafePasswordScrypt = fmap ScryptPassword . hashPasswordScrypt . Text.encodeUtf8 . fromPlainTextPassword

mkSafePassword :: (MonadIO m) => Argon2.Options -> PlainTextPassword' t -> m Password
mkSafePassword opts = fmap Argon2Password . hashPasswordArgon2id opts . Text.encodeUtf8 . fromPlainTextPassword

-- | Verify a plaintext password from user input against a stretched
-- password from persistent storage.
verifyPassword :: PlainTextPassword' t -> Password -> Bool
verifyPassword = (fst .) . verifyPasswordWithStatus

verifyPasswordWithStatus :: PlainTextPassword' t -> Password -> (Bool, PasswordStatus)
verifyPasswordWithStatus (fromPlainTextPassword -> plain) hashed =
  case hashed of
    (Argon2Password Argon2HashedPassword {..}) ->
      let producedKey = hashPasswordWithOptions opts (Text.encodeUtf8 plain) salt
       in (hashedKey `constEq` producedKey, PasswordStatusOk)
    (ScryptPassword ScryptHashedPassword {..}) ->
      let producedKey = hashPasswordWithParams params (Text.encodeUtf8 plain) salt
       in (hashedKey `constEq` producedKey, PasswordStatusNeedsUpdate)

hashPasswordScrypt :: (MonadIO m) => ByteString -> m ScryptHashedPassword
hashPasswordScrypt password = do
  salt <- newSalt $ fromIntegral defaultScryptParams.saltLength
  let params = defaultScryptParams
  let hashedKey = hashPasswordWithParams params password salt
  pure $! ScryptHashedPassword {..}

encodeScryptPassword :: ScryptHashedPassword -> Text
encodeScryptPassword ScryptHashedPassword {..} =
  Text.intercalate
    "|"
    [ showT defaultScryptParams.rounds,
      showT defaultScryptParams.blockSize,
      showT defaultScryptParams.parallelism,
      Text.decodeUtf8 . B64.encode $ salt,
      Text.decodeUtf8 . B64.encode $ hashedKey
    ]

hashPasswordArgon2id :: (MonadIO m) => Argon2.Options -> ByteString -> m Argon2HashedPassword
hashPasswordArgon2id opts pwd = do
  salt <- newSalt 16
  pure $! hashPasswordArgon2idWithSalt opts salt pwd

hashPasswordArgon2idWithSalt :: Argon2.Options -> ByteString -> ByteString -> Argon2HashedPassword
hashPasswordArgon2idWithSalt opts salt pwd = do
  let hashedKey = hashPasswordWithOptions opts pwd salt
   in Argon2HashedPassword {..}

encodeArgon2HashedPassword :: Argon2HashedPassword -> Text
encodeArgon2HashedPassword Argon2HashedPassword {..} =
  let optsStr =
        Text.intercalate
          ","
          [ "m=" <> showT opts.memory,
            "t=" <> showT opts.iterations,
            "p=" <> showT opts.parallelism
          ]
   in "$argon2"
        <> Text.intercalate
          "$"
          [ variantToCode opts.variant,
            "v=" <> versionToNum opts.version,
            optsStr,
            encodeWithoutPadding salt,
            encodeWithoutPadding hashedKey
          ]
  where
    encodeWithoutPadding = Text.dropWhileEnd (== '=') . Text.decodeUtf8 . B64.encode

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

newSalt :: (MonadIO m) => Int -> m ByteString
newSalt i = liftIO $ getRandomBytes i
{-# INLINE newSalt #-}

parseArgon2idPasswordHashOptions :: Text -> Either String Argon2HashedPassword
parseArgon2idPasswordHashOptions passwordHash = do
  let paramsList = Text.split (== '$') passwordHash
  -- The first param is empty string b/c the string begins with a separator `$`.
  case paramsList of
    ["", variantStr, verStr, opts, salt, hashedKey64] -> do
      version <- parseVersion verStr
      parseAll variantStr version opts salt hashedKey64
    ["", variantStr, opts, salt, hashedKey64] -> do
      parseAll variantStr Argon2.Version10 opts salt hashedKey64
    _ -> Left $ "failed to parse argon2id hashed password, expected 5 or 6 params, got: " <> show (length paramsList)
  where
    parseVersion =
      maybe (Left "failed to parse argon2 version") Right
        . splitMaybe "v=" numToVersion

    parseAll :: Text -> Argon2.Version -> Text -> Text -> Text -> Either String Argon2HashedPassword
    parseAll variantStr version parametersStr salt64 hashedKey64 = do
      variant <- parseVariant variantStr
      (memory, iterations, parallelism) <- parseParameters parametersStr
      -- We pad the Base64 with '=' chars because we drop them while encoding this.
      -- At the time of implementation we've opted to be consistent with how the
      -- CLI of the reference implementation of Argon2id outputs this.
      salt <- from64 $ unsafePad64 salt64
      hashedKey <- from64 $ unsafePad64 hashedKey64
      pure $ Argon2HashedPassword {opts = (Argon2.Options {..}), ..}
      where
        parseVariant =
          maybe (Left "failed to parse argon2 variant") Right
            . splitMaybe "argon2" letterToVariant
        parseParameters paramsT =
          let paramsList = Text.split (== ',') paramsT
           in go paramsList (Nothing, Nothing, Nothing)
          where
            go [] (Just m, Just t, Just p) = Right (m, t, p)
            go [] (Nothing, _, _) = Left "failed to parse Argon2Options: failed to read parameter 'm'"
            go [] (_, Nothing, _) = Left "failed to parse Argon2Options: failed to read parameter 't'"
            go [] (_, _, Nothing) = Left "failed to parse Argon2Options: failed to read parameter 'p'"
            go (x : xs) (m, t, p) =
              case Text.splitAt 2 x of
                ("m=", i) -> go xs (readT i, t, p)
                ("t=", i) -> go xs (m, readT i, p)
                ("p=", i) -> go xs (m, t, readT i)
                (unknownParam, _) -> Left $ "failed to parse Argon2Options: Unknown param: " <> Text.unpack unknownParam

parseScryptPasswordHashParams :: ByteString -> Either String ScryptHashedPassword
parseScryptPasswordHashParams passwordHash = do
  let paramList = Text.split (== '|') . Text.decodeUtf8 $ passwordHash
  case paramList of
    [roundsStr, blockSizeStr, parallelismStr, salt64, hashedKey64] -> do
      rounds <- eitherFromMaybe "rounds" $ readT roundsStr
      blockSize <- eitherFromMaybe "blockSize" $ readT blockSizeStr
      parallelism <- eitherFromMaybe "parellelism" $ readT parallelismStr
      salt <- from64 salt64
      hashedKey <- from64 hashedKey64
      let outputLength = fromIntegral $ C8.length hashedKey
          saltLength = fromIntegral $ C8.length salt
      pure $ ScryptHashedPassword {params = ScryptParameters {..}, ..}
    _ -> Left $ "failed to parse ScryptHashedPassword: expected exactly 5 params"
  where
    eitherFromMaybe :: String -> Maybe a -> Either String a
    eitherFromMaybe paramName = maybe (Left $ "failed to parse scrypt parameter: " <> paramName) Right

-------------------------------------------------------------------------------

hashPasswordWithOptions :: Argon2.Options -> ByteString -> ByteString -> ByteString
hashPasswordWithOptions opts password salt = do
  let tagSize = 16
  case (Argon2.hash opts password salt tagSize) of
    -- CryptoFailed occurs when salt, output or input are too small/big.
    -- since we control those values ourselves, it should never have a runtime error
    CryptoFailed cErr -> error $ "Impossible error: " <> show cErr
    CryptoPassed hash -> hash

hashPasswordWithParams ::
  ( ByteArrayAccess password,
    ByteArrayAccess salt
  ) =>
  ScryptParameters ->
  password ->
  salt ->
  ByteString
hashPasswordWithParams parameters password salt = convert (generate (fromScrypt parameters) password salt :: Bytes)

--------------------------------------------------------------------------------

-- | Makes a letter out of the variant
variantToCode :: Argon2.Variant -> Text
variantToCode = \case
  Argon2.Argon2i -> "i"
  Argon2.Argon2d -> "d"
  Argon2.Argon2id -> "id"

-- | Parses the variant parameter in the encoded hash
letterToVariant :: Text -> Maybe Argon2.Variant
letterToVariant = \case
  "i" -> Just Argon2.Argon2i
  "d" -> Just Argon2.Argon2d
  "id" -> Just Argon2.Argon2id
  _ -> Nothing

-- | Parses the "v=" parameter in the encoded hash
numToVersion :: Text -> Maybe Argon2.Version
numToVersion "16" = Just Argon2.Version10
numToVersion "19" = Just Argon2.Version13
numToVersion _ = Nothing

-- | Makes number for the "v=" parameter in the encoded hash
versionToNum :: Argon2.Version -> Text
versionToNum Argon2.Version10 = "16"
versionToNum Argon2.Version13 = "19"

-- | Strips the given 'match' if it matches and uses
--   the function on the remainder of the given text.
splitMaybe :: Text -> (Text -> Maybe a) -> Text -> Maybe a
splitMaybe match f t =
  Text.stripPrefix match t >>= f

-- | (UNSAFE) Pad a base64 text to "length `rem` 4 == 0" with "="
--
-- prop> \bs -> let b64 = encodeBase64 bs in unsafePad64 (T.dropWhileEnd (== '=') b64) == b64
unsafePad64 :: Text -> Text
unsafePad64 t
  | remains == 0 = t
  | otherwise = t <> pad
  where
    remains = Text.length t `rem` 4
    pad = Text.replicate (4 - remains) "="

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
