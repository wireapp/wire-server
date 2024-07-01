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
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Wire.API.Password
  ( Password,
    PasswordStatus (..),
    genPassword,
    mkSafePasswordScrypt,
    mkSafePasswordArgon2id,
    verifyPassword,
    verifyPasswordWithStatus,
    unsafeMkPassword,
    hashPasswordArgon2idWithSalt,
    hashPasswordArgon2idWithOptions,
  )
where

import Cassandra
import Crypto.Error
import Crypto.KDF.Argon2 qualified as Argon2
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

data PasswordStatus
  = PasswordStatusOk
  | PasswordStatusNeedsUpdate
  deriving (Show, Eq)

-------------------------------------------------------------------------------

type Argon2idOptions = Argon2.Options

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

-- | These are the default values suggested, as extracted from the crypton library.
defaultOptions :: Argon2idOptions
defaultOptions =
  Argon2.Options
    { iterations = 5,
      memory = 2 ^ (17 :: Int),
      parallelism = 4,
      variant = Argon2.Argon2id,
      version = Argon2.Version13
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
genPassword :: (MonadIO m) => m PlainTextPassword8
genPassword =
  liftIO . fmap (plainTextPassword8Unsafe . Text.decodeUtf8 . B64.encode) $
    randBytes 12

mkSafePasswordScrypt :: (MonadIO m) => PlainTextPassword' t -> m Password
mkSafePasswordScrypt = fmap Password . hashPasswordScrypt . Text.encodeUtf8 . fromPlainTextPassword

mkSafePasswordArgon2id :: (MonadIO m) => PlainTextPassword' t -> m Password
mkSafePasswordArgon2id = fmap Password . hashPasswordArgon2id . Text.encodeUtf8 . fromPlainTextPassword

-- | Verify a plaintext password from user input against a stretched
-- password from persistent storage.
verifyPassword :: PlainTextPassword' t -> Password -> Bool
verifyPassword = (fst .) . verifyPasswordWithStatus

verifyPasswordWithStatus :: PlainTextPassword' t -> Password -> (Bool, PasswordStatus)
verifyPasswordWithStatus plain opaque =
  let actual = fromPlainTextPassword plain
      expected = fromPassword opaque
   in checkPassword actual expected

hashPasswordScrypt :: (MonadIO m) => ByteString -> m Text
hashPasswordScrypt password = do
  salt <- newSalt $ fromIntegral defaultScryptParams.saltLength
  let key = hashPasswordWithParams defaultScryptParams password salt
  pure $
    Text.intercalate
      "|"
      [ showT defaultScryptParams.rounds,
        showT defaultScryptParams.blockSize,
        showT defaultScryptParams.parallelism,
        Text.decodeUtf8 . B64.encode $ salt,
        Text.decodeUtf8 . B64.encode $ key
      ]

hashPasswordArgon2id :: (MonadIO m) => ByteString -> m Text
hashPasswordArgon2id pwd = do
  salt <- newSalt 32
  pure $ hashPasswordArgon2idWithSalt salt pwd

hashPasswordArgon2idWithSalt :: ByteString -> ByteString -> Text
hashPasswordArgon2idWithSalt = hashPasswordArgon2idWithOptions defaultOptions

hashPasswordArgon2idWithOptions :: Argon2idOptions -> ByteString -> ByteString -> Text
hashPasswordArgon2idWithOptions opts salt pwd = do
  let key = hashPasswordWithOptions opts pwd salt
      optsStr =
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
            encodeWithoutPadding key
          ]
  where
    encodeWithoutPadding = Text.dropWhileEnd (== '=') . Text.decodeUtf8 . B64.encode

checkPassword :: Text -> Text -> (Bool, PasswordStatus)
checkPassword actual expected =
  case parseArgon2idPasswordHashOptions expected of
    Just (opts, salt, hashedKey) ->
      let producedKey = hashPasswordWithOptions opts (Text.encodeUtf8 actual) salt
       in (hashedKey `constEq` producedKey, PasswordStatusOk)
    Nothing ->
      case parseScryptPasswordHashParams $ Text.encodeUtf8 expected of
        Just (sparams, saltS, hashedKeyS) ->
          let producedKeyS = hashPasswordWithParams sparams (Text.encodeUtf8 actual) saltS
           in (hashedKeyS `constEq` producedKeyS, PasswordStatusNeedsUpdate)
        Nothing -> (False, PasswordStatusNeedsUpdate)

newSalt :: (MonadIO m) => Int -> m ByteString
newSalt i = liftIO $ getRandomBytes i
{-# INLINE newSalt #-}

parseArgon2idPasswordHashOptions :: Text -> Maybe (Argon2idOptions, ByteString, ByteString)
parseArgon2idPasswordHashOptions passwordHash = do
  let paramList = Text.split (== '$') passwordHash
  guard (length paramList >= 5)
  let (_ : variantT : vp : ps : sh : rest) = paramList
  variant <- parseVariant variantT
  case rest of
    [hashedKey64] -> do
      version <- parseVersion vp
      parseAll variant version ps sh hashedKey64
    [] -> parseAll variant Argon2.Version10 vp ps sh
    _ -> Nothing
  where
    parseVariant = splitMaybe "argon2" letterToVariant
    parseVersion = splitMaybe "v=" numToVersion

parseAll :: Argon2.Variant -> Argon2.Version -> Text -> Text -> Text -> Maybe (Argon2idOptions, ByteString, ByteString)
parseAll variant version parametersT salt64 hashedKey64 = do
  (memory, iterations, parallelism) <- parseParameters parametersT
  salt <- from64 $ unsafePad64 salt64
  hashedKey <- from64 $ unsafePad64 hashedKey64
  pure (Argon2.Options {..}, salt, hashedKey)
  where
    parseParameters paramsT = do
      let paramsL = Text.split (== ',') paramsT
      guard $ Imports.length paramsL == 3
      go paramsL (Nothing, Nothing, Nothing)
      where
        go [] (Just m, Just t, Just p) = Just (m, t, p)
        go [] _ = Nothing
        go (x : xs) (m, t, p) =
          case Text.splitAt 2 x of
            ("m=", i) -> go xs (readT i, t, p)
            ("t=", i) -> go xs (m, readT i, p)
            ("p=", i) -> go xs (m, t, readT i)
            _ -> Nothing

parseScryptPasswordHashParams :: ByteString -> Maybe (ScryptParameters, ByteString, ByteString)
parseScryptPasswordHashParams passwordHash = do
  let paramList = Text.split (== '|') . Text.decodeUtf8 $ passwordHash
  guard (length paramList == 5)
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

-------------------------------------------------------------------------------

hashPasswordWithOptions :: Argon2idOptions -> ByteString -> ByteString -> ByteString
hashPasswordWithOptions opts password salt =
  case (Argon2.hash opts password salt 64) of
    -- CryptoFailed occurs when salt, output or input are too small/big.
    -- since we control those values ourselves, it should never have a runtime error
    -- unless we've caused it ourselves.
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
