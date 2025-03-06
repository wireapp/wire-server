{-# LANGUAGE RecordWildCards #-}

module Wire.API.Password.Argon2id where

import Crypto.KDF.Argon2 qualified as Argon2
import Data.ByteString.Base64 qualified as B64
import Data.Misc
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Imports
import Util.Options

data Argon2HashedPassword = Argon2HashedPassword
  { opts :: Argon2.Options,
    salt :: ByteString,
    hashedKey :: ByteString
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
