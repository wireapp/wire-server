module Wire.VerificationCodeGen
  ( VerificationCodeGen (genKey),
    mkVerificationCodeGen,
    mk6DigitVerificationCodeGen,
    mkKey,
    generateVerificationCode,
  )
where

import Crypto.Hash
import Data.ByteArray qualified as BA
import Data.ByteString qualified as BS
import Data.Code
import Data.Range
import Data.Text qualified as Text
import Data.Text.Ascii qualified as Ascii
import Data.Text.Encoding qualified as Text
import Data.UUID (UUID)
import Imports hiding (lookup)
import Polysemy
import Text.Printf
import Wire.API.User.Identity
import Wire.Arbitrary
import Wire.Sem.Random
import Wire.Sem.Random qualified as Random
import Wire.UserKeyStore
import Wire.VerificationCode

--------------------------------------------------------------------------------
-- VerificationCodeGeneration

data RandomValueType
  = Random6DigitNumber
  | Random15Bytes
  deriving (Show, Eq, Generic)
  deriving (Arbitrary) via GenericUniform RandomValueType

-- | A contextual string that is hashed into the key to yield distinct keys in
-- different contexts for the same email address.
-- TODO: newtype KeyContext = KeyContext ByteString
data VerificationCodeGen = VerificationCodeGen
  { genFor :: !EmailAddress,
    genKey :: !Key, -- Note [Unique keys]
    genValueType :: !RandomValueType
  }
  deriving (Show, Eq, Generic)
  deriving (Arbitrary) via GenericUniform VerificationCodeGen

-- | Initialise a 'Code' 'VerificationCodeGen'erator for a given natural key.
-- This generates a link for emails and a 6-digit code for phone. See also:
-- `mk6DigitVerificationCodeGen`.
mkVerificationCodeGen :: EmailAddress -> VerificationCodeGen
mkVerificationCodeGen email =
  VerificationCodeGen email (mkKey email) Random15Bytes

-- | Initialise a 'Code' 'VerificationCodeGen'erator for a given natural key.
-- This generates a 6-digit code, matter whether it is sent to a phone or to an
-- email address. See also: `mkVerificationCodeGen`.
mk6DigitVerificationCodeGen :: EmailAddress -> VerificationCodeGen
mk6DigitVerificationCodeGen email = VerificationCodeGen email (mkKey email) Random6DigitNumber

mkKey :: EmailAddress -> Key
mkKey email =
  Key
    . unsafeRange
    . Ascii.encodeBase64Url
    . BS.take 15
    . BA.convert
    . hash @_ @SHA256
    . Text.encodeUtf8
    . emailKeyUniq
    $ mkEmailKey email

-- | VerificationCodeGenerate a new 'Code'.
generateVerificationCode ::
  (Member Random r) =>
  -- | The 'VerificationCodeGen'erator to use.
  VerificationCodeGen ->
  -- | The scope of the generated code.
  Scope ->
  -- | Maximum verification attempts.
  Retries ->
  -- | Time-to-live in seconds.
  Timeout ->
  -- | Associated account ID.
  Maybe UUID ->
  Sem r Code
generateVerificationCode gen scope retries ttl account = do
  let key = genKey gen
  val <- genValue gen.genValueType
  pure $ mkCode key val
  where
    mkCode key val =
      Code
        { codeKey = key,
          codeValue = val,
          codeScope = scope,
          codeRetries = retries,
          codeTTL = ttl,
          codeFor = genFor gen,
          codeAccount = account
        }

genValue :: (Member Random r) => RandomValueType -> Sem r Value
genValue Random15Bytes =
  Value . unsafeRange . Ascii.encodeBase64Url
    <$> Random.bytes 15
genValue Random6DigitNumber =
  Value . unsafeRange . Ascii.unsafeFromText . Text.pack . printf "%06d"
    <$> Random.nDigitNumber 6
