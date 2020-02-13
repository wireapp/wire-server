{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Random, time-limited codes for e-mail addresses and phone numbers
-- for use in a variety of 'Scope's.
--
-- TODO: This module is supposed to (eventually) supersede the existing
--       code verification functionality in the following modules:
--           Brig.Data.Activation
--           Brig.Data.PasswordReset
--           Brig.Data.LoginCode
module Brig.Code
  ( -- * Code
    Code,
    CodeFor (..),
    Key (..),
    Scope (..),
    Value (..),
    KeyValuePair (..),
    Timeout (..),
    Retries (..),
    codeFor,
    codeForEmail,
    codeForPhone,
    codeKey,
    codeValue,
    codeTTL,
    codeAccount,

    -- * Generation
    Gen (genKey),
    mkGen,
    generate,

    -- * Storage
    insert,
    lookup,
    verify,
    delete,
  )
where

import Brig.Data.Instances ()
import Brig.Email (emailKeyUniq, mkEmailKey)
import Brig.Phone (mkPhoneKey, phoneKeyUniq)
import Brig.Types (Email, Phone)
import Brig.Types.Code (Key (..), KeyValuePair (..), Timeout (..), Value (..))
import Cassandra hiding (Value)
import qualified Data.ByteString as BS
import Data.Range
import qualified Data.Text as Text
import qualified Data.Text.Ascii as Ascii
import qualified Data.Text.Encoding as Text
import Data.UUID (UUID)
import Imports hiding (lookup)
import OpenSSL.BN (randIntegerZeroToNMinusOne)
import OpenSSL.EVP.Digest (digestBS, getDigestByName)
import OpenSSL.Random (randBytes)
import Text.Printf (printf)

--------------------------------------------------------------------------------
-- Code

data Code
  = Code
      { codeKey :: !Key,
        codeScope :: !Scope,
        codeValue :: !Value,
        codeRetries :: !Retries,
        codeTTL :: !Timeout,
        codeFor :: !CodeFor,
        codeAccount :: !(Maybe UUID)
      }
  deriving (Eq, Show)

data CodeFor
  = ForEmail !Email
  | ForPhone !Phone
  deriving (Eq, Show)

codeForEmail :: Code -> Maybe Email
codeForEmail c
  | ForEmail e <- codeFor c = Just e
  | otherwise = Nothing

codeForPhone :: Code -> Maybe Phone
codeForPhone c
  | ForPhone p <- codeFor c = Just p
  | otherwise = Nothing

-- | The same 'Key' can exist with different 'Value's in different
-- 'Scope's at the same time.
data Scope
  = AccountDeletion
  | IdentityVerification
  | PasswordReset
  | AccountLogin
  | AccountApproval
  deriving (Eq, Show)

instance Cql Scope where
  ctype = Tagged IntColumn

  toCql AccountDeletion = CqlInt 1
  toCql IdentityVerification = CqlInt 2
  toCql PasswordReset = CqlInt 3
  toCql AccountLogin = CqlInt 4
  toCql AccountApproval = CqlInt 5

  fromCql (CqlInt 1) = return AccountDeletion
  fromCql (CqlInt 2) = return IdentityVerification
  fromCql (CqlInt 3) = return PasswordReset
  fromCql (CqlInt 4) = return AccountLogin
  fromCql (CqlInt 5) = return AccountApproval
  fromCql _ = fail "fromCql: Scope: int expected"

newtype Retries = Retries {numRetries :: Word8}
  deriving (Eq, Show, Ord, Num, Integral, Enum, Real)

instance Cql Retries where
  ctype = Tagged IntColumn

  toCql = CqlInt . fromIntegral . numRetries

  fromCql (CqlInt n) = return (Retries (fromIntegral n))
  fromCql _ = fail "fromCql: Retries: int expected"

--------------------------------------------------------------------------------
-- Generation

-- | A contextual string that is hashed into the key to yield distinct keys in
-- different contexts for the same email address or phone number.
-- TODO: newtype KeyContext = KeyContext ByteString
data Gen
  = Gen
      { genFor :: !CodeFor,
        genKey :: !Key, -- Note [Unique keys]
        genValue :: IO Value
      }

-- | Initialise a 'Code' 'Gen'erator for a given natural key.
mkGen :: MonadIO m => CodeFor -> m Gen
mkGen cfor = liftIO $ do
  Just sha256 <- getDigestByName "SHA256"
  return (initGen sha256 cfor)
  where
    initGen d (ForEmail e) =
      let key = mkKey d (Text.encodeUtf8 (emailKeyUniq (mkEmailKey e)))
          val = Value . unsafeRange . Ascii.encodeBase64Url <$> randBytes 15
       in Gen cfor key val
    initGen d (ForPhone p) =
      let key = mkKey d (Text.encodeUtf8 (phoneKeyUniq (mkPhoneKey p)))
          val =
            Value . unsafeRange . Ascii.unsafeFromText . Text.pack . printf "%06d"
              <$> randIntegerZeroToNMinusOne (10 ^ (6 :: Int))
       in Gen cfor key val
    mkKey d = Key . unsafeRange . Ascii.encodeBase64Url . BS.take 15 . digestBS d

-- | Generate a new 'Code'.
generate ::
  MonadIO m =>
  -- | The 'Gen'erator to use.
  Gen ->
  -- | The scope of the generated code.
  Scope ->
  -- | Maximum verification attempts.
  Retries ->
  -- | Time-to-live in seconds.
  Timeout ->
  -- | Associated account ID.
  Maybe UUID ->
  m Code
generate gen scope retries ttl account = do
  let key = genKey gen
  val <- liftIO $ genValue gen
  return $ mkCode key val
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

-- Note [Unique keys]
--
-- We want unique, stable keys that we can associate the secret values with.
-- Using the plain natural identifiers (e.g. e-mail addresses or phone numbers)
-- has a few downsides:
--
--   * The keys are often placed in URLs for verification purposes,
--     giving them unnecessary exposure.
--   * If the keys are not opaque, it can be harder to change their
--     structure, possibly embedding additional information.
--   * Since the keys are often placed in URLs, they must only contain
--     URL-safe characters or otherwise require appropriate encoding.
--
-- Therefore we use the following simple construction:
--
--   * Compute the SHA-256 truncated to 120 bits of the plain, normalised,
--     utf8-encoded natural identifier (i.e. e-mail address or phone number).
--   * Apply URL-safe base64 encoding to yield the final key of length 20.
--
-- Truncation of SHA-2 outputs is a safe and common practice, only reducing
-- collision resistance (e.g. after 2^60 for truncated SHA-256/120 due to the
-- birthday paradox). Collisions have no security implications in this context;
-- at most it enables verification of one random e-mail address or phone
-- number via another, at least one of which must be accessible. It is only
-- important that keys be sufficiently unique and random collisions rare
-- while keeping the length reasonably short, so that keys may be used in
-- length-constrained contexts (e.g. SMS) or even be spelled out or typed.

--------------------------------------------------------------------------------
-- Storage

insert :: MonadClient m => Code -> m ()
insert c = do
  let k = codeKey c
  let s = codeScope c
  let v = codeValue c
  let r = fromIntegral (codeRetries c)
  let a = codeAccount c
  let e = codeForEmail c
  let p = codeForPhone c
  let t = round (codeTTL c)
  retry x5 (write cql (params Quorum (k, s, v, r, e, p, a, t)))
  where
    cql :: PrepQuery W (Key, Scope, Value, Retries, Maybe Email, Maybe Phone, Maybe UUID, Int32) ()
    cql =
      "INSERT INTO vcodes (key, scope, value, retries, email, phone, account) \
      \VALUES (?, ?, ?, ?, ?, ?, ?) USING TTL ?"

-- | Lookup a pending code.
lookup :: MonadClient m => Key -> Scope -> m (Maybe Code)
lookup k s = fmap (toCode k s) <$> retry x1 (query1 cql (params Quorum (k, s)))
  where
    cql :: PrepQuery R (Key, Scope) (Value, Int32, Retries, Maybe Email, Maybe Phone, Maybe UUID)
    cql =
      "SELECT value, ttl(value), retries, email, phone, account \
      \FROM vcodes WHERE key = ? AND scope = ?"

-- | Lookup and verify the code for the given key and scope
-- against the given value.
verify :: MonadClient m => Key -> Scope -> Value -> m (Maybe Code)
verify k s v = lookup k s >>= maybe (return Nothing) continue
  where
    continue c
      | codeValue c == v = return (Just c)
      | codeRetries c > 0 = do
        insert (c {codeRetries = codeRetries c - 1})
        return Nothing
      | otherwise = return Nothing

-- | Delete a code associated with the given key and scope.
delete :: MonadClient m => Key -> Scope -> m ()
delete k s = retry x5 $ write cql (params Quorum (k, s))
  where
    cql :: PrepQuery W (Key, Scope) ()
    cql = "DELETE FROM vcodes WHERE key = ? AND scope = ?"

--------------------------------------------------------------------------------
-- Internal

toCode :: Key -> Scope -> (Value, Int32, Retries, Maybe Email, Maybe Phone, Maybe UUID) -> Code
toCode k s (val, ttl, retries, email, phone, account) =
  let ek = ForEmail <$> email
      pk = ForPhone <$> phone
      to = Timeout (fromIntegral ttl)
   in case ek <|> pk of
        Nothing -> error "toCode: email or phone must be present"
        Just cf ->
          Code
            { codeKey = k,
              codeScope = s,
              codeValue = val,
              codeTTL = to,
              codeRetries = retries,
              codeFor = cf,
              codeAccount = account
            }
