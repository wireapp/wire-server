{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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
    scopeFromAction,

    -- * Generation
    Gen (genKey),
    mkGen,
    generate,
    mk6DigitGen,
    mkKey,

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
import Cassandra hiding (Value)
import qualified Data.ByteString as BS
import Data.Code
import Data.Range
import Data.RetryAfter (RetryAfter (RetryAfter))
import qualified Data.Text as Text
import qualified Data.Text.Ascii as Ascii
import qualified Data.Text.Encoding as Text
import Data.UUID (UUID)
import Imports hiding (lookup)
import OpenSSL.BN (randIntegerZeroToNMinusOne)
import OpenSSL.EVP.Digest (Digest, digestBS, getDigestByName)
import OpenSSL.Random (randBytes)
import Text.Printf (printf)
import qualified Wire.API.User as User
import Wire.API.User.Identity

--------------------------------------------------------------------------------
-- Code

data Code = Code
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

scopeFromAction :: User.VerificationAction -> Scope
scopeFromAction = \case
  User.CreateScimToken -> CreateScimToken
  User.Login -> AccountLogin
  User.DeleteTeam -> DeleteTeam

-- | The same 'Key' can exist with different 'Value's in different
-- 'Scope's at the same time.
data Scope
  = AccountDeletion
  | IdentityVerification
  | PasswordReset
  | AccountLogin
  | AccountApproval
  | CreateScimToken
  | DeleteTeam
  deriving (Eq, Show)

instance Cql Scope where
  ctype = Tagged IntColumn

  toCql AccountDeletion = CqlInt 1
  toCql IdentityVerification = CqlInt 2
  toCql PasswordReset = CqlInt 3
  toCql AccountLogin = CqlInt 4
  toCql AccountApproval = CqlInt 5
  toCql CreateScimToken = CqlInt 6
  toCql DeleteTeam = CqlInt 7

  fromCql (CqlInt 1) = pure AccountDeletion
  fromCql (CqlInt 2) = pure IdentityVerification
  fromCql (CqlInt 3) = pure PasswordReset
  fromCql (CqlInt 4) = pure AccountLogin
  fromCql (CqlInt 5) = pure AccountApproval
  fromCql (CqlInt 6) = pure CreateScimToken
  fromCql (CqlInt 7) = pure DeleteTeam
  fromCql _ = Left "fromCql: Scope: int expected"

newtype Retries = Retries {numRetries :: Word8}
  deriving (Eq, Show, Ord, Num, Integral, Enum, Real)

instance Cql Retries where
  ctype = Tagged IntColumn
  toCql = CqlInt . fromIntegral . numRetries
  fromCql (CqlInt n) = pure (Retries (fromIntegral n))
  fromCql _ = Left "fromCql: Retries: int expected"

--------------------------------------------------------------------------------
-- Generation

-- | A contextual string that is hashed into the key to yield distinct keys in
-- different contexts for the same email address or phone number.
-- TODO: newtype KeyContext = KeyContext ByteString
data Gen = Gen
  { genFor :: !CodeFor,
    genKey :: !Key, -- Note [Unique keys]
    genValue :: IO Value
  }

mkKey :: MonadIO m => CodeFor -> m Key
mkKey cfor = liftIO $ do
  Just sha256 <- getDigestByName "SHA256"
  let uniqueK = case cfor of
        ForEmail e -> emailKeyUniq (mkEmailKey e)
        ForPhone p -> phoneKeyUniq (mkPhoneKey p)
  pure $ mkKey' sha256 (Text.encodeUtf8 uniqueK)

-- | Initialise a 'Code' 'Gen'erator for a given natural key.  This generates a link for emails and a 6-digit code for phone.  See also: `mk6DigitGen`.
mkGen :: MonadIO m => CodeFor -> m Gen
mkGen cfor = liftIO $ do
  Just sha256 <- getDigestByName "SHA256"
  pure (initGen sha256 cfor)
  where
    initGen d (ForEmail e) = mkEmailLinkGen e d
    initGen d _ = mk6DigitGen' cfor d

-- | Initialise a 'Code' 'Gen'erator for a given natural key.  This generates a 6-digit code, matter whether it is sent to a phone or to an email address.  See also: `mkGen`.
mk6DigitGen :: MonadIO m => CodeFor -> m Gen
mk6DigitGen cfor = liftIO $ do
  Just sha256 <- getDigestByName "SHA256"
  pure $ mk6DigitGen' cfor sha256

mk6DigitGen' :: CodeFor -> Digest -> Gen
mk6DigitGen' cfor d =
  let uniqueK = case cfor of
        ForEmail e -> emailKeyUniq (mkEmailKey e)
        ForPhone p -> phoneKeyUniq (mkPhoneKey p)
      key = mkKey' d $ Text.encodeUtf8 uniqueK
      val = Value . unsafeRange . Ascii.unsafeFromText . Text.pack . printf "%06d" <$> randIntegerZeroToNMinusOne (10 ^ (6 :: Int))
   in Gen cfor key val

mkEmailLinkGen :: Email -> Digest -> Gen
mkEmailLinkGen e d =
  let key = mkKey' d (Text.encodeUtf8 (emailKeyUniq (mkEmailKey e)))
      val = Value . unsafeRange . Ascii.encodeBase64Url <$> randBytes 15
   in Gen (ForEmail e) key val

mkKey' :: Digest -> ByteString -> Key
mkKey' d = Key . unsafeRange . Ascii.encodeBase64Url . BS.take 15 . digestBS d

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

insert :: MonadClient m => Code -> Int -> m (Maybe RetryAfter)
insert code ttl = do
  mRetryAfter <- lookupThrottle (codeKey code) (codeScope code)
  case mRetryAfter of
    Just ra -> pure (Just ra)
    Nothing -> do
      insertThrottle code ttl
      insertInternal code
      pure Nothing
  where
    insertThrottle :: MonadClient m => Code -> Int -> m ()
    insertThrottle c t = do
      let k = codeKey c
      let s = codeScope c
      retry x5 (write cql (params LocalQuorum (k, s, fromIntegral t, fromIntegral t)))
      where
        cql :: PrepQuery W (Key, Scope, Int32, Int32) ()
        cql =
          "INSERT INTO vcodes_throttle (key, scope, initial_delay) \
          \VALUES (?, ?, ?) USING TTL ?"

insertInternal :: MonadClient m => Code -> m ()
insertInternal c = do
  let k = codeKey c
  let s = codeScope c
  let v = codeValue c
  let r = fromIntegral (codeRetries c)
  let a = codeAccount c
  let e = codeForEmail c
  let p = codeForPhone c
  let t = round (codeTTL c)
  retry x5 (write cql (params LocalQuorum (k, s, v, r, e, p, a, t)))
  where
    cql :: PrepQuery W (Key, Scope, Value, Retries, Maybe Email, Maybe Phone, Maybe UUID, Int32) ()
    cql =
      "INSERT INTO vcodes (key, scope, value, retries, email, phone, account) \
      \VALUES (?, ?, ?, ?, ?, ?, ?) USING TTL ?"

-- | Check if code generation should be throttled.
lookupThrottle :: MonadClient m => Key -> Scope -> m (Maybe RetryAfter)
lookupThrottle k s = do
  fmap (RetryAfter . fromIntegral . runIdentity) <$> retry x1 (query1 cql (params LocalQuorum (k, s)))
  where
    cql :: PrepQuery R (Key, Scope) (Identity Int32)
    cql =
      "SELECT ttl(initial_delay) \
      \FROM vcodes_throttle WHERE key = ? AND scope = ?"

-- | Lookup a pending code.
lookup :: MonadClient m => Key -> Scope -> m (Maybe Code)
lookup k s = fmap (toCode k s) <$> retry x1 (query1 cql (params LocalQuorum (k, s)))
  where
    cql :: PrepQuery R (Key, Scope) (Value, Int32, Retries, Maybe Email, Maybe Phone, Maybe UUID)
    cql =
      "SELECT value, ttl(value), retries, email, phone, account \
      \FROM vcodes WHERE key = ? AND scope = ?"

-- | Lookup and verify the code for the given key and scope
-- against the given value.
verify :: MonadClient m => Key -> Scope -> Value -> m (Maybe Code)
verify k s v = lookup k s >>= maybe (pure Nothing) continue
  where
    continue c
      | codeValue c == v && codeRetries c > 0 = pure (Just c)
      | codeRetries c > 0 = do
          insertInternal (c {codeRetries = codeRetries c - 1})
          pure Nothing
      | otherwise = pure Nothing

-- | Delete a code associated with the given key and scope.
delete :: MonadClient m => Key -> Scope -> m ()
delete k s = retry x5 $ write cql (params LocalQuorum (k, s))
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
