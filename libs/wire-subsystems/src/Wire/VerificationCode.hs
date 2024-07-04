module Wire.VerificationCode
  ( Code (..),
    Key (..),
    Scope (..),
    Value (..),
    KeyValuePair (..),
    Timeout (..),
    Retries (..),
    codeToKeyValuePair,
    scopeFromAction,
  )
where

import Cassandra hiding (Value)
import Data.Code
import Data.UUID (UUID)
import Imports hiding (lookup)
import Wire.API.User qualified as User
import Wire.API.User.Identity
import Wire.Arbitrary

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
-- Code

data Code = Code
  { codeKey :: !Key,
    codeScope :: !Scope,
    codeValue :: !Value,
    -- | This field is actually used as number of allowed "tries" rather than
    -- "retries", so if a code has a retries = 1, verification can only be tried
    -- once, and it cannot actually be "re"-tried after that.
    codeRetries :: !Retries,
    codeTTL :: !Timeout,
    codeFor :: !Email,
    codeAccount :: !(Maybe UUID)
  }
  deriving (Eq, Show)

scopeFromAction :: User.VerificationAction -> Scope
scopeFromAction = \case
  User.CreateScimToken -> CreateScimToken
  User.Login -> AccountLogin
  User.DeleteTeam -> DeleteTeam

codeToKeyValuePair :: Code -> KeyValuePair
codeToKeyValuePair code = KeyValuePair code.codeKey code.codeValue

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
  deriving (Eq, Show, Ord, Generic)
  deriving (Arbitrary) via GenericUniform Scope

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
  deriving (Eq, Show, Ord, Num, Integral, Enum, Real, Arbitrary)

instance Cql Retries where
  ctype = Tagged IntColumn
  toCql = CqlInt . fromIntegral . numRetries
  fromCql (CqlInt n) = pure (Retries (fromIntegral n))
  fromCql _ = Left "fromCql: Retries: int expected"
