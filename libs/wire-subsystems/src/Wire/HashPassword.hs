module Wire.HashPassword where

import Data.Misc
import Imports
import Polysemy
import Polysemy.Error
import Wire.API.Password
import Wire.RateLimit

data PasswordStatus
  = PasswordStatusOk
  | PasswordStatusNeedsUpdate
  deriving (Show, Eq)

data HashPassword m a where
  HashPassword6 :: PlainTextPassword6 -> HashPassword m Password
  HashPassword8 :: PlainTextPassword8 -> HashPassword m Password
  VerifyPasswordWithStatus :: PlainTextPassword' t -> Password -> HashPassword m (Bool, PasswordStatus)

hashPassword6 ::
  (Member RateLimit r, Member HashPassword r, Member (Error RateLimitExceeded) r) =>
  RateLimitKey ->
  PlainTextPassword6 ->
  Sem r Password
hashPassword6 key plain =
  doRateLimited key $ send $ HashPassword6 plain

tryHashPassword6 ::
  (Member RateLimit r, Member HashPassword r) =>
  RateLimitKey ->
  PlainTextPassword6 ->
  Sem r (Either RateLimitExceeded Password)
tryHashPassword6 key plain =
  tryRateLimited key $ send $ HashPassword6 plain

hashPassword8 ::
  (Member RateLimit r, Member HashPassword r, Member (Error RateLimitExceeded) r) =>
  RateLimitKey ->
  PlainTextPassword8 ->
  Sem r Password
hashPassword8 key plain =
  doRateLimited key $ send $ HashPassword8 plain

tryHashPassword8 ::
  (Member RateLimit r, Member HashPassword r) =>
  RateLimitKey ->
  PlainTextPassword8 ->
  Sem r (Either RateLimitExceeded Password)
tryHashPassword8 key plain =
  tryRateLimited key $ send $ HashPassword8 plain

verifyPasswordWithStatus ::
  (Member RateLimit r, Member HashPassword r, Member (Error RateLimitExceeded) r) =>
  RateLimitKey ->
  PlainTextPassword' t ->
  Password ->
  Sem r (Bool, PasswordStatus)
verifyPasswordWithStatus key plain hashed =
  doRateLimited key $ send $ VerifyPasswordWithStatus plain hashed

-- | Verify a plaintext password from user input against a stretched
-- password from persistent storage.
verifyPassword ::
  (Member HashPassword r, Member (Error RateLimitExceeded) r, Member RateLimit r) =>
  RateLimitKey ->
  PlainTextPassword' t ->
  Password ->
  Sem r Bool
verifyPassword rateLimitKey plain pwd = fst <$> verifyPasswordWithStatus rateLimitKey plain pwd
