-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.HashPassword where

import Data.Misc
import Imports
import Polysemy
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
  (Member RateLimit r, Member HashPassword r) =>
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
  (Member RateLimit r, Member HashPassword r) =>
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
  (Member RateLimit r, Member HashPassword r) =>
  RateLimitKey ->
  PlainTextPassword' t ->
  Password ->
  Sem r (Bool, PasswordStatus)
verifyPasswordWithStatus key plain hashed =
  doRateLimited key $ send $ VerifyPasswordWithStatus plain hashed

-- | Verify a plaintext password from user input against a stretched
-- password from persistent storage.
verifyPassword ::
  (Member HashPassword r, Member RateLimit r) =>
  RateLimitKey ->
  PlainTextPassword' t ->
  Password ->
  Sem r Bool
verifyPassword rateLimitKey plain pwd = fst <$> verifyPasswordWithStatus rateLimitKey plain pwd
