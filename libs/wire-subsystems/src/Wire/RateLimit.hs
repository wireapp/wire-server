{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

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

module Wire.RateLimit where

import Data.ByteString.Conversion
import Data.Hashable
import Data.Id
import Data.Misc
import Imports
import Polysemy
import Wire.API.Error
import Wire.API.Error.Brig qualified as E
import Wire.Arbitrary
import Wire.Error

data RateLimitKey
  = RateLimitIp IpAddr
  | RateLimitUser UserId
  | RateLimitProvider ProviderId
  | RateLimitInternal
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Hashable)
  deriving (Arbitrary) via (GenericUniform RateLimitKey)

withIpAddress :: RateLimitKey -> a -> (IpAddr -> a) -> a
withIpAddress (RateLimitIp ip) _ f = f ip
withIpAddress _ x _ = x

data RateLimit m a where
  -- | To be run before an action which should be rate limited. Returns min.
  -- wait time in microseconds before the rate limit is lifted. 0 means, no rate
  -- limit.
  CheckRateLimit :: RateLimitKey -> RateLimit m Word64
  DoRateLimited :: RateLimitKey -> m a -> RateLimit m a

makeSem ''RateLimit

data RateLimitExceeded = RateLimitExceeded Word64
  deriving (Show, Eq)

instance Exception RateLimitExceeded

rateLimitExceededToHttpError :: RateLimitExceeded -> HttpError
rateLimitExceededToHttpError (RateLimitExceeded micros) =
  RichError
    (errorToWai @E.RateLimitExceeded)
    ()
    [("Retry-After", toByteString' (micros `div` 1_000_000))]

-- Returns Left <min wait time in microseconds> when rate limited.
tryRateLimited :: (Member RateLimit r) => RateLimitKey -> Sem r a -> Sem r (Either RateLimitExceeded a)
tryRateLimited key action = do
  retryWait <- checkRateLimit key
  if retryWait == 0
    then Right <$> action
    else pure $ Left $ RateLimitExceeded retryWait
