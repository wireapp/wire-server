-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
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

module Brig.User.Auth.Cookie.Limit where

import Brig.Types.User.Auth
import Data.Aeson
import Data.List (sortBy)
import Data.Time.Clock
import Data.Time.Clock.POSIX
import qualified Data.Vector as Vector
import Imports
import qualified Statistics.Sample as Stats

--------------------------------------------------------------------------------
-- Quantitive Limiting

newtype CookieLimit = CookieLimit
  {cookieLimitTotal :: Int}

-- | Limit the given list of cookies, returning those excess
-- cookies which should be evicted. Based on the following order
-- of preference:
--
--   1. Expired cookies with regards to the given 'UTCTime'.
--   2. Oldest cookies, as judged by their creation dates,
--      with a preference for those which are superseded
--      (i.e. have a successor cookie).
limitCookies :: CookieLimit -> UTCTime -> [Cookie a] -> [Cookie a]
limitCookies lim now cs
  | freeSlots > 0 = []
  | otherwise =
    let carry = max 1 (abs freeSlots)
     in take carry (sortBy preference cs)
  where
    freeSlots = cookieLimitTotal lim - length cs
    preference a b
      | cookieExpires a < now = LT
      | cookieExpires b < now = GT
      | otherwise = case (cookieSucc a, cookieSucc b) of
        (Just _, Nothing) -> LT
        (Nothing, Just _) -> GT
        (_, _) -> comparing cookieCreated a b

--------------------------------------------------------------------------------
-- Temporal Throttling

-- | The fields are:
--
-- * Min. standard deviation cookie creation
-- * Wait time when the min deviation is violated
--
-- Both fields are in seconds.
data CookieThrottle
  = StdDevThrottle StdDev RetryAfter
  deriving (Show)

newtype StdDev = StdDev Double
  deriving (Eq, Ord, Show, Generic)

newtype RetryAfter = RetryAfter
  {retryAfterSeconds :: Int64}
  deriving (Eq, Show)

instance FromJSON StdDev

instance FromJSON CookieThrottle where
  parseJSON = withObject "User.Auth.Cookie.Limit.CookieThrottle" $ \o ->
    StdDevThrottle
      <$> o .: "stdDev"
      <*> (RetryAfter <$> o .: "retryAfter")

-- | Check that the standard deviation of cookie creation dates is /higher/
-- than the specified minimum. If the standard deviation is below the
-- minimum, check if the specified wait time (i.e. penalty) is over to
-- enable recovery.
throttleCookies :: UTCTime -> CookieThrottle -> [Cookie a] -> Maybe RetryAfter
throttleCookies now (StdDevThrottle dev del) cc =
  let (sd, mx) = cookieStats cc
      delta = round (utcTimeToPOSIXSeconds now) - mx
      delay = retryAfterSeconds del
   in if sd > dev || delta >= delay
        then Nothing
        else Just (RetryAfter (delay - delta))

-- | Calculate the standard deviation and maximum cookie creation times.
cookieStats :: [Cookie a] -> (StdDev, Int64)
cookieStats cc =
  let xs = map (round . utcTimeToPOSIXSeconds . cookieCreated) cc
      sd = Stats.stdDev (Vector.fromList (map fromIntegral (xs :: [Int64])))
   in (StdDev sd, maximum xs)
