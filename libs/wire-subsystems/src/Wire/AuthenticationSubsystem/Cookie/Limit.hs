module Wire.AuthenticationSubsystem.Cookie.Limit where

import Data.Aeson
import Data.RetryAfter
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Vector qualified as Vector
import Imports
import Statistics.Sample qualified as Stats
import Wire.API.User.Auth

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
  deriving (Eq, Ord, Show, Generic, Num)

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
