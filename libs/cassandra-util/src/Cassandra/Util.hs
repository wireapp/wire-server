module Cassandra.Util where

import Data.Time (UTCTime)
import Data.Time.Clock.POSIX(posixSecondsToUTCTime)
import Data.Int (Int64)

type Writetime a = Int64

writeTimeToUTC :: Writetime a -> UTCTime
writeTimeToUTC = posixSecondsToUTCTime . fromIntegral . (`div` 1000000)
