module Data.Time.Clock.DiffTime
  ( DiffTime,
    weeksToDiffTime,
    daysToDiffTime,
    hoursToDiffTime,
    minutesToDiffTime,
    secondsToDiffTime,
    millisecondsToDiffTime,
    microsecondsToDiffTime,
    nanosecondsToDiffTime,
    picosecondsToDiffTime,
    diffTimeToFullMicroseconds,
    diffTimeToPicoseconds,
  )
where

import Data.Time
import Imports

weeksToDiffTime,
  daysToDiffTime,
  hoursToDiffTime,
  minutesToDiffTime,
  millisecondsToDiffTime,
  microsecondsToDiffTime,
  nanosecondsToDiffTime ::
    Integer -> DiffTime
weeksToDiffTime = daysToDiffTime . (7 *)
daysToDiffTime = hoursToDiffTime . (24 *)
hoursToDiffTime = minutesToDiffTime . (60 *)
minutesToDiffTime = secondsToDiffTime . (60 *)
millisecondsToDiffTime = picosecondsToDiffTime . (e9 *)
microsecondsToDiffTime = picosecondsToDiffTime . (e6 *)
nanosecondsToDiffTime = picosecondsToDiffTime . (e3 *)

-- | Rounds down. Useful for 'threadDelay', 'timeout', etc.
diffTimeToFullMicroseconds :: DiffTime -> Int
diffTimeToFullMicroseconds = fromInteger . (`div` e6) . diffTimeToPicoseconds

e3, e6, e9 :: Integer
e3 = 1_000
e6 = 1_000_000
e9 = 1_000_000_000
