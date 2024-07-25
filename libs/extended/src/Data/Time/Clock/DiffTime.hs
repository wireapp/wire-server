module Data.Time.Clock.DiffTime
  ( DiffTime,
    secondsToDiffTime,
    millisecondsToDiffTime,
    picosecondsToDiffTime,
    diffTimeToFullMicroseconds,
    diffTimeToPicoseconds,
  )
where

import Data.Time
import Imports

millisecondsToDiffTime :: Integer -> DiffTime
millisecondsToDiffTime = picosecondsToDiffTime . (e9 *)

-- | Rounds down. Useful for 'threadDelay', 'timeout', etc.
diffTimeToFullMicroseconds :: DiffTime -> Int
diffTimeToFullMicroseconds = fromInteger . (`div` e6) . diffTimeToPicoseconds

e6, e9 :: Integer
e6 = 1_000_000
e9 = 1_000_000_000
