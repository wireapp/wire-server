module Data.Hourglass.Const (midnight) where

import Data.Hourglass

-- | Midnight time (00:00:00.000)
midnight :: TimeOfDay
midnight = TimeOfDay (Hours 0) (Minutes 0) (Seconds 0) 0
