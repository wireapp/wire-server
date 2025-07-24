module Wire.API.Team.HardTruncationLimit where

import Data.Proxy
import Data.Range
import GHC.TypeLits
import Imports

type HardTruncationLimit = (2000 :: Nat)

hardTruncationLimit :: (Integral a) => a
hardTruncationLimit = fromIntegral $ natVal (Proxy @HardTruncationLimit)

hardTruncationLimitRange :: (Integral a, n <= HardTruncationLimit, HardTruncationLimit <= m) => Range n m a
hardTruncationLimitRange = toRange $ Proxy @HardTruncationLimit
