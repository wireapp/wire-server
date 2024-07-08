module Wire.API.Team.HardTruncationLimit where

import Data.Proxy
import GHC.TypeLits
import Imports

type HardTruncationLimit = (2000 :: Nat)

hardTruncationLimit :: (Integral a) => a
hardTruncationLimit = fromIntegral $ natVal (Proxy @HardTruncationLimit)
