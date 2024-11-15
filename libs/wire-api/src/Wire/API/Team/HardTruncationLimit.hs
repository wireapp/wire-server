module Wire.API.Team.HardTruncationLimit where

import Data.Proxy
import GHC.TypeLits
import Imports

-- TODO: do not merge this change...
type HardTruncationLimit = (6000 :: Nat)

hardTruncationLimit :: (Integral a) => a
hardTruncationLimit = fromIntegral $ natVal (Proxy @HardTruncationLimit)
