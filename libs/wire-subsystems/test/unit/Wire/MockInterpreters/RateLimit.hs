module Wire.MockInterpreters.RateLimit where

import Imports
import Polysemy
import Wire.RateLimit

noRateLimit :: Sem (RateLimit ': r) a -> Sem r a
noRateLimit =
  interpret $ \case
    CheckRateLimit _ -> pure 0
