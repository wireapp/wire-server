module Wire.MockInterpreters.RateLimit where

import Imports
import Polysemy
import Wire.RateLimit

noRateLimit :: Sem (RateLimit ': r) a -> Sem r a
noRateLimit =
  interpretH $ \case
    CheckRateLimit _ -> pureT 0
    DoRateLimited _ action -> runTSimple action
