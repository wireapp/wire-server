module Spar.Sem.GalleyAccess where

import Bilge
import Polysemy

-- FUTUREWORK: A better effect here would be to exactly model the requests we
-- want to make of Galley. This would allow for non-HTTP interpretations.
data GalleyAccess m a where
  Call :: (Request -> Request) -> GalleyAccess m ResponseLBS

makeSem ''GalleyAccess
