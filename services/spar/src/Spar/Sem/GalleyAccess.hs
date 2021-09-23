module Spar.Sem.GalleyAccess where

import Bilge
import Polysemy

data GalleyAccess m a where
  Call :: (Request -> Request) -> GalleyAccess m ResponseLBS

makeSem ''GalleyAccess
