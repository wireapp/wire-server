module Spar.Sem.AssIDStore where

import Imports
import Polysemy
import qualified SAML2.WebSSO.Types as SAML
import Wire.API.User.Saml (AssId)

data AssIDStore m a where
  Store :: AssId -> SAML.Time -> AssIDStore m ()
  UnStore :: AssId -> AssIDStore m ()
  IsAlive :: AssId -> AssIDStore m Bool

makeSem ''AssIDStore
