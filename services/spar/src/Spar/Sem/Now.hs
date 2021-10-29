module Spar.Sem.Now where

import Polysemy
import qualified SAML2.WebSSO as SAML

data Now m a where
  Get :: Now m SAML.Time

makeSem ''Now
