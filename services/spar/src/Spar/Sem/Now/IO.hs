module Spar.Sem.Now.IO where

import Imports
import Polysemy
import SAML2.WebSSO (getNowIO)
import Spar.Sem.Now

nowToIO :: Member (Embed IO) r => Sem (Now ': r) a -> Sem r a
nowToIO = interpret $ \case
  Get -> embed @IO getNowIO
