module Spar.Sem.Now.Input where

import Polysemy
import qualified SAML2.WebSSO as SAML
import Polysemy.Input
import Imports
import Spar.Sem.Now

nowToInput
    :: Member (Input SAML.Time) r
    => Sem (Now ': r) a
    -> Sem r a
nowToInput = interpret $ \case
  Get -> input

