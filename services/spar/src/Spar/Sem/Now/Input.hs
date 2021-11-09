module Spar.Sem.Now.Input where

import Imports
import Polysemy
import Polysemy.Input
import qualified SAML2.WebSSO as SAML
import Spar.Sem.Now

nowToInput ::
  Member (Input SAML.Time) r =>
  Sem (Now ': r) a ->
  Sem r a
nowToInput = interpret $ \case
  Get -> input
