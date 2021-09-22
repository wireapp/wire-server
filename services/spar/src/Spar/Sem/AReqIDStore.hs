module Spar.Sem.AReqIDStore where

import Wire.API.User.Saml (AReqId)
import qualified SAML2.WebSSO.Types as SAML
import Imports
import Polysemy

data AReqIDStore m a where
  Store :: AReqId -> SAML.Time -> AReqIDStore m ()
  UnStore :: AReqId -> AReqIDStore m ()
  IsAlive :: AReqId -> AReqIDStore m Bool

makeSem ''AReqIDStore

