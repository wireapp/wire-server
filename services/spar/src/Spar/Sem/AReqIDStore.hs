module Spar.Sem.AReqIDStore where

import Data.Time (NominalDiffTime)
import Imports
import Polysemy
import qualified SAML2.WebSSO.Types as SAML
import Wire.API.User.Saml (AReqId, VerdictFormat)

data AReqIDStore m a where
  Store :: AReqId -> SAML.Time -> AReqIDStore m ()
  UnStore :: AReqId -> AReqIDStore m ()
  IsAlive :: AReqId -> AReqIDStore m Bool

makeSem ''AReqIDStore
