module Spar.Sem.AReqIDStore where

import Imports
import Polysemy
import qualified SAML2.WebSSO.Types as SAML
import Wire.API.User.Saml (AReqId, VerdictFormat)
import Data.Time (NominalDiffTime)

data AReqIDStore m a where
  Store :: AReqId -> SAML.Time -> AReqIDStore m ()
  UnStore :: AReqId -> AReqIDStore m ()
  IsAlive :: AReqId -> AReqIDStore m Bool
  StoreVerdictFormat :: NominalDiffTime -> AReqId -> VerdictFormat -> AReqIDStore m ()
  GetVerdictFormat :: AReqId -> AReqIDStore m (Maybe VerdictFormat)

makeSem ''AReqIDStore
