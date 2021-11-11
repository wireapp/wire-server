module Spar.Sem.VerdictFormatStore where

import Data.Time (NominalDiffTime)
import Imports
import Polysemy
import Wire.API.User.Saml (AReqId, VerdictFormat)

data VerdictFormatStore m a where
  Store :: NominalDiffTime -> AReqId -> VerdictFormat -> VerdictFormatStore m ()
  Get :: AReqId -> VerdictFormatStore m (Maybe VerdictFormat)

makeSem ''VerdictFormatStore
