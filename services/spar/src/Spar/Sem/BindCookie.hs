module Spar.Sem.BindCookie where

import Data.Id
import Data.Time (NominalDiffTime)
import Imports
import Polysemy
import qualified Wire.API.Cookie as C

data BindCookie m a where
  Insert :: C.SetBindCookie -> UserId -> NominalDiffTime -> BindCookie m ()
  Lookup :: C.BindCookie -> BindCookie m (Maybe UserId)

makeSem ''BindCookie
