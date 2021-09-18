module Spar.Sem.BindCookie where

import qualified Wire.API.Cookie as C
import Data.Id
import Imports
import Polysemy
import Data.Time (NominalDiffTime)


data BindCookie m a where
  Insert :: C.SetBindCookie -> UserId -> NominalDiffTime -> BindCookie m ()
  Lookup :: C.BindCookie -> BindCookie m (Maybe UserId)

makeSem ''BindCookie
