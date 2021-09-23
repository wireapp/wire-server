module Spar.Sem.BindCookieStore where

import Data.Id (UserId)
import Data.Time (NominalDiffTime)
import Imports
import Polysemy
import Wire.API.Cookie

data BindCookieStore m a where
  Insert :: SetBindCookie -> UserId -> NominalDiffTime -> BindCookieStore m ()
  Lookup :: BindCookie -> BindCookieStore m (Maybe UserId)

makeSem ''BindCookieStore
