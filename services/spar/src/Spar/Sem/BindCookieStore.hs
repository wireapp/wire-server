module Spar.Sem.BindCookieStore where

import Imports
import Polysemy
import Data.Time (NominalDiffTime)
import Wire.API.Cookie
import Data.Id (UserId)

data BindCookieStore m a where
  Insert :: SetBindCookie -> UserId -> NominalDiffTime -> BindCookieStore m ()
  Lookup :: BindCookie -> BindCookieStore m (Maybe UserId)

makeSem ''BindCookieStore

