-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

module Spar.Sem.BindCookieStore.Mem
  ( bindCookieStoreToMem,
  )
where

import Data.Id (UserId)
import qualified Data.Map as M
import Data.String.Conversions (cs)
import Imports
import Polysemy
import Polysemy.State
import SAML2.WebSSO
import qualified SAML2.WebSSO.Cookie as SAML
import qualified SAML2.WebSSO.Types as SAML
import Spar.Sem.BindCookieStore
import qualified Web.Cookie as Cky
import Wire.API.Cookie
import Wire.Sem.Now
import qualified Wire.Sem.Now as Now

bindCookieStoreToMem :: Member Now r => Sem (BindCookieStore ': r) a -> Sem r (Map BindCookie (SAML.Time, UserId), a)
bindCookieStoreToMem = (runState mempty .) $
  reinterpret $ \case
    Insert sbc uid ndt -> do
      let ckyval = BindCookie . cs . Cky.setCookieValue . SAML.fromSimpleSetCookie . getSimpleSetCookie $ sbc
      now <- Now.get
      modify $ M.insert ckyval (addTime ndt now, uid)
    Lookup bc -> do
      gets (M.lookup bc) >>= \case
        Just (time, uid) -> boolTTL @SAML.Time Nothing (Just uid) time
        Nothing -> pure Nothing
