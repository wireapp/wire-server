module Spar.Sem.BindCookieStore.Mem where

import Data.Id (UserId)
import Imports
import Polysemy
import Polysemy.State
import Wire.API.Cookie
import Spar.Sem.BindCookieStore
import Spar.Sem.Now
import qualified SAML2.WebSSO.Types as SAML
import qualified Data.Map as M
import qualified Web.Cookie as Cky
import qualified SAML2.WebSSO.Cookie as SAML
import Data.String.Conversions (cs)
import qualified Spar.Sem.Now as Now
import SAML2.WebSSO

bindCookieStoreToMem :: Member Now r => Sem (BindCookieStore ': r) a -> Sem r a
bindCookieStoreToMem = (evalState @(Map BindCookie (SAML.Time, UserId)) mempty .) $ reinterpret $ \case
  Insert sbc uid ndt -> do
    let ckyval = BindCookie . cs . Cky.setCookieValue . SAML.fromSimpleSetCookie . getSimpleSetCookie $ sbc
    now <- Now.get
    modify $ M.insert ckyval (addTime ndt now, uid)
  Lookup bc -> do
    gets (M.lookup bc) >>= \case
      Just (time, uid) -> boolTTL Nothing (Just uid) time
      Nothing -> pure Nothing

