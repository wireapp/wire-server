module Spar.Sem.BindCookieStore.Mem where

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
import Spar.Sem.Now
import qualified Spar.Sem.Now as Now
import qualified Web.Cookie as Cky
import Wire.API.Cookie

bindCookieStoreToMem :: Member Now r => Sem (BindCookieStore ': r) a -> Sem r a
bindCookieStoreToMem = (evalState @(Map BindCookie (SAML.Time, UserId)) mempty .) $
  reinterpret $ \case
    Insert sbc uid ndt -> do
      let ckyval = BindCookie . cs . Cky.setCookieValue . SAML.fromSimpleSetCookie . getSimpleSetCookie $ sbc
      now <- Now.get
      modify $ M.insert ckyval (addTime ndt now, uid)
    Lookup bc -> do
      gets (M.lookup bc) >>= \case
        Just (time, uid) -> boolTTL Nothing (Just uid) time
        Nothing -> pure Nothing
