{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module Spar.Sem.VerdictFormatStore.Mem where

import qualified Data.Map as M
import Imports
import Polysemy
import Polysemy.State hiding (Get)
import SAML2.WebSSO (addTime)
import qualified SAML2.WebSSO.Types as SAML
import Spar.Sem.VerdictFormatStore
import Spar.Sem.Now (Now, boolTTL)
import qualified Spar.Sem.Now as Now
import Wire.API.User.Saml (AReqId, VerdictFormat)

verdictFormatStoreToMem ::
  Member Now r =>
  Sem (VerdictFormatStore ': r) a ->
  Sem r a
verdictFormatStoreToMem =
  (evalState @(Map AReqId (SAML.Time, VerdictFormat)) mempty .) $
    reinterpret $ \case
      Store ndt areqid vf -> do
        now <- Now.get
        modify $ M.insert areqid (addTime ndt now, vf)
      Get areqid -> do
        gets (M.lookup areqid) >>= \case
          Just (time, vf) -> do
            boolTTL Nothing (Just vf) time
          Nothing -> pure Nothing

