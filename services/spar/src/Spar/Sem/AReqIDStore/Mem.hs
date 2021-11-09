{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module Spar.Sem.AReqIDStore.Mem where

import qualified Data.Map as M
import Imports
import Polysemy
import Polysemy.State
import qualified SAML2.WebSSO.Types as SAML
import Wire.API.User.Saml (AReqId, VerdictFormat)
import Spar.Sem.AReqIDStore
import Spar.Sem.Now
import qualified Spar.Sem.Now as Now
import SAML2.WebSSO (addTime)


aReqIDStoreToMem
    :: Member Now r
    => Sem (AReqIDStore ': r) a
    -> Sem r a
aReqIDStoreToMem = (evalState @(Map AReqId (SAML.Time, VerdictFormat)) mempty .) $ (evalState @(Map AReqId SAML.Time) mempty .) $ reinterpret2 $ \x -> case x of
  Store areqid ti -> modify $ M.insert areqid ti
  UnStore areqid -> modify $ M.delete areqid
  IsAlive areqid ->
    gets (M.lookup areqid) >>= \case
      Just time -> do
        now <- Now.get
        pure $ now <= time
      Nothing -> pure False
  StoreVerdictFormat ndt areqid vf -> do
    now <- Now.get
    modify $ M.insert areqid (addTime ndt now, vf)
  GetVerdictFormat areqid -> do
    gets (M.lookup areqid) >>= \case
      Just (time, vf) -> do
        now <- Now.get
        pure $
          if (now <= time)
           then Just vf
           else Nothing
      Nothing -> pure Nothing

