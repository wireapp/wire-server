{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module Spar.Sem.AReqIDStore.Mem where

import qualified Data.Map as M
import Imports
import Polysemy
import Polysemy.State
import qualified SAML2.WebSSO.Types as SAML
import Spar.Sem.AReqIDStore
import Spar.Sem.Now
import Wire.API.User.Saml (AReqId)

aReqIDStoreToMem ::
  Member Now r =>
  Sem (AReqIDStore ': r) a ->
  Sem r a
aReqIDStoreToMem = (evalState @(Map AReqId SAML.Time) mempty .) $
  reinterpret $ \case
    Store areqid ti -> modify $ M.insert areqid ti
    UnStore areqid -> modify $ M.delete areqid
    IsAlive areqid ->
      gets (M.lookup areqid) >>= \case
        Just time -> do
          boolTTL False True time
        Nothing -> pure False
