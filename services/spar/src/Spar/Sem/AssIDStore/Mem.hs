{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module Spar.Sem.AssIDStore.Mem where

import qualified Data.Map as M
import Imports
import Polysemy
import Polysemy.State
import qualified SAML2.WebSSO.Types as SAML
import Spar.Sem.AssIDStore
import Spar.Sem.Now
import qualified Spar.Sem.Now as Now
import Wire.API.User.Saml (AssId)

assIdStoreToMem ::
  Member Now r =>
  Sem (AssIDStore ': r) a ->
  Sem r a
assIdStoreToMem = (evalState @(Map AssId SAML.Time) mempty .) $
  reinterpret $ \case
    Store assid ti -> modify $ M.insert assid ti
    UnStore assid -> modify $ M.delete assid
    IsAlive assid ->
      gets (M.lookup assid) >>= \case
        Just time -> boolTTL False True time
        Nothing -> pure False
