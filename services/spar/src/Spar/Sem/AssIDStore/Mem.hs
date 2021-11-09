{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module Spar.Sem.AssIDStore.Mem where

import qualified Data.Map as M
import Imports
import Polysemy
import Polysemy.State
import qualified SAML2.WebSSO.Types as SAML
import Wire.API.User.Saml (AssId)
import Spar.Sem.AssIDStore
import Spar.Sem.Now
import qualified Spar.Sem.Now as Now


assIdStoreToMem
    :: Member Now r
    => Sem (AssIDStore ': r) a
    -> Sem r a
assIdStoreToMem = (evalState @(Map AssId SAML.Time) mempty .) $ reinterpret $ \x -> case x of
  Store assid ti -> modify $ M.insert assid ti
  UnStore assid -> modify $ M.delete assid
  IsAlive assid ->
    gets (M.lookup assid) >>= \case
      Just time -> do
        now <- Now.get
        pure $ now <= time
      Nothing -> pure False


