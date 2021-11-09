{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module Spar.Sem.ScimTokenStore.Mem where

import qualified Data.Map as M
import Imports
import Polysemy
import Polysemy.State
import Spar.Scim
import Spar.Sem.ScimTokenStore

scimTokenStoreToMem ::
  Sem (ScimTokenStore ': r) a ->
  Sem r a
scimTokenStoreToMem = (evalState @(Map ScimToken ScimTokenInfo) mempty .) $
  reinterpret $ \case
    Insert st sti -> modify $ M.insert st sti
    Lookup st -> gets $ M.lookup st
    GetByTeam tid -> gets $ filter ((== tid) . stiTeam) . M.elems
    Delete tid stid -> modify $ M.filter $ \sti -> not $ stiTeam sti == tid && stiId sti == stid
    DeleteByTeam tid -> modify $ M.filter (not . (== tid) . stiTeam)
