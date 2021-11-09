{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module Spar.Sem.ScimExternalIdStore.Mem where

import Data.Id (TeamId, UserId)
import qualified Data.Map as M
import Imports
import Polysemy
import Polysemy.State
import Spar.Sem.ScimExternalIdStore
import Wire.API.User.Identity (Email)

scimExternalIdStoreToMem ::
  Sem (ScimExternalIdStore ': r) a ->
  Sem r a
scimExternalIdStoreToMem = (evalState @(Map (TeamId, Email) UserId) mempty .) $
  reinterpret $ \case
    Insert tid em uid -> modify $ M.insert (tid, em) uid
    Lookup tid em -> gets $ M.lookup (tid, em)
    Delete tid em -> modify $ M.delete (tid, em)
