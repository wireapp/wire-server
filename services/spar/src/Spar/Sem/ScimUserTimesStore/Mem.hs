{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module Spar.Sem.ScimUserTimesStore.Mem where

import qualified Data.Map as M
import Imports
import Polysemy
import Polysemy.State
import Spar.Sem.ScimUserTimesStore
import Web.Scim.Schema.Meta (WithMeta(WithMeta), created, lastModified)
import Web.Scim.Schema.Common (WithId(WithId))
import Data.Json.Util (UTCTimeMillis, toUTCTimeMillis)
import Data.Id (UserId)

scimUserTimesStoreToMem ::
  Sem (ScimUserTimesStore ': r) a ->
  Sem r a
scimUserTimesStoreToMem = (evalState @(Map UserId (UTCTimeMillis, UTCTimeMillis)) mempty .) $
  reinterpret $ \case
  Write (WithMeta meta (WithId uid _)) -> modify $ M.insert uid (toUTCTimeMillis $ created meta, toUTCTimeMillis $ lastModified meta)
  Read uid -> gets $ M.lookup uid
  Delete uid -> modify $ M.delete uid

