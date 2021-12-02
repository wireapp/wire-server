{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module Spar.Sem.ScimUserTimesStore.Mem where

import Data.Id (UserId)
import Data.Json.Util (UTCTimeMillis, toUTCTimeMillis)
import qualified Data.Map as M
import Imports
import Polysemy
import Polysemy.State
import Spar.Sem.ScimUserTimesStore
import Web.Scim.Schema.Common (WithId (WithId))
import Web.Scim.Schema.Meta (WithMeta (WithMeta), created, lastModified)

scimUserTimesStoreToMem ::
  Sem (ScimUserTimesStore ': r) a ->
  Sem r (Map UserId (UTCTimeMillis, UTCTimeMillis), a)
scimUserTimesStoreToMem = (runState mempty .) $
  reinterpret $ \case
    Write (WithMeta meta (WithId uid _)) -> modify $ M.insert uid (toUTCTimeMillis $ created meta, toUTCTimeMillis $ lastModified meta)
    Read uid -> gets $ M.lookup uid
    Delete uid -> modify $ M.delete uid
