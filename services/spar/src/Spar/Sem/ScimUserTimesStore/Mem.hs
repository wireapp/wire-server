{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

module Spar.Sem.ScimUserTimesStore.Mem (
  scimUserTimesStoreToMem ) where

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
