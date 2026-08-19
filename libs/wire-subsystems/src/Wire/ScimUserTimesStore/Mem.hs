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

module Wire.ScimUserTimesStore.Mem
  ( scimUserTimesStoreToMem,
  )
where

import Data.Id (UserId)
import Data.Json.Util (toUTCTimeMillis)
import Data.Map qualified as M
import Imports
import Polysemy
import Polysemy.State
import Web.Scim.Schema.Common (WithId (WithId))
import Web.Scim.Schema.Meta (WithMeta (WithMeta), created, lastModified)
import Wire.ScimUserTimesStore

scimUserTimesStoreToMem ::
  Sem (ScimUserTimesStore ': r) a ->
  Sem r (Map UserId ScimUserTimes, a)
scimUserTimesStoreToMem = (runState mempty .) $
  reinterpret $ \case
    Write emailType emailPrimary (WithMeta meta (WithId uid _)) ->
      modify $
        M.insert uid $
          ScimUserTimes
            (toUTCTimeMillis $ created meta)
            (toUTCTimeMillis $ lastModified meta)
            emailType
            emailPrimary
    Read uid -> gets $ M.lookup uid
    ReadMulti uids -> gets $ filter ((`elem` uids) . fst) . M.toList
    Delete uid -> modify $ M.delete uid
