-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
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

module Galley.API.IdMapping
  ( -- * other functions
    resolveOpaqueUserId,
    resolveOpaqueConvId,
  )
where

import Data.Id (Id (Id, toUUID), OpaqueConvId, OpaqueUserId)
import qualified Data.Id as Id
import Data.IdMapping (MappedOrLocalId (Local))
import Galley.App (Galley)
import Imports

-- DEPRECATED, REMOVE
resolveOpaqueUserId :: OpaqueUserId -> Galley (MappedOrLocalId Id.U)
resolveOpaqueUserId = resolveOpaqueId

-- DEPRECATED, REMOVE
resolveOpaqueConvId :: OpaqueConvId -> Galley (MappedOrLocalId Id.C)
resolveOpaqueConvId = resolveOpaqueId

-- DEPRECATED, REMOVE
resolveOpaqueId :: forall a. Id (Id.Opaque a) -> Galley (MappedOrLocalId a)
resolveOpaqueId opaqueId = pure $ Local assumedLocalId
  where
    assumedLocalId = Id (toUUID opaqueId) :: Id a
