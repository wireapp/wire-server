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

module Galley.API.MLS.Types
  ( ClientMap,
    mkClientMap,
    cmAssocs,
  )
where

import Data.Domain
import Data.Id
import qualified Data.Map as Map
import Data.Qualified
import qualified Data.Set as Set
import Imports
import Wire.API.MLS.KeyPackage

type ClientMap = Map (Qualified UserId) (Set (ClientId, KeyPackageRef))

mkClientMap :: [(Domain, UserId, ClientId, KeyPackageRef)] -> ClientMap
mkClientMap = foldr addEntry mempty
  where
    addEntry :: (Domain, UserId, ClientId, KeyPackageRef) -> ClientMap -> ClientMap
    addEntry (dom, usr, c, kpr) =
      Map.insertWith (<>) (Qualified usr dom) (Set.singleton (c, kpr))

cmAssocs :: ClientMap -> [(Qualified UserId, (ClientId, KeyPackageRef))]
cmAssocs cm = Map.assocs cm >>= traverse toList
