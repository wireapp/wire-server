-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.MockInterpreters.PropertyStore where

import Data.Id
import Data.Map qualified as Map
import Imports
import Polysemy
import Polysemy.State
import Wire.API.Properties
import Wire.PropertyStore

inMemoryPropertyStoreInterpreter :: (Member (State (Map UserId (Map PropertyKey RawPropertyValue))) r) => InterpreterFor PropertyStore r
inMemoryPropertyStoreInterpreter = interpret $ \case
  InsertProperty u k v -> modify $ Map.insertWith (Map.union) u (Map.singleton k v)
  LookupProperty u k -> gets $ Map.lookup k <=< Map.lookup u
  CountProperties u -> gets $ Map.size . Map.findWithDefault mempty u
  DeleteProperty u k -> modify $ Map.adjust (Map.delete k) u
  ClearProperties u -> modify $ Map.delete u
  GetPropertyKeys u -> gets $ Map.keys . Map.findWithDefault mempty u
  GetAllProperties u -> gets $ Map.toAscList . Map.findWithDefault mempty u
