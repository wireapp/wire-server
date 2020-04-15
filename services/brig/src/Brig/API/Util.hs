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

module Brig.API.Util where

import Brig.API.Handler
import Brig.App (Env, settings)
import qualified Brig.Data.User as Data
import Brig.Options (defEnableFederation, enableFederation)
import Brig.Types
import Control.Lens (view)
import Control.Monad
import Data.Id as Id
import Data.IdMapping (MappedOrLocalId (Local))
import Data.Maybe
import Imports

lookupProfilesMaybeFilterSameTeamOnly :: UserId -> [UserProfile] -> Handler [UserProfile]
lookupProfilesMaybeFilterSameTeamOnly self us = do
  selfTeam <- lift $ Data.lookupUserTeam self
  return $ case selfTeam of
    Just team -> filter (\x -> profileTeam x == Just team) us
    Nothing -> us

-- FUTUREWORK(federation, #1178): implement function to resolve IDs in batch

-- | this exists as a shim to find and mark places where we need to handle 'OpaqueUserId's.
resolveOpaqueUserId :: MonadReader Env m => OpaqueUserId -> m (MappedOrLocalId Id.U)
resolveOpaqueUserId (Id opaque) = do
  mEnabled <- view (settings . enableFederation)
  case fromMaybe defEnableFederation mEnabled of
    False ->
      -- don't check the ID mapping, just assume it's local
      pure . Local $ Id opaque
    True ->
      -- FUTUREWORK(federation, #1178): implement database lookup
      pure . Local $ Id opaque
