{-# LANGUAGE TemplateHaskell #-}

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

module Wire.IndexedUserStore where

import Data.Id
import Database.Bloodhound qualified as ES
import Database.Bloodhound.Types hiding (SearchResult)
import Imports
import Polysemy
import Wire.API.Team.Size
import Wire.API.User.Search
import Wire.UserSearch.Types

data IndexedUserStoreError
  = IndexUpdateError ES.EsError
  | IndexLookupError ES.EsError
  | IndexError Text
  deriving (Show)

instance Exception IndexedUserStoreError

data IndexedUserStore m a where
  Upsert :: DocId -> UserDoc -> VersionControl -> IndexedUserStore m ()
  UpdateTeamSearchVisibilityInbound ::
    TeamId ->
    SearchVisibilityInbound ->
    IndexedUserStore m ()
  -- | Will only be applied to main ES index and not the additional one
  BulkUpsert :: [(DocId, UserDoc, VersionControl)] -> IndexedUserStore m ()
  DoesIndexExist :: IndexedUserStore m Bool
  SearchUsers ::
    UserId ->
    Maybe TeamId ->
    TeamSearchInfo ->
    Text ->
    Int ->
    IndexedUserStore m (SearchResult UserDoc)
  PaginateTeamMembers ::
    BrowseTeamFilters ->
    Int ->
    Maybe PagingState ->
    IndexedUserStore m (SearchResult UserDoc)
  GetTeamSize :: TeamId -> IndexedUserStore m TeamSize

makeSem ''IndexedUserStore
