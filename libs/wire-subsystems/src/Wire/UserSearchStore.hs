{-# LANGUAGE TemplateHaskell #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2026 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.UserSearchStore where

import Data.Id
import Data.Qualified (Local)
import Imports
import Polysemy
import Wire.API.Team.Size
import Wire.API.User.Search

-- | User search queries backed directly by brig's user store (Postgres),
-- replacing the former ElasticSearch-backed 'Wire.IndexedUserStore'.
data UserSearchStore m a where
  -- | Full-text search over user names and handles (prefix matching, see the
  -- swagger docs of @/users/search@ for rank ordering).  Excludes the searcher
  -- and exact-handle matches (the latter are fetched from the user store by
  -- the caller).
  SearchUsers ::
    -- | The searcher; its domain qualifies the returned contacts.
    Local UserId ->
    Maybe TeamId ->
    TeamSearchInfo ->
    Text ->
    Int ->
    Maybe [UserTypeFilter] ->
    UserSearchStore m (SearchResult Contact)
  -- | Team member browse (formerly @/teams/:tid/browse@ via ES).  Fills
  -- 'TeamContact.teamContactRole'; 'teamContactUserGroups' is left empty and is
  -- filled by the caller.
  PaginateTeamMembers ::
    BrowseTeamFilters ->
    Int ->
    Maybe PagingState ->
    UserSearchStore m (SearchResult TeamContact)
  -- | Number of activated, non-deleted team members, split by regular users
  -- and apps.  Used for max-team-size enforcement.
  GetTeamSize :: TeamId -> UserSearchStore m TeamSize
  -- | Inbound federated search (a remote backend searching this backend).
  -- @Nothing@ allows non-team users and members of teams opted in to
  -- searchable-by-all-teams; @Just []@ matches nothing; @Just teams@ allows
  -- only opted-in members of the given teams.
  SearchUsersFederated ::
    Maybe [TeamId] ->
    Text ->
    Int ->
    Maybe [UserTypeFilter] ->
    UserSearchStore m (SearchResult Contact)
  -- | Upsert the inbound search visibility setting of a team (pushed from
  -- galley when the corresponding team feature changes).
  SetTeamSearchVisibilityInbound :: TeamId -> SearchVisibilityInbound -> UserSearchStore m ()

makeSem ''UserSearchStore
