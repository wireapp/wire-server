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

module Galley.Cassandra.Team
  ( interpretTeamListToCassandra,
    interpretInternalTeamListToCassandra,
  )
where

import Cassandra
import Data.Id
import Data.Range
import Galley.Cassandra.Store
import Galley.Cassandra.Util
import Imports hiding (Set, max)
import Polysemy
import Polysemy.Input
import Polysemy.TinyLog
import Wire.ListItems
import Wire.Sem.Paging.Cassandra
import Wire.TeamStore.Cassandra.Queries qualified as Cql

interpretTeamListToCassandra ::
  ( Member (Embed IO) r,
    Member (Input ClientState) r,
    Member TinyLog r
  ) =>
  Sem (ListItems LegacyPaging TeamId ': r) a ->
  Sem r a
interpretTeamListToCassandra = interpret $ \case
  ListItems uid ps lim -> do
    logEffect "TeamList.ListItems"
    embedClient $ teamIdsFrom uid ps lim

interpretInternalTeamListToCassandra ::
  ( Member (Embed IO) r,
    Member (Input ClientState) r,
    Member TinyLog r
  ) =>
  Sem (ListItems InternalPaging TeamId ': r) a ->
  Sem r a
interpretInternalTeamListToCassandra = interpret $ \case
  ListItems uid mps lim -> do
    logEffect "InternalTeamList.ListItems"
    embedClient $ case mps of
      Nothing -> do
        page <- teamIdsForPagination uid Nothing lim
        mkInternalPage page pure
      Just ps -> ipNext ps

teamIdsFrom :: UserId -> Maybe TeamId -> Range 1 100 Int32 -> Client (ResultSet TeamId)
teamIdsFrom usr range (fromRange -> max) =
  mkResultSet . fmap runIdentity . strip <$> case range of
    Just c -> paginate Cql.selectUserTeamsFrom (paramsP LocalQuorum (usr, c) (max + 1))
    Nothing -> paginate Cql.selectUserTeams (paramsP LocalQuorum (Identity usr) (max + 1))
  where
    strip p = p {result = take (fromIntegral max) (result p)}

teamIdsForPagination :: UserId -> Maybe TeamId -> Range 1 100 Int32 -> Client (Page TeamId)
teamIdsForPagination usr range (fromRange -> max) =
  fmap runIdentity <$> case range of
    Just c -> paginate Cql.selectUserTeamsFrom (paramsP LocalQuorum (usr, c) max)
    Nothing -> paginate Cql.selectUserTeams (paramsP LocalQuorum (Identity usr) max)
