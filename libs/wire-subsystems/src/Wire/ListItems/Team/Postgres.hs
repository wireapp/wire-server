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
-- ANY WARRANTY; or even the implied warranty of MERCHANTABILITY or FITNESS FOR
-- A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

-- | PostgreSQL interpreters for the team instantiation of the 'ListItems'
-- effect (user -> teams pagination), reading the @user_team@ table created by
-- @postgres-migrations/20261021175638-team-store.sql@.
--
-- Both the Cassandra and Postgres interpreters serve the 'LegacyPaging'
-- instantiation as keyset paging on the ordered @team@ column, so the
-- semantics (stable ordering, truncation detection via an extra fetched row,
-- exclusive TeamId cursors) are preserved exactly across the cutover. The
-- 'InternalPaging' instantiation is Cassandra-only (its continuation token
-- existentially hides a Cassandra @Client@ closure and cannot be replayed
-- against Postgres); its sole consumer, 'Galley.API.Internal.rmUser', was
-- switched to the 'LegacyPaging' instantiation for exactly this reason.
module Wire.ListItems.Team.Postgres
  ( interpretTeamListToPostgres,
  )
where

import Data.Id
import Data.UUID (UUID)
import Data.Range
import Data.Vector (Vector)
import Data.Vector qualified as V
import Hasql.Statement qualified as Hasql
import Hasql.TH
import Imports
import Polysemy
import Polysemy.TinyLog
import Wire.ListItems
import Wire.Postgres
import Wire.Sem.Paging.Cassandra
import Wire.Util (logEffect)

interpretTeamListToPostgres ::
  (PGConstraints r, Member TinyLog r) =>
  Sem (ListItems LegacyPaging TeamId ': r) a ->
  Sem r a
interpretTeamListToPostgres = interpret $ \case
  ListItems uid ps lim -> do
    logEffect "TeamList.ListItems"
    rows <- selectUserTeamsFromPg uid ps (fromRange lim)
    -- One extra row is fetched purely to detect truncation; mkResultSetByLength
    -- strips it and reports ResultSetTruncated.
    pure $ mkResultSetByLength (fromIntegral (fromRange lim)) (V.toList rows)

-- | Keyset-paged fetch of a user's team memberships, ordered by team id.
-- Mirrors the Cassandra split between Cql.selectUserTeams and
-- Cql.selectUserTeamsFrom: Hasql.TH does not allow an optional placeholder
-- to be reused, so the with/without-cursor cases get separate statements.
selectUserTeamsFromPg ::
  (PGConstraints r) =>
  UserId ->
  Maybe TeamId ->
  Int32 ->
  Sem r (Vector TeamId)
selectUserTeamsFromPg usr mcursor mx = do
  -- mx is capped by PagingBounds (Range 1 100), so mx + 1 cannot overflow.
  let fetchLimit = mx + 1
  rows <- case toUUID <$> mcursor of
    Nothing -> runStatement (toUUID usr, fetchLimit) selectUserTeamsStatement
    Just cursor -> runStatement (toUUID usr, cursor, fetchLimit) selectUserTeamsFromStatement
  pure (Id <$> rows)

selectUserTeamsStatement :: Hasql.Statement (UUID, Int32) (Vector UUID)
selectUserTeamsStatement =
  [vectorStatement|
    select team :: uuid
      from user_team
     where user_id = $1 :: uuid
     order by team
     limit $2 :: int4
  |]

selectUserTeamsFromStatement :: Hasql.Statement (UUID, UUID, Int32) (Vector UUID)
selectUserTeamsFromStatement =
  [vectorStatement|
    select team :: uuid
      from user_team
     where user_id = $1 :: uuid
       and team > $2 :: uuid
     order by team
     limit $3 :: int4
  |]
