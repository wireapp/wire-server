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

module Wire.TeamStore.DualWrite
  ( interpretTeamStoreMetadataToCassandraAndPostgres,
  )
where

import Cassandra (ClientState)
import Control.Lens ((^.))
import Data.Id (TeamId, UserId)
import Imports
import Polysemy
import Polysemy.Async
import Polysemy.Conc.Effect.Race (Race)
import Polysemy.Error (Error)
import Polysemy.Input
import Polysemy.Resource (Resource)
import Polysemy.Time (MilliSeconds (..))
import Polysemy.TinyLog
import Wire.API.Team.Member (userId)
import Wire.ConversationStore (ConversationStore)
import Wire.MigrationLock
import Wire.Postgres (PGConstraints)
import Wire.TeamStore
import Wire.TeamStore.Cassandra qualified as Cassandra
import Wire.TeamStore.Postgres qualified as Postgres

-- | Dual-write interpreter for 'TeamStore' (WPB-22956 + WPB-22957): writes go
-- to both Cassandra and Postgres; reads go to Cassandra, which remains the
-- source of truth during migration.
interpretTeamStoreMetadataToCassandraAndPostgres ::
  ( PGConstraints r,
    Member (Input ClientState) r,
    Member TinyLog r,
    Member ConversationStore r,
    Member Async r,
    Member Race r,
    Member Resource r,
    Member (Error MigrationLockError) r
  ) =>
  InterpreterFor TeamStore r
interpretTeamStoreMetadataToCassandraAndPostgres = interpret $ \case
  -- Metadata writes: both stores.  The Cassandra leg runs through the full
  -- Cassandra interpreter, which stays the source of truth.
  CreateTeam mtid uid n i k b -> do
    t <- Cassandra.interpretTeamStoreToCassandra (createTeam mtid uid n i k b)
    Postgres.insertTeamPg t
    pure t
  SetTeamData tid upd -> do
    Cassandra.interpretTeamStoreToCassandra (setTeamData tid upd)
    Postgres.updateTeamPg tid upd
  SetTeamStatus tid st -> do
    Cassandra.interpretTeamStoreToCassandra (setTeamStatus tid st)
    Postgres.updateTeamStatusPg tid st
  SetSearchVisibility tid v -> do
    Cassandra.interpretTeamStoreToCassandra (setSearchVisibility tid v)
    Postgres.setSearchVisibilityPg tid v
  ResetSearchVisibility tid -> do
    Cassandra.interpretTeamStoreToCassandra (resetSearchVisibility tid)
    Postgres.resetSearchVisibilityPg tid
  DeleteTeam tid -> do
    -- The Postgres halves (metadata rewrite plus member-row sweep; the member
    -- removal lives in the Cassandra implementation, member tables are still
    -- Cassandra-owned in this PR), so the whole constructor is additionally
    -- routed through the Cassandra interpreter, which also rewrites the
    -- metadata there.
    Postgres.deleteTeamPg tid
    Postgres.removeTeamMemberRowsPg tid
    Cassandra.interpretTeamStoreToCassandra (deleteTeam tid)
  -- Metadata reads: Cassandra is the source of truth.
  GetTeam tid -> Cassandra.interpretTeamStoreToCassandra (getTeam tid)
  GetTeamName tid -> Cassandra.interpretTeamStoreToCassandra (getTeamName tid)
  GetTeamsBindings tids -> Cassandra.interpretTeamStoreToCassandra (getTeamsBindings tids)
  GetTeamBinding tid -> Cassandra.interpretTeamStoreToCassandra (getTeamBinding tid)
  GetTeamCreationTime tid -> Cassandra.interpretTeamStoreToCassandra (getTeamCreationTime tid)
  GetSearchVisibility tid -> Cassandra.interpretTeamStoreToCassandra (getSearchVisibility tid)
  -- Member writes: both stores.  The shared (team, user) lock serializes
  -- against the copy loop's exclusive lock, so a concurrent write can not be
  -- clobbered by a stale copy; on lock timeout only the Cassandra leg has
  -- run, and the copy loop reconciles Postgres afterwards.
  CreateTeamMember tid mem ->
    withMemberLock tid (mem ^. userId) $ do
      Cassandra.interpretTeamStoreToCassandra (createTeamMember tid mem)
      Postgres.addTeamMemberPg tid mem
  SetTeamMemberPermissions old tid uid new ->
    withMemberLock tid uid $ do
      Cassandra.interpretTeamStoreToCassandra (setTeamMemberPermissions old tid uid new)
      Postgres.updateTeamMemberPg old tid uid new
  DeleteTeamMember tid uid ->
    withMemberLock tid uid $ do
      Cassandra.interpretTeamStoreToCassandra (deleteTeamMember tid uid)
      Postgres.removeTeamMemberPg tid uid
  -- Member reads: Cassandra is the source of truth during migration.
  GetTeamMember tid uid -> Cassandra.interpretTeamStoreToCassandra (getTeamMember tid uid)
  GetTeamMembers tid -> Cassandra.interpretTeamStoreToCassandra (getTeamMembers tid)
  GetTeamMembersWithLimit tid n -> Cassandra.interpretTeamStoreToCassandra (getTeamMembersWithLimit tid n)
  SelectTeamMembers tid uids -> Cassandra.interpretTeamStoreToCassandra (selectTeamMembers tid uids)
  SelectTeamMemberInfos tid uids -> Cassandra.interpretTeamStoreToCassandra (selectTeamMemberInfos tid uids)
  GetBillingTeamMembers tid -> Cassandra.interpretTeamStoreToCassandra (getBillingTeamMembers tid)
  GetTeamAdmins tid -> Cassandra.interpretTeamStoreToCassandra (getTeamAdmins tid)
  SelectTeams uid tids -> Cassandra.interpretTeamStoreToCassandra (selectTeams uid tids)
  GetUserTeams uid -> Cassandra.interpretTeamStoreToCassandra (getUserTeams uid)
  GetUsersTeams uids -> Cassandra.interpretTeamStoreToCassandra (getUsersTeams uids)
  GetOneUserTeam uid -> Cassandra.interpretTeamStoreToCassandra (getOneUserTeam uid)

-- | The shared half of the copy loop's per-(team, user) advisory lock: held
-- by member writes in dual-write mode so the migration worker's exclusive
-- lock can not interleave a stale copy.
withMemberLock ::
  ( PGConstraints r,
    Member TinyLog r,
    Member Async r,
    Member Race r,
    Member Resource r,
    Member (Error MigrationLockError) r
  ) =>
  TeamId ->
  UserId ->
  Sem r x ->
  Sem r x
withMemberLock tid uid =
  withMigrationLocks LockShared (MilliSeconds 500) [(tid, uid)]
