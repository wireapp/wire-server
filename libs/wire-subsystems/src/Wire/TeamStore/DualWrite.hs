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
import Cassandra.Util (Writetime (..), writetimeToInt64, writetimeToUTC)
import Control.Lens ((^.))
import Data.Id (TeamId, UserId)
import Data.Time.Clock (getCurrentTime)
import Imports
import Wire.API.Routes.Internal.Galley.TeamsIntra (TeamStatus)
import Wire.API.Team.SearchVisibility (TeamSearchVisibility (SearchVisibilityStandard))
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
import Wire.TeamStore.Postgres (routeCassandra)
import Wire.TeamStore.Postgres qualified as Postgres
import Wire.Util (embedClientInput)

-- | Client-chosen LWW stamp for one dual write: picked once per constructor,
-- written to Cassandra with @using timestamp@ and applied to Postgres behind
-- per-column writetime guards, so both stores converge to the same final
-- value under any interleaving of concurrent writes.
type TeamWriteStamp = Writetime TeamStatus

newWriteStamp :: (Member (Embed IO) r) => Sem r TeamWriteStamp
newWriteStamp = Writetime <$> embed getCurrentTime

-- | Dual-write interpreter for 'TeamStore' (WPB-22956 metadata + WPB-22957
-- members): writes go to both Cassandra and Postgres; reads go to Cassandra,
-- which remains the source of truth during migration.
--
-- DUAL-WRITE CUTOVER-INCOMPLETE: @team_member.legalhold_status@ is also
-- written by the LegalHoldStore directly against Cassandra
-- (@setUserLegalHoldStatus@ in "Wire.LegalHoldStore.Cassandra"), outside this
-- interpreter.  Those writes are deliberately NOT mirrored here — per the
-- WPB-22960 decision the LegalHoldStore boundary is kept Cassandra-only until
-- that store migrates, rather than restructuring it in this PR.  Consequence:
-- the Postgres @legalhold_status@ column can go stale relative to Cassandra
-- while both stores are written; reads stay on Cassandra until cutover so
-- this is benign during migration, but it MUST be resolved (mirrored write or
-- Postgres-side LegalHoldStore) before team members cut over to Postgres.
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
  -- Metadata writes: both stores, sharing one client-chosen writetime per
  -- write (see 'TeamWriteStamp').  Cassandra stays the source of truth for
  -- reads.
  CreateTeam mtid uid n i k b -> do
    stamp <- newWriteStamp
    t <- Cassandra.createTeamWithTimestamp mtid uid n i k b (writetimeToInt64 stamp)
    Postgres.insertTeamPg t (writetimeToUTC stamp)
    pure t
  SetTeamData tid upd -> do
    stamp <- newWriteStamp
    embedClientInput $ Cassandra.updateTeamWithTimestamp tid upd (writetimeToInt64 stamp)
    Postgres.updateTeamPg tid upd (writetimeToUTC stamp)
  SetTeamStatus tid st -> do
    stamp <- newWriteStamp
    embedClientInput $ Cassandra.updateTeamStatusWithTimestamp tid st (writetimeToInt64 stamp)
    Postgres.updateTeamStatusPg tid st (writetimeToUTC stamp)
  SetSearchVisibility tid v -> do
    stamp <- newWriteStamp
    embedClientInput $ Cassandra.setSearchVisibilityWithTimestamp tid v (writetimeToInt64 stamp)
    Postgres.updateTeamSearchVisibilityPg tid v (writetimeToUTC stamp)
  ResetSearchVisibility tid -> do
    stamp <- newWriteStamp
    embedClientInput $ Cassandra.setSearchVisibilityWithTimestamp tid SearchVisibilityStandard (writetimeToInt64 stamp)
    Postgres.updateTeamSearchVisibilityPg tid SearchVisibilityStandard (writetimeToUTC stamp)
  DeleteTeam tid -> do
    -- Serialize the whole deletion against the member copy loop and the
    -- locked member dual writes: take the exclusive (team, user) locks for
    -- the team's current members (re-read from Cassandra, the source of
    -- truth), then park PendingDelete, run the Cassandra cleanup, finalize,
    -- and sweep the Postgres member rows — all inside the locked section so
    -- no in-flight copy can resurrect a deleted member row.
    mems <- routeCassandra (getTeamMembers tid)
    withMigrationLocks
      LockExclusive
      (MilliSeconds 2000)
      [(tid, mem ^. userId) | mem <- mems]
      $ do
        Postgres.deleteTeamWithCassandra tid
        Postgres.removeTeamMemberRowsPg tid
  -- Metadata reads: Cassandra is the source of truth.
  GetTeam tid -> routeCassandra (getTeam tid)
  GetTeamName tid -> routeCassandra (getTeamName tid)
  GetTeamsBindings tids -> routeCassandra (getTeamsBindings tids)
  GetTeamBinding tid -> routeCassandra (getTeamBinding tid)
  GetTeamCreationTime tid -> routeCassandra (getTeamCreationTime tid)
  GetSearchVisibility tid -> routeCassandra (getSearchVisibility tid)
  -- Member writes: both stores.  The shared (team, user) lock serializes
  -- against the copy loop's exclusive lock, so a concurrent write can not be
  -- clobbered by a stale copy; on lock timeout only the Cassandra leg has
  -- run, and the copy loop reconciles Postgres afterwards.
  CreateTeamMember tid mem ->
    withMemberLock tid (mem ^. userId) $ do
      routeCassandra (createTeamMember tid mem)
      Postgres.addTeamMemberPg tid mem
  SetTeamMemberPermissions old tid uid new ->
    withMemberLock tid uid $ do
      routeCassandra (setTeamMemberPermissions old tid uid new)
      Postgres.updateTeamMemberPg old tid uid new
  DeleteTeamMember tid uid ->
    withMemberLock tid uid $ do
      routeCassandra (deleteTeamMember tid uid)
      Postgres.removeTeamMemberPg tid uid
  -- Member reads: Cassandra is the source of truth during migration.
  GetTeamMember tid uid -> routeCassandra (getTeamMember tid uid)
  GetTeamMembers tid -> routeCassandra (getTeamMembers tid)
  GetTeamMembersWithLimit tid n -> routeCassandra (getTeamMembersWithLimit tid n)
  SelectTeamMembers tid uids -> routeCassandra (selectTeamMembers tid uids)
  SelectTeamMemberInfos tid uids -> routeCassandra (selectTeamMemberInfos tid uids)
  GetBillingTeamMembers tid -> routeCassandra (getBillingTeamMembers tid)
  GetTeamAdmins tid -> routeCassandra (getTeamAdmins tid)
  SelectTeams uid tids -> routeCassandra (selectTeams uid tids)
  GetUserTeams uid -> routeCassandra (getUserTeams uid)
  GetUsersTeams uids -> routeCassandra (getUsersTeams uids)
  GetOneUserTeam uid -> routeCassandra (getOneUserTeam uid)

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
