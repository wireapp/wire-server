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

-- | Migration of team invitations and pending SCIM users from Cassandra to
-- Postgres, mirroring the connections migration (WPB-22953): entities are
-- copied per team under an exclusive migration lock, and the copied Cassandra
-- partitions are deleted in the same locked section once everything for that
-- team is fully copied.
--
-- The denormalized @team_invitation_info@ and @team_invitation_email@
-- Cassandra tables are not copied (the Postgres @team_invitation@ table
-- covers them with plain indexes on @code@ and @email@); their rows are
-- deleted together with the @team_invitation@ partition. Pending SCIM users
-- are migrated as part of their team, like the second table of a connection
-- partition.
module Wire.InvitationStore.Migration
  ( migrateInvitationsLoop,
  )
where

import Cassandra hiding (Value)
import Data.Conduit
import Data.Conduit.List qualified as C
import Data.Id (TeamId)
import Data.Time (diffUTCTime, getCurrentTime)
import Hasql.Pool qualified as Hasql
import Hasql.Pool.Extended qualified as HasqlPoolExt
import Imports
import Polysemy
import Polysemy.Async
import Polysemy.AtomicState
import Polysemy.Conc (interpretRace)
import Polysemy.Conc.Effect.Race hiding (Timeout)
import Polysemy.Error (Error, runError)
import Polysemy.Input
import Polysemy.Resource (Resource, resourceToIOFinal)
import Polysemy.TinyLog
import Prometheus qualified
import System.Logger qualified as Log
import UnliftIO.Exception (try)
import Wire.API.PostgresMarshall
import Wire.InvitationStore.Cassandra
  ( invitationClear,
    pendingScimUserClear,
    teamInvitationsSelect,
    teamPendingScimUsersSelect,
  )
import Wire.InvitationStore.Postgres qualified as Postgres
import Wire.Migration
import Wire.MigrationLock (MigrationLockError)
import Wire.Postgres
import Wire.Sem.Concurrency
import Wire.Sem.Concurrency.IO (unsafelyPerformConcurrency)
import Wire.Sem.Logger (mapLogger)
import Wire.Sem.Logger.TinyLog (loggerToTinyLog)

type EffectStack =
  [ AtomicState Int,
    Input ClientState,
    Input HasqlPoolExt.Pool,
    Resource,
    Async,
    Race,
    TinyLog,
    Embed IO,
    Concurrency 'Unsafe,
    Final IO
  ]

migrateInvitationsLoop ::
  MigrationOptions ->
  ClientState ->
  HasqlPoolExt.Pool ->
  Log.Logger ->
  Prometheus.Counter ->
  Prometheus.Counter ->
  Prometheus.Counter ->
  Prometheus.Vector Text Prometheus.Histogram ->
  IO ()
migrateInvitationsLoop migOpts cassClient pgPool logger migCounter migFinished migFailed migDuration =
  migrationLoop
    logger
    "invitations"
    migFinished
    migFailed
    (interpreter cassClient pgPool logger "invitations")
    (migrateAllInvitations migOpts migCounter migDuration)

interpreter :: ClientState -> HasqlPoolExt.Pool -> Log.Logger -> ByteString -> Sem EffectStack a -> IO (Int, a)
interpreter cassClient pgPool logger name =
  runFinal
    . unsafelyPerformConcurrency
    . embedToFinal
    . loggerToTinyLog logger
    . mapLogger (Log.field "migration" (Log.val name) .)
    . raiseUnder
    . interpretRace
    . asyncToIOFinal
    . resourceToIOFinal
    . runInputConst pgPool
    . runInputConst cassClient
    . atomicStateToIO 0

migrateAllInvitations ::
  ( Member TinyLog r,
    Member (Input HasqlPoolExt.Pool) r,
    Member (Input ClientState) r,
    Member (Embed IO) r,
    Member (AtomicState Int) r,
    Member (Concurrency Unsafe) r,
    Member Async r,
    Member Resource r,
    Member Race r
  ) =>
  MigrationOptions ->
  Prometheus.Counter ->
  Prometheus.Vector Text Prometheus.Histogram ->
  ConduitM () Void (Sem r) ()
migrateAllInvitations migOpts migCounter migDuration = do
  lift $ info $ Log.msg (Log.val "migrating team_invitation and team_scim_pending_user_email tables")
  -- Pending SCIM users have no table of their own to scan: their partition is
  -- keyed by the same team as the invitation partition, so one scan covers
  -- both tables of a team (the connection/connection_remote pattern). A team
  -- with only pending SCIM users and no invitations is picked up when its
  -- next invitation is scanned; SCIM-only teams are migrated by the SCIM
  -- provisioning path itself, which writes to both stores under the shared
  -- lock.
  withCount (paginateSem selectInvitationTeams (paramsP LocalQuorum () migOpts.pageSize) x5)
    .| logRetrievedPage migOpts.pageSize runIdentity
    .| C.mapM_
      ( unsafePooledMapConcurrentlyN_
          migOpts.parallelism
          (\tid -> handleLockAndDBErrors "invitation team" (migrateInvitationsForTeam migOpts migCounter migDuration tid))
      )
  where
    selectInvitationTeams :: PrepQuery R () (Identity TeamId)
    selectInvitationTeams = "SELECT DISTINCT team FROM team_invitation"

-- | Migrate one team's @team_invitation@ and @team_scim_pending_user_email@
-- partitions into Postgres, then delete the Cassandra partitions. All of this
-- happens under an exclusive per-team lock, so it is serialized against the
-- dual interpreter's shared-locked reads and writes: a partition is only
-- removed from Cassandra after every row is in Postgres and readers have
-- moved to Postgres for this team.
--
-- The payloads are re-read from Cassandra inside the lock: the snapshot teams
-- this loop was seeded with may have been mutated concurrently, and copying
-- a stale snapshot could resurrect a deleted invitation in Postgres.
--
-- The Cassandra partitions are deleted only if every row copied without an
-- error; otherwise the team is left untouched and the migration loop retries
-- it on the next pass (the timestamp-guarded upserts make re-copying a
-- no-op). The re-scan of @SELECT DISTINCT team@ is the crash recovery — a
-- worker restart picks up half-migrated teams from zero.
--
-- Trade-off (raised by @akshaymankar@ on PR #5649): an invitation created for
-- a team concurrently by another admin is mirrored to Postgres by the dual
-- interpreter under the shared team lock, so it cannot be lost by the
-- partition delete; a mutation that happens while the copier waits for the
-- exclusive lock is ordered either entirely before (copied) or entirely
-- after (mirrored) the locked section. The residual risk of the mirroring
-- leg failing after Cassandra succeeded is the same one the connections
-- migration accepted, bounded to the dual-write mode window.
--
-- A per-team 'MigrationTimedOut' aborts the whole loop, exactly as for users
-- and conversations; the ops lever is the shared @migrationOptions.timeout@.
migrateInvitationsForTeam ::
  ( PGConstraints r,
    Member TinyLog r,
    Member (Input ClientState) r,
    Member (AtomicState Int) r,
    Member (Error MigrationLockError) r,
    Member Async r,
    Member Race r,
    Member Resource r
  ) =>
  MigrationOptions ->
  Prometheus.Counter ->
  Prometheus.Vector Text Prometheus.Histogram ->
  TeamId ->
  Sem r ()
migrateInvitationsForTeam migOpts migCounter migDuration team =
  withExclusiveMigrationLockAndTimeout migOpts.timeout migDuration [team] $ do
    invitationsCopied <- copyPartition migOpts migCounter migDuration teamInvitationsSelect upsertInvitationRow team
    scimCopied <- copyPartition migOpts migCounter migDuration teamPendingScimUsersSelect upsertPendingScimUserRow team
    when (invitationsCopied && scimCopied) $ clearCassandraPartitions team
  where
    -- Preserve the original created_at; the TTL becomes expires_at = now()+ttl.
    upsertInvitationRow (teamId, role, invId, createdAt, createdBy, email, name, code, ttlSecs) =
      when (ttlSecs > 0) $
        -- Expired rows have already vanished from Cassandra; skip them.
        Postgres.insertInvitationImpl
          (teamId, invId, postgresMarshall <$> role, createdAt, createdBy, postgresMarshall email, name, code, ttlSecs)
    upsertPendingScimUserRow (tid, email, user) =
      Postgres.insertPendingScimUserImpl (tid, email, user)

-- | Page all rows of one partition out of Cassandra and upsert them into
-- Postgres. Returns whether every row copied without error.
copyPartition ::
  forall row r.
  ( Tuple row,
    Member (Input ClientState) r,
    Member (Embed IO) r,
    Member TinyLog r,
    Member (AtomicState Int) r
  ) =>
  MigrationOptions ->
  Prometheus.Counter ->
  Prometheus.Vector Text Prometheus.Histogram ->
  PrepQuery R (Identity TeamId) row ->
  (row -> Sem (Error Hasql.UsageError ': r) ()) ->
  TeamId ->
  Sem (Error MigrationLockError ': r) Bool
copyPartition migOpts migCounter migDuration selectRows upsertRow team = do
  cleanRef <- embed @IO (newIORef True)
  runConduit $
    paginateSem selectRows (paramsP LocalQuorum (Identity team) migOpts.pageSize) x1
      .| C.mapM_ (traverse_ (copyRow cleanRef))
  embed @IO (readIORef cleanRef)
  where
    copyRow :: IORef Bool -> row -> Sem (Error MigrationLockError ': r) ()
    copyRow cleanRef row = do
      let rowAction :: Sem (Error Hasql.UsageError ': Error MigrationLockError ': r) ()
          rowAction = raiseUnder (upsertRow row)
      ok <- migrateRow team migCounter migDuration rowAction
      unless ok $ embed @IO (writeIORef cleanRef False)

-- | Copy one row and observe the duration histogram and copied-rows counter.
-- On failure, warn and bump the atomic count (which makes migrationLoop retry
-- the pass) and report failure to the caller.
migrateRow ::
  ( Member (Embed IO) r,
    Member TinyLog r,
    Member (AtomicState Int) r
  ) =>
  TeamId ->
  Prometheus.Counter ->
  Prometheus.Vector Text Prometheus.Histogram ->
  Sem (Error Hasql.UsageError ': r) () ->
  Sem r Bool
migrateRow team migCounter migDuration action = do
  start <- embed @IO getCurrentTime
  result <- runError action
  end <- embed @IO getCurrentTime
  let outcome = either (const "failure") (const "success") result
  liftIO $
    Prometheus.withLabel migDuration outcome (`Prometheus.observe` realToFrac (diffUTCTime end start))
  case result of
    Right () -> do
      liftIO $ Prometheus.incCounter migCounter
      pure True
    Left failure -> do
      warn $
        Log.msg (Log.val "invitation row migration failed")
          . Log.field "team" (show team)
          . Log.field "error" (show failure)
      -- This only bumps the retry counter, which makes migrationLoop retry
      -- the failed rows; the counter is reserved for fatal errors (see
      -- Wire.Migration.migrationLoop).
      atomicModify (+ 1)
      pure False

-- | Delete the migrated @team_invitation@, @team_invitation_info@,
-- @team_invitation_email@ and @team_scim_pending_user_email@ rows of one team
-- from Cassandra. Must only run under the exclusive per-team migration lock
-- after both partitions are fully copied. On failure the atomic count is
-- bumped so the loop retries the team.
clearCassandraPartitions ::
  ( Member (Input ClientState) r,
    Member (Embed IO) r,
    Member TinyLog r,
    Member (AtomicState Int) r
  ) =>
  TeamId ->
  Sem r ()
clearCassandraPartitions team = do
  client <- input
  outcome <- embed @IO $
    try @IO @SomeException $
      runClient client $
        retry x5 $ do
          write invitationClear (params LocalQuorum (Identity team))
          write pendingScimUserClear (params LocalQuorum (Identity team))
  case outcome of
    Right () -> pure ()
    Left e -> do
      warn $
        Log.msg (Log.val "failed to clear migrated cassandra invitation partitions")
          . Log.field "team" (show team)
          . Log.field "error" (show e)
      atomicModify (+ 1)
