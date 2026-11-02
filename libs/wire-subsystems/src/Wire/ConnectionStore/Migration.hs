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

module Wire.ConnectionStore.Migration
  ( migrateConnectionsLoop,
  )
where

import Cassandra
import Data.Conduit
import Data.Conduit.List qualified as C
import Data.Domain (Domain)
import Data.Id
import Data.Json.Util
import Data.Time
import Hasql.Pool qualified as Hasql
import Hasql.Pool.Extended qualified as HasqlPoolExt
import Hasql.Statement qualified as Hasql
import Hasql.TH
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
import Wire.API.Connection (RelationWithHistory)
import Wire.API.PostgresMarshall (PostgresMarshall (..), lmapPG)
import Wire.ConnectionStore.Cassandra
  ( connectionClear,
    connectionsSelect,
    remoteConnectionClear,
    remoteConnectionSelect,
  )
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

-- | Migration of the @connection@ and @connection_remote@ tables from
-- Cassandra to Postgres. Entities are copied per @left@ user under an
-- exclusive migration lock, and the copied Cassandra partitions are deleted
-- in the same locked section once both tables for that user are fully copied
-- (the @UserStore@ pattern). Two sequential scans share one @finished@
-- counter; the loop reports 1.0 once both scans are done. Remote-only users
-- exist, so both scans run; visiting a user twice is a no-op because the
-- per-user action is idempotent.
migrateConnectionsLoop ::
  MigrationOptions ->
  ClientState ->
  HasqlPoolExt.Pool ->
  Log.Logger ->
  Prometheus.Counter ->
  Prometheus.Counter ->
  Prometheus.Counter ->
  Prometheus.Vector Text Prometheus.Histogram ->
  IO ()
migrateConnectionsLoop migOpts cassClient pgPool logger migCounter migFinished migFailed migDuration =
  migrationLoop
    logger
    "connections"
    migFinished
    migFailed
    (interpreter cassClient pgPool logger "connections")
    (migrateAllConnections migOpts migCounter migDuration)

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

migrateAllConnections ::
  ( Member TinyLog r,
    Member (Input HasqlPoolExt.Pool) r,
    Member (Input ClientState) r,
    Member (Embed IO) r,
    Member (AtomicState Int) r,
    Member (Concurrency Unsafe) r,
    Member Async r,
    Member Race r,
    Member Resource r
  ) =>
  MigrationOptions ->
  Prometheus.Counter ->
  Prometheus.Vector Text Prometheus.Histogram ->
  ConduitM () Void (Sem r) ()
migrateAllConnections migOpts migCounter migDuration = do
  lift $ info $ Log.msg (Log.val "migrating connection table")
  scanLefts selectConnectionLefts
  lift $ info $ Log.msg (Log.val "migrating connection_remote table")
  scanLefts selectRemoteConnectionLefts
  where
    scanLefts select =
      withCount (paginateSem select (paramsP LocalQuorum () migOpts.pageSize) x5)
        .| logRetrievedPage migOpts.pageSize runIdentity
        .| C.mapM_
          ( unsafePooledMapConcurrentlyN_
              migOpts.parallelism
              ( \uid ->
                  handleLockAndDBErrors "connection" (migrateConnectionsForUser migOpts migCounter migDuration uid)
              )
          )

-- | Migrate one @left@ user's @connection@ and @connection_remote@
-- partitions into Postgres, then delete the Cassandra partitions. All of this
-- happens under an exclusive per-user lock, so it is serialized against the
-- dual-write interpreter's shared-locked reads and deletes: a partition is
-- only removed from Cassandra after every row is in Postgres and readers have
-- moved to Postgres for this user.
--
-- The Cassandra partitions are deleted only if both tables were copied
-- without a single row error; otherwise the user is left untouched and the
-- migration loop retries it on the next pass (the timestamp-guarded upserts
-- make re-copying a no-op). The re-scan of @SELECT DISTINCT left@ is the
-- crash recovery — a worker restart picks up half-migrated users from zero.
--
-- A per-user 'MigrationTimedOut' aborts the whole loop, exactly as for users
-- and conversations; the ops lever is the shared @migrationOptions.timeout@.
migrateConnectionsForUser ::
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
  UserId ->
  Sem r ()
migrateConnectionsForUser migOpts migCounter migDuration left =
  withExclusiveMigrationLockAndTimeout migOpts.timeout migDuration [left] $ do
    localCopied <- copyPartition migOpts migCounter migDuration connectionsSelect upsertConnection left
    remoteCopied <- copyPartition migOpts migCounter migDuration remoteConnectionSelect upsertRemoteConnection left
    when (localCopied && remoteCopied) $ clearCassandraPartitions left

-- | Page all rows of one partition out of Cassandra and upsert them into
-- Postgres, preserving the original @last_update@. Returns whether every row
-- copied without error.
copyPartition ::
  forall row r.
  ( Member (Input ClientState) r,
    Member (Embed IO) r,
    Member TinyLog r,
    Member (AtomicState Int) r,
    Tuple row
  ) =>
  MigrationOptions ->
  Prometheus.Counter ->
  Prometheus.Vector Text Prometheus.Histogram ->
  PrepQuery R (Identity UserId) row ->
  ( UserId ->
    row ->
    Sem (Error Hasql.UsageError ': r) ()
  ) ->
  UserId ->
  Sem (Error MigrationLockError ': r) Bool
copyPartition migOpts migCounter migDuration selectRows upsertRow left = do
  cleanRef <- embed @IO (newIORef True)
  runConduit $
    paginateSem selectRows (paramsP LocalQuorum (Identity left) migOpts.pageSize) x1
      .| C.mapM_ (traverse_ (copyRow cleanRef))
  embed @IO (readIORef cleanRef)
  where
    copyRow :: IORef Bool -> row -> Sem (Error MigrationLockError ': r) ()
    copyRow cleanRef row = do
      let rowAction :: Sem (Error Hasql.UsageError ': Error MigrationLockError ': r) ()
          rowAction = raiseUnder (upsertRow left row)
      ok <- migrateRow left migCounter migDuration rowAction
      unless ok $ embed @IO (writeIORef cleanRef False)

-- | Copy one row and observe the duration histogram and copied-rows counter.
-- On failure, warn and bump the atomic count (which makes migrationLoop retry
-- the pass) and report failure to the caller.
migrateRow ::
  ( Member (Embed IO) r,
    Member TinyLog r,
    Member (AtomicState Int) r
  ) =>
  UserId ->
  Prometheus.Counter ->
  Prometheus.Vector Text Prometheus.Histogram ->
  Sem (Error Hasql.UsageError ': r) () ->
  Sem r Bool
migrateRow left migCounter migDuration action = do
  start <- embed @IO getCurrentTime
  result <- runError action
  end <- embed @IO getCurrentTime
  let outcome = either (const "error") (const "success") result
  liftIO $
    Prometheus.withLabel migDuration outcome (`Prometheus.observe` realToFrac (diffUTCTime end start))
  case result of
    Right () -> do
      liftIO $ Prometheus.incCounter migCounter
      pure True
    Left failure -> do
      warn $
        Log.msg (Log.val "connection row migration failed")
          . Log.field "left" (show left)
          . Log.field "error" (show failure)
      -- NOTE: unlike the Prometheus counter, the AtomicState count is what
      -- makes migrationLoop retry the failed rows; the counter is reserved for
      -- fatal errors (see Wire.Migration.migrationLoop).
      atomicModify (+ 1)
      pure False

-- | Delete the migrated @connection@ and @connection_remote@ partitions of
-- one user from Cassandra. Must only run under the exclusive per-user
-- migration lock after both partitions are fully copied. On failure the
-- atomic count is bumped so the loop retries the user.
clearCassandraPartitions ::
  ( Member (Input ClientState) r,
    Member (Embed IO) r,
    Member TinyLog r,
    Member (AtomicState Int) r
  ) =>
  UserId ->
  Sem r ()
clearCassandraPartitions left = do
  client <- input
  outcome <- embed @IO $
    try @IO @SomeException $
      runClient client $
        retry x5 $ do
          write connectionClear (params LocalQuorum (Identity left))
          write remoteConnectionClear (params LocalQuorum (Identity left))
  case outcome of
    Right () -> pure ()
    Left e -> do
      warn $
        Log.msg (Log.val "failed to clear migrated cassandra connection partitions")
          . Log.field "left" (show left)
          . Log.field "error" (show e)
      atomicModify (+ 1)

selectConnectionLefts :: PrepQuery R () (Identity UserId)
selectConnectionLefts = "SELECT DISTINCT left FROM connection"

selectRemoteConnectionLefts :: PrepQuery R () (Identity UserId)
selectRemoteConnectionLefts = "SELECT DISTINCT left FROM connection_remote"

-- Upserts preserving the original Cassandra timestamps. On conflict, an
-- existing row is only overwritten when the Cassandra row is at least as new
-- (ties go to Cassandra), so a replayed stale row cannot clobber a newer row
-- that dual-write already put in Postgres.

upsertConnection ::
  ( Member (Input HasqlPoolExt.Pool) r,
    Member (Embed IO) r
  ) =>
  UserId ->
  (UserId, UserId, RelationWithHistory, UTCTimeMillis, Maybe ConvId) ->
  Sem (Error Hasql.UsageError ': r) ()
upsertConnection _left (l, r, rel, ts, mconv) =
  runStatement (l, r, postgresMarshall rel, fromUTCTimeMillis ts, mconv) stmt
  where
    stmt :: Hasql.Statement (UserId, UserId, Int32, UTCTime, Maybe ConvId) ()
    stmt =
      lmapPG
        [resultlessStatement|INSERT INTO connection ("left", "right", status, last_update, conv)
                           VALUES ($1 :: uuid, $2 :: uuid, $3 :: int4, $4 :: timestamptz, $5 :: uuid?)
                           ON CONFLICT ("left", "right") DO UPDATE
                           SET status = ($3 :: int4), last_update = ($4 :: timestamptz), conv = ($5 :: uuid?)
                           WHERE connection.last_update <= ($4 :: timestamptz)
                          |]

upsertRemoteConnection ::
  ( Member (Input HasqlPoolExt.Pool) r,
    Member (Embed IO) r
  ) =>
  UserId ->
  (Domain, UserId, RelationWithHistory, UTCTimeMillis, Domain, ConvId) ->
  Sem (Error Hasql.UsageError ': r) ()
upsertRemoteConnection left (domain, r, rel, ts, convDomain, convId) =
  runStatement (left, domain, r, postgresMarshall rel, fromUTCTimeMillis ts, convDomain, convId) stmt
  where
    stmt :: Hasql.Statement (UserId, Domain, UserId, Int32, UTCTime, Domain, ConvId) ()
    stmt =
      lmapPG
        [resultlessStatement|INSERT INTO connection_remote ("left", right_domain, right_user, status, last_update, conv_domain, conv_id)
                           VALUES ($1 :: uuid, $2 :: text, $3 :: uuid, $4 :: int4, $5 :: timestamptz, $6 :: text, $7 :: uuid)
                           ON CONFLICT ("left", right_domain, right_user) DO UPDATE
                           SET status = ($4 :: int4), last_update = ($5 :: timestamptz), conv_domain = ($6 :: text), conv_id = ($7 :: uuid)
                           WHERE connection_remote.last_update <= ($5 :: timestamptz)
                          |]
