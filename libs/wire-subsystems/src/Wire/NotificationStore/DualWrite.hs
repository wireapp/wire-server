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

module Wire.NotificationStore.DualWrite
  ( interpretNotificationStoreToCassandraAndPostgres,
  )
where

import Cassandra (ClientState)
import Control.Concurrent.Async (async, cancel, wait)

import Control.Exception (throwIO)
import Data.Id (UserId)
import Data.Vector (Vector)
import Hasql.Pool qualified as Hasql
import Hasql.Pool.Extended qualified as HasqlPoolExt
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Hasql
import Hasql.TH
import Imports
import Polysemy
import Polysemy.Error (runError, throw)
import Polysemy.Input
import System.IO.Error (userError)
import System.Timeout (timeout)
import Wire.API.PostgresMarshall (lmapPG)
import Wire.MigrationLock (hashUUID)
import Wire.NotificationStore
import Wire.NotificationStore qualified as Store
import Wire.NotificationStore.Cassandra qualified as Cassandra
import Wire.NotificationStore.Postgres qualified as Postgres
import Wire.Postgres (PGConstraints)

-- | Cassandra is the source of truth during migration; writes are mirrored
-- to Postgres, reads still come from Cassandra.
interpretNotificationStoreToCassandraAndPostgres ::
  ( Member (Input ClientState) r,
    Member (Input NotificationStoreConfig) r,
    PGConstraints r
  ) =>
  Sem (NotificationStore ': r) a ->
  Sem r a
interpretNotificationStoreToCassandraAndPostgres = interpret $ \case
  AddNotification nid tgts objs ttl -> do
    runCassandra $ Store.addNotification nid tgts objs ttl
    Postgres.interpretNotificationStoreToPostgres $ Store.addNotification nid tgts objs ttl
  FetchNotifications u c msince range ->
    runCassandra $ Store.fetchNotifications u c msince range
  FetchNotificationById u n c ->
    runCassandra $ Store.fetchNotificationById u n c
  FetchLastNotification u c ->
    runCassandra $ Store.fetchLastNotification u c
  DeleteAllNotifications u -> do
    -- Dual deletes race the copier's full scan: without a lock, the copier
    -- can re-insert rows that were just deleted from both stores (the
    -- Cassandra partition is cleared, so nothing self-heals until row
    -- expiry). Hold the shared per-user migration lock while deleting so
    -- the copier's exclusive per-user lock serializes against this delete
    -- (same 'Wire.MigrationLock' advisory-lock scheme; see
    -- 'withDualDeleteLocks').
    pool <- input @HasqlPoolExt.Pool
    release <- embed $ acquireDualDeleteLocks pool [u]
    e <- runError $ do
      runCassandra $ Store.deleteAllNotifications u
      Postgres.interpretNotificationStoreToPostgres $ Store.deleteAllNotifications u
    embed release
    either throw pure e

-- | Acquire the shared per-user migration lock for a dual delete, mirroring
-- the semantics of @Wire.MigrationLock.withMigrationLocks LockShared@: a
-- session-level @pg_advisory_lock_shared@ held on a dedicated connection for
-- the duration of the action, with a bounded acquire wait. Lock keys are
-- derived with the same 'hashUUID' the copier's exclusive
-- @MigrationLockable UserId@ instance uses, so the two lock families
-- contend.
acquireDualDeleteLocks :: HasqlPoolExt.Pool -> [UserId] -> IO (IO ())
acquireDualDeleteLocks pool userIds = do
  acquired <- newEmptyMVar
  done <- newEmptyMVar
  lockThread <-
    async . void . Hasql.use (HasqlPoolExt.rawPool pool) $ do
      Session.statement lockIds acquireSharedLocks
      liftIO (putMVar acquired ())
      liftIO (takeMVar done)
      Session.statement lockIds releaseSharedLocks
  waitResult <- timeout 500_000 (takeMVar acquired)
  case waitResult of
    Nothing -> do
      cancel lockThread
      throwIO . userError $ "notification store: timed out acquiring the dual-delete migration lock"
    Just () -> pure $ do
      _ <- tryPutMVar done ()
      -- Best-effort: if the unlock thread does not finish in time, the
      -- session locks die with the connection when the pool recycles it.
      _ <- timeout 1_000_000 (wait lockThread)
      pure ()
  where
    lockIds = sort (map hashUUID userIds)

    acquireSharedLocks :: Hasql.Statement [Int64] ()
    acquireSharedLocks =
      lmapPG @(Vector _)
        [resultlessStatement|SELECT (1 :: int)
                             FROM (SELECT pg_advisory_lock_shared(lockId)
                                   FROM (SELECT UNNEST($1 :: bigint[]) as lockId) AS t) AS t2|]

    releaseSharedLocks :: Hasql.Statement [Int64] ()
    releaseSharedLocks =
      lmapPG @(Vector _)
        [resultlessStatement|SELECT (1 :: int)
                             FROM (SELECT pg_advisory_unlock_shared(lockId)
                                   FROM (SELECT UNNEST($1 :: bigint[]) as lockId) AS t) AS t2|]

-- | Runs the Cassandra interpreter with the client state and store config
-- taken from the ambient 'Input' effects.
runCassandra ::
  ( Member (Input ClientState) r,
    Member (Input NotificationStoreConfig) r,
    Member (Embed IO) r
  ) =>
  Sem (NotificationStore ': r) a ->
  Sem r a
runCassandra = Cassandra.interpretNotificationStoreToCassandra
