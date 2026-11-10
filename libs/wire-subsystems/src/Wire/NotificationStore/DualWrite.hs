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
import Control.Exception (throwIO)
import Control.Lens ((^.))
import Data.Id (UserId)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Set qualified as Set
import Imports
import Polysemy
import Polysemy.Async
import Polysemy.Conc.Effect.Race (Race)
import Polysemy.Error (runError)
import Polysemy.Input
import Polysemy.Resource (Resource)
import Polysemy.Time.Data.TimeUnit (MilliSeconds (..))
import Polysemy.TinyLog (TinyLog)
import Wire.API.Internal.Notification (targetUser)
import Wire.MigrationLock (LockType (LockShared), MigrationLockError (..), withMigrationLocks)
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
    PGConstraints r,
    Member TinyLog r,
    Member Async r,
    Member Race r,
    Member Resource r
  ) =>
  Sem (NotificationStore ': r) a ->
  Sem r a
interpretNotificationStoreToCassandraAndPostgres = interpret $ \case
  AddNotification nid tgts objs ttl -> do
    runCassandra $ Store.addNotification nid tgts objs ttl
    -- The Postgres leg runs under the shared per-user migration lock so a
    -- concurrent dual delete cannot interleave and leave a PG-only ghost
    -- row (the copy loop's inserts are DO NOTHING, so a ghost row would
    -- never self-heal). The Cassandra leg stays unlocked: it is the source
    -- of truth during migration.
    underSharedUserLocks (Set.toList (Set.fromList (map (^. targetUser) (NonEmpty.toList tgts)))) $
      Postgres.interpretNotificationStoreToPostgres $ Store.addNotification nid tgts objs ttl
  FetchNotifications u c msince range ->
    runCassandra $ Store.fetchNotifications u c msince range
  FetchNotificationById u n c ->
    runCassandra $ Store.fetchNotificationById u n c
  FetchLastNotification u c ->
    runCassandra $ Store.fetchLastNotification u c
  DeleteAllNotifications u ->
    -- Dual deletes race the copier's full scan: without a lock, the copier
    -- can re-insert rows that were just deleted from both stores (the
    -- Cassandra partition is cleared, so nothing self-heals until row
    -- expiry). 'withMigrationLocks' brackets acquire/release (session-level
    -- pg_advisory_lock_shared on a dedicated connection) so no async
    -- exception can leak the lock; the copier's exclusive per-user lock
    -- serializes against this delete.
    underSharedUserLocks [u] $ do
      runCassandra $ Store.deleteAllNotifications u
      Postgres.interpretNotificationStoreToPostgres $ Store.deleteAllNotifications u

-- | Run an action under the shared per-user migration lock
-- ('Wire.MigrationLock', 500ms wait — the shared-lock wait used by the
-- connections migration). A lock wait timeout is rethrown as the typed
-- 'MigrationLockError' exception; the lock itself is bracketed inside
-- 'withMigrationLocks', so the timeout surfaces only after a clean release.
underSharedUserLocks ::
  ( Member TinyLog r,
    Member Async r,
    Member Race r,
    Member Resource r,
    PGConstraints r
  ) =>
  [UserId] ->
  Sem r a ->
  Sem r a
underSharedUserLocks users action = do
  e <- runError $ withMigrationLocks LockShared (MilliSeconds 500) users (raise action)
  case e of
    Right a -> pure a
    Left TimedOutAcquiringLock -> embed (throwIO TimedOutAcquiringLock)

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
