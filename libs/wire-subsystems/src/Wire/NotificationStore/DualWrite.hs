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
import Imports
import Polysemy
import Polysemy.Input
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
    runCassandra $ Store.deleteAllNotifications u
    Postgres.interpretNotificationStoreToPostgres $ Store.deleteAllNotifications u

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
