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

module Wire.TeamNotificationStore.DualWrite
  ( interpretTeamNotificationStoreToCassandraAndPostgres,
  )
where

import Cassandra (ClientState)
import Imports
import Polysemy
import Polysemy.Input
import Polysemy.TinyLog
import Wire.Postgres (PGConstraints)
import Wire.TeamNotificationStore
import Wire.TeamNotificationStore qualified as Store
import Wire.TeamNotificationStore.Cassandra qualified as Cassandra
import Wire.TeamNotificationStore.Postgres qualified as Postgres

-- | Cassandra is the source of truth during migration; writes are mirrored to Postgres.
interpretTeamNotificationStoreToCassandraAndPostgres ::
  ( Member (Input ClientState) r,
    Member TinyLog r,
    PGConstraints r
  ) =>
  Sem (TeamNotificationStore ': r) a ->
  Sem r a
interpretTeamNotificationStoreToCassandraAndPostgres = interpret $ \case
  CreateTeamNotification tid nid objs -> do
    Cassandra.interpretTeamNotificationStoreToCassandra $ Store.createTeamNotification tid nid objs
    Postgres.interpretTeamNotificationStoreToPostgres $ Store.createTeamNotification tid nid objs
  GetTeamNotifications tid mnid lim ->
    Cassandra.interpretTeamNotificationStoreToCassandra $ Store.getTeamNotifications tid mnid lim
  MkNotificationId ->
    Cassandra.interpretTeamNotificationStoreToCassandra Store.mkNotificationId
