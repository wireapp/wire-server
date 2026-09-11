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

module Wire.ServiceStore.DualWrite
  ( interpretServiceStoreToCassandraAndPostgres,
  )
where

import Cassandra (ClientState)
import Imports
import Polysemy
import Polysemy.TinyLog (TinyLog)
import Wire.Postgres (PGConstraints)
import Wire.ServiceStore (ServiceStore (..))
import Wire.ServiceStore qualified as ServiceStore
import Wire.ServiceStore.Cassandra qualified as Cassandra
import Wire.ServiceStore.Postgres qualified as Postgres

-- | Cassandra is the source of truth during migration; writes are mirrored to Postgres.
interpretServiceStoreToCassandraAndPostgres ::
  ( Member TinyLog r,
    PGConstraints r
  ) =>
  ClientState ->
  Sem (ServiceStore ': r) a ->
  Sem r a
interpretServiceStoreToCassandraAndPostgres cassClient = interpret $ \case
  GetService sr ->
    Cassandra.interpretServiceStoreToCassandra cassClient $ ServiceStore.getService sr
  CreateService s -> do
    Cassandra.interpretServiceStoreToCassandra cassClient $ ServiceStore.createService s
    Postgres.interpretServiceStoreToPostgres $ ServiceStore.createService s
  DeleteService sr -> do
    Cassandra.interpretServiceStoreToCassandra cassClient $ ServiceStore.deleteService sr
    Postgres.interpretServiceStoreToPostgres $ ServiceStore.deleteService sr
