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

module Wire.CustomBackendStore.DualWrite
  ( interpretCustomBackendStoreToCassandraAndPostgres,
  )
where

import Cassandra (ClientState)
import Imports
import Polysemy
import Polysemy.Input
import Polysemy.TinyLog
import Wire.CustomBackendStore
import Wire.CustomBackendStore.Cassandra qualified as Cassandra
import Wire.CustomBackendStore.Postgres qualified as Postgres
import Wire.Postgres (PGConstraints)

-- | Cassandra is the source of truth during migration; writes are mirrored to Postgres.
interpretCustomBackendStoreToCassandraAndPostgres ::
  ( Member (Input ClientState) r,
    Member TinyLog r,
    PGConstraints r
  ) =>
  InterpreterFor CustomBackendStore r
interpretCustomBackendStoreToCassandraAndPostgres = interpret $ \case
  GetCustomBackend dom ->
    Cassandra.interpretCustomBackendStoreToCassandra $ getCustomBackend dom
  SetCustomBackend dom cb -> do
    Cassandra.interpretCustomBackendStoreToCassandra $ setCustomBackend dom cb
    Postgres.interpretCustomBackendStoreToPostgres $ setCustomBackend dom cb
  DeleteCustomBackend dom -> do
    Cassandra.interpretCustomBackendStoreToCassandra $ deleteCustomBackend dom
    Postgres.interpretCustomBackendStoreToPostgres $ deleteCustomBackend dom
