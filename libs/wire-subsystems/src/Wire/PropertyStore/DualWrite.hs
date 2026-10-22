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

module Wire.PropertyStore.DualWrite
  ( interpretPropertyStoreToCassandraAndPostgres,
  )
where

import Cassandra (ClientState)
import Imports
import Polysemy
import Wire.Postgres (PGConstraints)
import Wire.PropertyStore
import Wire.PropertyStore qualified as PropertyStore
import Wire.PropertyStore.Cassandra qualified as Cassandra
import Wire.PropertyStore.Postgres qualified as Postgres

-- | Cassandra is the source of truth during migration; writes are mirrored to Postgres.
interpretPropertyStoreToCassandraAndPostgres ::
  (PGConstraints r) =>
  ClientState ->
  InterpreterFor PropertyStore r
interpretPropertyStoreToCassandraAndPostgres cs = interpret $ \case
  InsertProperty u k v -> do
    Cassandra.interpretPropertyStoreCassandra cs $ PropertyStore.insertProperty u k v
    Postgres.interpretPropertyStoreToPostgres $ PropertyStore.insertProperty u k v
  LookupProperty u k ->
    Cassandra.interpretPropertyStoreCassandra cs $ PropertyStore.lookupProperty u k
  CountProperties u ->
    Cassandra.interpretPropertyStoreCassandra cs $ PropertyStore.countProperties u
  DeleteProperty u k -> do
    Cassandra.interpretPropertyStoreCassandra cs $ PropertyStore.deleteProperty u k
    Postgres.interpretPropertyStoreToPostgres $ PropertyStore.deleteProperty u k
  ClearProperties u -> do
    Cassandra.interpretPropertyStoreCassandra cs $ PropertyStore.clearProperties u
    Postgres.interpretPropertyStoreToPostgres $ PropertyStore.clearProperties u
  GetPropertyKeys u ->
    Cassandra.interpretPropertyStoreCassandra cs $ PropertyStore.getPropertyKeys u
  GetAllProperties u ->
    Cassandra.interpretPropertyStoreCassandra cs $ PropertyStore.getAllProperties u
