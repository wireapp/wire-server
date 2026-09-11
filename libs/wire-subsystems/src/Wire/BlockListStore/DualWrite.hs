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

module Wire.BlockListStore.DualWrite
  ( interpretBlockListStoreToCassandraAndPostgres,
  )
where

import Cassandra (ClientState)
import Imports
import Polysemy
import Wire.BlockListStore
import Wire.BlockListStore qualified as BlockListStore
import Wire.BlockListStore.Cassandra qualified as Cassandra
import Wire.BlockListStore.Postgres qualified as Postgres
import Wire.Postgres (PGConstraints)

-- | Cassandra is the source of truth during migration; writes are mirrored to Postgres.
interpretBlockListStoreToCassandraAndPostgres ::
  (PGConstraints r) =>
  ClientState ->
  InterpreterFor BlockListStore r
interpretBlockListStoreToCassandraAndPostgres cs = interpret $ \case
  Insert key -> do
    Cassandra.interpretBlockListStoreToCassandra cs $ BlockListStore.insert key
    Postgres.interpretBlockListStoreToPostgres $ BlockListStore.insert key
  Exists key ->
    Cassandra.interpretBlockListStoreToCassandra cs $ BlockListStore.exists key
  Delete key -> do
    Cassandra.interpretBlockListStoreToCassandra cs $ BlockListStore.delete key
    Postgres.interpretBlockListStoreToPostgres $ BlockListStore.delete key
