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

-- | During migration Cassandra remains the source of truth: all reads go to
-- Cassandra, all writes are applied to Cassandra first and then mirrored to
-- Postgres.
module Wire.UserClientIndexStore.DualWrite
  ( interpretUserClientIndexStoreToCassandraAndPostgres,
  )
where

import Cassandra (ClientState)
import Imports
import Polysemy
import Wire.Postgres (PGConstraints)
import Wire.UserClientIndexStore
import Wire.UserClientIndexStore.Cassandra qualified as Cassandra
import Wire.UserClientIndexStore.Postgres qualified as Postgres

interpretUserClientIndexStoreToCassandraAndPostgres ::
  (PGConstraints r) =>
  ClientState ->
  InterpreterFor UserClientIndexStore r
interpretUserClientIndexStoreToCassandraAndPostgres cas = interpret $ \case
  GetClients uids -> Cassandra.interpretUserClientIndexStoreToCassandra cas $ getClients uids
  CreateClient uid cid -> do
    Cassandra.interpretUserClientIndexStoreToCassandra cas $ createClient uid cid
    Postgres.interpretUserClientIndexStoreToPostgres $ createClient uid cid
  DeleteClient uid cid -> do
    Cassandra.interpretUserClientIndexStoreToCassandra cas $ deleteClient uid cid
    Postgres.interpretUserClientIndexStoreToPostgres $ deleteClient uid cid
  DeleteClients uid -> do
    Cassandra.interpretUserClientIndexStoreToCassandra cas $ deleteClients uid
    Postgres.interpretUserClientIndexStoreToPostgres $ deleteClients uid
