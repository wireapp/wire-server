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

module Wire.NonceStore.DualWrite
  ( interpretNonceStoreToCassandraAndPostgres,
  )
where

import Cassandra (ClientState)
import Imports
import Polysemy
import Wire.NonceStore
import Wire.NonceStore qualified as NonceStore
import Wire.NonceStore.Cassandra qualified as Cassandra
import Wire.NonceStore.Postgres qualified as Postgres
import Wire.Postgres (PGConstraints)

-- | Cassandra is the source of truth during migration; writes are mirrored to Postgres.
interpretNonceStoreToCassandraAndPostgres ::
  (PGConstraints r) =>
  ClientState ->
  InterpreterFor NonceStore r
interpretNonceStoreToCassandraAndPostgres cs = interpret $ \case
  NonceStore.InsertNonce ttl uid key nonce -> do
    Cassandra.interpretNonceStoreToCassandra cs $ NonceStore.insertNonce ttl uid key nonce
    Postgres.interpretNonceStoreToPostgres $ NonceStore.insertNonce ttl uid key nonce
  NonceStore.LookupAndDeleteNonce uid key -> do
    mnonce <- Cassandra.interpretNonceStoreToCassandra cs $ NonceStore.lookupAndDeleteNonce uid key
    Postgres.interpretNonceStoreToPostgres $ NonceStore.deleteNonce uid key
    pure mnonce
  NonceStore.DeleteNonce uid key -> do
    Cassandra.interpretNonceStoreToCassandra cs $ NonceStore.deleteNonce uid key
    Postgres.interpretNonceStoreToPostgres $ NonceStore.deleteNonce uid key
