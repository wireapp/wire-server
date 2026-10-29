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

module Wire.UserKeyStore.DualWrite
  ( interpretUserKeyStoreToCassandraAndPostgres,
  )
where

import Cassandra (ClientState)
import Imports
import Polysemy
import Wire.Postgres (PGConstraints)
import Wire.UserKeyStore
import Wire.UserKeyStore qualified as UserKeyStore
import Wire.UserKeyStore.Cassandra qualified as Cassandra
import Wire.UserKeyStore.Postgres qualified as Postgres
import Wire.UserStore

-- | Cassandra is the source of truth during migration; writes are mirrored to Postgres.
interpretUserKeyStoreToCassandraAndPostgres ::
  ( PGConstraints r,
    Member UserStore r
  ) =>
  ClientState ->
  InterpreterFor UserKeyStore r
interpretUserKeyStoreToCassandraAndPostgres casClient = interpret $ \case
  LookupKey key ->
    Cassandra.interpretUserKeyStoreCassandra casClient $ UserKeyStore.lookupKey key
  InsertKey uid key -> do
    Cassandra.interpretUserKeyStoreCassandra casClient $ UserKeyStore.insertKey uid key
    Postgres.interpretUserKeyStoreToPostgres $ UserKeyStore.insertKey uid key
  DeleteKey key -> do
    Cassandra.interpretUserKeyStoreCassandra casClient $ UserKeyStore.deleteKey key
    Postgres.interpretUserKeyStoreToPostgres $ UserKeyStore.deleteKey key
  DeleteKeyForUser uid key -> do
    Cassandra.interpretUserKeyStoreCassandra casClient $ UserKeyStore.deleteKeyForUser uid key
    Postgres.interpretUserKeyStoreToPostgres $ UserKeyStore.deleteKeyForUser uid key
  KeyAvailable key uid ->
    Cassandra.interpretUserKeyStoreCassandra casClient $ UserKeyStore.keyAvailable key uid
  ClaimKey key uid -> do
    -- Read-then-write, mirroring the Cassandra implementation: availability is
    -- decided against Cassandra (the source of truth); a successful claim is
    -- mirrored to Postgres.
    free <- Cassandra.interpretUserKeyStoreCassandra casClient $ UserKeyStore.keyAvailable key (Just uid)
    when free $ do
      Cassandra.interpretUserKeyStoreCassandra casClient $ UserKeyStore.insertKey uid key
      Postgres.interpretUserKeyStoreToPostgres $ UserKeyStore.insertKey uid key
    pure free
