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

module Wire.PasswordStore.DualWrite
  ( interpretPasswordStoreToCassandraAndPostgres,
  )
where

import Cassandra (ClientState)
import Imports
import Polysemy
import Wire.PasswordStore
import Wire.PasswordStore qualified as PasswordStore
import Wire.PasswordStore.Cassandra qualified as Cassandra
import Wire.PasswordStore.Postgres qualified as Postgres
import Wire.Postgres (PGConstraints)

-- | Cassandra is the source of truth during migration: reads go to Cassandra
-- and writes are mirrored to Postgres, so a background-worker copy pass can be
-- re-run at any time and dual-write traffic heals rows written after a
-- completed pass before the cutover.
interpretPasswordStoreToCassandraAndPostgres ::
  (PGConstraints r) =>
  ClientState ->
  InterpreterFor PasswordStore r
interpretPasswordStoreToCassandraAndPostgres cas = interpret $ \case
  LookupHashedProviderPassword pid ->
    Cassandra.interpretPasswordStore cas $ PasswordStore.lookupHashedProviderPassword pid
  InsertHashedProviderPassword pid pw -> do
    Cassandra.interpretPasswordStore cas $ PasswordStore.insertHashedProviderPassword pid pw
    Postgres.interpretPasswordStoreToPostgres $ PasswordStore.insertHashedProviderPassword pid pw
  DeleteHashedProviderPassword pid -> do
    Cassandra.interpretPasswordStore cas $ PasswordStore.deleteHashedProviderPassword pid
    Postgres.interpretPasswordStoreToPostgres $ do
      Postgres.insertProviderPasswordDeletionMarker pid
      PasswordStore.deleteHashedProviderPassword pid
