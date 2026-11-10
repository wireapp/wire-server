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

module Wire.ScimExternalIdStore.DualWrite
  ( interpretScimExternalIdStoreToCassandraAndPostgres,
  )
where

import Cassandra (Client)
import Imports
import Polysemy
import Polysemy.Async (Async)
import Polysemy.Conc (Race)
import Polysemy.Error (Error)
import Polysemy.Resource (Resource)
import Polysemy.TinyLog (TinyLog)
import Polysemy.Time.Data.TimeUnit (MilliSeconds (..))
import Wire.API.User.Scim (validScimIdExternal)
import Wire.MigrationLock
import Wire.Postgres (PGConstraints)
import Wire.ScimExternalIdStore
import Wire.ScimExternalIdStore.Cassandra qualified as Cassandra
import Wire.ScimExternalIdStore.Postgres qualified as Postgres

-- | Cassandra is the source of truth during migration; writes are mirrored to
-- Postgres.
--
-- Every mapping mutation takes the shared (team, external-id) migration lock
-- and completes its Cassandra write and Postgres mirror while holding it, so
-- it is serialized against the background copier, which re-reads the row from
-- Cassandra and copies it under the exclusive lock on the same key
-- ('Wire.ScimExternalIdStore.Migration'). Without the lock, a copier running
-- from a stale scanned page could recreate a deleted mapping (deleted SCIM
-- external ids must stay deleted) or overwrite a newer user re-assignment or
-- creation status with the stale scanned values. Reads stay on Cassandra (the
-- source of truth) and need no lock.
interpretScimExternalIdStoreToCassandraAndPostgres ::
  ( Member (Embed Client) r,
    PGConstraints r,
    Member TinyLog r,
    Member (Error MigrationLockError) r,
    Member Async r,
    Member Race r,
    Member Resource r
  ) =>
  InterpreterFor ScimExternalIdStore r
interpretScimExternalIdStoreToCassandraAndPostgres = interpret $ \case
  Insert tid eid uid ->
    withMigrationLocks LockShared (MilliSeconds 500) [ScimExternalIdLockKey (tid, eid)] $ do
      Cassandra.scimExternalIdStoreToCassandra (insert tid eid uid)
      Postgres.interpretScimExternalIdStoreToPostgres (insert tid eid uid)
  Lookup tid eid ->
    Cassandra.scimExternalIdStoreToCassandra (Wire.ScimExternalIdStore.lookup tid eid)
  Delete tid eid ->
    withMigrationLocks LockShared (MilliSeconds 500) [ScimExternalIdLockKey (tid, eid)] $ do
      Cassandra.scimExternalIdStoreToCassandra (delete tid eid)
      Postgres.interpretScimExternalIdStoreToPostgres (delete tid eid)
  InsertStatus tid veid uid status ->
    let eid = validScimIdExternal veid
     in withMigrationLocks LockShared (MilliSeconds 500) [ScimExternalIdLockKey (tid, eid)] $ do
          Cassandra.scimExternalIdStoreToCassandra (insertStatus tid veid uid status)
          Postgres.interpretScimExternalIdStoreToPostgres (insertStatus tid veid uid status)
  LookupStatus tid veid ->
    Cassandra.scimExternalIdStoreToCassandra (Wire.ScimExternalIdStore.lookupStatus tid veid)
