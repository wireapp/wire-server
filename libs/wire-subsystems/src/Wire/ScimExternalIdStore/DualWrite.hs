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
import Wire.Postgres (PGConstraints)
import Wire.ScimExternalIdStore
import Wire.ScimExternalIdStore.Cassandra qualified as Cassandra
import Wire.ScimExternalIdStore.Postgres qualified as Postgres

-- | Cassandra is the source of truth during migration; writes are mirrored to
-- Postgres.
interpretScimExternalIdStoreToCassandraAndPostgres ::
  ( Member (Embed Client) r,
    PGConstraints r
  ) =>
  InterpreterFor ScimExternalIdStore r
interpretScimExternalIdStoreToCassandraAndPostgres = interpret $ \case
  Insert tid eid uid -> do
    Cassandra.scimExternalIdStoreToCassandra (insert tid eid uid)
    Postgres.interpretScimExternalIdStoreToPostgres (insert tid eid uid)
  Lookup tid eid ->
    Cassandra.scimExternalIdStoreToCassandra (Wire.ScimExternalIdStore.lookup tid eid)
  Delete tid eid -> do
    Cassandra.scimExternalIdStoreToCassandra (delete tid eid)
    Postgres.interpretScimExternalIdStoreToPostgres (delete tid eid)
  InsertStatus tid veid uid status -> do
    Cassandra.scimExternalIdStoreToCassandra (insertStatus tid veid uid status)
    Postgres.interpretScimExternalIdStoreToPostgres (insertStatus tid veid uid status)
  LookupStatus tid veid ->
    Cassandra.scimExternalIdStoreToCassandra (Wire.ScimExternalIdStore.lookupStatus tid veid)
