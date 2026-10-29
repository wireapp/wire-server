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

module Wire.ScimUserMetaStore.DualWrite
  ( interpretScimUserMetaStoreToCassandraAndPostgres,
  )
where

import Cassandra (Client)
import Imports
import Polysemy
import Wire.Postgres (PGConstraints)
import Wire.ScimUserMetaStore
import Wire.ScimUserMetaStore.Cassandra qualified as Cassandra
import Wire.ScimUserMetaStore.Postgres qualified as Postgres

-- | Cassandra is the source of truth during migration; writes are mirrored to
-- Postgres.
interpretScimUserMetaStoreToCassandraAndPostgres ::
  ( Member (Embed Client) r,
    PGConstraints r
  ) =>
  InterpreterFor ScimUserMetaStore r
interpretScimUserMetaStoreToCassandraAndPostgres = interpret $ \case
  Write emailType emailPrimary wm -> do
    Cassandra.scimUserMetaStoreToCassandra (write emailType emailPrimary wm)
    Postgres.interpretScimUserMetaStoreToPostgres (write emailType emailPrimary wm)
  Read uid ->
    Cassandra.scimUserMetaStoreToCassandra (Wire.ScimUserMetaStore.read uid)
  ReadMulti uids ->
    Cassandra.scimUserMetaStoreToCassandra (Wire.ScimUserMetaStore.readMulti uids)
  Delete uid -> do
    Cassandra.scimUserMetaStoreToCassandra (delete uid)
    Postgres.interpretScimUserMetaStoreToPostgres (delete uid)
