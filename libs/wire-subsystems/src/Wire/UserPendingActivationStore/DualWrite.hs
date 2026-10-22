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

module Wire.UserPendingActivationStore.DualWrite
  ( interpretUserPendingActivationStoreToCassandraAndPostgres,
  )
where

import Cassandra (Client)
import Imports
import Polysemy
import Wire.Postgres (PGConstraints)
import Wire.UserPendingActivationStore
import Wire.UserPendingActivationStore qualified as UserPendingActivationStore
import Wire.UserPendingActivationStore.Cassandra qualified as Cassandra
import Wire.UserPendingActivationStore.Postgres qualified as Postgres

-- | Cassandra is the source of truth during migration; writes are mirrored to Postgres.
interpretUserPendingActivationStoreToCassandraAndPostgres ::
  ( Member (Embed Client) r,
    PGConstraints r
  ) =>
  InterpreterFor UserPendingActivationStore r
interpretUserPendingActivationStoreToCassandraAndPostgres = interpret $ \case
  Add upa -> do
    Cassandra.userPendingActivationStoreToCassandra $ UserPendingActivationStore.add upa
    Postgres.interpretUserPendingActivationStoreToPostgres $ UserPendingActivationStore.add upa
  List ->
    Cassandra.userPendingActivationStoreToCassandra UserPendingActivationStore.list
  RemoveMultiple uids -> do
    Cassandra.userPendingActivationStoreToCassandra $ UserPendingActivationStore.removeMultiple uids
    Postgres.interpretUserPendingActivationStoreToPostgres $ UserPendingActivationStore.removeMultiple uids
