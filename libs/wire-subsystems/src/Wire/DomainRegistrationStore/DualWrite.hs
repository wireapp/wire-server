

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

module Wire.DomainRegistrationStore.DualWrite
  ( interpretDomainRegistrationStoreToCassandraAndPostgres,
  )
where

import Cassandra (ClientState)
import Imports
import Polysemy
import Wire.DomainRegistrationStore
import Wire.DomainRegistrationStore qualified as DomainRegistrationStore
import Wire.DomainRegistrationStore.Cassandra qualified as Cassandra
import Wire.DomainRegistrationStore.Postgres qualified as Postgres
import Wire.Postgres

interpretDomainRegistrationStoreToCassandraAndPostgres ::
  (PGConstraints r) =>
  ClientState ->
  InterpreterFor DomainRegistrationStore r
interpretDomainRegistrationStoreToCassandraAndPostgres cs = interpret $ \case
  UpsertInternal dr -> do
    Cassandra.interpretDomainRegistrationStoreToCassandra cs $ DomainRegistrationStore.upsertInternal dr
    Postgres.interpretDomainRegistrationStoreToPostgres $ DomainRegistrationStore.upsertInternal dr
  LookupInternal domain ->
    Cassandra.interpretDomainRegistrationStoreToCassandra cs $ DomainRegistrationStore.lookupInternal domain
  LookupByTeamInternal tid ->
    Cassandra.interpretDomainRegistrationStoreToCassandra cs $ DomainRegistrationStore.lookupByTeamInternal tid
  DeleteInternal domain -> do
    Cassandra.interpretDomainRegistrationStoreToCassandra cs $ DomainRegistrationStore.deleteInternal domain
    Postgres.interpretDomainRegistrationStoreToPostgres $ DomainRegistrationStore.deleteInternal domain
