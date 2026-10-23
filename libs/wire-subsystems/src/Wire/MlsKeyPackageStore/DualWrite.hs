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

module Wire.MlsKeyPackageStore.DualWrite
  ( interpretMlsKeyPackageStoreToCassandraAndPostgres,
  )
where

import Cassandra (ClientState)
import Imports
import Polysemy
import Wire.MlsKeyPackageStore
import Wire.MlsKeyPackageStore qualified as MlsKeyPackageStore
import Wire.MlsKeyPackageStore.Cassandra qualified as Cassandra
import Wire.MlsKeyPackageStore.Postgres qualified as Postgres
import Wire.Postgres (PGConstraints)

-- | Cassandra is the source of truth during migration; writes are mirrored to
-- Postgres. Copy/backfill races are accepted (final clean migration pass before
-- cutover).
interpretMlsKeyPackageStoreToCassandraAndPostgres ::
  (PGConstraints r) =>
  ClientState ->
  InterpreterFor MlsKeyPackageStore r
interpretMlsKeyPackageStoreToCassandraAndPostgres cas = interpret $ \case
  InsertKeyPackages u c kps -> do
    Cassandra.interpretMlsKeyPackageStoreToCassandra cas $ MlsKeyPackageStore.insertKeyPackages u c kps
    Postgres.interpretMlsKeyPackageStoreToPostgres $ MlsKeyPackageStore.insertKeyPackages u c kps
  LookupKeyPackages u c s ->
    Cassandra.interpretMlsKeyPackageStoreToCassandra cas $ MlsKeyPackageStore.lookupKeyPackages u c s
  LookupKeyPackagesBulk requests ->
    Cassandra.interpretMlsKeyPackageStoreToCassandra cas $ MlsKeyPackageStore.lookupKeyPackagesBulk requests
  DeleteKeyPackages u c s rs -> do
    Cassandra.interpretMlsKeyPackageStoreToCassandra cas $ MlsKeyPackageStore.deleteKeyPackages u c s rs
    Postgres.interpretMlsKeyPackageStoreToPostgres $ MlsKeyPackageStore.deleteKeyPackages u c s rs
  DeleteAllKeyPackages u c ss -> do
    Cassandra.interpretMlsKeyPackageStoreToCassandra cas $ MlsKeyPackageStore.deleteAllKeyPackages u c ss
    Postgres.interpretMlsKeyPackageStoreToPostgres $ MlsKeyPackageStore.deleteAllKeyPackages u c ss
  DeleteKeyPackage u c s r -> do
    Cassandra.interpretMlsKeyPackageStoreToCassandra cas $ MlsKeyPackageStore.deleteKeyPackage u c s r
    Postgres.interpretMlsKeyPackageStoreToPostgres $ MlsKeyPackageStore.deleteKeyPackage u c s r
