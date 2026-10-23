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

module Wire.SessionStore.DualWrite
  ( interpretSessionStoreToCassandraAndPostgres,
  )
where

import Cassandra (ClientState)
import Imports
import Polysemy
import Wire.Postgres (PGConstraints)
import Wire.SessionStore
import Wire.SessionStore qualified as SessionStore
import Wire.SessionStore.Cassandra qualified as Cassandra
import Wire.SessionStore.Postgres qualified as Postgres

-- | Cassandra is the source of truth during migration; writes are mirrored to Postgres.
interpretSessionStoreToCassandraAndPostgres ::
  (PGConstraints r) =>
  ClientState ->
  InterpreterFor SessionStore r
interpretSessionStoreToCassandraAndPostgres cs = interpret $ \case
  InsertCookie uid cookie ttl -> do
    Cassandra.interpretSessionStoreCassandra cs $ SessionStore.insertCookie uid cookie ttl
    Postgres.interpretSessionStoreToPostgres $ SessionStore.insertCookie uid cookie ttl
  LookupCookie uid expiry cid ->
    Cassandra.interpretSessionStoreCassandra cs $ SessionStore.lookupCookie uid expiry cid
  ListCookies uid ->
    Cassandra.interpretSessionStoreCassandra cs $ SessionStore.listCookies uid
  DeleteAllCookies uid -> do
    Cassandra.interpretSessionStoreCassandra cs $ SessionStore.deleteAllCookies uid
    Postgres.interpretSessionStoreToPostgres $ SessionStore.deleteAllCookies uid
  DeleteCookies uid cc -> do
    Cassandra.interpretSessionStoreCassandra cs $ SessionStore.deleteCookies uid cc
    Postgres.interpretSessionStoreToPostgres $ SessionStore.deleteCookies uid cc
