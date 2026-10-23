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

module Wire.ConnectionStore.DualWrite
  ( interpretConnectionStoreToCassandraAndPostgres,
  )
where

import Cassandra qualified as Cas
import Imports
import Polysemy
import Wire.ConnectionStore
import Wire.ConnectionStore.Cassandra (connectionStoreToCassandra)
import Wire.ConnectionStore.Postgres qualified as Postgres
import Wire.Postgres (PGConstraints)
import Wire.Sem.Paging.Cassandra (InternalPaging)

-- | During migration, Cassandra remains the source of truth for reads while
-- writes are mirrored to Postgres. Connections have no consume-on-read
-- semantics, so every write is a plain mirror.
interpretConnectionStoreToCassandraAndPostgres ::
  forall r.
  ( Member (Embed Cas.Client) r,
    PGConstraints r
  ) =>
  InterpreterFor (ConnectionStore InternalPaging) r
interpretConnectionStoreToCassandraAndPostgres = interpret $ \case
  InsertConnection self target rel qcnv -> do
    res <- connectionStoreToCassandra $ insertConnection self target rel qcnv
    _ <- Postgres.interpretConnectionStoreToPostgres $ insertConnection self target rel qcnv
    pure res
  UpdateConnectionStatus self target status -> do
    res <- connectionStoreToCassandra $ updateConnectionStatus self target status
    _ <- Postgres.interpretConnectionStoreToPostgres $ updateConnectionStatus self target status
    pure res
  DeleteConnections u -> do
    connectionStoreToCassandra $ deleteConnections u
    Postgres.interpretConnectionStoreToPostgres $ deleteConnections u
  DeleteRemoteConnections u rs -> do
    connectionStoreToCassandra $ deleteRemoteConnections u rs
    Postgres.interpretConnectionStoreToPostgres $ deleteRemoteConnections u rs
  LookupConnection self target ->
    connectionStoreToCassandra $ lookupConnection self target
  LookupRelationWithHistory self target ->
    connectionStoreToCassandra $ lookupRelationWithHistory self target
  LookupLocalConnections lfrom start size ->
    connectionStoreToCassandra $ lookupLocalConnections lfrom start size
  LookupLocalConnectionsPage self ps size ->
    connectionStoreToCassandra $ lookupLocalConnectionsPage self ps size
  LookupRemoteConnectionsPage self ps size ->
    connectionStoreToCassandra $ lookupRemoteConnectionsPage self ps size
  LookupConnectionStatus from to ->
    connectionStoreToCassandra $ lookupConnectionStatus from to
  LookupConnectionStatus' from ->
    connectionStoreToCassandra $ lookupConnectionStatus' from
  LookupLocalConnectionStatuses froms tos ->
    connectionStoreToCassandra $ lookupLocalConnectionStatuses froms tos
  LookupRemoteConnectionStatuses froms tos ->
    connectionStoreToCassandra $ lookupRemoteConnectionStatuses froms tos
  LookupAllStatuses lfroms ->
    connectionStoreToCassandra $ lookupAllStatuses lfroms
  RemoteConnectedUsersPaginated uid mps bounds ->
    connectionStoreToCassandra $ remoteConnectedUsersPaginated uid mps bounds
  LookupContactListWithRelation u ->
    connectionStoreToCassandra $ lookupContactListWithRelation u
  CountConnections u r ->
    connectionStoreToCassandra $ countConnections u r
