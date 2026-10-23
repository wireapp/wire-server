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

module Wire.InvitationStore.DualWrite
  ( interpretInvitationStoreToCassandraAndPostgres,
  )
where

import Cassandra (ClientState)
import Imports
import Polysemy
import Wire.InvitationStore
import Wire.InvitationStore.Cassandra qualified as Cassandra
import Wire.InvitationStore.Postgres qualified as Postgres
import Wire.Postgres (PGConstraints)

-- | Cassandra is the source of truth during migration; reads go to Cassandra,
-- writes are mirrored to Postgres (Cassandra first, mirroring PR #5417's
-- DualWrite ordering).
interpretInvitationStoreToCassandraAndPostgres ::
  (PGConstraints r) =>
  ClientState ->
  InterpreterFor InvitationStore r
interpretInvitationStoreToCassandraAndPostgres casClient = interpret $ \case
  InsertInvitation inv timeout -> do
    inv' <- Cassandra.interpretInvitationStoreToCassandra casClient $ insertInvitation inv timeout
    void $ Postgres.interpretInvitationStoreToPostgres $ insertInvitation inv timeout
    pure inv'
  LookupInvitation tid iid ->
    Cassandra.interpretInvitationStoreToCassandra casClient $ lookupInvitation tid iid
  LookupInvitationByCode code ->
    Cassandra.interpretInvitationStoreToCassandra casClient $ lookupInvitationByCode code
  LookupInvitationsByEmail email ->
    Cassandra.interpretInvitationStoreToCassandra casClient $ lookupInvitationsByEmail email
  InsertPendingScimUser tid email uid -> do
    Cassandra.interpretInvitationStoreToCassandra casClient $ insertPendingScimUser tid email uid
    Postgres.interpretInvitationStoreToPostgres $ insertPendingScimUser tid email uid
  LookupPendingScimUsers tid email ->
    Cassandra.interpretInvitationStoreToCassandra casClient $ lookupPendingScimUsers tid email
  DeletePendingScimUser tid email uid -> do
    Cassandra.interpretInvitationStoreToCassandra casClient $ deletePendingScimUser tid email uid
    Postgres.interpretInvitationStoreToPostgres $ deletePendingScimUser tid email uid
  LookupInvitationsPaginated mSize tid miid ->
    Cassandra.interpretInvitationStoreToCassandra casClient $ lookupInvitationsPaginated mSize tid miid
  CountInvitations tid ->
    Cassandra.interpretInvitationStoreToCassandra casClient $ countInvitations tid
  DeleteInvitation tid invId -> do
    Cassandra.interpretInvitationStoreToCassandra casClient $ deleteInvitation tid invId
    Postgres.interpretInvitationStoreToPostgres $ deleteInvitation tid invId
  DeleteAllTeamInvitations tid -> do
    Cassandra.interpretInvitationStoreToCassandra casClient $ deleteAllTeamInvitations tid
    Postgres.interpretInvitationStoreToPostgres $ deleteAllTeamInvitations tid
