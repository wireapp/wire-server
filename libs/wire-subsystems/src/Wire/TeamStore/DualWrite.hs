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

module Wire.TeamStore.DualWrite
  ( interpretTeamStoreMetadataToCassandraAndPostgres,
  )
where

import Cassandra (ClientState)
import Imports
import Polysemy
import Polysemy.Input
import Polysemy.TinyLog
import Wire.ConversationStore (ConversationStore)
import Wire.Postgres (PGConstraints)
import Wire.TeamStore
import Wire.TeamStore.Postgres (routeCassandra)
import Wire.TeamStore.Postgres qualified as Postgres

-- | Dual-write interpreter for the team-metadata slice: metadata writes go to
-- both Cassandra and Postgres; everything else (all reads and the team-member
-- constructors) goes to Cassandra, which remains the source of truth during
-- migration.
interpretTeamStoreMetadataToCassandraAndPostgres ::
  ( PGConstraints r,
    Member (Input ClientState) r,
    Member TinyLog r,
    Member ConversationStore r
  ) =>
  InterpreterFor TeamStore r
interpretTeamStoreMetadataToCassandraAndPostgres = interpret $ \case
  -- Metadata writes: both stores.  The Cassandra leg runs through the full
  -- Cassandra interpreter, which stays the source of truth.
  CreateTeam mtid uid n i k b -> do
    t <- routeCassandra (createTeam mtid uid n i k b)
    Postgres.insertTeamPg t
    pure t
  SetTeamData tid upd -> do
    routeCassandra (setTeamData tid upd)
    Postgres.updateTeamPg tid upd
  SetTeamStatus tid st -> do
    routeCassandra (setTeamStatus tid st)
    Postgres.updateTeamStatusPg tid st
  SetSearchVisibility tid v -> do
    routeCassandra (setSearchVisibility tid v)
    Postgres.setSearchVisibilityPg tid v
  ResetSearchVisibility tid -> do
    routeCassandra (resetSearchVisibility tid)
    Postgres.resetSearchVisibilityPg tid
  DeleteTeam tid -> do
    -- The Postgres metadata half; the member removal lives in the Cassandra
    -- implementation (member tables are still Cassandra-owned in this PR), so
    -- the whole constructor is additionally routed through the Cassandra
    -- interpreter, which also rewrites the metadata there.
    Postgres.deleteTeamPg tid
    routeCassandra (deleteTeam tid)
  -- Metadata reads: Cassandra is the source of truth.
  GetTeam tid -> routeCassandra (getTeam tid)
  GetTeamName tid -> routeCassandra (getTeamName tid)
  GetTeamsBindings tids -> routeCassandra (getTeamsBindings tids)
  GetTeamBinding tid -> routeCassandra (getTeamBinding tid)
  GetTeamCreationTime tid -> routeCassandra (getTeamCreationTime tid)
  GetSearchVisibility tid -> routeCassandra (getSearchVisibility tid)
  -- Team members: Cassandra.
  CreateTeamMember tid mem -> routeCassandra (createTeamMember tid mem)
  SetTeamMemberPermissions p0 tid uid p1 -> routeCassandra (setTeamMemberPermissions p0 tid uid p1)
  DeleteTeamMember tid uid -> routeCassandra (deleteTeamMember tid uid)
  GetTeamMember tid uid -> routeCassandra (getTeamMember tid uid)
  GetTeamMembers tid -> routeCassandra (getTeamMembers tid)
  GetTeamMembersWithLimit tid n -> routeCassandra (getTeamMembersWithLimit tid n)
  SelectTeamMembers tid uids -> routeCassandra (selectTeamMembers tid uids)
  SelectTeamMemberInfos tid uids -> routeCassandra (selectTeamMemberInfos tid uids)
  GetBillingTeamMembers tid -> routeCassandra (getBillingTeamMembers tid)
  GetTeamAdmins tid -> routeCassandra (getTeamAdmins tid)
  SelectTeams uid tids -> routeCassandra (selectTeams uid tids)
  GetUserTeams uid -> routeCassandra (getUserTeams uid)
  GetUsersTeams uids -> routeCassandra (getUsersTeams uids)
  GetOneUserTeam uid -> routeCassandra (getOneUserTeam uid)
