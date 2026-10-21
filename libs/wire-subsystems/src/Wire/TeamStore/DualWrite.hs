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
-- FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program.  If not, see <https://www.gnu.org/licenses/>.

-- | Dual-write interpreter for 'TeamStore': Cassandra stays the source of
-- truth for reads, writes are mirrored to Postgres.
module Wire.TeamStore.DualWrite
  ( interpretTeamStoreToCassandraAndPostgres,
  )
where

import Cassandra (ClientState)
import Data.Id
import Data.UUID.V4 (nextRandom)
import Imports
import Polysemy
import Polysemy.Input
import Polysemy.TinyLog
import Wire.ConversationStore (ConversationStore)
import Wire.Postgres (PGConstraints)
import Wire.TeamStore
import Wire.TeamStore.Cassandra qualified as Cassandra
import Wire.TeamStore.Postgres qualified as Postgres

interpretTeamStoreToCassandraAndPostgres ::
  ( Member (Input ClientState) r,
    Member ConversationStore r,
    PGConstraints r,
    Member TinyLog r
  ) =>
  Sem (TeamStore ': r) a ->
  Sem r a
interpretTeamStoreToCassandraAndPostgres = interpret $ \case
  -- writes: mirrored to both stores
  CreateTeamMember tid mem -> do
    Cassandra.interpretTeamStoreToCassandra (createTeamMember tid mem)
    Postgres.interpretTeamStoreToPostgres (createTeamMember tid mem)
  SetTeamMemberPermissions perm0 tid uid perm1 -> do
    Cassandra.interpretTeamStoreToCassandra (setTeamMemberPermissions perm0 tid uid perm1)
    Postgres.interpretTeamStoreToPostgres (setTeamMemberPermissions perm0 tid uid perm1)
  CreateTeam t uid n i k b -> do
    -- both legs must agree on the generated id
    tid <- embed @IO (maybe (Id <$> liftIO nextRandom) pure t)
    _ <- Cassandra.interpretTeamStoreToCassandra (createTeam (Just tid) uid n i k b)
    Postgres.interpretTeamStoreToPostgres (createTeam (Just tid) uid n i k b)
  DeleteTeamMember tid uid -> do
    Cassandra.interpretTeamStoreToCassandra (deleteTeamMember tid uid)
    Postgres.interpretTeamStoreToPostgres (deleteTeamMember tid uid)
  -- reads: Cassandra only (source of truth during migration)
  GetBillingTeamMembers tid -> Cassandra.interpretTeamStoreToCassandra (getBillingTeamMembers tid)
  GetTeamAdmins tid -> Cassandra.interpretTeamStoreToCassandra (getTeamAdmins tid)
  GetTeam tid -> Cassandra.interpretTeamStoreToCassandra (getTeam tid)
  GetTeamName tid -> Cassandra.interpretTeamStoreToCassandra (getTeamName tid)
  SelectTeams uid tids -> Cassandra.interpretTeamStoreToCassandra (selectTeams uid tids)
  GetTeamMember tid uid -> Cassandra.interpretTeamStoreToCassandra (getTeamMember tid uid)
  GetTeamMembers tid -> Cassandra.interpretTeamStoreToCassandra (getTeamMembers tid)
  GetTeamMembersWithLimit tid n -> Cassandra.interpretTeamStoreToCassandra (getTeamMembersWithLimit tid n)
  SelectTeamMembers tid uids -> Cassandra.interpretTeamStoreToCassandra (selectTeamMembers tid uids)
  SelectTeamMemberInfos tid uids -> Cassandra.interpretTeamStoreToCassandra (selectTeamMemberInfos tid uids)
  GetUserTeams uid -> Cassandra.interpretTeamStoreToCassandra (getUserTeams uid)
  GetUsersTeams uids -> Cassandra.interpretTeamStoreToCassandra (getUsersTeams uids)
  GetOneUserTeam uid -> Cassandra.interpretTeamStoreToCassandra (getOneUserTeam uid)
  GetTeamsBindings tids -> Cassandra.interpretTeamStoreToCassandra (getTeamsBindings tids)
  GetTeamBinding tid -> Cassandra.interpretTeamStoreToCassandra (getTeamBinding tid)
  GetTeamCreationTime tid -> Cassandra.interpretTeamStoreToCassandra (getTeamCreationTime tid)
  -- DeleteTeam triggers conversation deletion through the residual
  -- ConversationStore effect; that must happen exactly once, so the Postgres
  -- leg runs the store-only variant.
  DeleteTeam tid -> do
    Cassandra.interpretTeamStoreToCassandra (deleteTeam tid)
    Postgres.deleteTeamStore tid
  SetTeamData tid upd -> do
    Cassandra.interpretTeamStoreToCassandra (setTeamData tid upd)
    Postgres.interpretTeamStoreToPostgres (setTeamData tid upd)
  SetTeamStatus tid st -> do
    Cassandra.interpretTeamStoreToCassandra (setTeamStatus tid st)
    Postgres.interpretTeamStoreToPostgres (setTeamStatus tid st)
  GetSearchVisibility tid -> Cassandra.interpretTeamStoreToCassandra (getSearchVisibility tid)
  SetSearchVisibility tid value -> do
    Cassandra.interpretTeamStoreToCassandra (setSearchVisibility tid value)
    Postgres.interpretTeamStoreToPostgres (setSearchVisibility tid value)
  ResetSearchVisibility tid -> do
    Cassandra.interpretTeamStoreToCassandra (resetSearchVisibility tid)
    Postgres.interpretTeamStoreToPostgres (resetSearchVisibility tid)
