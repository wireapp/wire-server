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
import Cassandra.Util (Writetime (..), writetimeToInt64, writetimeToUTC)
import Data.Time.Clock (getCurrentTime)
import Imports
import Wire.API.Routes.Internal.Galley.TeamsIntra (TeamStatus)
import Wire.API.Team.SearchVisibility (TeamSearchVisibility (SearchVisibilityStandard))
import Polysemy
import Polysemy.Input
import Polysemy.TinyLog
import Wire.ConversationStore (ConversationStore)
import Wire.Postgres (PGConstraints)
import Wire.TeamStore
import Wire.TeamStore.Cassandra qualified as Cassandra
import Wire.TeamStore.Postgres (routeCassandra)
import Wire.TeamStore.Postgres qualified as Postgres
import Wire.Util (embedClientInput)

-- | Client-chosen LWW stamp for one dual write: picked once per constructor,
-- written to Cassandra with @using timestamp@ and applied to Postgres behind
-- per-column writetime guards, so both stores converge to the same final
-- value under any interleaving of concurrent writes.
type TeamWriteStamp = Writetime TeamStatus

newWriteStamp :: (Member (Embed IO) r) => Sem r TeamWriteStamp
newWriteStamp = Writetime <$> embed getCurrentTime

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
  -- Metadata writes: both stores, sharing one client-chosen writetime per
  -- write (see 'TeamWriteStamp').  Cassandra stays the source of truth for
  -- reads.
  CreateTeam mtid uid n i k b -> do
    stamp <- newWriteStamp
    t <- Cassandra.createTeamWithTimestamp mtid uid n i k b (writetimeToInt64 stamp)
    Postgres.insertTeamPg t (writetimeToUTC stamp)
    pure t
  SetTeamData tid upd -> do
    stamp <- newWriteStamp
    embedClientInput $ Cassandra.updateTeamWithTimestamp tid upd (writetimeToInt64 stamp)
    Postgres.updateTeamPg tid upd (writetimeToUTC stamp)
  SetTeamStatus tid st -> do
    stamp <- newWriteStamp
    embedClientInput $ Cassandra.updateTeamStatusWithTimestamp tid st (writetimeToInt64 stamp)
    Postgres.updateTeamStatusPg tid st (writetimeToUTC stamp)
  SetSearchVisibility tid v -> do
    stamp <- newWriteStamp
    embedClientInput $ Cassandra.setSearchVisibilityWithTimestamp tid v (writetimeToInt64 stamp)
    Postgres.updateTeamSearchVisibilityPg tid v (writetimeToUTC stamp)
  ResetSearchVisibility tid -> do
    stamp <- newWriteStamp
    embedClientInput $ Cassandra.setSearchVisibilityWithTimestamp tid SearchVisibilityStandard (writetimeToInt64 stamp)
    Postgres.updateTeamSearchVisibilityPg tid SearchVisibilityStandard (writetimeToUTC stamp)
  DeleteTeam tid -> Postgres.deleteTeamWithCassandra tid
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
