{-# LANGUAGE TemplateHaskell #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
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

module Galley.Effects.TeamStore
  ( -- * Team store effect
    TeamStore (..),

    -- * Teams

    -- ** Create teams
    createTeam,

    -- ** Read teams
    getTeam,
    getTeamName,
    getTeamBinding,
    getTeamsBindings,
    getTeamConversation,
    getTeamConversations,
    getTeamCreationTime,
    listTeams,
    selectTeams,
    getUserTeams,
    getUsersTeams,
    getOneUserTeam,
    lookupBindingTeam,

    -- ** Update teams
    deleteTeamConversation,
    setTeamData,
    setTeamStatus,

    -- ** Delete teams
    deleteTeam,

    -- * Team Members

    -- ** Create team members
    createTeamMember,

    -- ** Read team members
    getTeamMember,
    getTeamMembersWithLimit,
    getTeamMembers,
    getBillingTeamMembers,
    getTeamAdmins,
    selectTeamMembers,
    selectTeamMembersPaginated,

    -- ** Update team members
    setTeamMemberPermissions,

    -- ** Delete team members
    deleteTeamMember,

    -- * Configuration
    fanoutLimit,
    getLegalHoldFlag,

    -- * Events
    enqueueTeamEvent,
  )
where

import Data.Id
import Data.Range
import Galley.Effects.ListItems
import Galley.Types.Teams
import Imports
import Polysemy
import Proto.TeamEvents qualified as E
import Wire.API.Error
import Wire.API.Error.Galley
import Wire.API.Routes.Internal.Galley.TeamsIntra
import Wire.API.Team
import Wire.API.Team.Conversation
import Wire.API.Team.Feature
import Wire.API.Team.Member (HardTruncationLimit, TeamMember, TeamMemberList)
import Wire.API.Team.Permission
import Wire.Sem.Paging
import Wire.Sem.Paging.Cassandra (CassandraPaging)

data TeamStore m a where
  CreateTeamMember :: TeamId -> TeamMember -> TeamStore m ()
  SetTeamMemberPermissions :: Permissions -> TeamId -> UserId -> Permissions -> TeamStore m ()
  CreateTeam ::
    Maybe TeamId ->
    UserId ->
    Range 1 256 Text ->
    Icon ->
    Maybe (Range 1 256 Text) ->
    TeamBinding ->
    TeamStore m Team
  DeleteTeamMember :: TeamId -> UserId -> TeamStore m ()
  GetBillingTeamMembers :: TeamId -> TeamStore m [UserId]
  GetTeamAdmins :: TeamId -> TeamStore m [UserId]
  GetTeam :: TeamId -> TeamStore m (Maybe TeamData)
  GetTeamName :: TeamId -> TeamStore m (Maybe Text)
  GetTeamConversation :: TeamId -> ConvId -> TeamStore m (Maybe TeamConversation)
  GetTeamConversations :: TeamId -> TeamStore m [TeamConversation]
  SelectTeams :: UserId -> [TeamId] -> TeamStore m [TeamId]
  GetTeamMember :: TeamId -> UserId -> TeamStore m (Maybe TeamMember)
  GetTeamMembersWithLimit :: TeamId -> Range 1 HardTruncationLimit Int32 -> TeamStore m TeamMemberList
  GetTeamMembers :: TeamId -> TeamStore m [TeamMember]
  SelectTeamMembers :: TeamId -> [UserId] -> TeamStore m [TeamMember]
  SelectTeamMembersPaginated ::
    TeamId ->
    [UserId] ->
    Maybe (PagingState CassandraPaging TeamMember) ->
    PagingBounds CassandraPaging TeamMember ->
    TeamStore m (Page CassandraPaging TeamMember)
  -- FUTUREWORK(mangoiv): this should be a single 'TeamId' (@'Maybe' 'TeamId'@), there's no way
  -- a user could be part of multiple teams
  GetUserTeams :: UserId -> TeamStore m [TeamId]
  GetUsersTeams :: [UserId] -> TeamStore m (Map UserId TeamId)
  GetOneUserTeam :: UserId -> TeamStore m (Maybe TeamId)
  GetTeamsBindings :: [TeamId] -> TeamStore m [TeamBinding]
  GetTeamBinding :: TeamId -> TeamStore m (Maybe TeamBinding)
  GetTeamCreationTime :: TeamId -> TeamStore m (Maybe TeamCreationTime)
  DeleteTeam :: TeamId -> TeamStore m ()
  DeleteTeamConversation :: TeamId -> ConvId -> TeamStore m ()
  SetTeamData :: TeamId -> TeamUpdateData -> TeamStore m ()
  SetTeamStatus :: TeamId -> TeamStatus -> TeamStore m ()
  FanoutLimit :: TeamStore m (Range 1 HardTruncationLimit Int32)
  GetLegalHoldFlag :: TeamStore m (FeatureDefaults LegalholdConfig)
  EnqueueTeamEvent :: E.TeamEvent -> TeamStore m ()

makeSem ''TeamStore

listTeams ::
  (Member (ListItems p TeamId) r) =>
  UserId ->
  Maybe (PagingState p TeamId) ->
  PagingBounds p TeamId ->
  Sem r (Page p TeamId)
listTeams = listItems

lookupBindingTeam ::
  ( Member (ErrorS 'TeamNotFound) r,
    Member (ErrorS 'NonBindingTeam) r,
    Member TeamStore r
  ) =>
  UserId ->
  Sem r TeamId
lookupBindingTeam zusr = do
  tid <- getOneUserTeam zusr >>= noteS @'TeamNotFound
  binding <- getTeamBinding tid >>= noteS @'TeamNotFound
  case binding of
    Binding -> pure tid
    NonBinding -> throwS @'NonBindingTeam
