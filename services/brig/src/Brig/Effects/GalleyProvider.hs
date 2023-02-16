-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2023 Wire Swiss GmbH <opensource@wire.com>
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
{-# LANGUAGE TemplateHaskell #-}

module Brig.Effects.GalleyProvider where

import Brig.API.Types
import Brig.Team.Types (ShowOrHideInvitationUrl (..))
import qualified Data.Currency as Currency
import Data.Id
import Data.Json.Util (UTCTimeMillis)
import Data.Qualified
import Imports
import qualified Network.Wai.Utilities.Error as Wai
import Polysemy
import Wire.API.Conversation
import qualified Wire.API.Routes.Internal.Galley.TeamsIntra as Team
import Wire.API.Team
import qualified Wire.API.Team.Conversation as Conv
import Wire.API.Team.Feature
import qualified Wire.API.Team.Member as Team
import Wire.API.Team.Role
import Wire.API.Team.SearchVisibility

data GalleyProvider m a where
  CreateSelfConv ::
    UserId ->
    GalleyProvider m ()
  GetConv ::
    UserId ->
    Local ConvId ->
    GalleyProvider m (Maybe Conversation)
  GetTeamConv ::
    UserId ->
    TeamId ->
    ConvId ->
    GalleyProvider m (Maybe Conv.TeamConversation)
  NewClient ::
    UserId ->
    ClientId ->
    GalleyProvider m ()
  CheckUserCanJoinTeam ::
    TeamId ->
    GalleyProvider m (Maybe Wai.Error)
  AddTeamMember ::
    UserId ->
    TeamId ->
    (Maybe (UserId, UTCTimeMillis), Role) ->
    GalleyProvider m Bool
  CreateTeam ::
    UserId ->
    BindingNewTeam ->
    TeamId ->
    GalleyProvider m CreateUserTeam
  GetTeamMember ::
    UserId ->
    TeamId ->
    GalleyProvider m (Maybe Team.TeamMember)
  GetTeamMembers ::
    TeamId ->
    GalleyProvider m Team.TeamMemberList
  GetTeamId ::
    UserId ->
    GalleyProvider m (Maybe TeamId)
  GetTeam ::
    TeamId ->
    GalleyProvider m Team.TeamData
  GetTeamName ::
    TeamId ->
    GalleyProvider m Team.TeamName
  GetTeamLegalHoldStatus ::
    TeamId ->
    GalleyProvider m (WithStatus LegalholdConfig)
  GetTeamSearchVisibility ::
    TeamId ->
    GalleyProvider m TeamSearchVisibility
  ChangeTeamStatus ::
    TeamId ->
    Team.TeamStatus ->
    Maybe Currency.Alpha ->
    GalleyProvider m ()
  MemberIsTeamOwner ::
    TeamId ->
    UserId ->
    GalleyProvider m Bool
  GetAllFeatureConfigsForUser ::
    Maybe UserId ->
    GalleyProvider m AllFeatureConfigs
  GetVerificationCodeEnabled ::
    TeamId ->
    GalleyProvider m Bool
  GetExposeInvitationURLsToTeamAdmin ::
    TeamId ->
    GalleyProvider m ShowOrHideInvitationUrl

makeSem ''GalleyProvider
