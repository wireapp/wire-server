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

module Wire.GalleyAPIAccess where

import Data.Currency qualified as Currency
import Data.Id
import Data.Json.Util (UTCTimeMillis)
import Data.Qualified
import Imports
import Network.Wai.Utilities.Error qualified as Wai
import Polysemy
import Wire.API.Conversation
import Wire.API.Routes.Internal.Brig.EJPD (EJPDConvInfo)
import Wire.API.Routes.Internal.Galley.TeamsIntra qualified as Team
import Wire.API.Team
import Wire.API.Team.Conversation qualified as Conv
import Wire.API.Team.Feature
import Wire.API.Team.LegalHold
import Wire.API.Team.Member qualified as Team
import Wire.API.Team.Role
import Wire.API.Team.SearchVisibility

data ShowOrHideInvitationUrl = ShowInvitationUrl | HideInvitationUrl
  deriving (Eq, Show)

data MLSOneToOneEstablished
  = Established
  | NotEstablished
  | NotAMember
  deriving (Eq, Show)

data GalleyAPIAccess m a where
  CreateSelfConv ::
    UserId ->
    GalleyAPIAccess m ()
  GetConv ::
    UserId ->
    Local ConvId ->
    GalleyAPIAccess m (Maybe ConversationV8)
  GetTeamConv ::
    UserId ->
    TeamId ->
    ConvId ->
    GalleyAPIAccess m (Maybe Conv.TeamConversation)
  NewClient ::
    UserId ->
    ClientId ->
    GalleyAPIAccess m ()
  CheckUserCanJoinTeam ::
    TeamId ->
    GalleyAPIAccess m (Maybe Wai.Error)
  AddTeamMember ::
    UserId ->
    TeamId ->
    Maybe (UserId, UTCTimeMillis) ->
    Role ->
    GalleyAPIAccess m Bool
  CreateTeam ::
    UserId ->
    NewTeam ->
    TeamId ->
    GalleyAPIAccess m ()
  GetTeamMember ::
    UserId ->
    TeamId ->
    GalleyAPIAccess m (Maybe Team.TeamMember)
  GetTeamMembers ::
    TeamId ->
    GalleyAPIAccess m Team.TeamMemberList
  GetTeamId ::
    UserId ->
    GalleyAPIAccess m (Maybe TeamId)
  GetTeam ::
    TeamId ->
    GalleyAPIAccess m Team.TeamData
  GetTeamName ::
    TeamId ->
    GalleyAPIAccess m Team.TeamName
  GetTeamLegalHoldStatus ::
    TeamId ->
    GalleyAPIAccess m (LockableFeature LegalholdConfig)
  GetUserLegalholdStatus ::
    Local UserId -> TeamId -> GalleyAPIAccess m UserLegalHoldStatusResponse
  GetTeamSearchVisibility ::
    TeamId ->
    GalleyAPIAccess m TeamSearchVisibility
  ChangeTeamStatus ::
    TeamId ->
    Team.TeamStatus ->
    Maybe Currency.Alpha ->
    GalleyAPIAccess m ()
  MemberIsTeamOwner ::
    TeamId ->
    UserId ->
    GalleyAPIAccess m Bool
  GetAllTeamFeaturesForUser ::
    Maybe UserId ->
    GalleyAPIAccess m AllTeamFeatures
  GetFeatureConfigForTeam ::
    ( IsFeatureConfig feature,
      Typeable feature
    ) =>
    TeamId ->
    GalleyAPIAccess m (LockableFeature feature)
  GetVerificationCodeEnabled ::
    TeamId ->
    GalleyAPIAccess m Bool
  GetExposeInvitationURLsToTeamAdmin ::
    TeamId ->
    GalleyAPIAccess m ShowOrHideInvitationUrl
  IsMLSOne2OneEstablished ::
    Local UserId ->
    Qualified UserId ->
    GalleyAPIAccess m MLSOneToOneEstablished
  UnblockConversation ::
    Local UserId ->
    Maybe ConnId ->
    Qualified ConvId ->
    GalleyAPIAccess m ConversationV8
  GetEJPDConvInfo ::
    UserId ->
    GalleyAPIAccess m [EJPDConvInfo]

makeSem ''GalleyAPIAccess
