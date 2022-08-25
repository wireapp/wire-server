{-# LANGUAGE TemplateHaskell #-}

module Brig.Sem.GalleyProvider where

import Brig.API.Types
import qualified Data.Currency as Currency
import Data.Id
import Data.Json.Util (UTCTimeMillis)
import Data.Qualified
import qualified Galley.Types.Teams.Intra as Team
import Imports
import qualified Network.Wai.Utilities.Error as Wai
import Polysemy
import Wire.API.Conversation
import Wire.API.Team
import qualified Wire.API.Team.Conversation as Conv
import Wire.API.Team.Feature
import qualified Wire.API.Team.Member as Team
import Wire.API.Team.Role
import Wire.API.Team.SearchVisibility
import Wire.API.User

data GalleyProvider m a where
  CreateSelfConv ::
    UserId ->
    GalleyProvider m ()
  CreateLocalConnectConv ::
    Local UserId ->
    Local UserId ->
    Maybe Text ->
    Maybe ConnId ->
    GalleyProvider m ConvId
  AcceptLocalConnectConv ::
    Local UserId ->
    Maybe ConnId ->
    ConvId ->
    GalleyProvider m Conversation
  BlockLocalConv ::
    Local UserId ->
    Maybe ConnId ->
    ConvId ->
    GalleyProvider m ()
  UnblockLocalConv ::
    Local UserId ->
    Maybe ConnId ->
    ConvId ->
    GalleyProvider m Conversation
  GetConv ::
    UserId ->
    ConvId ->
    GalleyProvider m (Maybe Conversation)
  GetTeamConv ::
    UserId ->
    TeamId ->
    ConvId ->
    GalleyProvider m (Maybe Conv.TeamConversation)
  RmUser ::
    UserId ->
    [Asset] ->
    GalleyProvider m ()
  NewClient ::
    UserId ->
    ClientId ->
    GalleyProvider m ()
  RmClient ::
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
  GetTeamContacts ::
    UserId ->
    GalleyProvider m (Maybe Team.TeamMemberList)
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

makeSem ''GalleyProvider
