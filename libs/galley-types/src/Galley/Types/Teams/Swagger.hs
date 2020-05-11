{-# LANGUAGE OverloadedStrings #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
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

module Galley.Types.Teams.Swagger where

import Data.Swagger.Build.Api
import qualified Wire.API.Event.Team as Event.Team
import qualified Wire.API.Swagger
import qualified Wire.API.Team as Team
import qualified Wire.API.Team.Conversation as Team.Conversation
import qualified Wire.API.Team.Feature as Team.Feature
import qualified Wire.API.Team.Member as Team.Member
import qualified Wire.API.Team.Permission as Team.Permission

-- | Actually all models of the whole API,
-- but it doesn't hurt and makes it less likely to forget one.
teamsModels :: [Model]
teamsModels = Wire.API.Swagger.models

team :: Model
team = Team.modelTeam

newBindingTeam :: Model
newBindingTeam = Team.modelNewBindingTeam

newNonBindingTeam :: Model
newNonBindingTeam = Team.modelNewNonBindingTeam

teamList :: Model
teamList = Team.modelTeamList

newTeamMember :: Model
newTeamMember = Team.Member.modelNewTeamMember

teamMember :: Model
teamMember = Team.Member.modelTeamMember

legalHoldStatusType :: DataType
legalHoldStatusType = Team.Feature.typeFeatureStatus

permissions :: Model
permissions = Team.Permission.modelPermissions

teamMemberList :: Model
teamMemberList = Team.Member.modelTeamMemberList

teamConversation :: Model
teamConversation = Team.Conversation.modelTeamConversation

teamConversationList :: Model
teamConversationList = Team.Conversation.modelTeamConversationList

event :: Model
event = Event.Team.modelEvent

eventType :: DataType
eventType = Event.Team.typeEventType

memberEvent :: Model
memberEvent = Event.Team.modelMemberEvent

convEvent :: Model
convEvent = Event.Team.modelConvEvent

updateEvent :: Model
updateEvent = Event.Team.modelUpdateEvent

member :: Model
member = Event.Team.modelMemberData

conversation :: Model
conversation = Event.Team.modelConversationData

update :: Model
update = Team.modelUpdateData

teamDelete :: Model
teamDelete = Team.modelTeamDelete

teamMemberDelete :: Model
teamMemberDelete = Team.Member.modelTeamMemberDelete
