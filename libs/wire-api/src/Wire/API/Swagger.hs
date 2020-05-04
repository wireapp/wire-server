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

module Wire.API.Swagger where

import Data.Swagger.Build.Api (Model)
import qualified Wire.API.Notification as Notification
import qualified Wire.API.Push.Token as Push.Token
import qualified Wire.API.Team as Team
import qualified Wire.API.Team.Conversation as Team.Conversation
import qualified Wire.API.Team.Event as Team.Event
import qualified Wire.API.Team.Feature as Team.Feature
import qualified Wire.API.Team.Member as Team.Member
import qualified Wire.API.Team.Permission as Team.Permission
import qualified Wire.API.Team.Role as Team.Role

models :: [Model]
models =
  [ Push.Token.modelPushToken,
    Push.Token.modelPushTokenList,
    Notification.modelEvent,
    Notification.modelNotification,
    Notification.modelNotificationList,
    Team.Feature.modelLegalHoldTeamConfig,
    Team.Feature.modelSsoTeamConfig,
    Team.Permission.modelPermissions,
    Team.Member.modelTeamMember,
    Team.Member.modelTeamMemberList,
    Team.Member.modelTeamMemberDelete,
    Team.Member.modelNewTeamMember,
    Team.Conversation.modelTeamConversation,
    Team.Conversation.modelTeamConversationList,
    Team.modelTeam,
    Team.modelTeamList,
    Team.modelNewBindingTeam,
    Team.modelNewNonBindingTeam,
    Team.modelTeamDelete,
    Team.Event.modelEvent,
    Team.Event.modelMemberEvent,
    Team.Event.modelMemberData,
    Team.Event.modelConvEvent,
    Team.Event.modelConversationData,
    Team.Event.modelUpdateEvent,
    Team.Event.modelUpdateData
  ]
