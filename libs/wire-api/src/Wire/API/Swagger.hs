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
import qualified Wire.API.Connection as Connection
import qualified Wire.API.Conversation.Code as Conversation.Code
import qualified Wire.API.Conversation.Event as Conversation.Event
import qualified Wire.API.Conversation.Member as Conversation.Member
import qualified Wire.API.Conversation.Role as Conversation.Role
import qualified Wire.API.Conversation.Typing as Conversation.Typing
import qualified Wire.API.CustomBackend as CustomBackend
import qualified Wire.API.Message as Message
import qualified Wire.API.Notification as Notification
import qualified Wire.API.Push.Token as Push.Token
import qualified Wire.API.Team as Team
import qualified Wire.API.Team.Conversation as Team.Conversation
import qualified Wire.API.Team.Event as Team.Event
import qualified Wire.API.Team.Feature as Team.Feature
import qualified Wire.API.Team.Member as Team.Member
import qualified Wire.API.Team.Permission as Team.Permission
import qualified Wire.API.User as User
import qualified Wire.API.User.Client as User.Client

models :: [Model]
models =
  [ Connection.modelConnectionList,
    Connection.modelConnection,
    Connection.modelConnectionRequest,
    Connection.modelConnectionUpdate,
    Conversation.Code.modelConversationCode,
    Conversation.Member.modelConversationMembers,
    Conversation.Member.modelOtherMember,
    Conversation.Member.modelMember,
    Conversation.Member.modelMemberUpdate,
    Conversation.Member.modelOtherMemberUpdate,
    Conversation.Role.modelConversationRole,
    Conversation.Role.modelConversationRolesList,
    Conversation.Typing.modelTyping,
    CustomBackend.modelCustomBackend,
    Message.modelNewOtrMessage,
    Message.modelOtrRecipients,
    Message.modelClientMismatch,
    Notification.modelEvent,
    Notification.modelNotification,
    Notification.modelNotificationList,
    Push.Token.modelPushToken,
    Push.Token.modelPushTokenList,
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
    Team.modelUpdateData,
    Team.modelTeamDelete,
    User.modelUserIdList,
    User.Client.modelOtrClientMap,
    User.Client.modelUserClients,
    Conversation.Event.modelEvent,
    Conversation.Event.modelMemberEvent,
    Conversation.Event.modelConnectEvent,
    Conversation.Event.modelConversationReceiptModeUpdateEvent,
    Conversation.Event.modelConversationNameUpdateEvent,
    Conversation.Event.modelConversationAccessUpdateEvent,
    Conversation.Event.modelConversationMessageTimerUpdateEvent,
    Conversation.Event.modelConversationCodeUpdateEvent,
    Conversation.Event.modelConversationCodeDeleteEvent,
    Conversation.Event.modelMemberUpdateEvent,
    Conversation.Event.modelTypingEvent,
    Conversation.Event.modelOtrMessageEvent,
    Conversation.Event.modelMembers,
    Conversation.Event.modelConnect,
    Conversation.Event.modelMemberUpdateData,
    Conversation.Event.modelOtrMessage,
    Team.Event.modelEvent,
    Team.Event.modelMemberEvent,
    Team.Event.modelMemberData,
    Team.Event.modelConvEvent,
    Team.Event.modelConversationData,
    Team.Event.modelUpdateEvent
  ]
