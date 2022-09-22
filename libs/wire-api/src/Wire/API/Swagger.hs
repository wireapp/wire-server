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

module Wire.API.Swagger where

import Data.Swagger.Build.Api (Model)
import qualified Wire.API.Call.Config as Call.Config
import qualified Wire.API.Connection as Connection
import qualified Wire.API.Conversation as Conversation
import qualified Wire.API.Conversation.Code as Conversation.Code
import qualified Wire.API.Conversation.Member as Conversation.Member
import qualified Wire.API.Conversation.Role as Conversation.Role
import qualified Wire.API.Conversation.Typing as Conversation.Typing
import qualified Wire.API.Event.Team as Event.Team
import qualified Wire.API.Message as Message
import qualified Wire.API.Notification as Notification
import qualified Wire.API.Properties as Properties
import qualified Wire.API.Provider.Service as Provider.Service
import qualified Wire.API.Push.Token as Push.Token
import qualified Wire.API.Team as Team
import qualified Wire.API.Team.Conversation as Team.Conversation
import qualified Wire.API.Team.Invitation as Team.Invitation
import qualified Wire.API.Team.Permission as Team.Permission
import qualified Wire.API.User as User
import qualified Wire.API.User.Auth as User.Auth
import qualified Wire.API.User.Client as User.Client
import qualified Wire.API.User.Client.Prekey as User.Client.Prekey
import qualified Wire.API.User.Handle as User.Handle
import qualified Wire.API.User.Profile as User.Profile
import qualified Wire.API.User.RichInfo as User.RichInfo
import qualified Wire.API.User.Search as User.Search

models :: [Model]
models =
  [ Call.Config.modelRtcConfiguration,
    Call.Config.modelRtcIceServer,
    Connection.modelConnectionList,
    Connection.modelConnection,
    Connection.modelConnectionUpdate,
    Conversation.modelConversation,
    Conversation.modelConversations,
    Conversation.modelConversationIds,
    Conversation.modelInvite,
    Conversation.modelNewConversation,
    Conversation.modelTeamInfo,
    Conversation.modelConversationUpdateName,
    Conversation.modelConversationAccessData,
    Conversation.modelConversationReceiptModeUpdate,
    Conversation.modelConversationMessageTimerUpdate,
    Conversation.Code.modelConversationCode,
    Conversation.Member.modelConversationMembers,
    Conversation.Member.modelOtherMember,
    Conversation.Member.modelMember,
    Conversation.Member.modelMemberUpdate,
    Conversation.Member.modelOtherMemberUpdate,
    Conversation.Role.modelConversationRole,
    Conversation.Role.modelConversationRolesList,
    Conversation.Typing.modelTyping,
    Event.Team.modelEvent,
    Event.Team.modelMemberEvent,
    Event.Team.modelMemberData,
    Event.Team.modelConvEvent,
    Event.Team.modelConversationData,
    Event.Team.modelUpdateEvent,
    Message.modelNewOtrMessage,
    Message.modelOtrRecipients,
    Message.modelClientMismatch,
    Notification.modelEvent,
    Notification.modelNotification,
    Notification.modelNotificationList,
    Properties.modelPropertyValue,
    Properties.modelPropertyDictionary,
    Provider.Service.modelServiceRef,
    Push.Token.modelPushToken,
    Push.Token.modelPushTokenList,
    Team.modelTeam,
    Team.modelTeamList,
    Team.modelTeamDelete,
    Team.Conversation.modelTeamConversation,
    Team.Conversation.modelTeamConversationList,
    Team.Invitation.modelTeamInvitation,
    Team.Invitation.modelTeamInvitationList,
    Team.Invitation.modelTeamInvitationRequest,
    Team.Permission.modelPermissions,
    User.modelUserIdList,
    User.modelUser,
    User.modelEmailUpdate,
    User.modelDelete,
    User.Auth.modelSendLoginCode,
    User.Auth.modelLoginCodeResponse,
    User.Auth.modelLogin,
    User.Auth.modelRemoveCookies,
    User.Auth.modelCookie,
    User.Auth.modelCookieList,
    User.Auth.modelAccessToken,
    User.Client.modelOtrClientMap,
    User.Client.modelUserClients,
    User.Client.modelNewClient,
    User.Client.modelUpdateClient,
    User.Client.modelDeleteClient,
    User.Client.modelSigkeys,
    User.Client.modelLocation, -- re-export from types-common
    User.Client.Prekey.modelPrekey,
    User.Handle.modelUserHandleInfo,
    User.Handle.modelCheckHandles,
    User.Profile.modelAsset,
    User.RichInfo.modelRichInfo,
    User.RichInfo.modelRichField,
    User.Search.modelSearchResult User.Search.modelSearchContact,
    User.Search.modelSearchResult User.Search.modelTeamContact,
    User.Search.modelSearchContact,
    User.Search.modelTeamContact
  ]
