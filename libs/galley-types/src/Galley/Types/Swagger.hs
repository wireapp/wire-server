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

module Galley.Types.Swagger where

import Data.Swagger.Build.Api as Swagger
import qualified Wire.API.Conversation as Conversation
import qualified Wire.API.Conversation.Code as Conversation.Code
import qualified Wire.API.Conversation.Member as Conversation.Member
import qualified Wire.API.Conversation.Role as Conversation.Role
import qualified Wire.API.Conversation.Typing as Conversation.Typing
import qualified Wire.API.CustomBackend as CustomBackend
import qualified Wire.API.Event.Conversation as Event.Conversation
import qualified Wire.API.Message as Message
import qualified Wire.API.Provider.Service as Provider.Service
import qualified Wire.API.Swagger
import qualified Wire.API.Team.Feature as Team.Feature
import qualified Wire.API.Team.SearchVisibility as Team.SearchVisibility
import qualified Wire.API.User as User
import qualified Wire.API.User.Client as User.Client
import qualified Wire.Swagger as Swagger

-- TODO remove all of this

-- | Actually all models of the whole API,
-- but it doesn't hurt and makes it less likely to forget one.
galleyModels :: [Model]
galleyModels = Wire.API.Swagger.models

event :: Model
event = Event.Conversation.modelEvent

eventType :: DataType
eventType = Event.Conversation.typeEventType

otrMessageEvent :: Model
otrMessageEvent = Event.Conversation.modelOtrMessageEvent

memberEvent :: Model
memberEvent = Event.Conversation.modelMemberEvent

connectEvent :: Model
connectEvent = Event.Conversation.modelConnectEvent

conversationNameUpdateEvent :: Model
conversationNameUpdateEvent = Event.Conversation.modelConversationNameUpdateEvent

conversationRole :: Model
conversationRole = Conversation.Role.modelConversationRole

conversationRoleAction :: DataType
conversationRoleAction = Conversation.Role.typeConversationRoleAction

conversationRolesList :: Model
conversationRolesList = Conversation.Role.modelConversationRolesList

conversationAccessUpdateEvent :: Model
conversationAccessUpdateEvent = Event.Conversation.modelConversationAccessUpdateEvent

conversationReceiptModeUpdateEvent :: Model
conversationReceiptModeUpdateEvent = Event.Conversation.modelConversationReceiptModeUpdateEvent

conversationMessageTimerUpdateEvent :: Model
conversationMessageTimerUpdateEvent = Event.Conversation.modelConversationMessageTimerUpdateEvent

conversationCodeUpdateEvent :: Model
conversationCodeUpdateEvent = Event.Conversation.modelConversationCodeUpdateEvent

conversationCodeDeleteEvent :: Model
conversationCodeDeleteEvent = Event.Conversation.modelConversationCodeDeleteEvent

memberUpdateEvent :: Model
memberUpdateEvent = Event.Conversation.modelMemberUpdateEvent

typingEvent :: Model
typingEvent = Event.Conversation.modelTypingEvent

conversation :: Model
conversation = Conversation.modelConversation

conversationType :: DataType
conversationType = Conversation.typeConversationType

otrMessage :: Model
otrMessage = Event.Conversation.modelOtrMessage

priority :: DataType
priority = Message.typePriority

newOtrMessage :: Model
newOtrMessage = Message.modelNewOtrMessage

otrRecipients :: Model
otrRecipients = Message.modelOtrRecipients

otrClientMap :: Model
otrClientMap = User.Client.modelOtrClientMap

clientMismatch :: Model
clientMismatch = Message.modelClientMismatch

userClients :: Model
userClients = User.Client.modelUserClients

userIdList :: Model
userIdList = User.modelUserIdList

members :: Model
members = Event.Conversation.modelMembers

conversationUpdateName :: Model
conversationUpdateName = Conversation.modelConversationUpdateName

conversationAccessUpdate :: Model
conversationAccessUpdate = Conversation.modelConversationAccessUpdate

access :: DataType
access = Conversation.typeAccess

conversationReceiptModeUpdate :: Model
conversationReceiptModeUpdate = Conversation.modelConversationReceiptModeUpdate

conversationMessageTimerUpdate :: Model
conversationMessageTimerUpdate = Conversation.modelConversationMessageTimerUpdate

conversationCode :: Model
conversationCode = Conversation.Code.modelConversationCode

conversationMembers :: Model
conversationMembers = Conversation.Member.modelConversationMembers

member :: Model
member = Conversation.Member.modelMember

otherMember :: Model
otherMember = Conversation.Member.modelOtherMember

newConversation :: Model
newConversation = Conversation.modelNewConversation

teamInfo :: Model
teamInfo = Conversation.modelTeamInfo

conversationIds :: Model
conversationIds = Conversation.modelConversationIds

conversations :: Model
conversations = Conversation.modelConversations

invite :: Model
invite = Conversation.modelInvite

memberUpdate :: Model
memberUpdate = Conversation.Member.modelMemberUpdate

otherMemberUpdate :: Model
otherMemberUpdate = Conversation.Member.modelOtherMemberUpdate

memberUpdateData :: Model
memberUpdateData = Event.Conversation.modelMemberUpdateData

typing :: Model
typing = Conversation.Typing.modelTyping

typingStatus :: DataType
typingStatus = Conversation.Typing.typeTypingStatus

connect :: Model
connect = Event.Conversation.modelConnect

serviceRef :: Model
serviceRef = Provider.Service.modelServiceRef

errorObj :: Model
errorObj = Swagger.errorModel

legalHoldTeamConfig :: Model
legalHoldTeamConfig = Team.Feature.modelLegalHoldTeamConfig

ssoTeamConfig :: Model
ssoTeamConfig = Team.Feature.modelSsoTeamConfig

teamSearchVisibilityAvailable :: Model
teamSearchVisibilityAvailable = Team.SearchVisibility.modelTeamSearchVisibilityAvailable

featureStatus :: DataType
featureStatus = Team.Feature.typeFeatureStatus

searchVisibilityType :: DataType
searchVisibilityType = Team.SearchVisibility.typeSearchVisibility

teamSearchVisibility :: Model
teamSearchVisibility = Team.SearchVisibility.modelTeamSearchVisibility

customBackend :: Model
customBackend = CustomBackend.modelCustomBackend
