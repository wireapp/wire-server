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

module Galley.API.Federation where

import Galley.App
import Polysemy
import Servant (ServerT)
import Servant.API
import Wire.API.Federation.API
import Wire.API.Federation.Endpoint
import Wire.API.Federation.Version
import Wire.API.Routes.Named
import Wire.ConversationSubsystem

type FederationAPI = "federation" :> FedApi 'Galley

-- | Convert a polysemy handler to an 'API' value.
federationSitemap ::
  ServerT FederationAPI (Sem GalleyEffects)
federationSitemap =
  Named @"on-conversation-created" federationOnConversationCreated
    :<|> Named @"get-conversations@v1" federationGetConversationsV1
    :<|> Named @"get-conversations" federationGetConversations
    :<|> Named @"leave-conversation" federationLeaveConversation
    :<|> Named @"send-message" federationSendMessage
    :<|> Named @"update-conversation" federationUpdateConversation
    :<|> Named @"mls-welcome" federationMlsSendWelcome
    :<|> Named @"send-mls-message" federationSendMLSMessage
    :<|> Named @"send-mls-commit-bundle" federationSendMLSCommitBundle
    :<|> Named @"query-group-info" federationQueryGroupInfo
    :<|> Named @"update-typing-indicator" federationUpdateTypingIndicator
    :<|> Named @"on-typing-indicator-updated" federationOnTypingIndicatorUpdated
    :<|> Named @"get-sub-conversation" federationGetSubConversationForRemoteUser
    :<|> Named @"delete-sub-conversation" federationDeleteSubConversationForRemoteUser
    :<|> Named @"leave-sub-conversation" federationLeaveSubConversation
    :<|> Named @"get-one2one-conversation@v1" federationGetOne2OneConversationV1
    :<|> Named @"get-one2one-conversation" federationGetOne2OneConversation
    :<|> Named @"on-client-removed" federationOnClientRemoved
    :<|> Named @"on-message-sent" federationOnMessageSent
    :<|> Named @"on-mls-message-sent" federationOnMLSMessageSent
    :<|> Named @(Versioned 'V0 "on-conversation-updated") federationOnConversationUpdatedV0
    :<|> Named @"on-conversation-updated" federationOnConversationUpdated
    :<|> Named @"on-user-deleted-conversations" federationOnUserDeleted
