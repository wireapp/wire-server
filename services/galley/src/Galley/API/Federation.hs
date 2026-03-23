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

import Galley.API.Federation.Handlers
import Galley.App
import Polysemy
import Servant (ServerT)
import Servant.API
import Wire.API.Federation.API
import Wire.API.Federation.Endpoint
import Wire.API.Federation.Version
import Wire.API.Routes.Named

type FederationAPI = "federation" :> FedApi 'Galley

-- | Convert a polysemy handler to an 'API' value.
federationSitemap ::
  ServerT FederationAPI (Sem GalleyEffects)
federationSitemap =
  Named @"on-conversation-created" onConversationCreated
    :<|> Named @"get-conversations@v1" getConversationsV1
    :<|> Named @"get-conversations" getConversations
    :<|> Named @"leave-conversation" leaveConversation
    :<|> Named @"send-message" sendMessage
    :<|> Named @"update-conversation" updateConversation
    :<|> Named @"mls-welcome" mlsSendWelcome
    :<|> Named @"send-mls-message" sendMLSMessage
    :<|> Named @"send-mls-commit-bundle" sendMLSCommitBundle
    :<|> Named @"query-group-info" queryGroupInfo
    :<|> Named @"update-typing-indicator" updateTypingIndicator
    :<|> Named @"on-typing-indicator-updated" onTypingIndicatorUpdated
    :<|> Named @"get-sub-conversation" getSubConversationForRemoteUser
    :<|> Named @"delete-sub-conversation" deleteSubConversationForRemoteUser
    :<|> Named @"leave-sub-conversation" leaveSubConversation
    :<|> Named @"get-one2one-conversation@v1" getOne2OneConversationV1
    :<|> Named @"get-one2one-conversation" getOne2OneConversation
    :<|> Named @"on-client-removed" onClientRemoved
    :<|> Named @"on-message-sent" onMessageSent
    :<|> Named @"on-mls-message-sent" onMLSMessageSent
    :<|> Named @(Versioned 'V0 "on-conversation-updated") onConversationUpdatedV0
    :<|> Named @"on-conversation-updated" onConversationUpdated
    :<|> Named @"on-user-deleted-conversations" onUserDeleted
