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

module Wire.API.Routes.Public.Galley.TeamConversation where

import Data.Id
import Servant
import Servant.OpenApi.Internal.Orphans ()
import Wire.API.Conversation.Role
import Wire.API.Error
import Wire.API.Error.Galley
import Wire.API.MakesFederatedCall
import Wire.API.Routes.MultiVerb
import Wire.API.Routes.Named
import Wire.API.Routes.Public
import Wire.API.Team.Conversation

type TeamConversationAPI =
  Named
    "get-team-conversation-roles"
    ( Summary "Get existing roles available for the given team"
        :> CanThrow 'NotATeamMember
        :> ZUser
        :> "teams"
        :> Capture "tid" TeamId
        :> "conversations"
        :> "roles"
        :> Get '[Servant.JSON] ConversationRolesList
    )
    :<|> Named
           "get-team-conversations"
           ( Summary "Get team conversations"
               :> CanThrow OperationDenied
               :> CanThrow 'NotATeamMember
               :> ZUser
               :> "teams"
               :> Capture "tid" TeamId
               :> "conversations"
               :> Get '[Servant.JSON] TeamConversationList
           )
    :<|> Named
           "get-team-conversation"
           ( Summary "Get one team conversation"
               :> CanThrow 'ConvNotFound
               :> CanThrow OperationDenied
               :> CanThrow 'NotATeamMember
               :> ZUser
               :> "teams"
               :> Capture "tid" TeamId
               :> "conversations"
               :> Capture "cid" ConvId
               :> Get '[Servant.JSON] TeamConversation
           )
    :<|> Named
           "delete-team-conversation"
           ( Summary "Remove a team conversation"
               :> MakesFederatedCall 'Galley "on-conversation-updated"
               :> MakesFederatedCall 'Galley "on-mls-message-sent"
               :> MakesFederatedCall 'Brig "get-users-by-ids"
               :> CanThrow ('ActionDenied 'DeleteConversation)
               :> CanThrow 'ConvNotFound
               :> CanThrow 'InvalidOperation
               :> CanThrow 'NotATeamMember
               :> ZLocalUser
               :> ZConn
               :> "teams"
               :> Capture "tid" TeamId
               :> "conversations"
               :> Capture "cid" ConvId
               :> MultiVerb 'DELETE '[JSON] '[RespondEmpty 200 "Conversation deleted"] ()
           )
