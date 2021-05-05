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
module Galley.API.Federation where

import qualified Galley.API.Mapping as Mapping
import Galley.App (Galley)
import qualified Galley.Data as Data
import Imports
import Servant (ServerT)
import Servant.API.Generic (ToServantApi)
import Servant.Server.Generic (genericServerT)
import Wire.API.Conversation (Conversation)
import Wire.API.Federation.API.Galley (GetConversationsRequest (..))
import qualified Wire.API.Federation.API.Galley as FederationAPIGalley
import Wire.API.Federation.Event (ConversationEvent (..), MembersJoin (..))

federationSitemap :: ServerT (ToServantApi FederationAPIGalley.Api) Galley
federationSitemap =
  genericServerT $
    FederationAPIGalley.Api
      getConversations
      conversationMemberChange

getConversations :: GetConversationsRequest -> Galley [Conversation]
getConversations GetConversationsRequest {gcrUserId, gcrConvIds} = do
  convs <- Data.conversations gcrConvIds
  for convs (Mapping.conversationView gcrUserId)

conversationMemberChange :: ConversationEvent MembersJoin -> Galley ()
conversationMemberChange = undefined
