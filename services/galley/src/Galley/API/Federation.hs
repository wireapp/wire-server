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

import Data.Qualified (Qualified (Qualified))
import qualified Galley.API.Mapping as Mapping
import Galley.API.Util (viewFederationDomain)
import Galley.App (Galley)
import qualified Galley.Data as Data
import Imports
import Servant (ServerT)
import Servant.API.Generic (ToServantApi)
import Servant.Server.Generic (genericServerT)
import Wire.API.Federation.API.Galley (ConversationMemberUpdate (..), GetConversationsRequest (..), GetConversationsResponse (..))
import qualified Wire.API.Federation.API.Galley as FederationAPIGalley

federationSitemap :: ServerT (ToServantApi FederationAPIGalley.Api) Galley
federationSitemap =
  genericServerT $
    FederationAPIGalley.Api
      getConversations
      updateConversationMembership

getConversations :: GetConversationsRequest -> Galley GetConversationsResponse
getConversations (GetConversationsRequest (Qualified uid domain) gcrConvIds) = do
  localDomain <- viewFederationDomain
  convs <- Data.conversations gcrConvIds
  if domain == localDomain
    then GetConversationsResponse . catMaybes <$> for convs (Mapping.conversationViewMaybe uid)
    else error "FUTUREWORK: implement & exstend integration test when schema ready"

updateConversationMembership :: ConversationMemberUpdate -> Galley ()
updateConversationMembership = error "FUTUREWORK: implement after schema change"
