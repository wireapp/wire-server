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

module Galley.API.Public.Servant (mkNamedAPI, servantSitemap) where

import Galley.API.Public.Bot
import Galley.API.Public.Conversation
import Galley.API.Public.CustomBackend
import Galley.API.Public.Feature
import Galley.API.Public.LegalHold
import Galley.API.Public.MLS
import Galley.API.Public.Messaging
import Galley.API.Public.Team
import Galley.API.Public.TeamConversation
import Galley.API.Public.TeamMember
import Galley.App
import Wire.API.Routes.API
import Wire.API.Routes.Public.Galley
import Wire.API.Federation.API

servantSitemap :: (CallsFed 'Galley "get-conversations",
  CallsFed 'Galley "leave-conversation", CallsFed 'Galley "on-conversation-created", CallsFed 'Galley "on-conversation-updated", CallsFed 'Brig "get-user-clients", CallsFed 'Galley "on-mls-message-sent", CallsFed 'Galley "on-message-sent", CallsFed 'Galley "on-new-remote-conversation", CallsFed 'Galley "on-typing-indicator-updated", CallsFed 'Galley "send-message", CallsFed 'Brig "get-mls-clients", CallsFed 'Galley "query-group-info", CallsFed 'Galley "mls-welcome", CallsFed 'Galley "update-conversation", CallsFed 'Galley "send-mls-commit-bundle", CallsFed 'Galley "send-mls-message") => API ServantAPI GalleyEffects
servantSitemap =
  conversationAPI
    <@> teamConversationAPI
    <@> messagingAPI
    <@> botAPI
    <@> teamAPI
    <@> featureAPI
    <@> mlsAPI
    <@> customBackendAPI
    <@> legalHoldAPI
    <@> teamMemberAPI
