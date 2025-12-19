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
import Galley.API.Public.Meetings
import Galley.API.Public.Messaging
import Galley.API.Public.Team
import Galley.API.Public.TeamConversation
import Galley.API.Public.TeamMember
import Galley.API.Public.TeamNotification
import Galley.App
import Wire.API.Routes.API
import Wire.API.Routes.Public.Galley

servantSitemap :: API GalleyAPI GalleyEffects
servantSitemap =
  conversationAPI
    <@> teamConversationAPI
    <@> messagingAPI
    <@> botAPI
    <@> teamAPI
    <@> featureAPI
    <@> mlsAPI
    <@> meetingsAPI
    <@> customBackendAPI
    <@> legalHoldAPI
    <@> teamMemberAPI
    <@> teamNotificationAPI
