{-# LANGUAGE DerivingVia #-}
{-# OPTIONS_GHC -Wno-orphans #-}

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

module Wire.API.Routes.Public.Galley where

import Servant
import Servant.OpenApi.Internal.Orphans ()
import Wire.API.Routes.API
import Wire.API.Routes.Public.Galley.Bot
import Wire.API.Routes.Public.Galley.Conversation
import Wire.API.Routes.Public.Galley.CustomBackend
import Wire.API.Routes.Public.Galley.Feature
import Wire.API.Routes.Public.Galley.LegalHold
import Wire.API.Routes.Public.Galley.MLS
import Wire.API.Routes.Public.Galley.Messaging
import Wire.API.Routes.Public.Galley.Team
import Wire.API.Routes.Public.Galley.TeamConversation
import Wire.API.Routes.Public.Galley.TeamMember
import Wire.API.Routes.Public.Galley.TeamNotification (TeamNotificationAPI)

type GalleyAPI =
  ConversationAPI
    :<|> TeamConversationAPI
    :<|> MessagingAPI
    :<|> BotAPI
    :<|> TeamAPI
    :<|> FeatureAPI
    :<|> MLSAPI
    :<|> CustomBackendAPI
    :<|> LegalHoldAPI
    :<|> TeamMemberAPI
    :<|> TeamNotificationAPI

data GalleyAPITag

instance ServiceAPI GalleyAPITag v where
  type ServiceAPIRoutes GalleyAPITag = GalleyAPI
