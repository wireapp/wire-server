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

module Galley.API.Public.Bot where

import Data.Id
import Galley.App
import Polysemy
import Wire.API.Event.Team qualified as Public ()
import Wire.API.Provider.Bot
import Wire.API.Routes.API
import Wire.API.Routes.Public.Galley.Bot
import Wire.ConversationSubsystem
import Wire.FeaturesConfigSubsystem

botAPI :: API BotAPI GalleyEffects
botAPI =
  mkNamedAPI @"post-bot-message-unqualified" postBotMessageUnqualified
    <@> mkNamedAPI @"get-bot-conversation" getBotConversationH

getBotConversationH ::
  forall r.
  ( Member ConversationSubsystem r,
    Member FeaturesConfigSubsystem r
  ) =>
  BotId ->
  ConvId ->
  Sem r BotConvView
getBotConversationH bid cnv = do
  guardSecondFactorDisabled (botUserId bid) cnv
  getBotConversation bid cnv
