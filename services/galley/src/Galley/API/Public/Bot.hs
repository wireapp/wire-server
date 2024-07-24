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
import Data.Qualified
import Galley.API.Query qualified as Query
import Galley.API.Teams.Features qualified as Features
import Galley.API.Update
import Galley.App
import Galley.Effects
import Galley.Effects qualified as E
import Galley.Options
import Polysemy
import Polysemy.Input
import Wire.API.Error
import Wire.API.Error.Galley
import Wire.API.Event.Team qualified as Public ()
import Wire.API.Federation.API
import Wire.API.Provider.Bot
import Wire.API.Routes.API
import Wire.API.Routes.Public.Galley.Bot

botAPI :: API BotAPI GalleyEffects
botAPI =
  mkNamedAPI @"post-bot-message-unqualified" (callsFed (exposeAnnotations postBotMessageUnqualified))
    <@> mkNamedAPI @"get-bot-conversation" getBotConversation

getBotConversation ::
  forall r.
  ( Member E.ConversationStore r,
    Member (Input (Local ())) r,
    Member (Input Opts) r,
    Member TeamFeatureStore r,
    Member (ErrorS 'AccessDenied) r,
    Member (ErrorS 'ConvNotFound) r,
    Member TeamStore r
  ) =>
  BotId ->
  ConvId ->
  Sem r BotConvView
getBotConversation bid cnv = do
  Features.guardSecondFactorDisabled (botUserId bid) cnv
  Query.getBotConversation bid cnv
