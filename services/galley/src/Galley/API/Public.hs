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

module Galley.API.Public
  ( sitemap,
    continueE,
  )
where

import Data.Id
import Data.Qualified
import Galley.API.Query qualified as Query
import Galley.API.Teams.Features qualified as Features
import Galley.App
import Galley.Effects
import Galley.Effects qualified as E
import Galley.Options
import Imports hiding (head)
import Network.Wai
import Network.Wai.Utilities.ZAuth hiding (ZAuthUser)
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.Internal
import Wire.API.Conversation.Role
import Wire.API.Error
import Wire.API.Error.Galley
import Wire.API.Event.Team qualified as Public ()
import Wire.API.Provider.Bot qualified as Bot
import Wire.API.Routes.API

-- | These are all the errors that can be thrown by wai-routing handlers.
-- We don't do any static checks on these errors, so we simply remap them to
-- dynamic errors. See 'continueE'.
type ErrorEffects =
  '[ ErrorS ('ActionDenied 'AddConversationMember),
     ErrorS ('ActionDenied 'LeaveConversation),
     ErrorS ('ActionDenied 'RemoveConversationMember),
     ErrorS 'ConvNotFound,
     ErrorS 'InvalidOperation,
     ErrorS 'NotConnected,
     ErrorS 'TeamNotFound,
     ErrorS 'InvalidTeamStatusUpdate,
     ErrorS 'TooManyTeamMembers,
     ErrorS 'TooManyMembers,
     ErrorS 'TeamMemberNotFound,
     ErrorS 'AccessDenied,
     ErrorS 'NotATeamMember,
     ErrorS 'NonBindingTeam,
     ErrorS OperationDenied,
     ErrorS 'InvalidPermissions,
     ErrorS 'NoAddToBinding,
     ErrorS 'UserBindingExists,
     ErrorS 'CustomBackendNotFound,
     ErrorS 'DeleteQueueFull,
     ErrorS 'NoBindingTeam,
     ErrorS 'NotAOneMemberTeam,
     ErrorS 'TeamSearchVisibilityNotEnabled,
     ErrorS 'TooManyTeamMembersOnTeamWithLegalhold,
     Error AuthenticationError
   ]

{-
-- Wrapper of 'continue' that remaps all static errors to dynamic ones.
continueE ::
  forall a r.
  Member (Error DynError) r =>
  (a -> Sem (Append ErrorEffects r) Response) ->
  a ->
  Continue (Sem r) ->
  Sem r ResponseReceived
continueE h = continue (interpretServerEffects @ErrorEffects . h)
-}

sitemap :: Routes () (Sem GalleyEffects) ()
sitemap = do
  -- Bot API ------------------------------------------------------------

  get "/bot/conversation" (continueE getBotConversationH) $
    zauth ZAuthBot
      .&> zauthBotId
        .&. zauthConvId
        .&. accept "application" "json"

type GetBotConversationH =
  Named
    "get-bot-conversation"
    ( "bot"
        :> "conversation"
        :> ZBot
        :> ZConversation
    )

getBotConversationH ::
  forall r.
  ( Member E.ConversationStore r,
    Member (Input (Local ())) r,
    Member (Input Opts) r,
    Member TeamFeatureStore r,
    Member (ErrorS 'AccessDenied) r,
    Member (ErrorS 'ConvNotFound) r,
    Member (ErrorS OperationDenied) r,
    Member (ErrorS 'NotATeamMember) r,
    Member (ErrorS 'TeamNotFound) r,
    Member TeamStore r
  ) =>
  BotId ->
  ConvId ->
  Sem r Bot.BotConvView
getBotConversationH bid cid =
  Features.guardSecondFactorDisabled (botUserId bid) cid (Query.getBotConversationH bid cid)
