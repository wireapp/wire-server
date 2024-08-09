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

module Wire.API.Routes.Public.Galley.Bot where

import Servant
import Servant.OpenApi.Internal.Orphans ()
import Wire.API.Error
import Wire.API.Error.Galley
import Wire.API.MakesFederatedCall
import Wire.API.Message
import Wire.API.Provider.Bot
import Wire.API.Routes.MultiVerb
import Wire.API.Routes.Named
import Wire.API.Routes.Public
import Wire.API.Routes.Public.Galley.Messaging

type BotAPI =
  Named
    "post-bot-message-unqualified"
    ( MakesFederatedCall 'Galley "on-message-sent"
        :> MakesFederatedCall 'Brig "get-user-clients"
        :> ZBot
        :> ZConversation
        :> CanThrow 'ConvNotFound
        :> "bot"
        :> "messages"
        :> QueryParam "ignore_missing" IgnoreMissing
        :> QueryParam "report_missing" ReportMissing
        :> ReqBody '[JSON] NewOtrMessage
        :> MultiVerb
             'POST
             '[Servant.JSON]
             (PostOtrResponses ClientMismatch)
             (PostOtrResponse ClientMismatch)
    )
    :<|> Named
           "get-bot-conversation"
           ( CanThrow 'AccessDenied
               :> CanThrow 'ConvNotFound
               :> CanThrow OperationDenied
               :> CanThrow 'NotATeamMember
               :> CanThrow 'TeamNotFound
               :> "bot"
               :> "conversation"
               :> ZBot
               :> ZConversation
               :> Get '[JSON] BotConvView
           )
