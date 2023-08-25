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

module Wire.API.Routes.Public.Brig.Bot where

import Data.Id as Id
import Imports
import Servant (JSON)
import Servant hiding (Handler, JSON, Tagged, addHeader, respond)
import Servant.Swagger.Internal.Orphans ()
import Wire.API.Conversation.Bot
import Wire.API.Error (CanThrow)
import Wire.API.Error.Brig (BrigError (..))
import Wire.API.Routes.API (ServiceAPI (..))
import Wire.API.Routes.MultiVerb
import Wire.API.Routes.Named (Named (..))
import Wire.API.Routes.Public
import Wire.API.User
import Wire.API.User.Client.Prekey (PrekeyId)

type DeleteResponses =
  '[ RespondEmpty 204 "",
     Respond 200 "User found" RemoveBotResponse
   ]

type BotAPI =
  Named
    "add-bot"
    ( Summary "Add bot"
        :> CanThrow 'AccessDenied
        :> CanThrow 'InvalidConversation
        :> CanThrow 'TooManyConversationMembers
        :> CanThrow 'ServiceDisabled
        :> ZAccess
        :> ZConn
        :> "conversations"
        :> Capture "Conversation ID" ConvId
        :> "bots"
        :> ReqBody '[JSON] AddBot
        :> MultiVerb1 'POST '[JSON] (Respond 201 "" AddBotResponse)
    )
    :<|> Named
           "remove-bot"
           ( Summary "Remove bot"
               :> CanThrow 'AccessDenied
               :> CanThrow 'InvalidConversation
               :> ZAccess
               :> ZConn
               :> "conversations"
               :> Capture "Conversation ID" ConvId
               :> "bots"
               :> Capture "Bot ID" BotId
               :> MultiVerb 'DELETE '[JSON] DeleteResponses (Maybe RemoveBotResponse)
           )
    :<|> Named
           "bot-get-self"
           ( Summary "Get self"
               :> CanThrow 'UserNotFound
               :> CanThrow 'AccessDenied
               :> ZBot
               :> "bot"
               :> "self"
               :> Get '[JSON] UserProfile
           )
    :<|> Named
           "bot-delete-self"
           ( Summary "Delete self"
               :> CanThrow 'AccessDenied
               :> CanThrow 'InvalidBot
               :> ZBot
               :> ZConversation
               :> "bot"
               :> "self"
               :> MultiVerb1 'DELETE '[JSON] (RespondEmpty 200 "")
           )
    :<|> Named
           "bot-list-prekeys"
           ( Summary "List prekeys for bot"
               :> ZBot
               :> "bot"
               :> "client"
               :> "prekeys"
               :> Get '[JSON] [PrekeyId]
           )
    :<|> Named
           "bot-update-prekeys"
           ( Summary "Update prekeys for bot"
               :> CanThrow 'AccessDenied
               :> CanThrow 'ClientNotFound
               :> ZBot
               :> "bot"
               :> "client"
               :> "prekeys"
               :> ReqBody '[JSON] UpdateBotPrekeys
               :> MultiVerb1 'POST '[JSON] (RespondEmpty 200 "")
           )

data BotAPITag

instance ServiceAPI BotAPITag v where
  type ServiceAPIRoutes BotAPITag = BotAPI
