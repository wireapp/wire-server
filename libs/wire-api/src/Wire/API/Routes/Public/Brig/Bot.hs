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

import Data.CommaSeparatedList (CommaSeparatedList)
import Data.Id as Id
import Imports
import Servant (JSON)
import Servant hiding (Handler, JSON, Tagged, addHeader, respond)
import Servant.OpenApi.Internal.Orphans ()
import Wire.API.Conversation.Bot
import Wire.API.Error (CanThrow, ErrorResponse)
import Wire.API.Error.Brig (BrigError (..))
import Wire.API.Provider.Bot (BotUserView)
import Wire.API.Routes.MultiVerb
import Wire.API.Routes.Named (Named)
import Wire.API.Routes.Public
import Wire.API.Routes.Version
import Wire.API.Routes.Versioned
import Wire.API.User
import Wire.API.User.Client
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
        :> Capture "conv" ConvId
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
               :> Capture "conv" ConvId
               :> "bots"
               :> Capture "bot" BotId
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
               :> CanThrow 'AccessDenied
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
    :<|> Named
           "bot-get-client-v6"
           ( Summary "Get client for bot"
               :> Until 'V7
               :> CanThrow 'AccessDenied
               :> CanThrow 'ClientNotFound
               :> ZBot
               :> "bot"
               :> "client"
               :> MultiVerb
                    'GET
                    '[JSON]
                    '[ ErrorResponse 'ClientNotFound,
                       VersionedRespond 'V6 200 "Client found" Client
                     ]
                    (Maybe Client)
           )
    :<|> Named
           "bot-get-client"
           ( Summary "Get client for bot"
               :> From 'V7
               :> CanThrow 'AccessDenied
               :> CanThrow 'ClientNotFound
               :> ZBot
               :> "bot"
               :> "client"
               :> MultiVerb
                    'GET
                    '[JSON]
                    '[ ErrorResponse 'ClientNotFound,
                       Respond 200 "Client found" Client
                     ]
                    (Maybe Client)
           )
    :<|> Named
           "bot-claim-users-prekeys"
           ( Summary "Claim users prekeys"
               :> CanThrow 'AccessDenied
               :> CanThrow 'TooManyClients
               :> CanThrow 'MissingLegalholdConsentOldClients
               :> CanThrow 'MissingLegalholdConsent
               :> ZBot
               :> "bot"
               :> "users"
               :> "prekeys"
               :> ReqBody '[JSON] UserClients
               :> Post '[JSON] UserClientPrekeyMap
           )
    :<|> Named
           "bot-list-users"
           ( Summary "List users"
               :> CanThrow 'AccessDenied
               :> ZBot
               :> "bot"
               :> "users"
               :> QueryParam' [Required, Strict] "ids" (CommaSeparatedList UserId)
               :> Get '[JSON] [BotUserView]
           )
    :<|> Named
           "bot-get-user-clients"
           ( Summary "Get user clients"
               :> CanThrow 'AccessDenied
               :> ZBot
               :> "bot"
               :> "users"
               :> Capture "user" UserId
               :> "clients"
               :> Get '[JSON] [PubClient]
           )
