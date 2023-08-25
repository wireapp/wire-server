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
import Servant (JSON)
import Servant hiding (Handler, JSON, Tagged, addHeader, respond)
import Servant.Swagger.Internal.Orphans ()
import Wire.API.Conversation.Bot
import Wire.API.Routes.API (ServiceAPI (..))
import Wire.API.Routes.Named (Named (..))
import Wire.API.Routes.Public

-- post "/conversations/:cnv/bots" (continue addBotH) $
--   accept "application" "json"
--     .&> zauth ZAuthAccess
--     .&> zauthUserId
--       .&. zauthConnId
--       .&. capture "cnv"
--       .&. jsonRequest @Public.AddBot

type BotAPI =
  Named
    "add-bot"
    ( Summary "Add bot"
        :> ZAccess
        :> ZConn
        :> "conversations"
        :> Capture "Conversation ID" ConvId
        :> "bots"
        :> ReqBody '[JSON] AddBot
        :> Post '[JSON] AddBotResponse
    )

data BotAPITag

instance ServiceAPI BotAPITag v where
  type ServiceAPIRoutes BotAPITag = BotAPI
