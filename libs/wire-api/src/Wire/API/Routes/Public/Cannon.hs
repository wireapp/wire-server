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

module Wire.API.Routes.Public.Cannon where

import Data.Id
import Servant
import Wire.API.Routes.API
import Wire.API.Routes.Named
import Wire.API.Routes.Public (ZConn, ZUser)
import Wire.API.Routes.WebSocket

type CannonAPI =
  Named
    "await-notifications"
    ( Summary "Establish websocket connection"
        -- Description "This is the legacy variant of \"consume-events\""
        :> "await"
        :> ZUser
        :> ZConn
        :> QueryParam'
             [ Optional,
               Strict,
               Description "Client ID"
             ]
             "client"
             ClientId
        -- FUTUREWORK: Consider higher-level web socket combinator
        :> WebSocketPending
    )
    :<|> Named
           "consume-events"
           ( Summary "Consume events over a websocket connection"
               :> Description "This is the rabbitMQ-based variant of \"await-notifications\""
               -- :> From 'V8 -- cannon is not versioned yet
               :> "events"
               :> ZUser
               :> QueryParam'
                    [ -- Make this optional in https://wearezeta.atlassian.net/browse/WPB-11173
                      Required,
                      Strict,
                      Description "Client ID"
                    ]
                    "client"
                    ClientId
               -- FUTUREWORK: Consider higher-level web socket combinator
               :> WebSocketPending
           )

data CannonAPITag

instance ServiceAPI CannonAPITag v where
  type ServiceAPIRoutes CannonAPITag = CannonAPI
