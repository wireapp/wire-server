-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2021 Wire Swiss GmbH <opensource@wire.com>
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
import Data.Swagger
import Servant
import Servant.API
import Servant.API.WebSocket
import Servant.Swagger
import Wire.API.Routes.Public (ZConn, ZUser)

type ServantAPI =
  Summary "Establish websocket connection"
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
    -- TODO(sven): Consider higher-level web socket combinator
    :> WebSocketPending

swaggerDoc :: Swagger
swaggerDoc = toSwagger (Proxy @ServantAPI)
