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

import Data.Aeson qualified as A
import Data.Id
import Data.OpenApi qualified as O
import Data.Schema as S
import Imports
import Servant
import Servant.API.Extended
import Wire.API.Routes.API
import Wire.API.Routes.Named
import Wire.API.Routes.Public (ZConn, ZUser)
import Wire.API.Routes.WebSocket

type CannonAPI =
  Named
    "await-notifications"
    ( Summary "Establish websocket connection"
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
           "config-options-cannon"
           ( Summary "Establish websocket connection"
               :> "config-options-cannon"
               :> Get '[JSON, YAML] JsonObject
           )

-- | Arbitrary aeson object value with helpful {to,from}json instances and schema.
newtype JsonObject = JsonObject {unJsonObject :: A.Object}
  deriving newtype (Eq, Ord, Show)
  deriving (O.ToSchema) via (Schema JsonObject)

instance A.FromJSON JsonObject where
  parseJSON = A.withObject "Object" (pure . JsonObject)

instance A.ToJSON JsonObject where
  toJSON (JsonObject obj) = A.Object obj

instance S.ToSchema JsonObject where
  schema = named "Object" $ unJsonObject .= (JsonObject <$> S.jsonObject)

data CannonAPITag

instance ServiceAPI CannonAPITag v where
  type ServiceAPIRoutes CannonAPITag = CannonAPI
