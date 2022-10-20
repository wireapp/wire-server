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

module Wire.API.Routes.Public.Gundeck where

import Data.Id (ClientId)
import Data.SOP
import qualified Data.Swagger as Swagger
import Imports
import Servant
import Servant.Swagger
import Wire.API.Error
import Wire.API.Error.Gundeck as E
import Wire.API.Notification
import Wire.API.Push.V2.Token
import Wire.API.Routes.MultiVerb
import Wire.API.Routes.Named
import Wire.API.Routes.Public

type GundeckAPI = PushAPI :<|> NotificationAPI

type PushAPI =
  Named
    "register-push-token"
    ( Summary "Register a native push token"
        :> ZUser
        :> ZConn
        :> "push"
        :> "tokens"
        :> ReqBody '[JSON] PushToken
        :> MultiVerb 'POST '[JSON] AddTokenResponses (Either AddTokenError AddTokenSuccess)
    )
    :<|> Named
           "delete-push-token"
           ( Summary "Unregister a native push token"
               :> ZUser
               :> "push"
               :> "tokens"
               :> Capture' '[Description "The push token to delete"] "pid" Token
               :> MultiVerb 'DELETE '[JSON] DeleteTokenResponses (Maybe ())
           )

type NotificationAPI =
  Named
    "get-notification-by-id"
    ( Summary "Fetch a notification by ID"
        :> ZUser
        :> "notifications"
        :> Capture' '[Description "Notification ID"] "id" NotificationId
        :> QueryParam' [Optional, Strict, Description "Only return notifications targeted at the given client"] "client" ClientId
        :> MultiVerb
             'GET
             '[JSON]
             '[ ErrorResponse 'E.NotificationNotFound,
                Respond 200 "Notification found" QueuedNotification
              ]
             (Maybe QueuedNotification)
    )
    :<|> Named
           "get-last-notification"
           ( Summary "Fetch the last notification"
               :> ZUser
               :> "notifications"
               :> "last"
               :> QueryParam' [Optional, Strict, Description "Only return notifications targeted at the given client"] "client" ClientId
               :> MultiVerb
                    'GET
                    '[JSON]
                    '[ ErrorResponse 'E.NotificationNotFound,
                       Respond 200 "Notification found" QueuedNotification
                     ]
                    (Maybe QueuedNotification)
           )

swaggerDoc :: Swagger.Swagger
swaggerDoc = toSwagger (Proxy @GundeckAPI)
