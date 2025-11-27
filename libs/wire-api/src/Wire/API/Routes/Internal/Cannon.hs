-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.API.Routes.Internal.Cannon where

import Control.Lens ((.~))
import Data.Id
import Data.OpenApi (HasInfo (info), HasTitle (title), OpenApi)
import Imports
import Servant
import Servant.OpenApi (HasOpenApi (toOpenApi))
import Wire.API.Error
import Wire.API.Error.Cannon
import Wire.API.Internal.BulkPush
import Wire.API.RawJson
import Wire.API.Routes.MultiVerb
import Wire.API.Routes.Named

type API =
  "i"
    :> ( Named
           "get-status"
           ( "status"
               :> MultiVerb
                    'GET
                    '[PlainText]
                    '[RespondEmpty 200 "Service is alive."]
                    ()
           )
           :<|> Named
                  "push-notification"
                  ( "push"
                      :> Capture "user" UserId
                      :> Capture "conn" ConnId
                      :> ReqBody '[JSON] RawJson
                      :> MultiVerb
                           'POST
                           '[JSON]
                           '[ ErrorResponse 'ClientGone,
                              RespondEmpty 200 "Successfully pushed."
                            ]
                           (Maybe ())
                  )
           :<|> Named
                  "bulk-push-notifications"
                  ( "bulkpush"
                      :> ReqBody '[JSON] BulkPushRequest
                      :> Post '[JSON] BulkPushResponse
                  )
           :<|> Named
                  "check-presence"
                  ( "presences"
                      :> Capture "uid" UserId
                      :> Capture "conn" ConnId
                      :> MultiVerb
                           'HEAD
                           '[JSON]
                           '[ ErrorResponse 'PresenceNotRegistered,
                              RespondEmpty 200 "Presence checked successfully."
                            ]
                           (Maybe ())
                  )
       )

swaggerDoc :: OpenApi
swaggerDoc =
  toOpenApi (Proxy @API)
    & info . title .~ "Wire-Server internal cannon API"
