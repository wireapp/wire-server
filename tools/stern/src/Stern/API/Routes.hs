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

module Stern.API.Routes
  ( SternAPI,
    SternAPIInternal,
    SwaggerDocsAPI,
    swaggerDocsAPI,
  )
where

import Brig.Types.Intra (UserAccount)
import Control.Lens
import Data.Handle
import Data.Id
import qualified Data.Swagger as S
import Imports hiding (head)
import Servant (JSON)
import Servant hiding (Handler, JSON, addHeader, respond)
import Servant.Swagger (HasSwagger (toSwagger))
import Servant.Swagger.Internal.Orphans ()
import Servant.Swagger.UI
import Wire.API.Routes.Named
import Wire.API.SwaggerHelper (cleanupSwagger)
import Wire.API.User

type SternAPIInternal =
  Named
    "status"
    ( "i"
        :> "status"
        :> Get '[JSON] NoContent
    )

type SternAPI =
  Named
    "suspend-user"
    ( Summary "Suspends user with this ID"
        :> "users"
        :> Capture "uid" UserId
        :> "suspend"
        :> Post '[JSON] NoContent
    )
    :<|> Named
           "unsuspend-user"
           ( Summary "Unsuspends user with this ID"
               :> "users"
               :> Capture "uid" UserId
               :> "unsuspend"
               :> Post '[JSON] NoContent
           )
    :<|> Named
           "get-users-by-email"
           ( Summary "Displays user's info given an email address"
               :> "users"
               :> QueryParam' [Required, Strict, Description "Email address"] "email" Email
               :> Get '[JSON] [UserAccount]
           )
    :<|> Named
           "get-users-by-phone"
           ( Summary "Displays user's info given a phone number"
               :> "users"
               :> QueryParam' [Required, Strict, Description "Phone number"] "phone" Phone
               :> Get '[JSON] [UserAccount]
           )
    :<|> Named
           "get-users-by-ids"
           ( Summary "Displays active users info given a list of ids"
               :> "users"
               :> QueryParam' [Required, Strict, Description "List of IDs of the users, separated by comma"] "ids" [UserId]
               :> Get '[JSON] [UserAccount]
           )
    :<|> Named
           "get-users-by-handles"
           ( Summary "Displays active users info given a list of handles"
               :> "users"
               :> QueryParam' [Required, Strict, Description "List of Handles of the users, without '@', separated by comma"] "ids" [Handle]
               :> Get '[JSON] [UserAccount]
           )

-------------------------------------------------------------------------------
-- Swagger

type SwaggerDocsAPI = "backoffice" :> "api" :> SwaggerSchemaUI "swagger-ui" "swagger.json"

swaggerDocsAPI :: Servant.Server SwaggerDocsAPI
swaggerDocsAPI =
  swaggerSchemaUIServer $
    (toSwagger (Proxy @SternAPI))
      & S.info . S.title .~ "Stern API"
      & cleanupSwagger
