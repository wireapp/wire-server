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

module Stern.API.Routes where

import Brig.Types.Intra (UserAccount)
import Control.Lens
import Data.Containers.ListUtils (nubOrd)
import Data.Swagger hiding (Contact, Header, Schema, ToSchema)
import qualified Data.Swagger as S
import Imports hiding (head)
import Servant (JSON)
import Servant hiding (Handler, JSON, addHeader, respond)
import Servant.Swagger (HasSwagger (toSwagger))
import Servant.Swagger.Internal.Orphans ()
import Servant.Swagger.UI
import Wire.API.Routes.Named
import Wire.API.User (Email)

type SternAPI =
  Named
    "get-users-by-email"
    ( Summary "Displays user's info given an email address"
        :> "users"
        :> QueryParam' [Required, Strict, Description "Email address"] "email" Email
        :> Get '[JSON] [UserAccount]
    )

-------------------------------------------------------------------------------
-- Swagger

sternSwagger :: Swagger
sternSwagger = toSwagger (Proxy @SternAPI)

type SwaggerDocsAPI = "backoffice" :> "api" :> SwaggerSchemaUI "swagger-ui" "swagger.json"

swaggerDocsAPI :: Servant.Server SwaggerDocsAPI
swaggerDocsAPI =
  swaggerSchemaUIServer $
    sternSwagger
      & S.info . S.title .~ "Stern API"
      & S.security %~ nub
      -- sanitise definitions
      & S.definitions . traverse %~ sanitise
      -- sanitise general responses
      & S.responses . traverse . S.schema . _Just . S._Inline %~ sanitise
      -- sanitise all responses of all paths
      & S.allOperations . S.responses . S.responses
        . traverse
        . S._Inline
        . S.schema
        . _Just
        . S._Inline
        %~ sanitise
  where
    sanitise :: S.Schema -> S.Schema
    sanitise =
      (S.properties . traverse . S._Inline %~ sanitise)
        . (S.required %~ nubOrd)
        . (S.enum_ . _Just %~ nub)
