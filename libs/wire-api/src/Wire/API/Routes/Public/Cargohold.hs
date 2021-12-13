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

module Wire.API.Routes.Public.Cargohold where

import qualified Data.Swagger as Swagger
import Imports
import Servant
import Servant.API.Generic (ToServantApi, (:-))
import Servant.Swagger.Internal
import Servant.Swagger.Internal.Orphans ()
import Wire.API.Asset
import Wire.API.Routes.AssetBody
import Wire.API.Routes.MultiVerb
import Wire.API.Routes.Public

data Api routes = Api
  { postAsset ::
      routes
        :- Summary
             "Upload an asset.\n\nIn the multipart/mixed body, the first section's content type \
             \should be `application/json`. The second section's content type should be always \
             \be `application/octet-stream`. Other content types will be ignored by the server."
        :> ZLocalUser
        :> "assets"
        :> "v4"
        :> AssetBody
        :> MultiVerb
             'POST
             '[JSON]
             '[ WithHeaders
                  '[DescHeader "Location" "Asset location" Text]
                  Asset
                  (Respond 201 "Asset posted" Asset)
              ]
             Asset
  }
  deriving (Generic)

type ServantAPI = ToServantApi Api

swaggerDoc :: Swagger.Swagger
swaggerDoc = toSwagger (Proxy @ServantAPI)
