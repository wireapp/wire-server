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

module Wire.API.Routes.AssetBody (AssetBody) where

import Control.Lens
import Data.Proxy
import Data.Swagger
import Data.Swagger.Declare
import Imports
import Servant
import Servant.Swagger
import Servant.Swagger.Internal
import Wire.API.Asset

data AssetBody

instance HasSwagger api => HasSwagger (AssetBody :> api) where
  toSwagger _ =
    toSwagger (Proxy @api)
      & addConsumes [contentType (Proxy @JSON), contentType (Proxy @OctetStream)]
      & definitions <>~ defs
      & addParam
        ( mempty
            & name .~ "asset"
            & description
              ?~ "A body with content type `multipart/mixed body`. The first section's \
                 \content type should be `application/json`. The second section's content \
                 \type should be always be `application/octet-stream`. Other content types \
                 \will be ignored by the server."
            & required ?~ True
            & schema .~ ParamBody ref
        )
    where
      (defs, ref) = runDeclare (declareSchemaRef (Proxy @Asset)) mempty
