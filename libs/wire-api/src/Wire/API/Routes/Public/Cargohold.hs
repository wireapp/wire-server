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
import GHC.TypeLits
import Imports
import Servant
import Servant.API.Generic (ToServantApi, (:-))
import Servant.Swagger.Internal
import Servant.Swagger.Internal.Orphans ()
import Wire.API.Asset
import Wire.API.Routes.AssetBody
import Wire.API.Routes.MultiVerb
import Wire.API.Routes.Public

newtype AssetLocation = AssetLocation {getAssetLocation :: Text}

instance KnownSymbol name => AsHeaders '[Header name Text] Asset (Asset, AssetLocation) where
  toHeaders (asset, loc) = addHeader (getAssetLocation loc) asset
  fromHeaders h = case lookupResponseHeader @name h of
    Header loc -> Just (getResponse h, AssetLocation loc)
    _ -> Nothing

data Api routes = Api
  { -- Simple (one-step) Upload
    --
    -- Doc.errorResponse Error.assetTooLarge
    -- Doc.errorResponse Error.invalidLength
    -- Doc.consumes "multipart/mixed"
    postAsset ::
      routes
        :- Summary
             "Upload an asset."
        :> ZLocalUser
        :> "assets"
        :> "v3"
        :> AssetBody
        :> MultiVerb
             'POST
             '[JSON]
             '[ WithHeaders
                  '[DescHeader "Location" "Asset location" Text]
                  (Asset, AssetLocation)
                  (Respond 201 "Asset posted" Asset)
              ]
             (Asset, AssetLocation)
  }
  deriving (Generic)

type ServantAPI = ToServantApi Api

swaggerDoc :: Swagger.Swagger
swaggerDoc = toSwagger (Proxy @ServantAPI)
