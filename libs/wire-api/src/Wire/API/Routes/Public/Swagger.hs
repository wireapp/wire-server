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

-- | The swagger docs for the public API of the development version.
--
-- Older versions are frozen and served from the pregenerated JSON files in
-- @services/brig/docs/@; this is the only version that is still assembled from
-- the routing tables.  It lives here rather than in brig so that tests in this
-- package can get at it -- see @Test.Wire.API.Routes.OAuthScopes@.
module Wire.API.Routes.Public.Swagger
  ( devVersion,
    devVersionSwagger,
  )
where

import Control.Lens ((.~))
import Data.OpenApi qualified as S
import Imports
import Servant.API (toUrlPiece)
import Wire.API.Routes.API (serviceSwagger)
import Wire.API.Routes.Public.Brig (BrigAPITag)
import Wire.API.Routes.Public.Brig.OAuth (OAuthAPITag)
import Wire.API.Routes.Public.Cannon (CannonAPITag)
import Wire.API.Routes.Public.Cargohold (CargoholdAPITag)
import Wire.API.Routes.Public.Galley (GalleyAPITag)
import Wire.API.Routes.Public.Gundeck (GundeckAPITag)
import Wire.API.Routes.Public.Proxy (ProxyAPITag)
import Wire.API.Routes.Public.Spar (SparAPITag)
import Wire.API.Routes.Version
import Wire.API.SwaggerHelper (cleanupSwagger)

-- | The version 'devVersionSwagger' describes.  Must stay in sync with the type
-- level @\'V18@ below; there is no way to tie the two together, since the
-- 'S.OpenApi' has to be assembled at a statically known version.
devVersion :: Version
devVersion =
  if maxBound == V18
    then maxBound
    else
      -- if you get this error, you also need to update the version literals below.
      error "libs/wire-api/src/Wire/API/Routes/Public/Swagger.hs#devVersion: please update to latest api version!"

-- | Note that brig additionally sets @info.description@ from
-- @services/brig/docs/swagger.md@, which cannot move here: it is embedded
-- relative to the brig package.  'cleanupSwagger' does not touch
-- @info.description@, so setting it afterwards is equivalent.
devVersionSwagger :: S.OpenApi
devVersionSwagger =
  ( serviceSwagger @VersionAPITag @'V18
      <> serviceSwagger @BrigAPITag @'V18
      <> serviceSwagger @GalleyAPITag @'V18
      <> serviceSwagger @SparAPITag @'V18
      <> serviceSwagger @CargoholdAPITag @'V18
      <> serviceSwagger @CannonAPITag @'V18
      <> serviceSwagger @GundeckAPITag @'V18
      <> serviceSwagger @ProxyAPITag @'V18
      <> serviceSwagger @OAuthAPITag @'V18
  )
    & S.info . S.title .~ "Wire-Server API"
    & S.servers .~ [S.Server ("/" <> toUrlPiece devVersion) Nothing mempty]
    & cleanupSwagger
