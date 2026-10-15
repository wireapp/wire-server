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

-- | Live-rendered OpenAPI of the public API for a given version, assembled
-- from the routing tables.  It lives here rather than in brig so that tests in
-- this package can get at it -- see @Test.Wire.API.Routes.OAuthScopes@.
module Wire.API.Routes.Public.Swagger
  ( allServicesSwagger,
    devVersion,
    devVersionSwagger,
    versionedSwagger,
  )
where

import Control.Lens ((.~))
import Data.OpenApi qualified as S
import Data.Proxy (Proxy (..))
import Imports
import Servant.API (toUrlPiece)
import Servant.OpenApi (HasOpenApi)
import Wire.API.Routes.API (SpecialisedAPIRoutes, serviceSwagger)
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
  if maxBound == V19
    then maxBound
    else
      -- if you get this error, you also need to update the version literals below.
      error "libs/wire-api/src/Wire/API/Routes/Public/Swagger.hs#devVersion: please update to latest api version!"

-- | Live-rendered OpenAPI of all public services restricted to version @v@.
allServicesSwagger ::
  forall v.
  ( HasOpenApi (SpecialisedAPIRoutes v BrigAPITag),
    HasOpenApi (SpecialisedAPIRoutes v GalleyAPITag),
    HasOpenApi (SpecialisedAPIRoutes v SparAPITag),
    HasOpenApi (SpecialisedAPIRoutes v CargoholdAPITag),
    HasOpenApi (SpecialisedAPIRoutes v CannonAPITag),
    HasOpenApi (SpecialisedAPIRoutes v GundeckAPITag),
    HasOpenApi (SpecialisedAPIRoutes v OAuthAPITag)
  ) =>
  S.OpenApi
allServicesSwagger =
  serviceSwagger @VersionAPITag @v
    <> serviceSwagger @BrigAPITag @v
    <> serviceSwagger @GalleyAPITag @v
    <> serviceSwagger @SparAPITag @v
    <> serviceSwagger @CargoholdAPITag @v
    <> serviceSwagger @CannonAPITag @v
    <> serviceSwagger @GundeckAPITag @v
    <> serviceSwagger @ProxyAPITag @v
    <> serviceSwagger @OAuthAPITag @v

-- | Note that brig additionally sets @info.description@ from
-- @services/brig/docs/swagger.md@, which cannot move here: it is embedded
-- relative to the brig package.  'cleanupSwagger' does not touch
-- @info.description@, so setting it afterwards is equivalent.
devVersionSwagger :: S.OpenApi
devVersionSwagger =
  (allServicesSwagger @'V19)
    & S.info . S.title .~ "Wire-Server API"
    & S.servers .~ [S.Server ("/" <> toUrlPiece devVersion) Nothing mempty]
    & cleanupSwagger

-- | Live-rendered OpenAPI of all public services restricted to version @v@,
-- adjusted like brig's frozen snapshots used to be: server URL set to @/vN@,
-- title set, and servant's default responses cleaned up.  This is the shared
-- core of brig's @liveSwagger@ and the frozen-baseline regression test in
-- @Test.Wire.API.Routes.SwaggerRegression@.
versionedSwagger ::
  forall v.
  ( HasOpenApi (SpecialisedAPIRoutes v BrigAPITag),
    HasOpenApi (SpecialisedAPIRoutes v GalleyAPITag),
    HasOpenApi (SpecialisedAPIRoutes v SparAPITag),
    HasOpenApi (SpecialisedAPIRoutes v CargoholdAPITag),
    HasOpenApi (SpecialisedAPIRoutes v CannonAPITag),
    HasOpenApi (SpecialisedAPIRoutes v GundeckAPITag),
    HasOpenApi (SpecialisedAPIRoutes v OAuthAPITag)
  ) =>
  Proxy v ->
  VersionNumber ->
  S.OpenApi
versionedSwagger _ vn =
  allServicesSwagger @v
    & S.info . S.title .~ "Wire-Server API"
    & S.servers .~ [S.Server ("/" <> toUrlPiece vn) Nothing mempty]
    & cleanupSwagger
