{-# LANGUAGE TemplateHaskell #-}

module Wire.API.Swagger where

import Control.Lens
import Data.OpenApi qualified as S
import Data.Singletons
import FileEmbedLzma
import Imports
import Language.Haskell.TH.Syntax
import Servant
import Servant.OpenApi
import Wire.API.Routes.API
import Wire.API.Routes.Public.Brig
import Wire.API.Routes.Public.Brig.OAuth
import Wire.API.Routes.Public.Cannon
import Wire.API.Routes.Public.Cargohold
import Wire.API.Routes.Public.Galley
import Wire.API.Routes.Public.Gundeck
import Wire.API.Routes.Public.Proxy
import Wire.API.Routes.Public.Spar
import Wire.API.Routes.Version
import Wire.API.SwaggerHelper

publicAPISwagger ::
  forall (v :: Version).
  ( HasOpenApi (SpecialisedAPIRoutes v BrigAPITag),
    HasOpenApi (SpecialisedAPIRoutes v GalleyAPITag),
    HasOpenApi (SpecialisedAPIRoutes v CargoholdAPITag),
    HasOpenApi (SpecialisedAPIRoutes v OAuthAPITag),
    HasOpenApi (SpecialisedAPIRoutes v GundeckAPITag),
    SingI v
  ) =>
  S.OpenApi
publicAPISwagger =
  ( serviceSwagger @VersionAPITag @v
      <> serviceSwagger @BrigAPITag @v
      <> serviceSwagger @GalleyAPITag @v
      <> serviceSwagger @SparAPITag @v
      <> serviceSwagger @CargoholdAPITag @v
      <> serviceSwagger @CannonAPITag @v
      <> serviceSwagger @GundeckAPITag @v
      <> serviceSwagger @ProxyAPITag @v
      <> serviceSwagger @OAuthAPITag @v
  )
    & S.info . S.title .~ "Wire-Server API"
    & S.info . S.description ?~ $(embedText =<< makeRelativeToProject "docs/swagger.md")
    & S.servers .~ [S.Server ("/" <> toUrlPiece (demote @v)) Nothing mempty]
    & cleanupSwagger
