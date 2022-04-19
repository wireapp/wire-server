{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
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
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Wire.API.Routes.Version
  ( -- * API version endpoint
    VersionAPI,
    VersionInfo (..),
    versionSwagger,
    versionHeader,

    -- * Version
    Version (..),
    supportedVersions,
    readVersionNumber,
    mkVersion,

    -- * Servant combinators
    Until,
    From,
  )
where

import Control.Lens ((?~))
import Data.Aeson (FromJSON, ToJSON (..))
import qualified Data.Aeson as Aeson
import qualified Data.CaseInsensitive as CI
import Data.Domain
import Data.Metrics.Servant
import Data.Schema
import Data.Singletons.TH
import qualified Data.Swagger as S
import qualified Data.Text as Text
import qualified Data.Text.Read as Text
import Imports
import qualified Network.Wai as Wai
import Servant
import Servant.Server.Internal.Delayed
import Servant.Server.Internal.DelayedIO
import Servant.Swagger
import Wire.API.Routes.Named
import Wire.API.VersionInfo

data Version = V0 | V1 | V2
  deriving stock (Eq, Ord, Bounded, Enum, Show)
  deriving (FromJSON, ToJSON) via (Schema Version)

instance ToSchema Version where
  schema =
    enum @Integer "Version" . mconcat $
      [ element 0 V0,
        element 1 V1,
        element 2 V2
      ]

readVersionNumber :: Text -> Maybe Integer
readVersionNumber v = do
  ('v', rest) <- Text.uncons v
  case Text.decimal rest of
    Right (n, "") -> pure n
    _ -> Nothing

mkVersion :: Integer -> Maybe Version
mkVersion n = case Aeson.fromJSON (Aeson.Number (fromIntegral n)) of
  Aeson.Error _ -> Nothing
  Aeson.Success v -> pure v

supportedVersions :: [Version]
supportedVersions = [minBound .. maxBound]

data VersionInfo = VersionInfo
  { vinfoSupported :: [Version],
    vinfoFederation :: Bool,
    vinfoDomain :: Domain
  }
  deriving (FromJSON, ToJSON, S.ToSchema) via (Schema VersionInfo)

instance ToSchema VersionInfo where
  schema =
    objectWithDocModifier "VersionInfo" (S.schema . S.example ?~ toJSON example) $
      VersionInfo
        <$> vinfoSupported .= vinfoObjectSchema schema
        <*> vinfoFederation .= field "federation" schema
        <*> vinfoDomain .= field "domain" schema
    where
      example :: VersionInfo
      example =
        VersionInfo
          { vinfoSupported = supportedVersions,
            vinfoFederation = False,
            vinfoDomain = Domain "example.com"
          }

type VersionAPI =
  Named
    "get-version"
    ( "api-version"
        :> Get '[JSON] VersionInfo
    )

versionSwagger :: S.Swagger
versionSwagger = toSwagger (Proxy @VersionAPI)

versionHeader :: CI.CI ByteString
versionHeader = "X-Wire-API-Version"

$(genSingletons [''Version])

--------------------------------------------------------------------------------
-- Servant combinators

data Until (v :: Version)

data From (v :: Version)

instance (SingI v, HasServer api ctx) => HasServer (Until v :> api) ctx where
  type ServerT (Until v :> api) m = ServerT api m

  route _ ctx action =
    route (Proxy @api) ctx $
      fmap const action `addHeaderCheck` withRequest headerCheck
    where
      headerCheck :: Wai.Request -> DelayedIO ()
      headerCheck req = do
        let v =
              toEnum $
                maybe
                  0
                  (either (const 0) id . parseHeader)
                  (lookup versionHeader (Wai.requestHeaders req))
        when (v >= demote @v) $
          delayedFail err404

  hoistServerWithContext _ ctx f =
    hoistServerWithContext (Proxy @api) ctx f

instance HasSwagger api => HasSwagger (Until v :> api) where
  toSwagger _ = mempty

instance RoutesToPaths api => RoutesToPaths (Until v :> api) where
  getRoutes = getRoutes @api

instance (SingI v, HasServer api ctx) => HasServer (From v :> api) ctx where
  type ServerT (From v :> api) m = ServerT api m

  route _ ctx action =
    route (Proxy @api) ctx $
      fmap const action `addHeaderCheck` withRequest headerCheck
    where
      headerCheck :: Wai.Request -> DelayedIO ()
      headerCheck req = do
        let v =
              toEnum $
                maybe
                  0
                  (either (const 0) id . parseHeader)
                  (lookup versionHeader (Wai.requestHeaders req))
        when (v < demote @v) $
          delayedFail err404

  hoistServerWithContext _ ctx f =
    hoistServerWithContext (Proxy @api) ctx f

instance HasSwagger api => HasSwagger (From v :> api) where
  toSwagger _ = toSwagger (Proxy @api)

instance RoutesToPaths api => RoutesToPaths (From v :> api) where
  getRoutes = getRoutes @api
