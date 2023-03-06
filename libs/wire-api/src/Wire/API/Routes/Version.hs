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
    VersionHeader,

    -- * Version
    Version (..),
    supportedVersions,
    developmentVersions,
    readVersionNumber,
    mkVersion,
    toPathComponent,

    -- * Servant combinators
    Until,
    From,
  )
where

import Control.Lens ((?~))
import Data.Aeson (FromJSON, ToJSON (..))
import qualified Data.Aeson as Aeson
import Data.Bifunctor
import Data.ByteString.Conversion (ToByteString (builder))
import qualified Data.ByteString.Lazy as LBS
import Data.Domain
import Data.Schema
import Data.Singletons.TH
import qualified Data.Swagger as S
import Data.Text as Text
import Data.Text.Encoding as Text
import Imports
import Servant
import Servant.Swagger
import Wire.API.Routes.Named
import Wire.API.VersionInfo

-- | Version of the public API.
data Version = V0 | V1 | V2 | V3 | V4
  deriving stock (Eq, Ord, Bounded, Enum, Show)
  deriving (FromJSON, ToJSON) via (Schema Version)

instance ToSchema Version where
  schema =
    enum @Integer "Version" . mconcat $
      [ element 0 V0,
        element 1 V1,
        element 2 V2,
        element 3 V3,
        element 4 V4
      ]

mkVersion :: Integer -> Maybe Version
mkVersion n = case Aeson.fromJSON (Aeson.Number (fromIntegral n)) of
  Aeson.Error _ -> Nothing
  Aeson.Success v -> pure v

instance FromHttpApiData Version where
  parseHeader = first Text.pack . Aeson.eitherDecode . LBS.fromStrict
  parseUrlPiece = parseHeader . Text.encodeUtf8

instance ToHttpApiData Version where
  toHeader = LBS.toStrict . Aeson.encode
  toUrlPiece = Text.decodeUtf8 . toHeader

instance ToByteString Version where
  builder = toEncodedUrlPiece

-- | `Version` as it appears in an URL path
--
-- >>> toPathComponent V1
-- "v1"
toPathComponent :: Version -> ByteString
toPathComponent v = "v" <> toHeader v

supportedVersions :: [Version]
supportedVersions = [minBound .. maxBound]

developmentVersions :: [Version]
developmentVersions = [V4]

-- | Information related to the public API version.
--
-- This record also contains whether federation is enabled and the federation
-- domain. Clients should fetch this information early when connecting to a
-- backend, in order to decide how to form request paths, and how to deal with
-- federated backends and qualified user IDs.
data VersionInfo = VersionInfo
  { vinfoSupported :: [Version],
    vinfoDevelopment :: [Version],
    vinfoFederation :: Bool,
    vinfoDomain :: Domain
  }
  deriving (FromJSON, ToJSON, S.ToSchema) via (Schema VersionInfo)

instance ToSchema VersionInfo where
  schema =
    objectWithDocModifier "VersionInfo" (S.schema . S.example ?~ toJSON example) $
      VersionInfo
        <$> vinfoSupported .= vinfoObjectSchema schema
        <*> vinfoDevelopment .= field "development" (array schema)
        <*> vinfoFederation .= field "federation" schema
        <*> vinfoDomain .= field "domain" schema
    where
      example :: VersionInfo
      example =
        VersionInfo
          { vinfoSupported = supportedVersions,
            vinfoDevelopment = [maxBound],
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

$(genSingletons [''Version])
