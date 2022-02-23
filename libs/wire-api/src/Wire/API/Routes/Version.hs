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

module Wire.API.Routes.Version
  ( -- * API version endpoint
    VersionAPI,
    VersionInfo (..),
    versionSwagger,

    -- * Version
    Version (..),
    supportedVersions,
    readVersionNumber,
    mkVersion,
  )
where

import Control.Lens ((?~))
import Data.Aeson (FromJSON, ToJSON (..))
import qualified Data.Aeson as Aeson
import Data.Attoparsec.ByteString.Char8
import Data.ByteString.Builder
import Data.ByteString.Conversion
import Data.Domain
import Data.Schema
import qualified Data.Swagger as S
import qualified Data.Text as Text
import qualified Data.Text.Read as Text
import Imports
import Servant
import Servant.Swagger
import Wire.API.Routes.Named
import Wire.API.VersionInfo

data Version = V0 | V1
  deriving stock (Eq, Ord, Bounded, Enum, Show)
  deriving (FromJSON, ToJSON) via (Schema Version)

versionNumber :: Version -> Integer
versionNumber V0 = 0
versionNumber V1 = 1

instance ToSchema Version where
  schema =
    enum @Integer "Version" $
      foldMap
        (\v -> element (versionNumber v) v)
        [minBound .. maxBound]

instance FromByteString Version where
  parser =
    maybe (fail "Unsupported version") pure
      =<< fmap mkVersion decimal

instance ToByteString Version where
  builder = integerDec . versionNumber

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
