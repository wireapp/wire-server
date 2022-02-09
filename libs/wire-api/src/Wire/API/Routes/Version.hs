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

module Wire.API.Routes.Version where

import Control.Lens ((?~))
import Data.Aeson (FromJSON, ToJSON (..))
import qualified Data.Aeson as Aeson
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

instance ToSchema Version where
  schema =
    enum @Integer "Version" . mconcat $
      [ element 0 V0,
        element 1 V1
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

newtype VersionInfo = VersionInfo {vinfoSupported :: [Version]}
  deriving (FromJSON, ToJSON, S.ToSchema) via (Schema VersionInfo)

instance ToSchema VersionInfo where
  schema =
    (S.schema . S.example ?~ toJSON (VersionInfo supportedVersions))
      (VersionInfo <$> vinfoSupported .= vinfoSchema schema)

type VersionAPI =
  Named
    "get-version"
    ( "api-version"
        :> Get '[JSON] VersionInfo
    )

versionAPI :: Applicative m => ServerT VersionAPI m
versionAPI =
  Named @"get-version" $
    pure . VersionInfo $ supportedVersions

versionSwagger :: S.Swagger
versionSwagger = toSwagger (Proxy @VersionAPI)
