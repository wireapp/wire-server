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

module Wire.API.Federation.API.Cargohold where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Id
import Imports
import Servant.API
import Wire.API.Asset
import Wire.API.Federation.Endpoint
import Wire.API.Routes.AssetBody
import Wire.API.Util.Aeson
import Wire.Arbitrary (Arbitrary, GenericUniform (..))

data GetAsset = GetAsset
  { -- | User requesting the asset. Implictly qualified with the source domain.
    user :: UserId,
    -- | Asset key for the asset to download. Implictly qualified with the
    -- target domain.
    key :: AssetKey,
    -- | Optional asset token.
    token :: Maybe AssetToken
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform GetAsset)
  deriving (ToJSON, FromJSON) via (CustomEncoded GetAsset)

data GetAssetResponse = GetAssetResponse
  {available :: Bool}
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform GetAssetResponse)
  deriving (ToJSON, FromJSON) via (CustomEncoded GetAssetResponse)

type CargoholdApi =
  FedEndpoint "get-asset" GetAsset GetAssetResponse
    :<|> StreamingFedEndpoint "stream-asset" GetAsset AssetSource
