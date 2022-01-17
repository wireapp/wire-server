{-# LANGUAGE RecordWildCards #-}

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

module CargoHold.API.Federation
  ( FederationAPI,
    federationSitemap,
  )
where

import CargoHold.API.Error
import CargoHold.API.V3
import CargoHold.App
import qualified CargoHold.S3 as S3
import Control.Error
import Data.Domain
import Imports
import Servant.API
import Servant.Server hiding (Handler)
import Wire.API.Federation.API
import qualified Wire.API.Federation.API.Cargohold as F
import Wire.API.Routes.AssetBody
import Wire.API.Routes.Named

type FederationAPI = "federation" :> VersionedFedApi 'Cargohold

federationSitemap :: ServerT FederationAPI Handler
federationSitemap =
  Named @"get-asset" getAsset
    :<|> Named @"stream-asset" streamAsset

checkAsset :: F.GetAsset -> Handler Bool
checkAsset ga =
  fmap isJust . runMaybeT $
    checkMetadata Nothing (F.gaKey ga) (F.gaToken ga)

streamAsset :: Domain -> F.GetAsset -> Handler AssetSource
streamAsset _ ga = do
  available <- checkAsset ga
  unless available (throwE assetNotFound)
  AssetSource <$> S3.downloadV3 (F.gaKey ga)

getAsset :: Domain -> F.GetAsset -> Handler F.GetAssetResponse
getAsset _ = fmap F.GetAssetResponse . checkAsset
