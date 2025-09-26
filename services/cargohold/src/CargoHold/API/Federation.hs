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

module CargoHold.API.Federation
  ( FederationAPI,
    federationSitemap,
  )
where

import CargoHold.API.AuditLog
import CargoHold.API.Error
import CargoHold.API.V3
import CargoHold.App
import CargoHold.Options
import CargoHold.S3 (S3AssetMeta)
import qualified CargoHold.S3 as S3
import CargoHold.Types.V3 (Principal (UserPrincipal))
import Control.Error
import Data.ByteString.Conversion (toByteString')
import Data.Domain
import Data.Misc (IpAddr)
import Data.Qualified (Qualified (Qualified))
import Data.Text.Encoding (decodeLatin1)
import Imports
import Servant.API
import Servant.Server hiding (Handler)
import Wire.API.Federation.API
import qualified Wire.API.Federation.API.Cargohold as F
import Wire.API.Routes.AssetBody
import Wire.API.Routes.Named

type FederationAPI = "federation" :> FedApi 'Cargohold

federationSitemap :: ServerT FederationAPI Handler
federationSitemap =
  Named @"get-asset" getAsset
    :<|> Named @"stream-asset" streamAsset

checkAsset :: Domain -> F.GetAsset -> Handler (Maybe S3AssetMeta)
checkAsset remote ga =
  runMaybeT $
    checkMetadata (Qualified (UserPrincipal ga.user) remote) (F.key ga) (F.token ga)

streamAsset :: Domain -> Maybe IpAddr -> F.GetAsset -> Handler AssetSource
streamAsset remote _ ga = do
  meta <- checkAsset remote ga >>= maybe (throwE assetNotFound) pure
  whenM (asks (.options.settings.assetAuditLogEnabled)) $ do
    let pathTxt = decodeLatin1 (toByteString' (S3.mkKey (F.key ga)))
    logDownload (Just $ Qualified (UserPrincipal ga.user) remote) meta pathTxt
  AssetSource <$> S3.downloadV3 (F.key ga)

getAsset :: Domain -> F.GetAsset -> Handler F.GetAssetResponse
getAsset remote ga = F.GetAssetResponse . isJust <$> checkAsset remote ga
