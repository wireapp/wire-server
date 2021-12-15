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

module CargoHold.API.Public.Servant
  ( servantSitemap,
  )
where

import qualified CargoHold.API.V3 as V3
import CargoHold.App
import qualified CargoHold.Types.V3 as V3
import Control.Lens
import Data.ByteString.Conversion
import Data.Id
import Data.Qualified
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as Text
import Imports hiding (head)
import Servant ((:<|>) (..))
import Servant.Server hiding (Handler)
import URI.ByteString
import Wire.API.Asset
import Wire.API.Routes.AssetBody
import Wire.API.Routes.Public.Cargohold

servantSitemap :: ServerT ServantAPI Handler
servantSitemap =
  renewTokenV3 :<|> deleteTokenV3
    :<|> (uploadAssetV3 :<|> downloadAssetV3 :<|> deleteAssetV3)
    :<|> (uploadAssetV3 :<|> downloadAssetV3 :<|> deleteAssetV3)
    :<|> (uploadAssetV3 :<|> downloadAssetV3 :<|> deleteAssetV3)

uploadAssetV3 :: Local UserId -> AssetSource -> Handler (Asset, AssetLocation)
uploadAssetV3 (tUnqualified -> usr) req = do
  let principal = V3.UserPrincipal usr
  asset <- V3.upload principal (getAssetSource req)
  let key = Text.decodeUtf8With Text.lenientDecode (toByteString' (asset ^. assetKey))
  let loc = case principal of
        V3.UserPrincipal {} -> "/assets/v3/" <> key
        V3.BotPrincipal {} -> "/bot/assets/" <> key
        V3.ProviderPrincipal {} -> "/provider/assets/" <> key
  pure (asset, AssetLocation loc)

downloadAssetV3 :: Local UserId -> AssetKey -> Maybe AssetToken -> Handler (Maybe AssetLocation)
downloadAssetV3 (tUnqualified -> usr) key tok = do
  url <- V3.download (V3.UserPrincipal usr) key tok
  pure $ fmap (AssetLocation . Text.decodeUtf8With Text.lenientDecode . serializeURIRef') url

deleteAssetV3 :: Local UserId -> AssetKey -> Handler ()
deleteAssetV3 (tUnqualified -> usr) key = V3.delete (V3.UserPrincipal usr) key

renewTokenV3 :: Local UserId -> AssetKey -> Handler NewAssetToken
renewTokenV3 (tUnqualified -> usr) key =
  NewAssetToken <$> V3.renewToken (V3.UserPrincipal usr) key

deleteTokenV3 :: Local UserId -> AssetKey -> Handler ()
deleteTokenV3 (tUnqualified -> usr) key = V3.deleteToken (V3.UserPrincipal usr) key
