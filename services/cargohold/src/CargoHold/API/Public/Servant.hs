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
    :<|> userAPI
    :<|> botAPI
    :<|> providerAPI
  where
    userAPI :: forall tag. tag ~ 'UserPrincipalTag => ServerT (BaseAPI tag) Handler
    userAPI = uploadAssetV3 @tag :<|> downloadAssetV3 @tag :<|> deleteAssetV3 @tag
    botAPI :: forall tag. tag ~ 'BotPrincipalTag => ServerT (BaseAPI tag) Handler
    botAPI = uploadAssetV3 @tag :<|> downloadAssetV3 @tag :<|> deleteAssetV3 @tag
    providerAPI :: forall tag. tag ~ 'ProviderPrincipalTag => ServerT (BaseAPI tag) Handler
    providerAPI = uploadAssetV3 @tag :<|> downloadAssetV3 @tag :<|> deleteAssetV3 @tag

class MakePrincipal (tag :: PrincipalTag) (id :: *) | id -> tag, tag -> id where
  mkPrincipal :: id -> V3.Principal

instance MakePrincipal 'UserPrincipalTag (Local UserId) where
  mkPrincipal = V3.UserPrincipal . tUnqualified

instance MakePrincipal 'BotPrincipalTag BotId where
  mkPrincipal = V3.BotPrincipal

instance MakePrincipal 'ProviderPrincipalTag ProviderId where
  mkPrincipal = V3.ProviderPrincipal

uploadAssetV3 ::
  MakePrincipal tag id =>
  id ->
  AssetSource ->
  Handler (Asset, AssetLocation)
uploadAssetV3 pid req = do
  let principal = mkPrincipal pid
  asset <- V3.upload principal (getAssetSource req)
  let key = Text.decodeUtf8With Text.lenientDecode (toByteString' (asset ^. assetKey))
  let loc = case principal of
        V3.UserPrincipal {} -> "/assets/v3/" <> key
        V3.BotPrincipal {} -> "/bot/assets/" <> key
        V3.ProviderPrincipal {} -> "/provider/assets/" <> key
  pure (asset, AssetLocation loc)

downloadAssetV3 ::
  MakePrincipal tag id =>
  id ->
  AssetKey ->
  Maybe AssetToken ->
  Handler (Maybe AssetLocation)
downloadAssetV3 usr key tok = do
  url <- V3.download (mkPrincipal usr) key tok
  pure $ fmap (AssetLocation . Text.decodeUtf8With Text.lenientDecode . serializeURIRef') url

deleteAssetV3 :: MakePrincipal tag id => id -> AssetKey -> Handler ()
deleteAssetV3 usr key = V3.delete (mkPrincipal usr) key

renewTokenV3 :: Local UserId -> AssetKey -> Handler NewAssetToken
renewTokenV3 (tUnqualified -> usr) key =
  NewAssetToken <$> V3.renewToken (V3.UserPrincipal usr) key

deleteTokenV3 :: Local UserId -> AssetKey -> Handler ()
deleteTokenV3 (tUnqualified -> usr) key = V3.deleteToken (V3.UserPrincipal usr) key
