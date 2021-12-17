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

module CargoHold.API.Public (servantSitemap) where

import qualified CargoHold.API.Legacy as LegacyAPI
import CargoHold.API.Util
import qualified CargoHold.API.V3 as V3
import CargoHold.App
import qualified CargoHold.Types.V3 as V3
import Control.Lens
import Data.ByteString.Builder
import qualified Data.ByteString.Lazy as LBS
import Data.Domain
import Data.Id
import Data.Qualified
import Imports hiding (head)
import qualified Network.HTTP.Types as HTTP
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
    :<|> qualifiedAPI
    :<|> legacyAPI
    :<|> internalAPI
  where
    userAPI :: forall tag. tag ~ 'UserPrincipalTag => ServerT (BaseAPIv3 tag) Handler
    userAPI = uploadAssetV3 @tag :<|> downloadAssetV3 @tag :<|> deleteAssetV3 @tag
    botAPI :: forall tag. tag ~ 'BotPrincipalTag => ServerT (BaseAPIv3 tag) Handler
    botAPI = uploadAssetV3 @tag :<|> downloadAssetV3 @tag :<|> deleteAssetV3 @tag
    providerAPI :: forall tag. tag ~ 'ProviderPrincipalTag => ServerT (BaseAPIv3 tag) Handler
    providerAPI = uploadAssetV3 @tag :<|> downloadAssetV3 @tag :<|> deleteAssetV3 @tag
    legacyAPI = legacyDownloadPlain :<|> legacyDownloadPlain :<|> legacyDownloadOtr
    qualifiedAPI = downloadAssetV4 :<|> deleteAssetV4
    internalAPI = pure ()

class HasLocation (tag :: PrincipalTag) where
  assetLocation :: Local AssetKey -> [Text]

instance HasLocation 'UserPrincipalTag where
  assetLocation key =
    [ "assets",
      "v4",
      domainText (tDomain key),
      assetKeyToText (tUnqualified key)
    ]

instance HasLocation 'BotPrincipalTag where
  assetLocation key =
    [ "bot",
      "assets",
      assetKeyToText (tUnqualified key)
    ]

instance HasLocation 'ProviderPrincipalTag where
  assetLocation key =
    [ "provider",
      "assets",
      assetKeyToText (tUnqualified key)
    ]

class HasLocation tag => MakePrincipal (tag :: PrincipalTag) (id :: *) | id -> tag, tag -> id where
  mkPrincipal :: id -> V3.Principal

instance MakePrincipal 'UserPrincipalTag (Local UserId) where
  mkPrincipal = V3.UserPrincipal . tUnqualified

instance MakePrincipal 'BotPrincipalTag BotId where
  mkPrincipal = V3.BotPrincipal

instance MakePrincipal 'ProviderPrincipalTag ProviderId where
  mkPrincipal = V3.ProviderPrincipal

mkAssetLocation ::
  forall (tag :: PrincipalTag).
  HasLocation tag =>
  Local AssetKey ->
  AssetLocation Relative
mkAssetLocation key =
  AssetLocation
    RelativeRef
      { rrAuthority = Nothing,
        rrPath = path,
        rrQuery = mempty,
        rrFragment = Nothing
      }
  where
    path =
      LBS.toStrict
        . toLazyByteString
        . HTTP.encodePathSegmentsRelative
        $ assetLocation @tag key

uploadAssetV3 ::
  forall tag id.
  MakePrincipal tag id =>
  id ->
  AssetSource ->
  Handler (Asset, AssetLocation Relative)
uploadAssetV3 pid req = do
  let principal = mkPrincipal pid
  asset <- V3.upload principal (getAssetSource req)
  pure (fmap qUntagged asset, mkAssetLocation @tag (asset ^. assetKey))

downloadAssetV3 ::
  MakePrincipal tag id =>
  id ->
  AssetKey ->
  Maybe AssetToken ->
  Maybe AssetToken ->
  Handler (Maybe (AssetLocation Absolute))
downloadAssetV3 usr key tok1 tok2 = do
  AssetLocation <$$> V3.download (mkPrincipal usr) key (tok1 <|> tok2)

downloadAssetV4 ::
  Local UserId ->
  Qualified AssetKey ->
  Maybe AssetToken ->
  Maybe AssetToken ->
  Handler (Maybe LocalOrRemoteAsset)
downloadAssetV4 usr qkey tok1 tok2 = do
  key <- tUnqualified <$> ensureLocal qkey
  LocalAsset <$$> downloadAssetV3 usr key tok1 tok2

deleteAssetV3 :: MakePrincipal tag id => id -> AssetKey -> Handler ()
deleteAssetV3 usr key = V3.delete (mkPrincipal usr) key

deleteAssetV4 :: Local UserId -> Qualified AssetKey -> Handler ()
deleteAssetV4 usr qkey = do
  key <- tUnqualified <$> ensureLocal qkey
  V3.delete (mkPrincipal usr) key

renewTokenV3 :: Local UserId -> AssetKey -> Handler NewAssetToken
renewTokenV3 (tUnqualified -> usr) key =
  NewAssetToken <$> V3.renewToken (V3.UserPrincipal usr) key

deleteTokenV3 :: Local UserId -> AssetKey -> Handler ()
deleteTokenV3 (tUnqualified -> usr) key = V3.deleteToken (V3.UserPrincipal usr) key

legacyDownloadPlain :: Local UserId -> ConvId -> AssetId -> Handler (Maybe (AssetLocation Absolute))
legacyDownloadPlain (tUnqualified -> usr) cnv ast =
  AssetLocation <$$> LegacyAPI.download usr cnv ast

legacyDownloadOtr :: Local UserId -> ConvId -> AssetId -> Handler (Maybe (AssetLocation Absolute))
legacyDownloadOtr (tUnqualified -> usr) cnv ast =
  AssetLocation <$$> LegacyAPI.downloadOtr usr cnv ast
