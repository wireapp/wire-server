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

module CargoHold.API.Public (servantSitemap, internalSitemap) where

import CargoHold.API.Error (unverifiedUser, userNotFound)
import qualified CargoHold.API.Legacy as LegacyAPI
import CargoHold.API.Util
import qualified CargoHold.API.V3 as V3
import CargoHold.App
import CargoHold.Federation
import qualified CargoHold.Types.V3 as V3
import Control.Lens
import Control.Monad.Trans.Except (throwE)
import Data.ByteString.Builder
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LBS
import Data.Domain
import Data.Id
import Data.Kind
import Data.Qualified
import Data.Text.Encoding
import Data.Text.Encoding.Error
import Imports hiding (head)
import qualified Network.HTTP.Types as HTTP
import Servant.API
import Servant.Server hiding (Handler)
import URI.ByteString as URI
import Wire.API.Asset
import Wire.API.Routes.AssetBody
import Wire.API.Routes.Internal.Brig (brigInternalClient)
import Wire.API.Routes.Internal.Cargohold
import Wire.API.Routes.Named
import Wire.API.Routes.Public.Cargohold
import Wire.API.User (AccountStatus (Active), AccountStatusResp (..))

servantSitemap :: ServerT CargoholdAPI Handler
servantSitemap =
  renewTokenV3
    :<|> deleteTokenV3
    :<|> userAPI
    :<|> botAPI
    :<|> providerAPI
    :<|> qualifiedAPI
    :<|> legacyAPI
    :<|> mainAPI
  where
    userAPI :: forall tag. (tag ~ 'UserPrincipalTag) => ServerT (BaseAPIv3 tag) Handler
    userAPI = uploadAssetV3 @tag :<|> downloadAssetV3 @tag :<|> deleteAssetV3 @tag
    botAPI :: forall tag. (tag ~ 'BotPrincipalTag) => ServerT (BaseAPIv3 tag) Handler
    botAPI = uploadAssetV3 @tag :<|> downloadAssetV3 @tag :<|> deleteAssetV3 @tag
    providerAPI :: forall tag. (tag ~ 'ProviderPrincipalTag) => ServerT (BaseAPIv3 tag) Handler
    providerAPI = uploadAssetV3 @tag :<|> downloadAssetV3 @tag :<|> deleteAssetV3 @tag
    legacyAPI = legacyDownloadPlain :<|> legacyDownloadPlain :<|> legacyDownloadOtr
    qualifiedAPI :: ServerT QualifiedAPI Handler
    qualifiedAPI = downloadAssetV4 :<|> deleteAssetV4
    mainAPI :: ServerT MainAPI Handler
    mainAPI =
      renewTokenV3
        :<|> deleteTokenV3
        :<|> uploadAssetV3 @'UserPrincipalTag
        :<|> downloadAssetV4
        :<|> deleteAssetV4

internalSitemap :: ServerT InternalAPI Handler
internalSitemap =
  pure ()
    :<|> Named @"iGetAsset" iDownloadAssetV3

-- | Like 'downloadAssetV3' below, but it works without user session token, and has a
-- different route type.
iDownloadAssetV3 :: V3.AssetKey -> Handler Text
iDownloadAssetV3 key = do
  render <$> V3.downloadUnsafe key Nothing
  where
    -- (NB: don't use HttpsUrl here, as in some test environments we legitimately use "http"!)
    render :: URI.URI -> Text
    render =
      decodeUtf8With lenientDecode
        . LBS.toStrict
        . Builder.toLazyByteString
        . URI.serializeURIRef

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

class (HasLocation tag) => MakePrincipal (tag :: PrincipalTag) (id :: Type) | id -> tag, tag -> id where
  mkPrincipal :: id -> V3.Principal

instance MakePrincipal 'UserPrincipalTag (Local UserId) where
  mkPrincipal = V3.UserPrincipal . tUnqualified

instance MakePrincipal 'BotPrincipalTag BotId where
  mkPrincipal = V3.BotPrincipal

instance MakePrincipal 'ProviderPrincipalTag ProviderId where
  mkPrincipal = V3.ProviderPrincipal

mkAssetLocation ::
  forall (tag :: PrincipalTag).
  (HasLocation tag) =>
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
  (MakePrincipal tag id) =>
  id ->
  AssetSource ->
  Handler (Asset, AssetLocation Relative)
uploadAssetV3 pid req = do
  let principal = mkPrincipal pid
  case principal of
    V3.UserPrincipal uid -> do
      status <-
        lift (executeBrigInteral $ brigInternalClient @"iGetUserStatus" uid)
          >>= either (const $ throwE userNotFound) pure
      case fromAccountStatusResp status of
        Active -> pure ()
        _ -> throwE unverifiedUser
    _ -> pure ()
  asset <- V3.upload principal (getAssetSource req)
  pure (fmap tUntagged asset, mkAssetLocation @tag (asset ^. assetKey))

downloadAssetV3 ::
  (MakePrincipal tag id) =>
  id ->
  AssetKey ->
  Maybe AssetToken ->
  Maybe AssetToken ->
  Maybe Text ->
  Handler (Maybe (AssetLocation Absolute))
downloadAssetV3 usr key tok1 tok2 mbHostHeader = do
  AssetLocation <$$> V3.download (mkPrincipal usr) key (tok1 <|> tok2) mbHostHeader

downloadAssetV4 ::
  () =>
  Local UserId ->
  Qualified AssetKey ->
  Maybe AssetToken ->
  Maybe AssetToken ->
  Maybe Text ->
  Handler (Maybe LocalOrRemoteAsset)
downloadAssetV4 usr qkey tok1 tok2 mbHostHeader =
  let tok = tok1 <|> tok2
   in foldQualified
        usr
        ( \lkey ->
            LocalAsset . AssetLocation
              <$$> V3.download (mkPrincipal usr) (tUnqualified lkey) tok mbHostHeader
        )
        ( \rkey ->
            RemoteAsset
              <$$> downloadRemoteAsset usr rkey tok
        )
        qkey

deleteAssetV3 :: (MakePrincipal tag id) => id -> AssetKey -> Handler ()
deleteAssetV3 usr = V3.delete (mkPrincipal usr)

deleteAssetV4 :: Local UserId -> Qualified AssetKey -> Handler ()
deleteAssetV4 usr qkey = do
  key <- tUnqualified <$> ensureLocal qkey
  V3.delete (mkPrincipal usr) key

renewTokenV3 :: Local UserId -> AssetKey -> Handler NewAssetToken
renewTokenV3 (tUnqualified -> usr) key =
  NewAssetToken <$> V3.renewToken (V3.UserPrincipal usr) key

deleteTokenV3 :: Local UserId -> AssetKey -> Handler ()
deleteTokenV3 (tUnqualified -> usr) = V3.deleteToken (V3.UserPrincipal usr)

legacyDownloadPlain :: Local UserId -> ConvId -> AssetId -> Handler (Maybe (AssetLocation Absolute))
legacyDownloadPlain (tUnqualified -> usr) cnv ast =
  AssetLocation <$$> LegacyAPI.download usr cnv ast

legacyDownloadOtr :: Local UserId -> ConvId -> AssetId -> Handler (Maybe (AssetLocation Absolute))
legacyDownloadOtr (tUnqualified -> usr) cnv ast =
  AssetLocation <$$> LegacyAPI.downloadOtr usr cnv ast
