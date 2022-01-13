-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
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

module API.Util where

import Bilge hiding (body)
import CargoHold.Options
import CargoHold.Run
import qualified Codec.MIME.Parse as MIME
import qualified Codec.MIME.Type as MIME
import Control.Lens
import Control.Monad.Catch
import Control.Monad.Codensity
import Data.ByteString.Builder
import Data.ByteString.Conversion
import qualified Data.ByteString.Lazy as Lazy
import Data.Domain
import Data.Id
import Data.Qualified
import Data.Text.Encoding (decodeLatin1)
import qualified Data.UUID as UUID
import Federator.MockServer
import Imports hiding (head)
import qualified Network.HTTP.Media as HTTP
import Network.HTTP.Types.Header
import Network.HTTP.Types.Method
import qualified Network.Wai as Wai
import TestSetup
import Util.Options
import Wire.API.Asset

uploadSimple ::
  (Request -> Request) ->
  UserId ->
  AssetSettings ->
  (MIME.Type, ByteString) ->
  TestM (Response (Maybe Lazy.ByteString))
uploadSimple c usr sts (ct, bs) =
  let mp = buildMultipartBody sts ct (Lazy.fromStrict bs)
   in uploadRaw c usr (toLazyByteString mp)

decodeHeaderOrFail :: (HasCallStack, FromByteString a) => HeaderName -> Response b -> a
decodeHeaderOrFail h =
  fromMaybe (error $ "decodeHeaderOrFail: missing or invalid header: " ++ show h)
    . fromByteString
    . getHeader' h

uploadRaw ::
  (Request -> Request) ->
  UserId ->
  Lazy.ByteString ->
  TestM (Response (Maybe Lazy.ByteString))
uploadRaw c usr bs = do
  cargohold <- viewCargohold
  post $
    c . cargohold
      . method POST
      . zUser usr
      . zConn "conn"
      . content "multipart/mixed"
      . lbytes bs

getContentType :: Response a -> Maybe MIME.Type
getContentType = MIME.parseContentType . decodeLatin1 . getHeader' "Content-Type"

applicationText :: MIME.Type
applicationText = MIME.Type (MIME.Application "text") []

applicationOctetStream :: MIME.Type
applicationOctetStream = MIME.Type (MIME.Application "octet-stream") []

zUser :: UserId -> Request -> Request
zUser = header "Z-User" . UUID.toASCIIBytes . toUUID

zConn :: ByteString -> Request -> Request
zConn = header "Z-Connection"

deleteAssetV3 :: UserId -> Qualified AssetKey -> TestM (Response (Maybe Lazy.ByteString))
deleteAssetV3 u k = do
  c <- viewCargohold
  delete $ c . zUser u . paths ["assets", "v3", toByteString' (qUnqualified k)]

deleteAsset :: UserId -> Qualified AssetKey -> TestM (Response (Maybe Lazy.ByteString))
deleteAsset u k = do
  c <- viewCargohold
  delete $
    c . zUser u
      . paths
        [ "assets",
          "v4",
          toByteString' (qDomain k),
          toByteString' (qUnqualified k)
        ]

class IsAssetLocation key where
  locationPath :: key -> Request -> Request

instance IsAssetLocation AssetKey where
  locationPath k = paths ["assets", "v3", toByteString' k]

instance IsAssetLocation (Qualified AssetKey) where
  locationPath k = paths ["assets", "v4", toByteString' (qDomain k), toByteString' (qUnqualified k)]

instance IsAssetLocation ByteString where
  locationPath = path

class IsAssetToken tok where
  tokenParam :: tok -> Request -> Request

instance IsAssetToken () where
  tokenParam _ = id

instance IsAssetToken (Maybe AssetToken) where
  tokenParam = maybe id (header "Asset-Token" . toByteString')

instance IsAssetToken (Request -> Request) where
  tokenParam = id

downloadAsset ::
  (IsAssetLocation loc, IsAssetToken tok) =>
  UserId ->
  loc ->
  tok ->
  TestM (Response (Maybe LByteString))
downloadAsset uid loc tok = do
  c <- viewCargohold
  get $
    c . zUser uid
      . locationPath loc
      . tokenParam tok
      . noRedirect

postToken :: UserId -> AssetKey -> TestM (Response (Maybe LByteString))
postToken uid key = do
  c <- viewCargohold
  post $
    c . zUser uid
      . paths ["assets", "v3", toByteString' key, "token"]

deleteToken :: UserId -> AssetKey -> TestM (Response (Maybe LByteString))
deleteToken uid key = do
  c <- viewCargohold
  delete $
    c . zUser uid
      . paths ["assets", "v3", toByteString' key, "token"]

viewFederationDomain :: TestM Domain
viewFederationDomain = view (tsOpts . optSettings . setFederationDomain)

--------------------------------------------------------------------------------
-- Mocking utilities

withMockServer :: Wai.Application -> Codensity IO Word16
withMockServer app = Codensity $ \k ->
  bracket
    (liftIO $ startMockServer Nothing app)
    (liftIO . fst)
    (k . fromIntegral . snd)

withSettingsOverrides :: (Opts -> Opts) -> TestM a -> TestM a
withSettingsOverrides f action = do
  ts <- ask
  let opts = f (view tsOpts ts)
  liftIO . lowerCodensity $ do
    (app, _) <- mkApp opts
    p <- withMockServer app
    liftIO $ runTestM (ts & tsEndpoint %~ setLocalEndpoint p) action

setLocalEndpoint :: Word16 -> Endpoint -> Endpoint
setLocalEndpoint p = (epPort .~ p) . (epHost .~ "127.0.0.1")

withMockFederator ::
  (FederatedRequest -> IO (HTTP.MediaType, LByteString)) ->
  TestM a ->
  TestM (a, [FederatedRequest])
withMockFederator respond action = do
  withTempMockFederator [] respond $ \p ->
    withSettingsOverrides
      (optFederator . _Just %~ setLocalEndpoint (fromIntegral p))
      action
