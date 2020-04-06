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

module CargoHold.API
  ( sitemap,
  )
where

import qualified CargoHold.API.Error as Error
import qualified CargoHold.API.Legacy as LegacyAPI
import qualified CargoHold.API.V3 as V3
import qualified CargoHold.API.V3.Resumable as Resumable
import CargoHold.App
import CargoHold.Options
import qualified CargoHold.TUS as TUS
import qualified CargoHold.Types.V3 as V3
import qualified CargoHold.Types.V3.Resumable as V3
import Control.Error
import Control.Lens ((^.), view)
import Data.ByteString.Conversion
import Data.Id
import Data.Predicate
import qualified Data.Swagger.Build.Api as Doc
import Data.Text.Encoding (decodeLatin1)
import Imports hiding (head)
import Network.HTTP.Types.Status
import Network.Wai (Request, Response)
import Network.Wai.Conduit (sourceRequestBody)
import Network.Wai.Predicate hiding (Error, setStatus)
import Network.Wai.Routing
import Network.Wai.Utilities hiding (message)
import Network.Wai.Utilities.Response (json)
import Network.Wai.Utilities.Swagger (document, mkSwaggerApi)
import qualified Network.Wai.Utilities.Swagger as Doc
import Network.Wai.Utilities.ZAuth
import URI.ByteString

sitemap :: Routes Doc.ApiBuilder Handler ()
sitemap = do
  get "/i/status" (continue $ const $ return empty) true
  head "/i/status" (continue $ const $ return empty) true
  get
    "/assets/api-docs"
    ( \(_ ::: url) k ->
        let doc = mkSwaggerApi (decodeLatin1 url) [] sitemap
         in k $ json doc
    )
    $ accept "application" "json"
      .&. query "base_url"
  ---------------------------------------------------------------------------
  -- User API

  --- Simple (one-step) Upload

  post "/assets/v3" (continue uploadAssetV3) $
    header "Z-User"
      .&. contentType "multipart" "mixed"
      .&. request
  document "POST" "uploadAsset" $ do
    Doc.summary "Upload an asset"
    Doc.consumes "multipart/mixed"
    Doc.errorResponse Error.assetTooLarge
    Doc.errorResponse Error.invalidLength
    Doc.response 201 "Asset posted" Doc.end
  -- Resumable (multi-step) Upload
  -- TODO: swagger doc

  options "/assets/v3/resumable" (continue resumableOptionsV3) $
    header "Z-User"
  -- TODO (Compliance): Require and check Tus-Resumable header
  -- against supported version(s).
  post "/assets/v3/resumable" (continue createResumableV3) $
    header "Z-User"
      .&. header "Upload-Length"
      .&. jsonRequest @V3.ResumableSettings
  -- TODO (Compliance): Require and check Tus-Resumable header
  -- against supported version(s).
  head "/assets/v3/resumable/:key" (continue statusResumableV3) $
    header "Z-User"
      .&. capture "key"
  -- TODO (Compliance): Require and check Tus-Resumable header
  -- against supported version(s).
  patch "/assets/v3/resumable/:key" (continue uploadResumableV3) $
    header "Z-User"
      .&. header "Upload-Offset"
      .&. header "Content-Length"
      .&. contentType "application" "offset+octet-stream"
      .&. capture "key"
      .&. request
  --- Download

  get "/assets/v3/:key" (continue downloadAssetV3) $
    header "Z-User"
      .&. capture "key"
      .&. opt (header "Asset-Token" .|. query "asset_token")
  document "GET" "downloadAsset" $ do
    Doc.summary "Download an asset"
    Doc.parameter Doc.Path "key" Doc.bytes' $
      Doc.description "Asset key"
    Doc.parameter Doc.Header "Asset-Token" Doc.bytes' $ do
      Doc.description "Asset token"
      Doc.optional
    Doc.errorResponse Error.assetNotFound
    Doc.response 302 "Asset found" Doc.end
  --- Token Management

  post "/assets/v3/:key/token" (continue renewTokenV3) $
    header "Z-User"
      .&. capture "key"
  document "POST" "renewAssetToken" $ do
    Doc.summary "Renew an asset token"
    Doc.parameter Doc.Path "key" Doc.bytes' $
      Doc.description "Asset key"
    Doc.response 200 "Asset token renewed" Doc.end
    Doc.errorResponse Error.assetNotFound
    Doc.errorResponse Error.unauthorised
  delete "/assets/v3/:key/token" (continue deleteTokenV3) $
    header "Z-User"
      .&. capture "key"
  document "DELETE" "deleteAssetToken" $ do
    Doc.summary "Delete an asset token"
    Doc.notes "Deleting the token makes the asset public."
    Doc.parameter Doc.Path "key" Doc.bytes' $
      Doc.description "Asset key"
    Doc.response 200 "Asset token deleted" Doc.end
  --- Deletion

  delete "/assets/v3/:key" (continue deleteAssetV3) $
    header "Z-User"
      .&. capture "key"
  document "DELETE" "deleteAsset" $ do
    Doc.summary "Delete an asset"
    Doc.parameter Doc.Path "key" Doc.bytes' $
      Doc.description "Asset key"
    Doc.response 200 "Asset deleted" Doc.end
    Doc.errorResponse Error.assetNotFound
    Doc.errorResponse Error.unauthorised
  ---------------------------------------------------------------------------
  -- Provider API

  post "/provider/assets" (continue providerUploadV3) $
    zauth ZAuthProvider
      .&> contentType "multipart" "mixed"
      .&> zauthProviderId
      .&. request
  get "/provider/assets/:key" (continue providerDownloadV3) $
    zauth ZAuthProvider
      .&> zauthProviderId
      .&. capture "key"
      .&. opt (header "Asset-Token" .|. query "asset_token")
  delete "/provider/assets/:key" (continue providerDeleteV3) $
    zauth ZAuthProvider
      .&> zauthProviderId
      .&. capture "key"
  ---------------------------------------------------------------------------
  -- Bot API

  post "/bot/assets" (continue botUploadV3) $
    zauth ZAuthBot
      .&> contentType "multipart" "mixed"
      .&> zauthBotId
      .&. request
  get "/bot/assets/:key" (continue botDownloadV3) $
    zauth ZAuthBot
      .&> zauthBotId
      .&. capture "key"
      .&. opt (header "Asset-Token" .|. query "asset_token")
  delete "/bot/assets/:key" (continue botDeleteV3) $
    zauth ZAuthBot
      .&> zauthBotId
      .&. capture "key"
  -- Legacy

  get "/assets/:id" (continue legacyDownloadPlain) $
    header "Z-User"
      .&. param "conv_id"
      .&. capture "id"
  get "/conversations/:cnv/assets/:id" (continue legacyDownloadPlain) $
    header "Z-User"
      .&. capture "cnv"
      .&. capture "id"
  get "/conversations/:cnv/otr/assets/:id" (continue legacyDownloadOtr) $
    header "Z-User"
      .&. capture "cnv"
      .&. capture "id"

-----------------------------------------------------------------------------
-- User API Handlers

uploadAssetV3 :: UserId ::: Media "multipart" "mixed" ::: Request -> Handler Response
uploadAssetV3 (usr ::: _ ::: req) = uploadSimpleV3 (V3.UserPrincipal usr) req

downloadAssetV3 :: UserId ::: V3.AssetKey ::: Maybe V3.AssetToken -> Handler Response
downloadAssetV3 (usr ::: key ::: tok) = do
  url <- V3.download (V3.UserPrincipal usr) key tok
  redirect url

deleteAssetV3 :: UserId ::: V3.AssetKey -> Handler Response
deleteAssetV3 (usr ::: key) = do
  V3.delete (V3.UserPrincipal usr) key
  return empty

renewTokenV3 :: UserId ::: V3.AssetKey -> Handler Response
renewTokenV3 (usr ::: key) = do
  tok <- V3.renewToken (V3.UserPrincipal usr) key
  return $ json (V3.NewAssetToken tok)

deleteTokenV3 :: UserId ::: V3.AssetKey -> Handler Response
deleteTokenV3 (usr ::: key) = do
  V3.deleteToken (V3.UserPrincipal usr) key
  return empty

resumableOptionsV3 :: UserId -> Handler Response
resumableOptionsV3 _ = do
  maxTotal <- view (settings . setMaxTotalBytes)
  return $ TUS.optionsResponse (fromIntegral maxTotal) empty

createResumableV3 :: UserId ::: V3.TotalSize ::: JsonRequest V3.ResumableSettings -> Handler Response
createResumableV3 (u ::: size ::: req) = do
  sets <- parseBody req !>> Error.clientError
  res <- Resumable.create (V3.UserPrincipal u) sets size
  let key = res ^. V3.resumableAsset . V3.assetKey
  let expiry = res ^. V3.resumableExpires
  let loc = "/assets/v3/resumable/" <> toByteString' key
  return . TUS.createdResponse loc expiry $ json res

statusResumableV3 :: UserId ::: V3.AssetKey -> Handler Response
statusResumableV3 (u ::: a) = do
  stat <- Resumable.status (V3.UserPrincipal u) a
  return $ case stat of
    Nothing -> setStatus status404 empty
    Just st -> TUS.headResponse st empty

-- Request = raw bytestring
uploadResumableV3 :: UserId ::: V3.Offset ::: Word ::: Media "application" "offset+octet-stream" ::: V3.AssetKey ::: Request -> Handler Response
uploadResumableV3 (usr ::: offset ::: size ::: _ ::: aid ::: req) = do
  (offset', expiry) <- Resumable.upload (V3.UserPrincipal usr) aid offset size (sourceRequestBody req)
  return $ TUS.patchResponse offset' expiry empty

--------------------------------------------------------------------------------
-- Provider API Handlers

providerUploadV3 :: ProviderId ::: Request -> Handler Response
providerUploadV3 (prv ::: req) = uploadSimpleV3 (V3.ProviderPrincipal prv) req

providerDownloadV3 :: ProviderId ::: V3.AssetKey ::: Maybe V3.AssetToken -> Handler Response
providerDownloadV3 (prv ::: key ::: tok) = do
  url <- V3.download (V3.ProviderPrincipal prv) key tok
  redirect url

providerDeleteV3 :: ProviderId ::: V3.AssetKey -> Handler Response
providerDeleteV3 (prv ::: key) = do
  V3.delete (V3.ProviderPrincipal prv) key
  return empty

--------------------------------------------------------------------------------
-- Bot API Handlers

botUploadV3 :: BotId ::: Request -> Handler Response
botUploadV3 (bot ::: req) = uploadSimpleV3 (V3.BotPrincipal bot) req

botDownloadV3 :: BotId ::: V3.AssetKey ::: Maybe V3.AssetToken -> Handler Response
botDownloadV3 (bot ::: key ::: tok) = do
  url <- V3.download (V3.BotPrincipal bot) key tok
  redirect url

botDeleteV3 :: BotId ::: V3.AssetKey -> Handler Response
botDeleteV3 (bot ::: key) = do
  V3.delete (V3.BotPrincipal bot) key
  return empty

--------------------------------------------------------------------------------
-- Helpers

uploadSimpleV3 ::
  V3.Principal ->
  Request -> -- Raw bytestring
  Handler Response
uploadSimpleV3 prc req = do
  let src = sourceRequestBody req
  asset <- V3.upload prc src
  return
    $ setStatus status201
      . loc (asset ^. V3.assetKey)
    $ json asset
  where
    loc k = location $ case prc of
      V3.UserPrincipal {} -> "/assets/v3/" <> toByteString k
      V3.BotPrincipal {} -> "/bot/assets/" <> toByteString k
      V3.ProviderPrincipal {} -> "/provider/assets/" <> toByteString k

redirect :: Maybe URI -> Handler Response
redirect (Just url) = return . setStatus status302 $ location (serializeURIRef url) empty
redirect Nothing = throwE Error.assetNotFound
{-# INLINE redirect #-}

location :: ToByteString a => a -> Response -> Response
location = addHeader "Location" . toByteString'
{-# INLINE location #-}

--------------------------------------------------------------------------------
-- Legacy

legacyDownloadPlain :: UserId ::: ConvId ::: AssetId -> Handler Response
legacyDownloadPlain (usr ::: cnv ::: ast) = LegacyAPI.download usr cnv ast >>= redirect

legacyDownloadOtr :: UserId ::: ConvId ::: AssetId -> Handler Response
legacyDownloadOtr (usr ::: cnv ::: ast) = LegacyAPI.downloadOtr usr cnv ast >>= redirect
