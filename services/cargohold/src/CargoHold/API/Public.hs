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

module CargoHold.API.Public
  ( sitemap,
    apiDocs,
  )
where

import qualified CargoHold.API.Error as Error
import qualified CargoHold.API.Legacy as LegacyAPI
import qualified CargoHold.API.V3 as V3
import CargoHold.App
import qualified CargoHold.Types.V3 as V3 (Principal (..))
import Control.Error
import Control.Lens ((^.))
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
import Network.Wai.Utilities.Swagger (document, mkSwaggerApi)
import qualified Network.Wai.Utilities.Swagger as Doc
import Network.Wai.Utilities.ZAuth
import URI.ByteString
import qualified Wire.API.Asset as Public

-- FUTUREWORK: restore (and servantify) resumable upload functionality, removed
-- in https://github.com/wireapp/wire-server/pull/1998

sitemap :: Routes Doc.ApiBuilder Handler ()
sitemap = do
  ---------------------------------------------------------------------------
  -- User API

  -- Simple (one-step) Upload

  post "/assets/v3" (continue uploadAssetV3) $
    header "Z-User"
      .&. contentType "multipart" "mixed"
      .&. request
  document "POST" "uploadAsset" $ do
    Doc.summary "Upload an asset. In the multipart/mixed body, the first section's content type should be application/json. The second section's content type should be always application/octet-stream. Other content types will be ignored by the server."
    Doc.consumes "multipart/mixed"
    Doc.errorResponse Error.assetTooLarge
    Doc.errorResponse Error.invalidLength
    Doc.response 201 "Asset posted" Doc.end

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

apiDocs :: Routes Doc.ApiBuilder Handler ()
apiDocs = do
  get
    "/assets/api-docs"
    ( \(_ ::: url) k ->
        let doc = mkSwaggerApi (decodeLatin1 url) [] sitemap
         in k $ json doc
    )
    $ accept "application" "json"
      .&. query "base_url"

-----------------------------------------------------------------------------
-- User API Handlers

-- FUTUREWORK: make these types more descriptive than 'Request' -> 'Response'
uploadAssetV3 :: UserId ::: Media "multipart" "mixed" ::: Request -> Handler Response
uploadAssetV3 (usr ::: _ ::: req) = do
  let principal = V3.UserPrincipal usr
  assetResponse principal <$> V3.upload principal (sourceRequestBody req)

downloadAssetV3 :: UserId ::: Public.AssetKey ::: Maybe Public.AssetToken -> Handler Response
downloadAssetV3 (usr ::: key ::: tok) = do
  url <- V3.download (V3.UserPrincipal usr) key tok
  redirect url

deleteAssetV3 :: UserId ::: Public.AssetKey -> Handler Response
deleteAssetV3 (usr ::: key) = do
  V3.delete (V3.UserPrincipal usr) key
  return empty

renewTokenV3 :: UserId ::: Public.AssetKey -> Handler Response
renewTokenV3 (usr ::: key) = do
  tok <- V3.renewToken (V3.UserPrincipal usr) key
  return $ json (Public.NewAssetToken tok)

deleteTokenV3 :: UserId ::: Public.AssetKey -> Handler Response
deleteTokenV3 (usr ::: key) = do
  V3.deleteToken (V3.UserPrincipal usr) key
  return empty

--------------------------------------------------------------------------------
-- Provider API Handlers

providerUploadV3 :: ProviderId ::: Request -> Handler Response
providerUploadV3 (prv ::: req) = do
  let principal = V3.ProviderPrincipal prv
  assetResponse principal <$> V3.upload principal (sourceRequestBody req)

providerDownloadV3 :: ProviderId ::: Public.AssetKey ::: Maybe Public.AssetToken -> Handler Response
providerDownloadV3 (prv ::: key ::: tok) = do
  url <- V3.download (V3.ProviderPrincipal prv) key tok
  redirect url

providerDeleteV3 :: ProviderId ::: Public.AssetKey -> Handler Response
providerDeleteV3 (prv ::: key) = do
  V3.delete (V3.ProviderPrincipal prv) key
  return empty

--------------------------------------------------------------------------------
-- Bot API Handlers

botUploadV3 :: BotId ::: Request -> Handler Response
botUploadV3 (bot ::: req) = do
  let principal = V3.BotPrincipal bot
  assetResponse principal <$> V3.upload principal (sourceRequestBody req)

botDownloadV3 :: BotId ::: Public.AssetKey ::: Maybe Public.AssetToken -> Handler Response
botDownloadV3 (bot ::: key ::: tok) = do
  url <- V3.download (V3.BotPrincipal bot) key tok
  redirect url

botDeleteV3 :: BotId ::: Public.AssetKey -> Handler Response
botDeleteV3 (bot ::: key) = do
  V3.delete (V3.BotPrincipal bot) key
  return empty

--------------------------------------------------------------------------------
-- Helpers

assetResponse :: V3.Principal -> Public.Asset -> Response
assetResponse prc asset =
  setStatus status201 . loc (asset ^. Public.assetKey) $ json asset
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
