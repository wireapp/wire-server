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

{-# LANGUAGE OverloadedStrings #-}

module Network.Wire.Client.API.Asset
  ( AssetData,
    AssetKey,
    AssetSettings,
    AssetToken,
    Asset,
    assetKey,
    assetToken,
    assetExpires,
    defAssetSettings,
    setAssetPublic,
    setAssetRetention,
    postAsset,
    getAsset,
  )
where

import Bilge
import CargoHold.Types
import qualified Codec.MIME.Type as MIME
import Data.ByteString.Builder
import Data.ByteString.Conversion
import Data.List.NonEmpty
import Imports
import Network.HTTP.Types.Method
import Network.HTTP.Types.Status hiding (statusCode)
import Network.Wire.Client.HTTP
import Network.Wire.Client.Session

type AssetData = LByteString

postAsset ::
  MonadSession m =>
  MIME.Type ->
  AssetSettings ->
  AssetData ->
  m Asset
postAsset ctyp sets dat = sessionRequest req rsc readBody
  where
    req =
      method POST
        . paths ["assets", "v3"]
        . acceptJson
        . header "Content-Type" "multipart/mixed"
        . body (RequestBodyLBS $ toLazyByteString $ buildMultipartBody sets ctyp dat)
        $ empty
    rsc = status201 :| []

getAsset :: MonadSession m => AssetKey -> Maybe AssetToken -> m (Maybe AssetData)
getAsset key tok = do
  rs <- sessionRequest req rsc consumeBody
  liftIO $ case statusCode rs of
    200 -> maybe (unexpected rs "getAsset: missing body") (return . Just) (responseBody rs)
    404 -> return Nothing
    _ -> unexpected rs "getAsset: response code"
  where
    req =
      method GET
        . paths ["assets", "v3", toByteString' key]
        . maybe id (header "Asset-Token" . toByteString') tok
        $ empty
    rsc = status200 :| [status404]
