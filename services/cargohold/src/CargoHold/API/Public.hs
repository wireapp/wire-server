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
import CargoHold.App
import Control.Error
import Data.ByteString.Conversion
import Data.Id
import Data.Predicate
import qualified Data.Swagger.Build.Api as Doc
import Data.Text.Encoding (decodeLatin1)
import Imports hiding (head)
import Network.HTTP.Types.Status
import Network.Wai (Response)
import Network.Wai.Predicate hiding (Error, setStatus)
import Network.Wai.Routing
import Network.Wai.Utilities hiding (message)
import Network.Wai.Utilities.Swagger (mkSwaggerApi)
import URI.ByteString

-- FUTUREWORK: restore (and servantify) resumable upload functionality, removed
-- in https://github.com/wireapp/wire-server/pull/1998

--------------------------------------------------------------------------------
-- Wai routes

sitemap :: Routes Doc.ApiBuilder Handler ()
sitemap = do
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

--------------------------------------------------------------------------------
-- Helpers

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
