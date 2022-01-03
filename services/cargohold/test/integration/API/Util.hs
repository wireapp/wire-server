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
import qualified CargoHold.Types.V3 as V3
import qualified Codec.MIME.Parse as MIME
import qualified Codec.MIME.Type as MIME
import Data.ByteString.Builder
import Data.ByteString.Conversion
import qualified Data.ByteString.Lazy as Lazy
import Data.Id
import Data.Qualified
import Data.Text.Encoding (decodeLatin1)
import qualified Data.UUID as UUID
import Imports hiding (head)
import Network.HTTP.Types.Header
import Network.HTTP.Types.Method
import TestSetup

uploadSimple ::
  CargoHold ->
  UserId ->
  V3.AssetSettings ->
  (MIME.Type, ByteString) ->
  Http (Response (Maybe Lazy.ByteString))
uploadSimple c usr sets (ct, bs) =
  let mp = V3.buildMultipartBody sets ct (Lazy.fromStrict bs)
   in uploadRaw c usr (toLazyByteString mp)

decodeHeaderOrFail :: (HasCallStack, FromByteString a) => HeaderName -> Response b -> a
decodeHeaderOrFail h =
  fromMaybe (error $ "decodeHeaderOrFail: missing or invalid header: " ++ show h)
    . fromByteString
    . getHeader' h

uploadRaw ::
  CargoHold ->
  UserId ->
  Lazy.ByteString ->
  Http (Response (Maybe Lazy.ByteString))
uploadRaw c usr bs =
  post $
    c
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

deleteAssetV3 :: CargoHold -> UserId -> Qualified V3.AssetKey -> Http (Response (Maybe Lazy.ByteString))
deleteAssetV3 c u k = delete $ c . zUser u . paths ["assets", "v3", toByteString' (qUnqualified k)]

deleteAsset :: CargoHold -> UserId -> Qualified V3.AssetKey -> Http (Response (Maybe Lazy.ByteString))
deleteAsset c u k =
  delete $
    c . zUser u
      . paths
        [ "assets",
          "v4",
          toByteString' (qDomain k),
          toByteString' (qUnqualified k)
        ]
