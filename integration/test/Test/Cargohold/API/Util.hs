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

module Test.Cargohold.API.Util where

import Codec.MIME.Parse qualified as MIME
import Codec.MIME.Type qualified as MIME
import Data.Aeson qualified as Aeson
import Data.ByteString.Builder
import Data.ByteString.Conversion
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy qualified as Lazy
import Data.ByteString.Lazy.Char8 qualified as Lazy8
import Data.String.Conversions
import Data.Text qualified as T
import Data.Text.Encoding (decodeLatin1)
import Data.Text.Lazy.Encoding (encodeUtf8Builder)
import GHC.Stack
import Network.HTTP.Client (Request (requestHeaders))
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Types.Header
import Testlib.Prelude

uploadSimple ::
  (HasCallStack, MakesValue user, MakesValue settings) =>
  user ->
  settings ->
  (MIME.MIMEType, String) ->
  App Response
uploadSimple usr sts (ct, bs) =
  uploadRaw usr $ buildMultipartBody sts (Lazy8.pack bs) ct

decodeHeaderOrFail :: (HasCallStack, FromByteString a) => HeaderName -> Response -> a
decodeHeaderOrFail h =
  fromMaybe (error $ "decodeHeaderOrFail: missing or invalid header: " ++ show h)
    . fromByteString
    . getHeader' h

-- | Like 'getHeader', but if no value exists for the given key, return the
-- static ByteString \"NO_HEADER_VALUE\".
getHeader' :: HeaderName -> Response -> ByteString
getHeader' h = fromMaybe (cs "NO_HEADER_VALUE") . getHeader h

getHeader :: HeaderName -> Response -> Maybe ByteString
getHeader h = fmap snd . find ((h ==) . fst) . headers

uploadRaw ::
  (HasCallStack, MakesValue user) =>
  user ->
  Lazy.ByteString ->
  App Response
uploadRaw usr bs = do
  req <- baseRequest usr Cargohold (ExplicitVersion 1) "assets/v3"
  submit "POST" $
    req
      & contentTypeMixed
      & (\r -> r {HTTP.requestBody = HTTP.RequestBodyLBS bs})

getContentType :: Response -> Maybe MIME.Type
getContentType = MIME.parseContentType . decodeLatin1 . getHeader' (cs "Content-Type")

applicationText :: MIME.Type
applicationText = MIME.Type (MIME.Application $ cs "text") []

applicationOctetStream :: MIME.Type
applicationOctetStream = MIME.Type (MIME.Application $ cs "octet-stream") []

deleteAssetV3 :: (HasCallStack, MakesValue user, MakesValue key) => user -> key -> App Response
deleteAssetV3 user key = do
  k <- key %. "id" & asString
  req <- baseRequest user Cargohold (ExplicitVersion 1) $ "assets/v3/" <> k
  submit "DELETE" req

deleteAsset :: (HasCallStack, MakesValue user, MakesValue key) => user -> key -> App Response
deleteAsset user key = do
  k <- key %. "id" & asString
  d <- key %. "domain" & asString
  req <- baseRequest user Cargohold Versioned $ "/assets/" <> d <> "/" <> show k
  submit "DELETE" req

class IsAssetToken tok where
  tokenParam :: tok -> HTTP.Request -> HTTP.Request

instance IsAssetToken (Maybe String) where
  tokenParam = maybe id (header "Asset-Token")

header :: String -> Request -> Request
header name req =
  req {requestHeaders = (cs name, _) : requestHeaders req}

downloadAssetWithAssetKey ::
  (HasCallStack, MakesValue user) =>
  (HTTP.Request -> HTTP.Request) ->
  user ->
  String ->
  App Response
downloadAssetWithAssetKey r user tok = do
  req <- baseRequest user Cargohold (ExplicitVersion 1) $ "asserts/v3/" <> tok
  submit "GET" $
    req
      & tokenParam tok

downloadAssetWithQualifiedAssetKey ::
  (HasCallStack, MakesValue tok, MakesValue user) =>
  (HTTP.Request -> HTTP.Request) ->
  user ->
  Maybe String ->
  App Response
downloadAssetWithQualifiedAssetKey r user tok = do
  dom <- tok %. "domain" & asString
  key <- tok %. "id" & asString
  req <- baseRequest user Cargohold (ExplicitVersion 2) $ "assets/" <> dom <> "/" <> key
  submit "GET" $
    req
      & tokenParam tok

postToken :: (MakesValue user, HasCallStack) => user -> String -> App Response
postToken user key = do
  req <- baseRequest user Cargohold Versioned $ "assets/" <> key <> "/token"
  submit "POST" req

deleteToken :: (MakesValue user, HasCallStack) => user -> String -> App Response
deleteToken user key = do
  req <- baseRequest user Cargohold Versioned $ "asserts/" <> key <> "/token"
  submit "DELETE" req

-- | Build a complete @multipart/mixed@ request body for a one-shot,
-- non-resumable asset upload.
buildMultipartBody :: Value -> Lazy.ByteString -> MIME.MIMEType -> Lazy.ByteString
buildMultipartBody header body bodyMimeType = toLazyByteString render
  where
    headerJson = Aeson.encode header

    render :: Builder
    render = renderBody <> endMultipartBody

    endMultipartBody :: Builder
    endMultipartBody = lineBreak <> boundary <> stringUtf8 "--" <> lineBreak

    renderBody :: Builder
    renderBody = mconcat $ map renderPart multipartContent

    renderPart :: MIME.MIMEValue -> Builder
    renderPart v =
      boundary
        <> lineBreak
        <> (contentType . MIME.mime_val_type) v
        <> lineBreak
        <> (headers . MIME.mime_val_headers) v
        <> lineBreak
        <> lineBreak
        <> (content . MIME.mime_val_content) v
        <> lineBreak

    boundary :: Builder
    boundary = stringUtf8 "--" <> stringUtf8 multipartBoundary

    lineBreak :: Builder
    lineBreak = stringUtf8 "\r\n"

    contentType :: MIME.Type -> Builder
    contentType t = stringUtf8 "Content-Type: " <> (encodeUtf8Builder . MIME.showType) t

    headers :: [MIME.MIMEParam] -> Builder
    headers [] = mempty
    headers (x : xs) = renderHeader x <> headers xs

    renderHeader :: MIME.MIMEParam -> Builder
    renderHeader p =
      encodeUtf8Builder (MIME.paramName p)
        <> stringUtf8 ": "
        <> encodeUtf8Builder (MIME.paramValue p)

    content :: MIME.MIMEContent -> Builder
    content (MIME.Single c) = encodeUtf8Builder c
    content (MIME.Multi _) = error "Not implemented."

    multipartContent :: [MIME.MIMEValue]
    multipartContent =
      [ part (MIME.Application (T.pack "json")) headerJson,
        part bodyMimeType body
      ]

    part :: MIME.MIMEType -> Lazy.ByteString -> MIME.MIMEValue
    part mtype c =
      MIME.nullMIMEValue
        { MIME.mime_val_type = MIME.Type mtype [],
          MIME.mime_val_headers = [MIME.MIMEParam (T.pack "Content-Length") ((T.pack . show . LBS.length) c)],
          MIME.mime_val_content = MIME.Single ((decodeUtf8 . LBS.toStrict) c)
        }
