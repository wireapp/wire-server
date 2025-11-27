-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
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

module API.Cargohold where

import API.Federator
import qualified Codec.MIME.Parse as MIME
import qualified Codec.MIME.Type as MIME
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as Aeson
import Data.ByteString.Builder
import Data.ByteString.Conversion
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString.Lazy.Char8 as LBSC
import qualified Data.ByteString.Lazy.Char8 as Lazy8
import Data.CaseInsensitive
import Data.String.Conversions
import qualified Data.Text as T
import Data.Text.Encoding (decodeLatin1, decodeUtf8, encodeUtf8, encodeUtf8Builder)
import GHC.Stack
import Network.HTTP.Client (Request (redirectCount, requestHeaders))
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Types.Header (HeaderName)
import Testlib.Prelude
import UnliftIO (catch)

type LByteString = LBS.ByteString

-- UPLOAD

uploadAsset :: (HasCallStack, MakesValue user) => user -> String -> App Response
uploadAsset user payload = do
  uid <- user & objId
  req <- baseRequest user Cargohold Versioned "/assets"
  bdy <- txtAsset payload
  submit "POST"
    $ req
    & zUser uid
    & addBody bdy multipartMixedMime

uploadRaw :: (HasCallStack, MakesValue user) => user -> Lazy.ByteString -> App Response
uploadRaw user bs = do
  uid <- user & objId
  req <- baseRequest user Cargohold Versioned "/assets"
  submit "POST"
    $ req
    & zUser uid
    & contentTypeMixed
    & (\r -> r {HTTP.requestBody = HTTP.RequestBodyLBS bs})

uploadAssetV3 :: (HasCallStack, MakesValue user, MakesValue assetRetention) => user -> Bool -> assetRetention -> MIME.MIMEType -> LByteString -> App Response
uploadAssetV3 user isPublic retention mimeType bdy = do
  uid <- user & objId
  req <- baseRequest user Cargohold (ExplicitVersion 1) "/assets/v3"
  body <- buildUploadAssetRequestBody isPublic retention bdy mimeType
  submit "POST"
    $ req
    & zUser uid
    & addBody body multipartMixedMime

uploadRawV3 ::
  (HasCallStack, MakesValue user) =>
  user ->
  Lazy.ByteString ->
  App Response
uploadRawV3 usr bs = do
  req <- baseRequest usr Cargohold (ExplicitVersion 1) "assets/v3"
  submit "POST" $ req & contentTypeMixed & (\r -> r {HTTP.requestBody = HTTP.RequestBodyLBS bs})

uploadProviderAsset :: (HasCallStack, MakesValue domain) => domain -> String -> String -> App Response
uploadProviderAsset domain pid payload = do
  req <- rawBaseRequest domain Cargohold Versioned $ joinHttpPath ["provider", "assets"]
  bdy <- txtAsset payload
  submit "POST"
    $ req
    & zProvider pid
    & zType "provider"
    & addBody bdy multipartMixedMime

-- DOWNLOAD

downloadAsset' ::
  (HasCallStack, MakesValue user, IsAssetLocation loc, IsAssetToken tok) =>
  user ->
  loc ->
  tok ->
  App Response
downloadAsset' user loc tok = do
  locPath <- locationPathFragment loc
  req <- baseRequest user Cargohold Unversioned $ locPath
  submit "GET" $ req & tokenParam tok & noRedirect

downloadAsset ::
  (HasCallStack, MakesValue user, MakesValue key, MakesValue assetDomain) =>
  user ->
  assetDomain ->
  key ->
  String ->
  (HTTP.Request -> HTTP.Request) ->
  App Response
downloadAsset user assetDomain key zHostHeader trans = do
  domain <- objDomain assetDomain
  key' <- asString key
  req <- baseRequest user Cargohold Versioned $ "/assets/" ++ domain ++ "/" ++ key'
  submit "GET"
    $ req
    & zHost zHostHeader
    & trans

downloadAssetWithQualifiedAssetKey ::
  (HasCallStack, IsAssetToken tok, MakesValue key, MakesValue user) =>
  (HTTP.Request -> HTTP.Request) ->
  user ->
  key ->
  tok ->
  App Response
downloadAssetWithQualifiedAssetKey r user key tok = do
  dom <- key %. "domain" & asString
  keyId <- key %. "id" & asString
  req <- baseRequest user Cargohold (ExplicitVersion 2) $ "assets/" <> dom <> "/" <> keyId
  submit "GET"
    $ req
    & tokenParam tok
    & r

-- DELETE

deleteAssetV3 :: (HasCallStack, MakesValue user, MakesValue key) => user -> key -> App Response
deleteAssetV3 user key = do
  k <- key %. "key" & asString
  req <- baseRequest user Cargohold (ExplicitVersion 1) $ "assets/v3/" <> k
  submit "DELETE" req

deleteAsset :: (HasCallStack, MakesValue user, MakesValue key) => user -> key -> App Response
deleteAsset user key = do
  k <- key %. "key" & asString
  d <- key %. "domain" & asString
  req <- baseRequest user Cargohold Versioned $ "/assets/" <> d <> "/" <> k
  submit "DELETE" req

-- TOKEN

postToken :: (MakesValue user, HasCallStack) => user -> String -> App Response
postToken user key = do
  req <- baseRequest user Cargohold Versioned $ "assets/" <> key <> "/token"
  submit "POST" req

deleteToken :: (MakesValue user, HasCallStack) => user -> String -> App Response
deleteToken user key = do
  req <- baseRequest user Cargohold Versioned $ "assets/" <> key <> "/token"
  submit "DELETE" req

--------------------------------------------------------------------------------
-- FEDERATION

getFederationAsset :: (HasCallStack, MakesValue asset) => asset -> App Response
getFederationAsset ga = do
  req <- rawBaseRequestF OwnDomain cargohold "federation/get-asset"
  bdy <- make ga
  submit "POST"
    $ req
    & addBody (HTTP.RequestBodyLBS $ encode bdy) "application/json"

--------------------------------------------------------------------------------
-- UTIL

uploadSomeAsset :: (HasCallStack, MakesValue user) => user -> App Response
uploadSomeAsset = flip uploadAsset "Hello World!"

txtAsset :: (HasCallStack) => String -> App HTTP.RequestBody
txtAsset payload =
  buildUploadAssetRequestBody
    True
    (Nothing :: Maybe String)
    (LBSC.pack payload)
    textPlainMime

textPlainMime :: MIME.MIMEType
textPlainMime = MIME.Text $ T.pack "plain"

-- This case is a bit special and doesn't fit to MIMEType: We need to define
-- the boundary.
multipartMixedMime :: String
multipartMixedMime = "multipart/mixed; boundary=" <> multipartBoundary

buildUploadAssetRequestBody ::
  (HasCallStack, MakesValue assetRetention) =>
  Bool ->
  assetRetention ->
  LByteString ->
  MIME.MIMEType ->
  App HTTP.RequestBody
buildUploadAssetRequestBody isPublic retention body mimeType = do
  mbRetention <- make retention
  let header' :: Aeson.Value
      header' =
        Aeson.object
          [ "public" .= isPublic,
            "retention" .= mbRetention
          ]
  HTTP.RequestBodyLBS <$> buildMultipartBody header' body mimeType

class IsAssetLocation key where
  locationPathFragment :: key -> App String

instance {-# OVERLAPS #-} IsAssetLocation String where
  locationPathFragment = pure

-- Pick out a path from the value
instance (MakesValue loc) => IsAssetLocation loc where
  locationPathFragment v =
    qualifiedFrag `catch` (\(_e :: SomeException) -> unqualifiedFrag)
    where
      qualifiedFrag = do
        domain <- v %. "domain" & asString
        key <- v %. "key" & asString
        pure $ "v2/assets/" <> domain <> "/" <> key
      unqualifiedFrag = do
        key <- asString v
        pure $ "v1/asssets/v3/" <> key

noRedirect :: Request -> Request
noRedirect r = r {redirectCount = 0}

uploadSimpleV3 ::
  (HasCallStack, MakesValue user, MakesValue settings) =>
  user ->
  settings ->
  (MIME.MIMEType, Lazy8.ByteString) ->
  App Response
uploadSimpleV3 usr sts (ct, bs) = do
  body <- buildMultipartBody sts bs ct
  uploadRawV3 usr body

uploadSimple ::
  (HasCallStack, MakesValue user, MakesValue settings) =>
  user ->
  settings ->
  (MIME.MIMEType, Lazy8.ByteString) ->
  App Response
uploadSimple usr sts (ct, bs) = do
  body <- buildMultipartBody sts bs ct
  uploadRaw usr body

decodeHeaderOrFail :: (HasCallStack, FromByteString a) => HeaderName -> Response -> a
decodeHeaderOrFail h =
  fromMaybe (error $ "decodeHeaderOrFail: missing or invalid header: " ++ show h)
    . fromByteString
    . getHeader' h

-- | Like 'getHeader', but if no value exists for the given key, return the
-- static ByteString "NO_HEADER_VALUE".
getHeader' :: HeaderName -> Response -> ByteString
getHeader' h = fromMaybe (cs "NO_HEADER_VALUE") . getHeader h

getHeader :: HeaderName -> Response -> Maybe ByteString
getHeader h = fmap snd . find ((h ==) . fst) . headers

getContentType :: Response -> Maybe MIME.Type
getContentType = MIME.parseContentType . decodeLatin1 . getHeader' (mk $ cs "Content-Type")

applicationText :: MIME.MIMEType
applicationText = MIME.Application $ cs "text"

applicationOctetStream :: MIME.MIMEType
applicationOctetStream = MIME.Application $ cs "octet-stream"

applicationOctetStream' :: MIME.Type
applicationOctetStream' = MIME.Type applicationOctetStream []

header :: String -> String -> Request -> Request
header name value req =
  req {requestHeaders = (mk $ cs name, cs value) : requestHeaders req}

class IsAssetToken tok where
  tokenParam :: tok -> Request -> Request

instance IsAssetToken () where
  tokenParam _ = id

instance IsAssetToken String where
  tokenParam = header "Asset-Token"

instance (IsAssetToken a, IsAssetToken b) => IsAssetToken (Either a b) where
  tokenParam = either tokenParam tokenParam

instance IsAssetToken Value where
  tokenParam v =
    case v of
      String s -> header h $ cs s
      Object o -> maybe id tokenParam $ Aeson.lookup (fromString "token") o
      _ -> error "Non-matching Asset-Token value"
    where
      h = "Asset-Token"

instance IsAssetToken (Request -> Request) where
  tokenParam = id

-- | Build a complete @multipart/mixed@ request body for a one-shot,
-- non-resumable asset upload.
buildMultipartBody :: (HasCallStack, MakesValue header) => header -> Lazy.ByteString -> MIME.MIMEType -> App Lazy.ByteString
buildMultipartBody header' body bodyMimeType = do
  h <- make header'
  let headerJson = Aeson.encode h

      render :: Builder
      render = renderBody <> endMultipartBody

      endMultipartBody :: Builder
      endMultipartBody = lineBreak <> boundary <> stringUtf8 "--" <> lineBreak

      renderBody :: Builder
      renderBody = mconcat $ renderPart <$> multipartContent

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

  pure $ toLazyByteString render

multipartBoundary :: String
multipartBoundary = "frontier"

buildMultipartBody' :: Value -> MIME.Type -> LBS.ByteString -> Builder
buildMultipartBody' sets typ bs =
  beginMultipartBody sets typ (fromIntegral $ LBS.length bs) <> lazyByteString bs <> endMultipartBody'

-- | Begin building a @multipart/mixed@ request body for a non-resumable upload.
-- The returned 'Builder' can be immediately followed by the actual asset bytes.
beginMultipartBody :: Value -> MIME.Type -> Word -> Builder
beginMultipartBody sets t l =
  byteString
    ( cs
        "--frontier\r\n\
        \Content-Type: application/json\r\n\
        \Content-Length: "
    )
    <> int64Dec (LBS.length settingsJson)
    <> byteString
      ( cs
          "\r\n\
          \\r\n"
      )
    <> lazyByteString settingsJson
    <> byteString
      ( cs
          "\r\n\
          \--frontier\r\n\
          \Content-Type: "
      )
    <> byteString (encodeUtf8 (MIME.showType t))
    <> byteString
      ( cs
          "\r\n\
          \Content-Length: "
      )
    <> wordDec l
    <> byteString
      ( cs
          "\r\n\
          \\r\n"
      )
  where
    settingsJson = Aeson.encode sets

-- | The trailer of a non-resumable @multipart/mixed@ request body initiated
-- via 'beginMultipartBody'.
endMultipartBody' :: Builder
endMultipartBody' = byteString $ cs "\r\n--frontier--\r\n"
