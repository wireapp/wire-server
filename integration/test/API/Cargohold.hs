module API.Cargohold where

import qualified Codec.MIME.Type as MIME
import qualified Data.Aeson as Aeson
import Data.ByteString.Builder
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBSC
import qualified Data.Text as T
import Data.Time.Clock
import GHC.Stack
import qualified Network.HTTP.Client as HTTP
import Testlib.Prelude

type LByteString = LBS.ByteString

uploadAsset :: (HasCallStack, MakesValue user) => user -> App Response
uploadAsset user = do
  uid <- user & objId
  req <- baseRequest user Cargohold Versioned "/assets"
  submit "POST" $
    req
      & zUser uid
      & addBody txtAsset (mimeTypeToString multipartMixedMime)
  where
    txtAsset :: HTTP.RequestBody
    txtAsset =
      buildUploadAssetRequestBody
        True
        Nothing
        (LBSC.pack "Hello World!")
        textPlainMime

    textPlainMime :: MIME.MIMEType
    textPlainMime = MIME.Text $ T.pack "plain"

    multipartMixedMime :: MIME.MIMEType
    multipartMixedMime = MIME.Multipart MIME.Mixed

mimeTypeToString :: MIME.MIMEType -> String
mimeTypeToString = T.unpack . MIME.showMIMEType

buildUploadAssetRequestBody :: Bool -> Maybe NominalDiffTime -> LByteString -> MIME.MIMEType -> HTTP.RequestBody
buildUploadAssetRequestBody isPublic mbRetention body mimeType =
  buildMultipartBody header body mimeType
  where
    header :: Aeson.Value
    header =
      Aeson.object
        [ "public" .= isPublic,
          "retention" .= mbRetention
        ]

-- | Build a complete @multipart/mixed@ request body for a one-shot,
-- non-resumable asset upload.
buildMultipartBody :: Aeson.Value -> LByteString -> MIME.MIMEType -> HTTP.RequestBody
buildMultipartBody header body bodyMimeType =
  HTTP.RequestBodyLBS . toLazyByteString $
    beginMultipartBody <> lazyByteString body <> endMultipartBody
  where
    -- \| Begin building a @multipart/mixed@ request body for a non-resumable upload.
    -- The returned 'Builder' can be immediately followed by the actual asset bytes.
    beginMultipartBody :: Builder
    beginMultipartBody =
      stringUtf8
        "--frontier\r\n\
        \Content-Type: application/json\r\n\
        \Content-Length: "
        <> int64Dec (LBS.length headerJson)
        <> stringUtf8
          "\r\n\
          \\r\n"
        <> lazyByteString (Aeson.encode header)
        <> stringUtf8
          "\r\n\
          \--frontier\r\n\
          \Content-Type: "
        <> stringUtf8 (mimeTypeToString bodyMimeType)
        <> stringUtf8
          "\r\n\
          \Content-Length: "
        <> int64Dec (LBS.length body)
        <> stringUtf8
          "\r\n\
          \\r\n"
      where
        headerJson = Aeson.encode header

    -- \| The trailer of a non-resumable @multipart/mixed@ request body initiated
    -- via 'beginMultipartBody'.
    endMultipartBody :: Builder
    endMultipartBody = stringUtf8 "\r\n--frontier--\r\n"

downloadAsset :: (HasCallStack, MakesValue user, MakesValue key) => user -> key -> (HTTP.Request -> HTTP.Request) -> App Response
downloadAsset user key trans = downloadAsset' user key "nginz-https.example.com" trans

downloadAsset' :: (HasCallStack, MakesValue user, MakesValue key) => user -> key -> String -> (HTTP.Request -> HTTP.Request) -> App Response
downloadAsset' user key zHostHeader trans = do
  uid <- user & objId
  key' <- key & asString
  req <- baseRequest user Cargohold Versioned $ "/assets/example.com/" ++ key'
  submit "GET" $
    req
      & zUser uid
      & zHost zHostHeader
      & trans
