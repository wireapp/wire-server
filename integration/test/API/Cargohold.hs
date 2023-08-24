module API.Cargohold where

import Codec.MIME.Type qualified as MIME
import Data.Aeson qualified as Aeson
import Data.ByteString.Builder
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBSC
import Data.Text qualified as T
import Data.Text.Encoding
import Data.Time.Clock
import GHC.Stack
import Network.HTTP.Client qualified as HTTP
import Testlib.Prelude

type LByteString = LBS.ByteString

uploadAsset :: (HasCallStack, MakesValue user) => user -> App Response
uploadAsset user = do
  uid <- user & objId
  req <- baseRequest user Cargohold Versioned "/assets"
  submit "POST" $
    req
      & zUser uid
      & addBody txtAsset multipartMixedMime
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

    -- This case is a bit special and doesn't fit to MIMEType: We need to define
    -- the boundary.
    multipartMixedMime :: String
    multipartMixedMime = "multipart/mixed; boundary=" <> multipartBoundary

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

multipartBoundary :: String
multipartBoundary = "frontier"

-- | Build a complete @multipart/mixed@ request body for a one-shot,
-- non-resumable asset upload.
buildMultipartBody :: Aeson.Value -> LByteString -> MIME.MIMEType -> HTTP.RequestBody
buildMultipartBody header body bodyMimeType =
  HTTP.RequestBodyLBS . toLazyByteString $ render
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

    part :: MIME.MIMEType -> LByteString -> MIME.MIMEValue
    part mtype c =
      MIME.nullMIMEValue
        { MIME.mime_val_type = MIME.Type mtype [],
          MIME.mime_val_headers = [MIME.MIMEParam (T.pack "Content-Length") ((T.pack . show . LBS.length) c)],
          MIME.mime_val_content = MIME.Single ((decodeUtf8 . LBS.toStrict) c)
        }

downloadAsset :: (HasCallStack, MakesValue user, MakesValue key, MakesValue assetDomain) => user -> assetDomain -> key -> String -> (HTTP.Request -> HTTP.Request) -> App Response
downloadAsset user assetDomain key zHostHeader trans = do
  uid <- objId user
  domain <- objDomain assetDomain
  key' <- asString key
  req <- baseRequest user Cargohold Versioned $ "/assets/" ++ domain ++ "/" ++ key'
  submit "GET" $
    req
      & zUser uid
      & zHost zHostHeader
      & trans
