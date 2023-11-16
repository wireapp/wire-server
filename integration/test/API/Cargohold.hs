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
import Test.Cargohold.API.Util
import Testlib.Prelude

type LByteString = LBS.ByteString

uploadAssetV3 :: (HasCallStack, MakesValue user) => user -> Bool -> Maybe NominalDiffTime -> MIME.MIMEType -> LByteString -> App Response
uploadAssetV3 user isPublic retention mimeType bdy = do
  uid <- user & objId
  req <- baseRequest user Cargohold (ExplicitVersion 1) "/assets/v3"
  submit "POST" $
    req
      & zUser uid
      & addBody body multipartMixedMime
  where
    body = buildUploadAssetRequestBody isPublic retention bdy mimeType
    multipartMixedMime :: String
    multipartMixedMime = "multipart/mixed; boundary=" <> multipartBoundary

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
