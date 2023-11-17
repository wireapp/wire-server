module API.Cargohold where

import Codec.MIME.Type qualified as MIME
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBSC
import Data.Text qualified as T
import GHC.Stack
import Network.HTTP.Client qualified as HTTP
import Test.Cargohold.API.Util
import Testlib.Prelude
import UnliftIO (catch)

type LByteString = LBS.ByteString

uploadAssetV3 :: (HasCallStack, MakesValue user, MakesValue assetRetention) => user -> Bool -> assetRetention -> MIME.MIMEType -> LByteString -> App Response
uploadAssetV3 user isPublic retention mimeType bdy = do
  uid <- user & objId
  req <- baseRequest user Cargohold (ExplicitVersion 1) "/assets/v3"
  body <- buildUploadAssetRequestBody isPublic retention bdy mimeType
  submit "POST" $
    req
      & zUser uid
      & addBody body multipartMixedMime
  where
    multipartMixedMime :: String
    multipartMixedMime = "multipart/mixed; boundary=" <> multipartBoundary

uploadAsset :: (HasCallStack, MakesValue user) => user -> App Response
uploadAsset user = do
  uid <- user & objId
  req <- baseRequest user Cargohold Versioned "/assets"
  bdy <- txtAsset
  submit "POST" $
    req
      & zUser uid
      & addBody bdy multipartMixedMime
  where
    txtAsset :: HasCallStack => App HTTP.RequestBody
    txtAsset =
      buildUploadAssetRequestBody
        True
        (Nothing :: Maybe String)
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

downloadAsset :: (HasCallStack, MakesValue user, MakesValue key, MakesValue tok) => user -> key -> tok -> App Response
downloadAsset user key tok = do
  uid <- objId user
  domain <- (pure <$> objDomain key) `catch` (\(_e@(AssertionFailure {})) -> pure Nothing)
  key' <- (key %. "id" & asString) `catch` (\(_e@(AssertionFailure {})) -> asString key)
  tok' <- pure <$> asString tok
  req <- baseRequest user Cargohold Versioned $ "/assets" ++ (maybe "" (\d -> "/" ++ d) domain) ++ "/" ++ key'
  submit "GET" $
    req
      & zUser uid
      & tokenParam tok'
