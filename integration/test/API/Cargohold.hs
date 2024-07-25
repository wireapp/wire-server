module API.Cargohold where

import qualified Codec.MIME.Type as MIME
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBSC
import qualified Data.Text as T
import GHC.Stack
import Network.HTTP.Client (Request (redirectCount))
import qualified Network.HTTP.Client as HTTP
import Test.Cargohold.API.Util
import Testlib.Prelude
import UnliftIO (catch)

type LByteString = LBS.ByteString

uploadAsset :: (HasCallStack, MakesValue user) => user -> App Response
uploadAsset = flip uploadFreshAsset "Hello World!"

uploadProviderAsset :: (HasCallStack, MakesValue domain) => domain -> String -> String -> App Response
uploadProviderAsset domain pid payload = do
  req <- rawBaseRequest domain Cargohold Versioned $ joinHttpPath ["provider", "assets"]
  bdy <- txtAsset payload
  submit "POST"
    $ req
    & zProvider pid
    & zType "provider"
    & addBody bdy multipartMixedMime

uploadFreshAsset :: (HasCallStack, MakesValue user) => user -> String -> App Response
uploadFreshAsset user payload = do
  uid <- user & objId
  req <- baseRequest user Cargohold Versioned "/assets"
  bdy <- txtAsset payload
  submit "POST"
    $ req
    & zUser uid
    & addBody bdy multipartMixedMime

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
