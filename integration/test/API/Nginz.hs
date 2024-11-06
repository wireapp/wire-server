module API.Nginz where

import qualified Codec.MIME.Type as MIME
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBSC
import qualified Data.Text as T
import qualified Network.HTTP.Client as HTTP
import Test.Cargohold.API.Util (buildMultipartBody, multipartBoundary)
import Testlib.Prelude

getSystemSettingsUnAuthorized :: (HasCallStack, MakesValue domain) => domain -> App Response
getSystemSettingsUnAuthorized domain = do
  req <- baseRequest domain Nginz Versioned "/system/settings/unauthorized"
  submit "GET" req

login :: (HasCallStack, MakesValue domain, MakesValue email, MakesValue password) => domain -> email -> password -> App Response
login domain email pw = do
  req <- rawBaseRequest domain Nginz Unversioned "/login"
  emailStr <- make email >>= asString
  pwStr <- make pw >>= asString
  submit "POST" (req & addJSONObject ["email" .= emailStr, "password" .= pwStr, "label" .= "auth"])

loginWith2ndFactor :: (HasCallStack, MakesValue domain, MakesValue email, MakesValue password, MakesValue sndFactor) => domain -> email -> password -> sndFactor -> App Response
loginWith2ndFactor domain email pw sf = do
  req <- rawBaseRequest domain Nginz Unversioned "/login"
  emailStr <- make email >>= asString
  pwStr <- make pw >>= asString
  sfStr <- make sf >>= asString
  submit "POST" (req & addJSONObject ["email" .= emailStr, "password" .= pwStr, "label" .= "auth", "verification_code" .= sfStr])

access :: (HasCallStack, MakesValue domain, MakesValue cookie) => domain -> cookie -> App Response
access domain cookie = do
  req <- rawBaseRequest domain Nginz Unversioned "/access"
  cookieStr <- make cookie >>= asString
  submit "POST" (req & setCookie cookieStr)

logout :: (HasCallStack, MakesValue domain, MakesValue cookie, MakesValue token) => domain -> cookie -> token -> App Response
logout d c t = do
  req <- rawBaseRequest d Nginz Unversioned "/access/logout"
  cookie <- make c & asString
  token <- make t & asString
  submit "POST" (req & setCookie cookie & addHeader "Authorization" ("Bearer " <> token))

getConversation :: (HasCallStack, MakesValue user, MakesValue qcnv, MakesValue token) => user -> qcnv -> token -> App Response
getConversation user qcnv t = do
  (domain, cnv) <- objQid qcnv
  token <- make t & asString
  req <- rawBaseRequest user Nginz Versioned (joinHttpPath ["conversations", domain, cnv])
  submit "GET" (req & addHeader "Authorization" ("Bearer " <> token))

uploadProviderAsset :: (HasCallStack, MakesValue domain) => domain -> String -> String -> App Response
uploadProviderAsset domain cookie payload = do
  req <- rawBaseRequest domain Nginz Versioned $ joinHttpPath ["provider", "assets"]
  bdy <- txtAsset payload
  submit "POST"
    $ req
    & setCookie cookie
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
  LBS.ByteString ->
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

upgradePersonalToTeam :: (HasCallStack, MakesValue user) => user -> String -> String -> App Response
upgradePersonalToTeam user token name = do
  req <- baseRequest user Brig Versioned $ joinHttpPath ["upgrade-personal-to-team"]
  submit "POST" $ req & addJSONObject ["name" .= name, "icon" .= "default"] & addHeader "Authorization" ("Bearer " <> token)
