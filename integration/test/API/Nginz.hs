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

module API.Nginz where

import API.Cargohold
import qualified Codec.MIME.Type as MIME
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Network.HTTP.Client as HTTP
import Testlib.Prelude

getSystemSettingsUnAuthorized :: (HasCallStack, MakesValue domain) => domain -> App Response
getSystemSettingsUnAuthorized domain = do
  req <- baseRequest domain Nginz Versioned "/system/settings/unauthorized"
  submit "GET" req

login :: (HasCallStack, MakesValue domain, MakesValue email, MakesValue password) => domain -> email -> password -> App Response
login domain email pw = do
  req <- rawBaseNginzRequest domain Unversioned "/login"
  emailStr <- make email >>= asString
  pwStr <- make pw >>= asString
  submit "POST" (req & addJSONObject ["email" .= emailStr, "password" .= pwStr, "label" .= "auth"])

loginWith2ndFactor :: (HasCallStack, MakesValue domain, MakesValue email, MakesValue password, MakesValue sndFactor) => domain -> email -> password -> sndFactor -> App Response
loginWith2ndFactor domain email pw sf = do
  req <- rawBaseNginzRequest domain Unversioned "/login"
  emailStr <- make email >>= asString
  pwStr <- make pw >>= asString
  sfStr <- make sf >>= asString
  submit "POST" (req & addJSONObject ["email" .= emailStr, "password" .= pwStr, "label" .= "auth", "verification_code" .= sfStr])

access :: (HasCallStack, MakesValue domain, MakesValue cookie) => domain -> cookie -> App Response
access domain cookie = do
  req <- rawBaseNginzRequest domain Unversioned "/access"
  cookieStr <- make cookie >>= asString
  submit "POST" (req & setCookie cookieStr)

logout :: (HasCallStack, MakesValue domain, MakesValue cookie, MakesValue token) => domain -> cookie -> token -> App Response
logout d c t = do
  req <- rawBaseNginzRequest d Unversioned "/access/logout"
  cookie <- make c & asString
  token <- make t & asString
  submit "POST" (req & setCookie cookie & addHeader "Authorization" ("Bearer " <> token))

getConversation :: (HasCallStack, MakesValue user, MakesValue qcnv, MakesValue token) => user -> qcnv -> token -> App Response
getConversation user qcnv t = do
  (domain, cnv) <- objQid qcnv
  token <- make t & asString
  req <- rawBaseNginzRequest user Versioned (joinHttpPath ["conversations", domain, cnv])
  submit "GET" (req & addHeader "Authorization" ("Bearer " <> token))

-- | The endpoints below take an OAuth access token, which is what nginz checks
-- scopes against.  A request with a zauth cookie or token is not affected by
-- any of that; see 'Test.OAuth'.
getSelf :: (HasCallStack, MakesValue user, MakesValue token) => user -> token -> App Response
getSelf user t = do
  token <- make t & asString
  req <- rawBaseNginzRequest user Versioned "/self"
  submit "GET" (req & addHeader "Authorization" ("Bearer " <> token))

postConversation :: (HasCallStack, MakesValue user, MakesValue conv, MakesValue token) => user -> conv -> token -> App Response
postConversation user conv t = do
  token <- make t & asString
  body <- make conv
  req <- rawBaseNginzRequest user Versioned "/conversations"
  submit "POST" (req & addJSON body & addHeader "Authorization" ("Bearer " <> token))

getConversationCode :: (HasCallStack, MakesValue user, MakesValue conv, MakesValue token) => user -> conv -> token -> App Response
getConversationCode user conv t = do
  convId <- objQidObject conv & objId
  token <- make t & asString
  req <- rawBaseNginzRequest user Versioned (joinHttpPath ["conversations", convId, "code"])
  submit "GET" (req & addHeader "Authorization" ("Bearer " <> token))

postConversationCode :: (HasCallStack, MakesValue user, MakesValue conv, MakesValue token) => user -> conv -> token -> App Response
postConversationCode user conv t = do
  convId <- objQidObject conv & objId
  token <- make t & asString
  req <- rawBaseNginzRequest user Versioned (joinHttpPath ["conversations", convId, "code"])
  submit "POST" (req & addJSONObject [] & addHeader "Authorization" ("Bearer " <> token))

deleteConversationCode :: (HasCallStack, MakesValue user, MakesValue conv, MakesValue token) => user -> conv -> token -> App Response
deleteConversationCode user conv t = do
  convId <- objQidObject conv & objId
  token <- make t & asString
  req <- rawBaseNginzRequest user Versioned (joinHttpPath ["conversations", convId, "code"])
  submit "DELETE" (req & addHeader "Authorization" ("Bearer " <> token))

uploadProviderAsset :: (HasCallStack, MakesValue domain) => domain -> String -> String -> App Response
uploadProviderAsset domain cookie payload = do
  req <- rawBaseNginzRequest domain Versioned $ joinHttpPath ["provider", "assets"]
  bdy <- txtAsset payload
  submit "POST"
    $ req
    & setCookie cookie
    & addBody bdy multipartMixedMime

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
