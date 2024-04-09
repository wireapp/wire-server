module API.Nginz where

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
