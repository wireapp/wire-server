module API.Nginz where

import Testlib.Prelude

getSystemSettingsUnAuthorized :: App Response
getSystemSettingsUnAuthorized = do
  req <- baseRequest OwnDomain Nginz Versioned "/system/settings/unauthorized"
  submit "GET" req
