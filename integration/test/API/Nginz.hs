module API.Nginz where

import Testlib.Prelude

getSystemSettingsUnAuthorized :: (HasCallStack, MakesValue domain) => domain -> App Response
getSystemSettingsUnAuthorized domain = do
  req <- baseRequest domain Nginz Versioned "/system/settings/unauthorized"
  submit "GET" req
