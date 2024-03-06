module API.Spar where

import API.Common (defPassword)
import GHC.Stack
import Testlib.Prelude

-- | https://staging-nginz-https.zinfra.io/v6/api/swagger-ui/#/default/get_scim_auth_tokens
getScimTokens :: (HasCallStack, MakesValue caller) => caller -> App Response
getScimTokens caller = do
  req <- baseRequest caller Spar Versioned "/scim/auth-tokens"
  submit "GET" req

-- https://staging-nginz-https.zinfra.io/v5/api/swagger-ui/#/default/post_scim_auth_tokens
createScimToken :: (HasCallStack, MakesValue caller) => caller -> App Response
createScimToken caller = do
  req <- baseRequest caller Spar Versioned "/scim/auth-tokens"
  submit "POST" $ req & addJSONObject ["password" .= defPassword, "description" .= "integration test"]

createScimUser :: (HasCallStack, MakesValue domain, MakesValue scimUser) => domain -> String -> scimUser -> App Response
createScimUser domain token scimUser = do
  req <- baseRequest domain Spar Versioned "/scim/v2/Users"
  body <- make scimUser
  submit "POST" $ req & addJSON body . addHeader "Authorization" ("Bearer " <> token)
