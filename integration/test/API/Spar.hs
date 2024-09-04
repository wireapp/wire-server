module API.Spar where

import API.Common (defPassword)
import Data.String.Conversions.Monomorphic (fromLT)
import GHC.Stack
import qualified SAML2.WebSSO as SAML
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

findUsersByExternalId :: (HasCallStack, MakesValue domain) => domain -> String -> String -> App Response
findUsersByExternalId domain scimToken externalId = do
  req <- baseRequest domain Spar Versioned "/scim/v2/Users"
  submit "GET" $ req
    & addQueryParams [("filter", "externalId eq \"" <> externalId <> "\"")]
    & addHeader "Authorization" ("Bearer " <> scimToken)
    & addHeader "Accept" "application/scim+json"

getUser :: (HasCallStack, MakesValue domain) => domain -> String -> String -> App Response
getUser domain scimToken uid = do
  req <- baseRequest domain Spar Versioned $ joinHttpPath ["scim", "v2", "Users", uid]
  submit "GET" $ req
    & addHeader "Authorization" ("Bearer " <> scimToken)
    & addHeader "Accept" "application/scim+json"

updateScimUser :: (HasCallStack, MakesValue domain, MakesValue scimUser) => domain -> String -> String -> scimUser -> App Response
updateScimUser domain scimToken userId scimUser = do
  req <- baseRequest domain Spar Versioned $ joinHttpPath ["scim", "v2", "Users", userId]
  body <- make scimUser
  submit "PUT" $ req
    & addJSON body . addHeader "Authorization" ("Bearer " <> scimToken)
    & addHeader "Accept" "application/scim+json"

createIdp :: (HasCallStack, MakesValue user) => user -> SAML.IdPMetadata -> App Response
createIdp user metadata = do
  req <- baseRequest user Spar Unversioned "/identity-providers"
  submit "POST" $ req
    & addQueryParams [("api_version", "v2")]
    & addXML (fromLT $ SAML.encode metadata)
    & addHeader "Content-Type" "application/xml"
