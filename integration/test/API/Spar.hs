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

data CreateScimToken = CreateScimToken
  { password :: String,
    description :: Maybe String,
    name :: Maybe String,
    idp :: Maybe String
  }
  deriving stock (Generic, Show)

instance Default CreateScimToken where
  def = CreateScimToken defPassword (Just "integration test") Nothing Nothing

instance ToJSON CreateScimToken where
  toJSON = genericToJSON $ defaultOptions {fieldLabelModifier = camelTo2 '_'}

-- | https://staging-nginz-https.zinfra.io/v5/api/swagger-ui/#/default/post_scim_auth_tokens
createScimToken :: (HasCallStack, MakesValue caller) => caller -> CreateScimToken -> App Response
createScimToken caller payload = do
  req <- baseRequest caller Spar Versioned "/scim/auth-tokens"
  j <- make payload
  submit "POST" $ req & addJSON j

putScimTokenName :: (HasCallStack, MakesValue caller) => caller -> String -> String -> App Response
putScimTokenName caller token name = do
  req <- baseRequest caller Spar Versioned $ joinHttpPath ["scim", "auth-tokens", token]
  submit "PUT" $ req & addJSONObject ["name" .= name]

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

getScimUser :: (HasCallStack, MakesValue domain) => domain -> String -> String -> App Response
getScimUser domain scimToken uid = do
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
