module API.Spar where

import API.Common (defPassword)
import qualified Data.ByteString.Base64.Lazy as EL
import Data.String.Conversions (cs)
import Data.String.Conversions.Monomorphic (fromLT)
import GHC.Stack
import Network.HTTP.Client.MultipartFormData
import qualified SAML2.WebSSO as SAML
import qualified SAML2.WebSSO.Test.MockResponse as SAML
import Testlib.Prelude
import qualified Text.XML as XML

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
createScimTokenV6 :: (HasCallStack, MakesValue caller) => caller -> CreateScimToken -> App Response
createScimTokenV6 caller payload = do
  req <- baseRequest caller Spar (ExplicitVersion 6) "/scim/auth-tokens"
  j <- make payload
  submit "POST" $ req & addJSON j

createScimToken :: (HasCallStack, MakesValue caller) => caller -> CreateScimToken -> App Response
createScimToken caller payload = do
  req <- baseRequest caller Spar Versioned "/scim/auth-tokens"
  j <- make payload
  submit "POST" $ req & addJSON j

-- | https://staging-nginz-https.zinfra.io/v7/api/swagger-ui/#/default/auth-tokens-delete
deleteScimToken :: (HasCallStack, MakesValue caller) => caller -> String -> App Response
deleteScimToken caller token = do
  req <- baseRequest caller Spar Versioned $ joinHttpPath ["scim", "auth-tokens"]
  submit "DELETE" $ req
    & addQueryParams [("id", token)]

putScimTokenName :: (HasCallStack, MakesValue caller) => caller -> String -> String -> App Response
putScimTokenName caller token name = do
  req <- baseRequest caller Spar Versioned $ joinHttpPath ["scim", "auth-tokens", token]
  submit "PUT" $ req & addJSONObject ["name" .= name]

createScimUser :: (HasCallStack, MakesValue domain, MakesValue scimUser) => domain -> String -> scimUser -> App Response
createScimUser domain token scimUser = do
  req <- baseRequest domain Spar Versioned "/scim/v2/Users"
  body <- make scimUser
  submit "POST" $ req & addJSON body . addHeader "Authorization" ("Bearer " <> token)

deleteScimUser :: (HasCallStack, MakesValue domain) => domain -> String -> String -> App Response
deleteScimUser domain token uid = do
  req <- baseRequest domain Spar Versioned $ joinHttpPath ["scim", "v2", "Users", uid]
  submit "DELETE" $ req
    & addHeader "Authorization" ("Bearer " <> token)

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
  req <- baseRequest user Spar Versioned "/identity-providers"
  submit "POST" $ req
    & addQueryParams [("api_version", "v2")]
    & addXML (fromLT $ SAML.encode metadata)
    & addHeader "Content-Type" "application/xml"

-- | https://staging-nginz-https.zinfra.io/v7/api/swagger-ui/#/default/idp-get-all
getIdps :: (HasCallStack, MakesValue user) => user -> App Response
getIdps user = do
  req <- baseRequest user Spar Versioned "/identity-providers"
  submit "GET" req

-- | https://staging-nginz-https.zinfra.io/v7/api/swagger-ui/#/default/sso-team-metadata
getSPMetadata :: (HasCallStack, MakesValue domain) => domain -> String -> App Response
getSPMetadata domain tid = do
  req <- baseRequest domain Spar Versioned $ joinHttpPath ["sso", "metadata", tid]
  submit "GET" req

-- | https://staging-nginz-https.zinfra.io/v7/api/swagger-ui/#/default/auth-req
initiateSamlLogin :: (HasCallStack, MakesValue domain) => domain -> String -> App Response
initiateSamlLogin domain idpId = do
  req <- baseRequest domain Spar Versioned $ joinHttpPath ["sso", "initiate-login", idpId]
  submit "GET" req

-- | https://staging-nginz-https.zinfra.io/v7/api/swagger-ui/#/default/auth-resp
finalizeSamlLogin :: (HasCallStack, MakesValue domain) => domain -> String -> SAML.SignedAuthnResponse -> App Response
finalizeSamlLogin domain tid (SAML.SignedAuthnResponse authnresp) = do
  baseRequest domain Spar Versioned (joinHttpPath ["sso", "finalize-login", tid])
    >>= formDataBody [partLBS (cs "SAMLResponse") . EL.encode . XML.renderLBS XML.def $ authnresp]
    >>= submit "POST"
