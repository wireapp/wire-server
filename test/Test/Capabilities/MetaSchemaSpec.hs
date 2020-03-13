{-# LANGUAGE QuasiQuotes #-}

module Test.Capabilities.MetaSchemaSpec
  ( spec,
  )
where

import Data.Aeson
import Data.Coerce
import qualified Data.List as List
import Data.Text (Text)
import Network.Wai.Test (SResponse (..))
import Servant
import Servant.API.Generic
import Test.Hspec hiding (shouldSatisfy)
import qualified Test.Hspec.Expectations as Expect
import Test.Hspec.Wai hiding (patch, post, put, shouldRespondWith)
import Web.Scim.Capabilities.MetaSchema
import Web.Scim.Server (ConfigAPI, mkapp)
import Web.Scim.Server.Mock
import Web.Scim.Test.Util

app :: IO Application
app = do
  storage <- emptyTestStorage
  pure $ mkapp @Mock (Proxy @ConfigAPI) (toServant (configServer empty)) (nt storage)

shouldSatisfy ::
  (Show a, FromJSON a) =>
  WaiSession SResponse ->
  (a -> Bool) ->
  WaiExpectation
shouldSatisfy resp predicate = do
  maybeDecoded <- eitherDecode . simpleBody <$> resp
  case maybeDecoded of
    Left err -> liftIO $ Expect.expectationFailure ("decode error: " <> err)
    Right decoded -> liftIO $ Expect.shouldSatisfy decoded predicate

-- | A type that helps us parse out the pieces that we're interested in
-- (specifically, a list of schema URIs). The whole response is very big and
-- we don't want to print it out when the response parses correctly but the
-- sets of schemas don't match.
type SchemasResponse = Field "Resources" [Field "id" Text]

coreSchemas :: [Text]
coreSchemas =
  [ "urn:ietf:params:scim:schemas:core:2.0:User",
    "urn:ietf:params:scim:schemas:core:2.0:ServiceProviderConfig",
    "urn:ietf:params:scim:schemas:core:2.0:Group",
    "urn:ietf:params:scim:schemas:core:2.0:Schema",
    "urn:ietf:params:scim:schemas:core:2.0:ResourceType"
  ]

spec :: Spec
spec = beforeAll app $ do
  describe "GET /Schemas" $ do
    it "lists schemas" $ do
      get "/Schemas" `shouldRespondWith` 200
      get "/Schemas" `shouldSatisfy` \(resp :: SchemasResponse) ->
        List.sort (coerce resp) == List.sort coreSchemas
  describe "GET /Schemas/:id" $ do
    it "returns valid schema" $ do
      -- TODO: (partially) verify content
      get "/Schemas/urn:ietf:params:scim:schemas:core:2.0:User"
        `shouldRespondWith` 200
    it "returns 404 on unknown schemaId" $ do
      get "/Schemas/unknown" `shouldRespondWith` 404
  describe "GET /ServiceProviderConfig" $ do
    it "returns configuration" $ do
      get "/ServiceProviderConfig" `shouldRespondWith` spConfig
  describe "GET /ResourceTypes" $ do
    it "returns resource types" $ do
      get "/ResourceTypes" `shouldRespondWith` resourceTypes

-- FIXME: missing some "supported" fields
spConfig :: ResponseMatcher
spConfig =
  [scim|
{"schemas":["urn:ietf:params:scim:schemas:core:2.0:User",
            "urn:ietf:params:scim:schemas:core:2.0:ServiceProviderConfig",
            "urn:ietf:params:scim:schemas:core:2.0:Group",
            "urn:ietf:params:scim:schemas:core:2.0:Schema",
            "urn:ietf:params:scim:schemas:core:2.0:ResourceType"],
 "etag":{"supported":false},
 "bulk":{"maxOperations":0,
         "maxPayloadSize":0,
         "supported":false
        },
 "patch":{"supported":true},
 "authenticationSchemes":[
   {"type":"httpbasic",
    "name":"HTTP Basic",
    "description":"Authentication via the HTTP Basic standard",
    "specUri":"https://tools.ietf.org/html/rfc7617",
    "documentationUri":"https://en.wikipedia.org/wiki/Basic_access_authentication"
   }
 ],
 "changePassword":{"supported":false},
 "sort":{"supported":false},
 "filter":{"maxResults":0,
           "supported":false
          }
}
|]

resourceTypes :: ResponseMatcher
resourceTypes =
  [scim|
{"schemas":["urn:ietf:params:scim:api:messages:2.0:ListResponse"],
 "Resources":[
   {"schema":"urn:ietf:params:scim:schemas:core:2.0:User",
    "name":"User",
    "endpoint":"/Users"},
   {"schema":"urn:ietf:params:scim:schemas:core:2.0:Group",
    "name":"Group",
    "endpoint":"/Groups"}],
 "totalResults":2,
 "startIndex":1,
 "itemsPerPage":2
}
|]
