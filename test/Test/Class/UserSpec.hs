{-# LANGUAGE QuasiQuotes #-}

module Test.Class.UserSpec (spec) where

import           Test.Util

import           Web.SCIM.Capabilities.MetaSchema (empty)
import           Web.SCIM.Server
import           Web.SCIM.Server.Mock
import           Test.Hspec
import           Test.Hspec.Wai      hiding (post, put, patch)
import           Data.ByteString.Lazy (ByteString)
import qualified STMContainers.Map   as Map


spec :: Spec
spec = beforeAll ((\s -> app empty (nt s)) <$> (TestStorage <$> Map.newIO <*> Map.newIO)) $ do
  describe "GET & POST /Users" $ do
    it "responds with [] in empty environment" $ do
      get "/Users" `shouldRespondWith` emptyList

    it "can insert then retrieve stored Users" $ do
      post "/Users" newBarbara `shouldRespondWith` 201
      get "/Users" `shouldRespondWith` users

  describe "GET /Users/:id" $ do
    it "responds with 404 for unknown user" $ do
      get "/Users/unknown" `shouldRespondWith` unknown

    it "retrieves stored user" $ do
      -- the test implementation stores users with uid [0,1..n-1]
      get "/Users/0" `shouldRespondWith` barbara

  describe "PUT /Users/:id" $ do
    it "updates mutable fields" $ do
      put "/Users/0" barbUpdate0 `shouldRespondWith` updatedBarb0

    it "does not create new user " $ do
      put "/Users/nonexisting" newBarbara `shouldRespondWith` 400

  describe "DELETE /Users/:id" $ do
    it "responds with 404 for unknown user" $ do
      delete "/Users/unknown" `shouldRespondWith` 404

    it "deletes a stored user" $ do
      delete "/Users/0" `shouldRespondWith` 204
      -- user should be gone
      get    "/Users/0" `shouldRespondWith` 404
      delete "/Users/0" `shouldRespondWith` 404


newBarbara :: ByteString
newBarbara = [scim|
        { "schemas":["urn:ietf:params:scim:schemas:core:2.0:User"],
          "userName":"bjensen",
          "externalId":"bjensen",
          "name":{
            "formatted":"Ms. Barbara J Jensen III",
            "familyName":"Jensen",
             "givenName":"Barbara"
          }
        }|]


barbara :: ResponseMatcher
barbara = [scim|
        { "schemas":["urn:ietf:params:scim:schemas:core:2.0:User"],
          "userName":"bjensen",
          "name": {
            "givenName":"Barbara",
            "formatted":"Ms. Barbara J Jensen III",
            "familyName":"Jensen"
          },
          "id":"0",
          "externalId":"bjensen",
          "meta":{
            "resourceType":"User",
            "location":"todo",
            "created":"2018-01-01T00:00:00Z",
            "version":"W/\"testVersion\"",
            "lastModified":"2018-01-01T00:00:00Z"
          }
        }|]

users :: ResponseMatcher
users = [scim|
        { "schemas":["urn:ietf:params:scim:api:messages:2.0:ListResponse"],
          "totalResults": 1,
          "Resources":
            [{ "schemas":["urn:ietf:params:scim:schemas:core:2.0:User"],
               "userName":"bjensen",
               "externalId":"bjensen",
               "id" : "0",
               "name":{
                 "formatted":"Ms. Barbara J Jensen III",
                 "familyName":"Jensen",
                 "givenName":"Barbara"
               },
               "meta":{
                 "resourceType":"User",
                 "location":"todo",
                 "created":"2018-01-01T00:00:00Z",
                 "version":"W/\"testVersion\"",
                 "lastModified":"2018-01-01T00:00:00Z"
               }
             }]
        }|]

-- source: https://tools.ietf.org/html/rfc7644#section-3.5.1 (p. 30)
barbUpdate0 :: ByteString
barbUpdate0 = [scim|
        { "schemas":["urn:ietf:params:scim:schemas:core:2.0:User"],
          "id":"0",
          "userName":"bjensen",
          "externalId":"bjensen",
          "name":{
            "formatted":"Ms. Barbara J Jensen III",
            "familyName":"Jensen",
            "givenName":"Barbara",
            "middleName":"Jane"
          },
          "roles":[],
          "emails":[
            {
              "value":"bjensen@example.com"
            },
            {
              "value":"babs@jensen.org"
            }
          ]
        }|]


updatedBarb0 :: ResponseMatcher
updatedBarb0 = [scim|
        { "schemas":["urn:ietf:params:scim:schemas:core:2.0:User"],
          "id":"0",
          "userName":"bjensen",
          "externalId":"bjensen",
          "name":{
            "formatted":"Ms. Barbara J Jensen III",
            "familyName":"Jensen",
            "givenName":"Barbara",
            "middleName":"Jane"
          },
          "emails":[
            {
              "value":"bjensen@example.com"
            },
            {
              "value":"babs@jensen.org"
            }
          ],
          "meta":{
            "resourceType":"User",
            "location":"todo",
            "created":"2018-01-01T00:00:00Z",
            "version":"W/\"testVersion\"",
            "lastModified":"2018-01-01T00:00:00Z"
          }
        }|]

emptyList :: ResponseMatcher
emptyList = [scim|
       { "schemas": ["urn:ietf:params:scim:api:messages:2.0:ListResponse"],
         "Resources":[],
         "totalResults":0
       }|]

unknown :: ResponseMatcher
unknown = [scim|
       { "schemas": ["urn:ietf:params:scim:api:messages:2.0:Error"],
         "status": "404",
         "detail": "Resource unknown not found"
       }|] { matchStatus = 404 }
