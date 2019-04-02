{-# LANGUAGE QuasiQuotes #-}

module Test.Class.UserSpec (spec) where

import           Test.Util

import           Web.Scim.Server (mkapp, UserAPI, userServer)
import           Web.Scim.Server.Mock
import           Test.Hspec
import           Test.Hspec.Wai      hiding (post, put, patch)
import           Data.ByteString.Lazy (ByteString)
import           Servant (Proxy(Proxy))
import           Network.Wai (Application)
import           Servant.API.Generic


app :: IO Application
app = do
  storage <- emptyTestStorage
  let auth = Just "authorized"
  pure $ mkapp @Mock (Proxy @(UserAPI Mock)) (toServant (userServer auth)) (nt storage)

spec :: Spec
spec = beforeAll app $ do
  describe "GET & POST /Users" $ do
    it "responds with [] in empty environment" $ do
      get "/" `shouldRespondWith` emptyList

    it "can insert then retrieve stored Users" $ do
      post "/" newBarbara `shouldRespondWith` 201
      post "/" newJim `shouldRespondWith` 201
      get "/" `shouldRespondWith` allUsers

    describe "filtering" $ do
      it "can filter by username" $ do
        get "/?filter=userName eq \"bjensen\"" `shouldRespondWith` onlyBarbara

      it "is case-insensitive regarding syntax" $ do
        get "/?filter=USERName EQ \"bjensen\"" `shouldRespondWith` onlyBarbara

      it "is case-insensitive regarding usernames" $ do
        get "/?filter=userName eq \"BJensen\"" `shouldRespondWith` onlyBarbara

      it "handles malformed filter syntax" $ do
        get "/?filter=userName eqq \"bjensen\"" `shouldRespondWith` 400
        -- TODO: would be nice to check the error message as well

      it "handles type errors in comparisons" $ do
        get "/?filter=userName eq true" `shouldRespondWith` 400

  describe "GET /Users/:id" $ do
    it "responds with 404 for unknown user" $ do
      get "/9999" `shouldRespondWith` 404

    -- FUTUREWORK: currently it returns 404:
    -- https://github.com/haskell-servant/servant/issues/1155
    xit "responds with 401 for unparseable user ID" $ do
      get "/unparseable" `shouldRespondWith` 401

    it "retrieves stored user" $ do
      -- the test implementation stores users with uid [0,1..n-1]
      get "/0" `shouldRespondWith` barbara

  describe "PUT /Users/:id" $ do
    it "overwrites the user" $ do
      put "/0" barbUpdate0 `shouldRespondWith` updatedBarb0

    it "does not create new users" $ do
      put "/9999" newBarbara `shouldRespondWith` 404

  describe "DELETE /Users/:id" $ do
    it "responds with 404 for unknown user" $ do
      delete "/9999" `shouldRespondWith` 404

    it "deletes a stored user" $ do
      delete "/0" `shouldRespondWith` 204
      -- user should be gone
      get    "/0" `shouldRespondWith` 404
      delete "/0" `shouldRespondWith` 404


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

newJim :: ByteString
newJim = [scim|
        { "schemas":["urn:ietf:params:scim:schemas:core:2.0:User"],
          "userName":"jim",
          "externalId":"jim",
          "name":{
            "formatted":"Jim",
            "familyName":"",
            "givenName":"Jim"
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

allUsers :: ResponseMatcher
allUsers = [scim|
        { "schemas":["urn:ietf:params:scim:api:messages:2.0:ListResponse"],
          "totalResults": 2,
          "itemsPerPage": 2,
          "startIndex": 0,
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
             },
             { "schemas":["urn:ietf:params:scim:schemas:core:2.0:User"],
               "userName":"jim",
               "externalId":"jim",
               "id" : "1",
               "name":{
                 "formatted":"Jim",
                 "familyName":"",
                 "givenName":"Jim"
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

onlyBarbara :: ResponseMatcher
onlyBarbara = [scim|
        { "schemas":["urn:ietf:params:scim:api:messages:2.0:ListResponse"],
          "totalResults": 1,
          "itemsPerPage": 1,
          "startIndex": 0,
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
         "totalResults":0,
         "itemsPerPage":0,
         "startIndex":0
       }|]
