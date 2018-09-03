{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuasiQuotes #-}

module Test.Class.UserSpec (spec) where

import           Test.Util

import           Web.SCIM.Server (mkapp, App, UserAPI, userServer)
import           Web.SCIM.Server.Mock
import           Test.Hspec
import           Test.Hspec.Wai      hiding (post, put, patch)
import           Data.ByteString.Lazy (ByteString)
import           Servant (Proxy(Proxy), Handler)
import           Servant.Generic
import           Network.Wai (Application)
import qualified STMContainers.Map   as STMMap


app :: App m UserAPI => (forall a. m a -> Handler a) -> IO Application
app = mkapp (Proxy :: Proxy UserAPI) (toServant userServer)

storage :: IO TestStorage
storage = TestStorage <$> STMMap.newIO <*> STMMap.newIO <*> STMMap.newIO

spec :: Spec
spec = beforeAll ((\s -> app (nt s)) =<< storage) $ do
  describe "GET & POST /Users" $ do
    it "responds with [] in empty environment" $ do
      get "/" `shouldRespondWith` emptyList

    it "can insert then retrieve stored Users" $ do
      post "/" newBarbara `shouldRespondWith` 201
      get "/" `shouldRespondWith` users

  describe "GET /Users/:id" $ do
    it "responds with 404 for unknown user" $ do
      get "/unknown" `shouldRespondWith` unknown

    it "retrieves stored user" $ do
      -- the test implementation stores users with uid [0,1..n-1]
      get "/0" `shouldRespondWith` barbara

  describe "PUT /Users/:id" $ do
    it "updates mutable fields" $ do
      put "/0" barbUpdate0 `shouldRespondWith` updatedBarb0

    it "does not create new user " $ do
      put "/nonexisting" newBarbara `shouldRespondWith` 400

  describe "DELETE /Users/:id" $ do
    it "responds with 404 for unknown user" $ do
      delete "/unknown" `shouldRespondWith` 404

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
