{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE QuasiQuotes #-}

module Test.Class.GroupSpec (spec) where

import           Network.Wai (Application)
import           Servant.Generic
import           Servant (Proxy(Proxy), Handler)
import           Web.SCIM.Class.Group (GroupAPI, GroupDB, groupServer)
import           Web.SCIM.Server (mkapp, App)
import           Data.ByteString.Lazy (ByteString)
import           Test.Hspec hiding (shouldSatisfy)
import           Test.Hspec.Wai      hiding (post, put, patch)
import           Test.Hspec.Wai.JSON
import           Mock
import qualified STMContainers.Map   as Map


app :: (GroupDB m, App m GroupAPI) => (forall a. m a -> Handler a) -> Application
app = mkapp (Proxy :: Proxy GroupAPI) (toServant groupServer)


spec :: Spec
spec = beforeAll ((\s -> app (nt s)) <$> (TestStorage <$> Map.newIO <*> Map.newIO)) $ do
  describe "GET & POST /" $ do
    it "responds with [] in empty environment" $ do
      get "/" `shouldRespondWith` [json|[]|]

    it "can insert then retrieve stored group" $ do
      post "/" adminGroup `shouldRespondWith` 201
      get "/" `shouldRespondWith` groups

  describe "GET /:id" $ do
    it "responds with 404 for unknown group" $ do
      get "/unknown" `shouldRespondWith` unknown

    it "retrieves stored user" $ do
      -- the test implementation stores users with uid [0,1..n-1]
      get "/0" `shouldRespondWith` admins

  describe "PUT /:id" $ do
    it "adds member to existing group" $ do
      put "/0" adminUpdate0 `shouldRespondWith` updatedAdmins0

    it "does not create new group" $ do
      put "/nonexisting" adminGroup `shouldRespondWith` 400

  describe "DELETE /:id" $ do
    it "responds with 404 for unknown group" $ do
      delete "/Users/unknown" `shouldRespondWith` 404

    it "deletes a stored group" $ do
      delete "/0" `shouldRespondWith` 204
      -- user should be gone
      get    "/0" `shouldRespondWith` 404
      delete "/0" `shouldRespondWith` 404



adminGroup :: ByteString
adminGroup = [json|
        { "schemas":["urn:ietf:params:scim:schemas:core:2.0:Group"],
          "displayName":"Admin",
          "members":[]
        }|]


groups :: ResponseMatcher
groups = [json|
        [{ "schemas":["urn:ietf:params:scim:schemas:core:2.0:Group"],
           "displayName":"Admin",
           "members":[],
           "id":"0",
           "meta":{
             "resourceType":"Group",
             "location":"todo",
             "created":"2018-01-01T00:00:00Z",
             "version":"W/\"testVersion\"",
             "lastModified":"2018-01-01T00:00:00Z"
           }
        }]|]

admins :: ResponseMatcher
admins = [json|
        { "schemas":["urn:ietf:params:scim:schemas:core:2.0:Group"],
           "displayName":"Admin",
           "members":[],
           "id":"0",
           "meta":{
             "resourceType":"Group",
             "location":"todo",
             "created":"2018-01-01T00:00:00Z",
             "version":"W/\"testVersion\"",
             "lastModified":"2018-01-01T00:00:00Z"
           }
        }|]

adminUpdate0 :: ByteString
adminUpdate0 = [json|
        { "schemas":["urn:ietf:params:scim:schemas:core:2.0:Group"],
          "displayName":"Admin",
          "members":[
            { "value": "0",
              "$ref": "https://example.com/Users/0",
              "type": "User"
            }
          ]
        }|]

updatedAdmins0 :: ResponseMatcher
updatedAdmins0 = [json|
        { "schemas":["urn:ietf:params:scim:schemas:core:2.0:Group"],
           "displayName":"Admin",
           "members":[
             { "value": "0",
               "$ref": "https://example.com/Users/0",
               "type": "User"
             }],
           "id":"0",
           "meta":{
             "resourceType":"Group",
             "location":"todo",
             "created":"2018-01-01T00:00:00Z",
             "version":"W/\"testVersion\"",
             "lastModified":"2018-01-01T00:00:00Z"
           }
        }|]


unknown :: ResponseMatcher
unknown = [json|
       { "schemas": ["urn:ietf:params:scim:api:messages:2.0:Error"],
         "status": "404",
         "detail": "Resource unknown not found"
       }|] { matchStatus = 404 }
