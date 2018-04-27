{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main, startApp) where

import           Network.Wai.Handler.Warp
import           Network.Wai.Logger
import           Web.SCIM.Capabilities.MetaSchema (empty)
import qualified Test.Class.Groups as Groups
import           Web.SCIM.Server
import qualified STMContainers.Map   as Map
import           Test.Hspec
import           Test.Hspec.Wai      hiding (post, put, patch)
import           Test.Hspec.Wai.JSON
import           Mock
import           Data.ByteString.Lazy (ByteString)

main :: IO ()
main = do
  emptyState <- TestStorage <$> Map.newIO <*> Map.newIO
  hspec $ describe "/Users" (spec emptyState)
  hspec $ describe "/Groups" (Groups.spec emptyState)
--  hspec $ describe "Configuration" (Config.spec emptyState)

startApp :: Int -> IO ()
startApp port = do
  emptyState <- TestStorage <$> Map.newIO <*> Map.newIO
  withStdoutLogger $ \aplogger -> do
    let settings = setPort port $ setLogger aplogger defaultSettings
    runSettings settings $ app empty $ nt emptyState

spec :: TestStorage -> Spec
spec s = with (return (app empty (nt s))) $ do
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
newBarbara = [json|
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
barbara = [json|
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
users = [json|
        { "schemas":["urn:ietf:params:scim:api:messages:2.0:ListResponse"],
          "totalResults": 1,
          "resources":
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
barbUpdate0 = [json|
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
updatedBarb0 = [json|
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
emptyList = [json|
       { "schemas": ["urn:ietf:params:scim:api:messages:2.0:ListResponse"],
         "resources":[],
         "totalResults":0
       }|]

unknown :: ResponseMatcher
unknown = [json|
       { "schemas": ["urn:ietf:params:scim:api:messages:2.0:Error"],
         "status": "404",
         "detail": "Resource unknown not found"
       }|] { matchStatus = 404 }
