{-# LANGUAGE QuasiQuotes #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

module Test.Class.GroupSpec
  ( spec,
  )
where

import Data.ByteString.Lazy (ByteString)
import Network.Wai (Application)
import Servant (Proxy (Proxy))
import Servant.API.Generic
import Test.Hspec hiding (shouldSatisfy)
import Test.Hspec.Wai hiding (patch, post, put, shouldRespondWith)
import Web.Scim.Server (GroupAPI, groupServer, mkapp)
import Web.Scim.Server.Mock
import Web.Scim.Test.Util

app :: IO Application
app = do
  storage <- emptyTestStorage
  let auth = Just "authorized"
  pure $
    mkapp @Mock
      (Proxy @(GroupAPI Mock))
      (toServant (groupServer @Mock auth))
      (nt storage)

spec :: Spec
spec = beforeAll app $ do
  describe "GET & POST /Groups" $ do
    it "responds with [] in empty environment" $ do
      get "/" `shouldRespondWith` emptyList
    it "can insert then retrieve stored group" $ do
      post "/" adminGroup `shouldRespondWith` 201
      get "/" `shouldRespondWith` groups
  describe "GET /Groups/:id" $ do
    it "responds with 404 for unknown group" $ do
      get "/9999" `shouldRespondWith` 404
    it "retrieves stored group" $ do
      -- the test implementation stores groups with uid [0,1..n-1]
      get "/0" `shouldRespondWith` admins
  describe "PUT /Groups/:id" $ do
    it "adds member to existing group" $ do
      put "/0" adminUpdate0 `shouldRespondWith` updatedAdmins0
    it "does not create new group" $ do
      put "/9999" adminGroup `shouldRespondWith` 404
  describe "DELETE /Groups/:id" $ do
    it "responds with 404 for unknown group" $ do
      delete "/Users/unknown" `shouldRespondWith` 404
    it "deletes a stored group" $ do
      delete "/0" `shouldRespondWith` 204
      -- group should be gone
      get "/0" `shouldRespondWith` 404
      delete "/0" `shouldRespondWith` 404

adminGroup :: ByteString
adminGroup =
  [scim|
        { "schemas":["urn:ietf:params:scim:schemas:core:2.0:Group"],
          "displayName":"Admin",
          "members":[]
        }|]

groups :: ResponseMatcher
groups =
  [scim|
        { "schemas":["urn:ietf:params:scim:api:messages:2.0:ListResponse"],
          "Resources":
            [{ "schemas":["urn:ietf:params:scim:schemas:core:2.0:Group"],
               "displayName":"Admin",
               "members":[],
               "id":"0",
               "meta":{
                 "resourceType":"Group",
                 "location":"https://example.com/Users/id",
                 "created":"2018-01-01T00:00:00Z",
                 "version":"W/\"testVersion\"",
                 "lastModified":"2018-01-01T00:00:00Z"
               }
             }
            ],
          "totalResults":1,
          "itemsPerPage":1,
          "startIndex":1
        }|]

admins :: ResponseMatcher
admins =
  [scim|
        { "schemas":["urn:ietf:params:scim:schemas:core:2.0:Group"],
           "displayName":"Admin",
           "members":[],
           "id":"0",
           "meta":{
             "resourceType":"Group",
             "location":"https://example.com/Users/id",
             "created":"2018-01-01T00:00:00Z",
             "version":"W/\"testVersion\"",
             "lastModified":"2018-01-01T00:00:00Z"
           }
        }|]

adminUpdate0 :: ByteString
adminUpdate0 =
  [scim|
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
updatedAdmins0 =
  [scim|
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
             "location":"https://example.com/Users/id",
             "created":"2018-01-01T00:00:00Z",
             "version":"W/\"testVersion\"",
             "lastModified":"2018-01-01T00:00:00Z"
           }
        }|]

emptyList :: ResponseMatcher
emptyList =
  [scim|
       { "schemas": ["urn:ietf:params:scim:api:messages:2.0:ListResponse"],
         "Resources":[],
         "totalResults":0,
         "itemsPerPage":0,
         "startIndex":1
       }|]
