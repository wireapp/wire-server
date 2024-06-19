{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
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

-- | A bunch of hspec acceptance tests that assert that your SCIM
-- implementation is compatible with popular SCIM 2.0 providers
module Web.Scim.Test.Acceptance
  ( module Web.Scim.Test.Acceptance,
    module Web.Scim.Test.Util,
  )
where

import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as L
import Data.String.Conversions (cs)
import Data.Text (Text)
import Network.HTTP.Types.Status
import Network.Wai.Test
import Servant.API as Servant
import Test.Hspec (Spec, beforeAll, describe, it, pending, pendingWith, shouldBe, shouldSatisfy)
import Test.Hspec.Wai (matchStatus)
import Test.Hspec.Wai.Internal (runWaiSession)
import Web.Scim.Class.User
import Web.Scim.Schema.Common as Hscim
import qualified Web.Scim.Schema.ListResponse as ListResponse
import Web.Scim.Schema.Meta
import Web.Scim.Schema.UserTypes
import Web.Scim.Test.Util

ignore :: (Monad m) => m a -> m ()
ignore _ = pure ()

-- https://docs.microsoft.com/en-us/azure/active-directory/manage-apps/use-scim-to-provision-users-and-groups#step-2-understand-the-azure-ad-scim-implementation
microsoftAzure :: forall tag. (Aeson.FromJSON (UserId tag), Aeson.FromJSON (UserExtra tag), ToHttpApiData (UserId tag)) => AcceptanceConfig tag -> Spec
microsoftAzure AcceptanceConfig {..} = do
  describe "Within the SCIM 2.0 protocol specification, your application must meet these requirements:" $ do
    it "Supports creating users, and optionally also groups, as per section 3.3 of the SCIM protocol." $ pending -- TODO(arianvp): Write test
    it "Supports modifying users or groups with PATCH requests, as per section 3.5.2 of the SCIM protocol." $ pending -- TODO(arianvp): Write test
    it "Supports retrieving a known resource for a user or group created earlier, as per section 3.4.1 of the SCIM protocol." $ pending -- TODO(arianvp): Write test
    it "Supports querying users or groups, as per section 3.4.2 of the SCIM protocol. By default, users are retrieved by their id and queried by their username and externalid, and groups are queried by displayName." $ pending
    describe "Supports querying user by ID and by manager, as per section 3.4.2 of the SCIM protocol." $ do
      it "query by id" $ pending -- TODO(arianvp): Write test
      it "query by manager" $ pending -- TODO(arianvp): Implement support for enterprise extension
    it "Supports querying groups by ID and by member, as per section 3.4.2 of the SCIM protocol." $ pending -- TODO(arianvp): Implement groups
    it "Accepts a single bearer token for authentication and authorization of Azure AD to your application." $
      -- This is provided by the library
      True `shouldBe` True
  describe "Follow these general guidelines when implementing a SCIM endpoint to ensure compatibility with Azure AD:" $ do
    it "id is a required property for all the resources. Every response that returns a resource should ensure each resource has this property, except for ListResponse with zero members." $
      -- NOTE: This is guaranteed by the type-system. No need for a test
      True `shouldBe` True
    it "Response to a query/filter request should always be a ListResponse." $
      -- NOTE: This is guaranteed by the type-system. No need for a test
      True `shouldBe` True
    it "Groups are optional, but only supported if the SCIM implementation supports PATCH requests." $
      -- TODO(arianvp): Implement groups
      True `shouldBe` True
    it "Don't require a case-sensitive match on structural elements in SCIM, in particular PATCH op operation values, as defined in https://tools.ietf.org/html/rfc7644#section-3.5.2. Azure AD emits the values of 'op' as Add, " $
      -- TODO(arianvp): Write test
      pending
    describe "Microsoft Azure AD only uses the following operators: eq and" $ do
      -- TODO(arianvp): Write test
      it "eq" $ pending
      -- TODO(arianvp): Implement 'and' as Azure needs it
      it "and" $ pending
  describe "good errors" $ do
    -- (we may touch servant for this?)
    it "surfaces parse errors of the user id path segment" $ do
      pendingWith "should contain the offending id and the error; currently contains neither"
    it "same for user id in query" $ do
      pending
    it "same for all other things parsed in path, query, body, ..." $ do
      pending
  beforeAll scimAppAndConfig $ do
    it "User Operations" $ \(app, queryConfig) -> flip runWaiSession app $ do
      userName1 <- liftIO genUserName
      userName2 <- liftIO genUserName
      -- POST /Users
      resp :: SResponse <- post' queryConfig "/Users" (sampleUser1 userName1)
      liftIO $ simpleStatus resp `shouldBe` status201
      let testuid :: BS.ByteString
          testuid =
            either (error . show . (,resp)) (cs . Servant.toUrlPiece . Hscim.id . thing) $
              Aeson.eitherDecode' @(StoredUser tag) (simpleBody resp)
      -- Get users without query
      get' queryConfig "/Users" >>= \rsp -> liftIO $ do
        simpleStatus rsp `shouldSatisfy` (`elem` [status200, status400])
      -- Get single user by query
      get' queryConfig (cs $ "/Users?filter=userName eq " <> show userName1) >>= \rsp -> liftIO $ do
        simpleStatus rsp `shouldBe` status200
        ListResponse.totalResults <$> Aeson.eitherDecode' @(ListResponse.ListResponse (StoredUser tag)) (simpleBody rsp) `shouldBe` Right 1
      -- Get single user by query, zero results
      get' queryConfig (cs $ "/Users?filter=userName eq " <> show userName2)
        `shouldRespondWith` [scim|
          {
            "schemas": ["urn:ietf:params:scim:api:messages:2.0:ListResponse"],
            "totalResults": 0,
            "Resources": [],
            "startIndex": 1,
            "itemsPerPage": 0
          }
        |]
          { matchStatus = 200
          }
      -- Get single user by externalId works
      ignore $ do
        get' queryConfig "/Users?filter=externalId eq \"0a21f0f2-8d2a-4f8e-479e-a20b-2d77186b5dd1\"" >>= \rsp -> liftIO $ do
          simpleStatus rsp `shouldBe` status200
          ListResponse.totalResults <$> Aeson.eitherDecode' @(ListResponse.ListResponse (StoredUser tag)) (simpleBody rsp) `shouldBe` Right 1
      -- Update user [Multi-valued properties]
      ignore $
        patch'
          queryConfig
          "/Users/0"
          [scim|
            {
              "schemas": ["urn:ietf:params:scim:api:messages:2.0:PatchOp"],
              "Operations": [
                  {
                      "op": "Replace",
                      "path": "emails[type eq \"work\"].value",
                      "value": "updatedEmail@microsoft.com"
                  },
                  {
                      "op": "Replace",
                      "path": "name.familyName",
                      "value": "updatedFamilyName"
                  }
              ]
            }
        |]
          `shouldRespondWith` 200
      -- update user [single-valued properties]
      -- replace userName
      let ops1 =
            [scim|
                   {
                         "schemas": ["urn:ietf:params:scim:api:messages:2.0:PatchOp"],
                         "Operations": [{
                                 "op": "Replace",
                                 "path": "userName",
                                 "value": #{userName2}
                         }]
                   }
                 |]
      patch' queryConfig ("/Users/" <> testuid) ops1 `shouldRespondWith` 200
      -- replace displayName
      let ops2 =
            [scim|
                   {
                           "schemas": ["urn:ietf:params:scim:api:messages:2.0:PatchOp"],
                           "Operations": [{
                                   "op": "Replace",
                                   "path": "displayName",
                                   "value": "newDisplayName"
                           }]
                   }
                 |]
          exactResult =
            [scim|
                     {
                       "schemas": [
                         "urn:ietf:params:scim:schemas:core:2.0:User",
                         "urn:ietf:params:scim:schemas:extension:enterprise:2.0:User"
                       ],
                       "userName": #{userName2},
                       "active": true,
                       "name": {
                         "givenName": "givenName",
                         "formatted": "givenName familyName",
                         "familyName": "familyName"
                       },
                       "emails": [
                         {
                           "value": #{userName1 <> "@testuser.com"},
                           "primary": true,
                           "type": "work"
                         }
                       ],
                       "displayName": "newDisplayName",
                       "id": "0",
                       "meta": {
                         "resourceType": "User",
                         "location": "https://example.com/Users/id",
                         "created": "2018-01-01T00:00:00Z",
                         "version": "W/\"testVersion\"",
                         "lastModified": "2018-01-01T00:00:00Z"
                       },
                       "externalId": "0a21f0f2-8d2a-4f8e-bf98-7363c4aed4ef"
                     }
                   |]
          result =
            if responsesFullyKnown
              then exactResult
              else 200 -- TODO(fisx): check the fields changed by the patch operations?
      patch' queryConfig ("/Users/" <> testuid) ops2 `shouldRespondWith` result
      -- remove displayName
      let op3 =
            [scim|
                 {
                         "schemas": ["urn:ietf:params:scim:api:messages:2.0:PatchOp"],
                         "Operations": [{
                                 "op": "Remove",
                                 "path": "displayName"
                         }]
                 }
               |]
          exactResult3 =
            [scim|
                     {
                       "schemas": [
                         "urn:ietf:params:scim:schemas:core:2.0:User",
                         "urn:ietf:params:scim:schemas:extension:enterprise:2.0:User"
                       ],
                       "userName": #{userName2},
                       "active": true,
                       "name": {
                         "givenName": "givenName",
                         "formatted": "givenName familyName",
                         "familyName": "familyName"
                       },
                       "emails": [
                         {
                           "value": #{userName1 <> "@testuser.com"},
                           "primary": true,
                           "type": "work"
                         }
                       ],
                       "id": "0",
                       "meta": {
                         "resourceType": "User",
                         "location": "https://example.com/Users/id",
                         "created": "2018-01-01T00:00:00Z",
                         "version": "W/\"testVersion\"",
                         "lastModified": "2018-01-01T00:00:00Z"
                       },
                       "externalId": "0a21f0f2-8d2a-4f8e-bf98-7363c4aed4ef"
                     }
                   |]
          result3 =
            if responsesFullyKnown
              then exactResult3
              else 200 -- TODO(fisx): check the fields changed by the patch operations?
      patch' queryConfig ("/Users/" <> testuid) op3 `shouldRespondWith` result3
      -- Delete User
      delete' queryConfig ("/Users/" <> testuid) "" `shouldRespondWith` 204
      delete' queryConfig ("/Users/" <> testuid) "" `shouldEventuallyRespondWith` 404
    it "Group operations" $ const pending

sampleUser1 :: Text -> L.ByteString
sampleUser1 userName1 =
  [scim|
  {
    "schemas": [
        "urn:ietf:params:scim:schemas:core:2.0:User",
        "urn:ietf:params:scim:schemas:extension:enterprise:2.0:User"],
    "externalId": "0a21f0f2-8d2a-4f8e-bf98-7363c4aed4ef",
    "userName": #{userName1},
    "active": true,
    "emails": [{
            "primary": true,
            "type": "work",
            "value": #{userName1 <> "@testuser.com"}
    }],
    "meta": {
            "resourceType": "User"
    },
    "name": {
            "formatted": "givenName familyName",
            "familyName": "familyName",
            "givenName": "givenName"
    },
    "roles": []
  }
|]
