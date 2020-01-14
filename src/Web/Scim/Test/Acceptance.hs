{-# LANGUAGE QuasiQuotes #-}
-- | A bunch of hspec acceptance tests that assert that your SCIM
-- implementation is compatible with popular SCIM 2.0 providers
module Web.Scim.Test.Acceptance where

import Web.Scim.Test.Util (scim, get', post', patch', delete')
import Test.Hspec (Spec, xit, it, shouldBe, beforeAll, pending, describe,)
import Test.Hspec.Wai (shouldRespondWith,  matchStatus)
import Network.Wai (Application)


-- https://docs.microsoft.com/en-us/azure/active-directory/manage-apps/use-scim-to-provision-users-and-groups#step-2-understand-the-azure-ad-scim-implementation
microsoftAzure :: IO Application -> Spec
microsoftAzure app = do
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

  beforeAll app $ do
    describe "User Operations" $ do
      it "POST /Users" $ do
        let user = [scim|
          {
            "schemas": [
                "urn:ietf:params:scim:schemas:core:2.0:User",
                "urn:ietf:params:scim:schemas:extension:enterprise:2.0:User"],
            "externalId": "0a21f0f2-8d2a-4f8e-bf98-7363c4aed4ef",
            "userName": "Test_User_ab6490ee-1e48-479e-a20b-2d77186b5dd1",
            "active": true,
            "emails": [{
                    "primary": true,
                    "type": "work",
                    "value": "Test_User_fd0ea19b-0777-472c-9f96-4f70d2226f2e@testuser.com"
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
        post' "/Users" user `shouldRespondWith` 201
      it "Get user by query" $ do
        get' "/Users?filter userName eq \"Test_User_ab6490ee-1e48-479e-a20b-2d77186b5dd1\"" `shouldRespondWith` 200
      it "Get user by query, zero results" $ do
        get' "/Users?filter=userName eq \"non-existent user\"" `shouldRespondWith` [scim|
          {
            "schemas": ["urn:ietf:params:scim:api:messages:2.0:ListResponse"],
            "totalResults": 0,
            "Resources": [],
            "startIndex": 1,
            "itemsPerPage": 0
          }
        |] { matchStatus = 200 }
      it "Get user by externalId works" $ do
        get' "/Users?filter externalId eq \"0a21f0f2-8d2a-4f8e-479e-a20b-2d77186b5dd1\"" `shouldRespondWith` 200
      xit "Update user [Multi-valued properties]" $ do
        patch' "/Users/0" [scim|
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
        |] `shouldRespondWith` 200
      describe "Update user [Single-valued properties]" $ do
        it "replace userName" $ patch' "/Users/0"
          [scim|
            {
                    "schemas": ["urn:ietf:params:scim:api:messages:2.0:PatchOp"],
                    "Operations": [{
                            "op": "Replace",
                            "path": "userName",
                            "value": "5b50642d-79fc-4410-9e90-4c077cdd1a59@testuser.com"
                    }]
            }
          |] `shouldRespondWith` 200
        it "replace displayName" $ patch' "/Users/0"
          [scim|
            {
                    "schemas": ["urn:ietf:params:scim:api:messages:2.0:PatchOp"],
                    "Operations": [{
                            "op": "Replace",
                            "path": "displayName",
                            "value": "newDisplayName"
                    }]
            }
          |] `shouldRespondWith` [scim|
            {
              "schemas": [
                "urn:ietf:params:scim:schemas:core:2.0:User",
                "urn:ietf:params:scim:schemas:extension:enterprise:2.0:User"
              ],
              "userName": "5b50642d-79fc-4410-9e90-4c077cdd1a59@testuser.com",
              "active": true,
              "name": {
                "givenName": "givenName",
                "formatted": "givenName familyName",
                "familyName": "familyName"
              },
              "emails": [
                {
                  "value": "Test_User_fd0ea19b-0777-472c-9f96-4f70d2226f2e@testuser.com",
                  "primary": true,
                  "type": "work"
                }
              ],
              "displayName": "newDisplayName",
              "id": "0",
              "meta": {
                "resourceType": "User",
                "location": "todo",
                "created": "2018-01-01T00:00:00Z",
                "version": "W/\"testVersion\"",
                "lastModified": "2018-01-01T00:00:00Z"
              },
              "externalId": "0a21f0f2-8d2a-4f8e-bf98-7363c4aed4ef"
            }
          |]
        it "remove displayName" $ patch' "/Users/0"
          [scim|
            {
                    "schemas": ["urn:ietf:params:scim:api:messages:2.0:PatchOp"],
                    "Operations": [{
                            "op": "Remove",
                            "path": "displayName"
                    }]
            }
          |] `shouldRespondWith`  [scim|
            {
              "schemas": [
                "urn:ietf:params:scim:schemas:core:2.0:User",
                "urn:ietf:params:scim:schemas:extension:enterprise:2.0:User"
              ],
              "userName": "5b50642d-79fc-4410-9e90-4c077cdd1a59@testuser.com",
              "active": true,
              "name": {
                "givenName": "givenName",
                "formatted": "givenName familyName",
                "familyName": "familyName"
              },
              "emails": [
                {
                  "value": "Test_User_fd0ea19b-0777-472c-9f96-4f70d2226f2e@testuser.com",
                  "primary": true,
                  "type": "work"
                }
              ],
              "id": "0",
              "meta": {
                "resourceType": "User",
                "location": "todo",
                "created": "2018-01-01T00:00:00Z",
                "version": "W/\"testVersion\"",
                "lastModified": "2018-01-01T00:00:00Z"
              },
              "externalId": "0a21f0f2-8d2a-4f8e-bf98-7363c4aed4ef"
            }
          |]
          -- TODO match body
      it "Delete User" $ do
        delete' "/Users/0" "" `shouldRespondWith` 204
        delete' "/Users/0" "" `shouldRespondWith` 404
    describe "Group operations" $
      it "is in progress" $ \_-> pending
