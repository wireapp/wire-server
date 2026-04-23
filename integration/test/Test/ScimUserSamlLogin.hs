{-# OPTIONS_GHC -Wno-incomplete-patterns -Wno-ambiguous-fields #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2026 Wire Swiss GmbH <opensource@wire.com>
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

module Test.ScimUserSamlLogin where

import API.Brig (getSelf')
import API.Common (randomEmail, randomHandle)
import API.GalleyInternal (setTeamFeatureStatus)
import API.Spar (CreateScimToken (..), createScimToken, createScimUser)
import Data.String.Conversions (cs)
import qualified SAML2.WebSSO as SAML
import SetupHelpers
import Testlib.JSON
import Testlib.Prelude

-- | Test that a SCIM-provisioned user can login via SAML
--
-- This test reproduces the issue where SCIM users cannot login via SAML even though
-- the IdP signals success. The backend returns "Could not find SAML credentials,
-- and auto-provisioning is disabled."
--
-- Flow:
-- 1. Create team and enable SSO
-- 2. Register an IdP
-- 3. Create SCIM token associated with the IdP
-- 4. Provision a SCIM user with externalId matching the SAML NameID
-- 5. Attempt SAML login with matching NameID
-- 6. Expect login to succeed (currently fails)
testScimUserCanLoginViaSaml :: (HasCallStack) => App ()
testScimUserCanLoginViaSaml = do
  -- Setup: Create team and enable SSO
  (owner, tid, _) <- createTeam OwnDomain 1
  void $ setTeamFeatureStatus owner tid "sso" "enabled"

  -- Register IdP and get metadata
  (idpResp, (idpMeta, privCreds)) <- registerTestIdPWithMetaWithPrivateCreds owner
  idpId <- idpResp.json %. "id" >>= asString

  -- Create SCIM token associated with the IdP
  scimToken <- createScimToken owner (def {idp = Just idpId}) >>= getJSON 200 >>= (%. "token") >>= asString

  -- Create SCIM user with externalId that will be used as SAML NameID
  -- Using a non-email externalId (unspecified NameID format)
  let externalId = "scimmy"
  handle <- randomHandle
  let scimUser =
        object
          [ "schemas" .= ["urn:ietf:params:scim:schemas:core:2.0:User"],
            "externalId" .= externalId,
            "userName" .= handle,
            "displayName" .= ("SCIM User" :: String)
          ]

  -- Provision the SCIM user
  scimUserId <- bindResponse (createScimUser OwnDomain scimToken scimUser) $ \resp -> do
    resp.status `shouldMatchInt` 201
    resp.json %. "id" >>= asString

  -- Construct SAML NameID matching the SCIM externalId
  -- The SCIM externalId "scimmy" should create an unspecified NameID
  let nameId = SAML.mkUNameIDUnspecified (cs externalId)
      samlNameId = case SAML.mkNameID nameId Nothing Nothing Nothing of
        Right nid -> nid
        Left err -> error $ "Failed to create NameID: " <> err

  -- Attempt SAML login with the NameID
  -- This should succeed since the SCIM user was provisioned with this externalId
  (Just uid, _authnResp) <- loginWithSaml True tid samlNameId (idpId, (idpMeta, privCreds))

  -- Verify the logged-in user matches the SCIM-provisioned user
  uid `shouldMatch` scimUserId

  -- Verify we can get the user's profile
  ownDomain <- objDomain OwnDomain
  getSelf' ownDomain uid `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "handle" `shouldMatch` handle

-- | Test SCIM user SAML login with email-based externalId
--
-- This variant tests the more common case where the externalId is an email address
testScimUserCanLoginViaSamlWithEmail :: (HasCallStack) => App ()
testScimUserCanLoginViaSamlWithEmail = do
  -- Setup: Create team and enable SSO
  (owner, tid, _) <- createTeam OwnDomain 1
  void $ setTeamFeatureStatus owner tid "sso" "enabled"

  -- Register IdP and get metadata
  (idpResp, (idpMeta, privCreds)) <- registerTestIdPWithMetaWithPrivateCreds owner
  idpId <- idpResp.json %. "id" >>= asString

  -- Create SCIM token associated with the IdP
  scimToken <- createScimToken owner (def {idp = Just idpId}) >>= getJSON 200 >>= (%. "token") >>= asString

  -- Create SCIM user with email as externalId
  email <- randomEmail
  handle <- randomHandle
  let scimUser =
        object
          [ "schemas" .= ["urn:ietf:params:scim:schemas:core:2.0:User"],
            "externalId" .= email,
            "userName" .= handle,
            "displayName" .= ("SCIM User" :: String)
          ]

  -- Provision the SCIM user
  scimUserId <- bindResponse (createScimUser OwnDomain scimToken scimUser) $ \resp -> do
    resp.status `shouldMatchInt` 201
    resp.json %. "id" >>= asString

  -- Attempt SAML login with email as NameID
  -- This should succeed since the SCIM user was provisioned with this email
  (Just uid, _authnResp) <- loginWithSamlEmail True tid email (idpId, (idpMeta, privCreds))

  -- Verify the logged-in user matches the SCIM-provisioned user
  uid `shouldMatch` scimUserId

  -- Verify we can get the user's profile
  ownDomain <- objDomain OwnDomain
  getSelf' ownDomain uid `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "handle" `shouldMatch` handle

-- | Test that SAML login fails for SCIM users created without IdP association
--
-- This test documents the issue where SCIM users CAN be created with email-based
-- externalIds even when the SCIM token has no IdP association, but these users
-- CANNOT login via SAML later.
--
-- This reproduces the production scenario: user creation succeeds, but SAML login
-- fails because there's no SAML identity (no IdP was associated with the SCIM token).
--
-- Flow:
-- 1. Create team and enable SSO
-- 2. Register an IdP
-- 3. Create SCIM token WITHOUT associating it with the IdP (idp = Nothing)
-- 4. Create a SCIM user with EMAIL as externalId (this succeeds)
-- 5. Attempt SAML login with the email as NameID
-- 6. Expect login to fail (no SAML credentials found)
testScimUserSamlLoginRequiresTokenWithIdp :: (HasCallStack) => App ()
testScimUserSamlLoginRequiresTokenWithIdp = do
  -- Setup: Create team and enable SSO
  (owner, tid, _) <- createTeam OwnDomain 1
  void $ setTeamFeatureStatus owner tid "sso" "enabled"

  -- Register IdP and get metadata
  (idpResp, (idpMeta, privCreds)) <- registerTestIdPWithMetaWithPrivateCreds owner
  idpId <- idpResp.json %. "id" >>= asString

  -- Create SCIM token WITHOUT associating it with the IdP
  -- This is the key issue: idp = Nothing instead of idp = Just idpId
  scimToken <- createScimToken owner (def {idp = Nothing}) >>= getJSON 200 >>= (%. "token") >>= asString

  -- Create SCIM user with EMAIL as externalId
  -- This succeeds even without IdP association because the externalId is a valid email
  email <- randomEmail
  handle <- randomHandle
  let scimUser =
        object
          [ "schemas" .= ["urn:ietf:params:scim:schemas:core:2.0:User"],
            "externalId" .= email,
            "userName" .= handle,
            "displayName" .= ("SCIM User No IDP" :: String)
          ]

  -- Provision the SCIM user - this SUCCEEDS because email is valid
  scimUserId <- bindResponse (createScimUser OwnDomain scimToken scimUser) $ \resp -> do
    resp.status `shouldMatchInt` 201
    resp.json %. "id" >>= asString

  -- Verify the user was created (has a valid ID)
  assertBool "SCIM user should have been created with valid ID" (length scimUserId > 0)

  -- Attempt SAML login with the email as NameID
  -- This should FAIL because the SCIM token was not associated with the IdP
  -- Therefore, no SAML identity was created for this user
  (maybeUid, _authnResp) <- loginWithSamlEmail False tid email (idpId, (idpMeta, privCreds))

  -- Verify login failed (no user ID returned)
  -- The user exists but has no SAML credentials
  maybeUid `shouldMatch` (Nothing :: Maybe String)

-- | Test that SCIM users cannot login via SAML across different multi-ingress domains
--
-- This test proves that SCIM users provisioned on one ingress domain cannot
-- authenticate via SAML on a different ingress domain, even if both domains
-- belong to the same team.
--
-- Multi-ingress setup:
-- - Two configured domains: domain1 and domain2
-- - Each domain has its own IdP instance
-- - SCIM user is provisioned with domain1's IdP association
--
-- Flow:
-- 1. Configure multi-ingress with two domains
-- 2. Create team with SSO enabled
-- 3. Create IdP on domain1 and domain2
-- 4. Create SCIM token associated with domain1's IdP
-- 5. Provision SCIM user via domain1
-- 6. Login successfully on domain1 (same IdP)
-- 7. Attempt login on domain2 - should FAIL (different IdP/domain)
testScimUserCannotLoginAcrossDifferentIngresses :: (HasCallStack) => App ()
testScimUserCannotLoginAcrossDifferentIngresses = do
  let domain1ZHost = "nginz-https.domain1.example.com"
      domain2ZHost = "nginz-https.domain2.example.com"

  withModifiedBackend
    def
      { sparCfg =
          removeField "saml.spSsoUri"
            >=> removeField "saml.spAppUri"
            >=> removeField "saml.contacts"
            >=> setField
              "saml.spDomainConfigs"
              ( object
                  [ domain1ZHost
                      .= object
                        [ "spAppUri" .= ("https://webapp.domain1.example.com" :: String),
                          "spSsoUri" .= ("https://" <> domain1ZHost <> "/sso" :: String),
                          "contacts" .= [object ["type" .= ("ContactTechnical" :: String)]]
                        ],
                    domain2ZHost
                      .= object
                        [ "spAppUri" .= ("https://webapp.domain2.example.com" :: String),
                          "spSsoUri" .= ("https://" <> domain2ZHost <> "/sso" :: String),
                          "contacts" .= [object ["type" .= ("ContactTechnical" :: String)]]
                        ]
                  ]
              )
      }
    $ \domain -> do
      -- Setup: Create team and enable SSO
      (owner, tid, _) <- createTeam domain 1
      void $ setTeamFeatureStatus owner tid "sso" "enabled"

      -- Create IdP for domain1 with its own metadata
      (idpResp1, (idpMeta1, privCreds1)) <- registerTestIdPWithMetaWithPrivateCreds owner
      idpId1 <- idpResp1.json %. "id" >>= asString

      -- Create SCIM token associated with domain1's IdP
      scimToken <- createScimToken owner (def {idp = Just idpId1}) >>= getJSON 200 >>= (%. "token") >>= asString

      -- Provision SCIM user with email as externalId
      email <- randomEmail
      handle <- randomHandle
      let scimUser =
            object
              [ "schemas" .= ["urn:ietf:params:scim:schemas:core:2.0:User"],
                "externalId" .= email,
                "userName" .= handle,
                "displayName" .= ("SCIM User Domain1" :: String)
              ]

      scimUserId <- bindResponse (createScimUser domain scimToken scimUser) $ \resp -> do
        resp.status `shouldMatchInt` 201
        resp.json %. "id" >>= asString

      -- Test 1: User CAN login on domain1 (where they were provisioned)
      (Just uid1, _) <-
        loginWithSamlWithZHost
          (Just domain1ZHost)
          domain
          True
          tid
          (fromRight (error "could not create name id") $ SAML.emailNameID (cs email))
          (idpId1, (idpMeta1, privCreds1))

      -- Verify login succeeded and matches the SCIM user
      uid1 `shouldMatch` scimUserId

      -- Create a separate IdP for domain2
      -- This simulates a different ingress with its own IdP configuration
      (idpResp2, (idpMeta2, privCreds2)) <- registerTestIdPWithMetaWithPrivateCreds owner
      idpId2 <- idpResp2.json %. "id" >>= asString

      -- Test 2: User CANNOT login on domain2
      -- The SCIM user is linked to domain1's IdP, not domain2's IdP
      -- Even though it's the same team, different ingresses are isolated
      (maybeUid2, _) <-
        loginWithSamlWithZHost
          (Just domain2ZHost)
          domain
          False -- expect failure
          tid
          (fromRight (error "could not create name id") $ SAML.emailNameID (cs email))
          (idpId2, (idpMeta2, privCreds2))

      -- Verify login failed - user cannot cross ingress boundaries
      maybeUid2 `shouldMatch` (Nothing :: Maybe String)

