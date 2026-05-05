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

module Test.Spar.MultiIngressCrossIdpSso where

import API.BrigInternal (getUsersId)
import API.Common (randomEmail)
import API.GalleyInternal (setTeamFeatureStatus)
import API.Spar (createIdpWithZHostV2, getSsoCodeByEmailWithZHost)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text (pack)
import GHC.Stack
import qualified SAML2.WebSSO as SAML
import SAML2.WebSSO.Test.Util (SampleIdP (..))
import SetupHelpers
import Testlib.Prelude
import qualified Text.XML.DSig as SAML

-- | Test that demonstrates current behavior in multi-ingress setups where each
-- domain has its own IdP: when a user (representing the same person) logs in via
-- SSO on different domains with different IdPs, TWO separate user accounts are created.
--
-- This behavior is problematic because the same person ends up with multiple Wire accounts,
-- one per domain/IdP combination. This test documents the current behavior and will serve
-- as a baseline for implementing cross-IdP SSO support.
--
-- Current behavior demonstrated:
-- 1. User logs in on domain A with IdP1 (ernie) → User account 1 created
-- 2. User re-logs in on domain A → Same user account 1 (correct - SSO works)
-- 3. User (same person) logs in on domain B with IdP2 (bert) → User account 2 created (duplicate!)
-- 4. Both users can re-login independently on their respective domains
--
-- Note: We use the SAME NameID for both logins to demonstrate the core issue:
-- even with identical SAML identity, logging in through different ingresses/IdPs
-- creates separate user accounts. This is the exact scenario we want to fix.
--
-- Expected future behavior (when multi-IdP is implemented):
-- - Step 3 should recognize that this is the same person and link to the existing user
-- - OR provide a controlled flow for identity linking/merging

-- | Helper to create IdP metadata with a fixed issuer suffix for deterministic tests
makeSampleIdPMetadataWithIssuer :: (HasCallStack) => String -> App SampleIdP
makeSampleIdPMetadataWithIssuer suffix = do
  let issuerUri = pack $ "https://issuer.net/_" <> suffix
      requriUri = pack $ "https://requri.net/_req_" <> suffix
  let issuer = either (error . show) SAML.Issuer $ SAML.parseURI' issuerUri
      requri = either (error . show) id $ SAML.parseURI' requriUri
  (privcreds, creds, cert) <- liftIO $ SAML.mkSignCredsWithCert Nothing 96
  pure $ SampleIdP (SAML.IdPMetadata issuer requri (cert :| [])) privcreds creds cert

testCrossIdpSsoCreatesDistinctUsers :: (HasCallStack) => App ()
testCrossIdpSsoCreatesDistinctUsers = do
  let ernieZHost = "nginz-https.ernie.example.com"
      bertZHost = "nginz-https.bert.example.com"

  withModifiedBackend
    def
      { sparCfg =
          removeField "saml.spSsoUri"
            >=> removeField "saml.spAppUri"
            >=> removeField "saml.contacts"
            >=> setField
              "saml.spDomainConfigs"
              ( object
                  [ ernieZHost
                      .= object
                        [ "spAppUri" .= ("https://webapp.ernie.example.com" :: String),
                          "spSsoUri" .= ("https://nginz-https.ernie.example.com/sso" :: String),
                          "contacts" .= [object ["type" .= ("ContactTechnical" :: String)]]
                        ],
                    bertZHost
                      .= object
                        [ "spAppUri" .= ("https://webapp.bert.example.com" :: String),
                          "spSsoUri" .= ("https://nginz-https.bert.example.com/sso" :: String),
                          "contacts" .= [object ["type" .= ("ContactTechnical" :: String)]]
                        ]
                  ]
              )
            >=> setField "enableIdPByEmailDiscovery" True
      }
    $ \domain -> do
      -- Create team and enable SSO
      (owner, tid, _) <- createTeam domain 1
      void $ setTeamFeatureStatus owner tid "sso" "enabled"

      -- Register IdP1 for Ernie domain
      (idp1, idpMeta1) <- registerTestIdPWithMetaWithPrivateCredsForZHost owner (Just ernieZHost)
      idpId1 <- asString $ idp1.json %. "id"

      -- Register IdP2 for Bert domain
      (idp2, idpMeta2) <- registerTestIdPWithMetaWithPrivateCredsForZHost owner (Just bertZHost)
      idpId2 <- asString $ idp2.json %. "id"

      -- Create user identity "bibo" - this same person will login on both ingresses
      -- Use unspecified NameID format (not email) to avoid email uniqueness constraint
      suffix <- take 8 <$> randomId
      let biboNameId =
            fromRight (error "could not create name id")
              $ SAML.mkNameID (SAML.mkUNameIDUnspecified (pack ("bibo" <> suffix))) Nothing Nothing Nothing

      -- Step 2: Bibo logs in on Ernie ingress
      (mUserIdErnie, _) <-
        loginWithSamlWithZHost
          (Just ernieZHost)
          domain
          True
          tid
          biboNameId
          (idpId1, idpMeta1)

      -- Extract user ID
      userIdErnie <- case mUserIdErnie of
        Just uid -> pure uid
        Nothing -> error "Expected user ID from SSO login on Ernie domain"

      -- No email activation needed - using username-based NameID

      -- Verify user was created
      getUsersId domain [userIdErnie] `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 200

      -- Step 2.5: Verify re-login on Ernie domain (prove SSO works correctly)
      (mUserIdErnieAgain, _) <-
        loginWithSamlWithZHost
          (Just ernieZHost)
          domain
          True
          tid
          biboNameId
          (idpId1, idpMeta1)

      -- Verify it's the same user ID (no new user created)
      case mUserIdErnieAgain of
        Just uid -> uid `shouldMatch` userIdErnie
        Nothing -> error "Expected user ID from re-login on Ernie domain"

      -- Step 3: SAME Bibo logs in on Bert ingress WITH THE SAME NAMEID
      -- This is the core of the test: same identity, different ingress → duplicate user!
      (mUserIdBert, _) <-
        loginWithSamlWithZHost
          (Just bertZHost)
          domain
          True
          tid
          biboNameId -- SAME NameID!
          (idpId2, idpMeta2)

      -- Extract user ID
      userIdBert <- case mUserIdBert of
        Just uid -> pure uid
        Nothing -> error "Expected user ID from SSO login on Bert domain"

      -- Verify user was created
      getUsersId domain [userIdBert] `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 200

      -- Step 4: Verification - CORE ASSERTION
      -- This is the key finding: same person (conceptually) has two separate Wire accounts
      userIdErnie `shouldNotMatch` userIdBert

      -- Verify both users exist independently and each user is bound to their respective IdP
      getUsersId domain [userIdErnie] `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 200
        ssoId <- resp.json %. "0.sso_id"
        ssoIdTenant <- ssoId %. "tenant" >>= asString
        idp1Issuer <- idp1.json %. "metadata.issuer" >>= asString
        -- tenant contains XML with issuer inside
        ssoIdTenant `shouldContain` idp1Issuer

      getUsersId domain [userIdBert] `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 200
        ssoId <- resp.json %. "0.sso_id"
        ssoIdTenant <- ssoId %. "tenant" >>= asString
        idp2Issuer <- idp2.json %. "metadata.issuer" >>= asString
        -- tenant contains XML with issuer inside
        ssoIdTenant `shouldContain` idp2Issuer

      -- Verify both users can re-login on their original ingresses
      -- Same biboNameId, but each ingress returns a different user!
      (mUidErnieFinal, _) <-
        loginWithSamlWithZHost
          (Just ernieZHost)
          domain
          True
          tid
          biboNameId
          (idpId1, idpMeta1)

      case mUidErnieFinal of
        Just uid -> uid `shouldMatch` userIdErnie
        Nothing -> error "Expected user ID from Ernie final re-login"

      (mUidBertFinal, _) <-
        loginWithSamlWithZHost
          (Just bertZHost)
          domain
          True
          tid
          biboNameId
          (idpId2, idpMeta2)

      case mUidBertFinal of
        Just uid -> uid `shouldMatch` userIdBert
        Nothing -> error "Expected user ID from Bert final re-login"

-- | Test that demonstrates email uniqueness constraint in multi-ingress SSO.
--
-- When using email-based NameID, the second login attempt on a different ingress
-- succeeds and returns the same user ID. This is cross-IdP SSO migration:
-- if the email matches an existing user in the team, the login succeeds.
--
-- This is different from username-based NameID (tested above) where duplicate
-- users are silently created because usernames are not unique identifiers.
testCrossIdpSsoEmailConflict :: (HasCallStack) => App ()
testCrossIdpSsoEmailConflict = do
  let ernieZHost = "nginz-https.ernie.example.com"
      bertZHost = "nginz-https.bert.example.com"

  withModifiedBackend
    def
      { sparCfg =
          removeField "saml.spSsoUri"
            >=> removeField "saml.spAppUri"
            >=> removeField "saml.contacts"
            >=> setField
              "saml.spDomainConfigs"
              ( object
                  [ ernieZHost
                      .= object
                        [ "spAppUri" .= ("https://webapp.ernie.example.com" :: String),
                          "spSsoUri" .= ("https://nginz-https.ernie.example.com/sso" :: String),
                          "contacts" .= [object ["type" .= ("ContactTechnical" :: String)]]
                        ],
                    bertZHost
                      .= object
                        [ "spAppUri" .= ("https://webapp.bert.example.com" :: String),
                          "spSsoUri" .= ("https://nginz-https.bert.example.com/sso" :: String),
                          "contacts" .= [object ["type" .= ("ContactTechnical" :: String)]]
                        ]
                  ]
              )
            >=> setField "enableIdPByEmailDiscovery" True
      }
    $ \domain -> do
      -- Create team and enable SSO
      (owner, tid, _) <- createTeam domain 1
      void $ setTeamFeatureStatus owner tid "sso" "enabled"

      -- Register IdP1 for Ernie domain with fixed issuer "ernie"
      SampleIdP idpMeta1 pCreds1 _ _ <- makeSampleIdPMetadataWithIssuer "ernie"
      _idp1 <- createIdpWithZHostV2 owner (Just ernieZHost) idpMeta1
      idpId1 <- asString $ _idp1.json %. "id"

      -- Register IdP2 for Bert domain with fixed issuer "bert"
      SampleIdP idpMeta2 pCreds2 _ _ <- makeSampleIdPMetadataWithIssuer "bert"
      _idp2 <- createIdpWithZHostV2 owner (Just bertZHost) idpMeta2
      idpId2 <- asString $ _idp2.json %. "id"

      -- Create email-based NameID for "bibo"
      biboEmail <- randomEmail
      let biboNameId =
            fromRight (error "could not create name id")
              $ SAML.emailNameID (pack biboEmail)

      -- Step 1: Bibo logs in on Ernie ingress (should succeed)
      (mUserIdErnie, _) <-
        loginWithSamlWithZHost
          (Just ernieZHost)
          domain
          True -- expect success
          tid
          biboNameId
          (idpId1, (idpMeta1, pCreds1))

      -- Verify user was created
      userIdErnie <- case mUserIdErnie of
        Just uid -> pure uid
        Nothing -> error "Expected user ID from SSO login on Ernie domain"

      activateEmail domain biboEmail

      -- Verify user's SSO ID has Ernie's issuer (not Bert's)
      getUsersId domain [userIdErnie] `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 200
        ssoId <- resp.json %. "0.sso_id"
        ssoIdTenant <- ssoId %. "tenant" >>= asString
        ernieIssuer <- _idp1.json %. "metadata.issuer" >>= asString
        bertIssuer <- _idp2.json %. "metadata.issuer" >>= asString
        ssoIdTenant `shouldContain` ernieIssuer
        ssoIdTenant `shouldNotMatch` bertIssuer

      -- Verify sso/get-by-email returns Ernie's IdP
      getSsoCodeByEmailWithZHost domain (Just ernieZHost) biboEmail `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 200
        ssoCodeStr <- resp.json %. "sso_code" >>= asString
        ssoCodeStr `shouldMatch` idpId1

      -- Step 1.5: Bibo re-logs in on Ernie (should succeed - proves SSO works on same ingress)
      (mUserIdErnieAgain, _) <-
        loginWithSamlWithZHost
          (Just ernieZHost)
          domain
          True -- expect success
          tid
          biboNameId
          (idpId1, (idpMeta1, pCreds1))

      case mUserIdErnieAgain of
        Just uid -> uid `shouldMatch` userIdErnie
        Nothing -> error "Expected user ID from re-login on Ernie domain"

      -- Step 2: Same Bibo logs in on Bert ingress with SAME email
      -- This should SUCCEED because cross-IdP SSO migration is enabled:
      -- the email matches an existing user in the team, so we return that user
      (mUserIdBert, _) <-
        loginWithSamlWithZHost
          (Just bertZHost)
          domain
          True -- expect success
          tid
          biboNameId
          (idpId2, (idpMeta2, pCreds2))

      -- Verify the same user ID is returned (cross-IdP SSO migration worked)
      case mUserIdBert of
        Just uid -> uid `shouldMatch` userIdErnie
        Nothing -> error "Expected user ID from cross-IdP SSO login on Bert domain"

      -- Verify user's SSO ID was migrated to Bert's issuer (not Ernie's anymore)
      getUsersId domain [userIdErnie] `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 200
        ssoId <- resp.json %. "0.sso_id"
        ssoIdTenant <- ssoId %. "tenant" >>= asString
        ernieIssuer <- _idp1.json %. "metadata.issuer" >>= asString
        bertIssuer <- _idp2.json %. "metadata.issuer" >>= asString
        ssoIdTenant `shouldContain` bertIssuer
        ssoIdTenant `shouldNotMatch` ernieIssuer

      -- Verify sso/get-by-email returns Bert's IdP after migration
      getSsoCodeByEmailWithZHost domain (Just bertZHost) biboEmail `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 200
        ssoCodeStr <- resp.json %. "sso_code" >>= asString
        ssoCodeStr `shouldMatch` idpId2

      -- Step 3: Login on Ernie again to show back-and-forth migration works
      (mUserIdErnieFinal, _) <-
        loginWithSamlWithZHost
          (Just ernieZHost)
          domain
          True -- expect success
          tid
          biboNameId
          (idpId1, (idpMeta1, pCreds1))

      case mUserIdErnieFinal of
        Just uid -> uid `shouldMatch` userIdErnie
        Nothing -> error "Expected user ID from final login on Ernie domain"

      -- Verify user's SSO ID was migrated back to Ernie's issuer
      getUsersId domain [userIdErnie] `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 200
        ssoId <- resp.json %. "0.sso_id"
        ssoIdTenant <- ssoId %. "tenant" >>= asString
        ernieIssuer <- _idp1.json %. "metadata.issuer" >>= asString
        bertIssuer <- _idp2.json %. "metadata.issuer" >>= asString
        ssoIdTenant `shouldContain` ernieIssuer
        ssoIdTenant `shouldNotMatch` bertIssuer

      -- Verify sso/get-by-email returns Ernie's IdP after migration back
      getSsoCodeByEmailWithZHost domain (Just ernieZHost) biboEmail `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 200
        ssoCodeStr <- resp.json %. "sso_code" >>= asString
        ssoCodeStr `shouldMatch` idpId1
