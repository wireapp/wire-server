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
import API.Spar (CreateScimToken (..), createIdpWithZHostV2, createScimToken, createScimUser, getSsoCodeByEmailWithZHost)
import Data.Either.Extra
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text (pack)
import GHC.Stack
import qualified SAML2.WebSSO as SAML
import SAML2.WebSSO.Test.Util (SampleIdP (..))
import SetupHelpers
import Testlib.Prelude
import qualified Text.XML.DSig as SAML

-- TODO:
-- - Test New user creation with email (user has NO representation in spar)
-- - Test with wrong IdP

-- | Test that demonstrates username-based NameID behavior in multi-ingress SSO.
--
-- When using username-based (unspecified) NameID, logging in via different
-- ingresses with different IdPs creates SEPARATE user accounts, even with the
-- same NameID. We decided this because username NameIDs are more likely
-- ambiguous across IdPs than email addresses.
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

      -- Register IdP for Ernie domain
      (idpErnie, idpMetaErnie) <- registerTestIdPWithMetaWithPrivateCredsForZHost owner (Just ernieZHost)
      idpIdErnie <- asString $ idpErnie.json %. "id"

      -- Register IdP for Bert domain
      (idpBert, idpMetaBert) <- registerTestIdPWithMetaWithPrivateCredsForZHost owner (Just bertZHost)
      idpIdBert <- asString $ idpBert.json %. "id"

      -- Create user identity "bibo" - this same person will login on both ingresses
      -- Use unspecified NameID format (not email) to avoid email uniqueness constraint
      suffix <- take 8 <$> randomId
      let biboNameId =
            fromRight (error "could not create name id")
              $ SAML.mkNameID (SAML.mkUNameIDUnspecified (pack ("bibo" <> suffix))) Nothing Nothing Nothing

      -- Step 2: Bibo logs in on Ernie ingress
      userIdErnie <- fst <$> loginWithSamlWithZHost
        (Just ernieZHost)
        domain
        True
        tid
        biboNameId
        (idpIdErnie, idpMetaErnie)
        >>= maybe (error "Expected user ID from SSO login on Ernie domain") pure

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
          (idpIdErnie, idpMetaErnie)

      -- Verify it's the same user ID (no new user created)
      case mUserIdErnieAgain of
        Just uid -> uid `shouldMatch` userIdErnie
        Nothing -> error "Expected user ID from re-login on Ernie domain"

      -- Step 3: SAME Bibo logs in on Bert ingress WITH THE SAME NAMEID
      -- This is the core of the test: same identity, different ingress → duplicate user!
      userIdBert <- fst <$> loginWithSamlWithZHost
        (Just bertZHost)
        domain
        True
        tid
        biboNameId -- SAME NameID!
        (idpIdBert, idpMetaBert)
        >>= maybe (error "Expected user ID from SSO login on Bert domain") pure

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
        idpErnieIssuer <- idpErnie.json %. "metadata.issuer" >>= asString
        -- tenant contains XML with issuer inside
        ssoIdTenant `shouldContain` idpErnieIssuer

      getUsersId domain [userIdBert] `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 200
        ssoId <- resp.json %. "0.sso_id"
        ssoIdTenant <- ssoId %. "tenant" >>= asString
        idpBertIssuer <- idpBert.json %. "metadata.issuer" >>= asString
        -- tenant contains XML with issuer inside
        ssoIdTenant `shouldContain` idpBertIssuer

      -- Verify both users can re-login on their original ingresses
      -- Same biboNameId, but each ingress returns a different user!
      (mUidErnieFinal, _) <-
        loginWithSamlWithZHost
          (Just ernieZHost)
          domain
          True
          tid
          biboNameId
          (idpIdErnie, idpMetaErnie)

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
          (idpIdBert, idpMetaBert)

      case mUidBertFinal of
        Just uid -> uid `shouldMatch` userIdBert
        Nothing -> error "Expected user ID from Bert final re-login"

-- | Test that demonstrates cross-IdP login with an email address
--
-- User can login with different IdPs. This is different from username-based
-- NameID (tested above) where duplicate users are created.
testCrossIdpSsoEmailConflict :: (HasCallStack) => Bool -> App ()
testCrossIdpSsoEmailConflict useSCIM = do
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

      -- Register IdP for Ernie domain with fixed issuer "ernie"
      SampleIdP idpMetaErnie pCredsErnie _ _ <- makeSampleIdPMetadataWithIssuer "ernie"
      idpErnie <- createIdpWithZHostV2 owner (Just ernieZHost) idpMetaErnie
      idpIdErnie <- asString $ idpErnie.json %. "id"

      -- Register IdP for Bert domain with fixed issuer "bert"
      SampleIdP idpMetaBert pCredsBert _ _ <- makeSampleIdPMetadataWithIssuer "bert"
      idpBert <- createIdpWithZHostV2 owner (Just bertZHost) idpMetaBert
      idpIdBert <- asString $ idpBert.json %. "id"

      -- Create email-based NameID for "bibo"
      biboEmail <- randomEmail
      let biboNameId =
            fromRight (error "could not create name id")
              $ SAML.emailNameID (pack biboEmail)

      -- Optionally create the user via SCIM (and not automatically)
      mScimUserId <-
        if useSCIM
          then do
            -- Create SCIM token associated with Ernie's IdP
            scimTok <- createScimToken owner (def {idp = Just idpIdErnie})
            scimToken <- scimTok.json %. "token" & asString

            -- Create SCIM user with the email
            scimUser <- randomScimUserWithEmail biboEmail biboEmail
            scimUid <- bindResponse (createScimUser domain scimToken scimUser) $ \resp -> do
              resp.status `shouldMatchInt` 201
              resp.json %. "id" >>= asString

            activateEmail domain biboEmail

            pure (Just scimUid)
          else pure Nothing

      -- Step 1: Bibo logs in on Ernie ingress (should succeed)
      userIdErnie <- fst <$> loginWithSamlWithZHost
        (Just ernieZHost)
        domain
        True -- expect success
        tid
        biboNameId
        (idpIdErnie, (idpMetaErnie, pCredsErnie))
        >>= maybe (error "Expected user ID from SSO login on Ernie domain") pure

      case mScimUserId of
        Just scimUid ->
          -- Validate that SCIM-created user matches SSO login user
          scimUid `shouldMatch` userIdErnie
        Nothing -> activateEmail domain biboEmail

      -- Verify user's SSO ID has Ernie's issuer (not Bert's)
      getUsersId domain [userIdErnie] `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 200
        ssoId <- resp.json %. "0.sso_id"
        ssoIdTenant <- ssoId %. "tenant" >>= asString
        ernieIssuer <- idpErnie.json %. "metadata.issuer" >>= asString
        bertIssuer <- idpBert.json %. "metadata.issuer" >>= asString
        ssoIdTenant `shouldContain` ernieIssuer
        ssoIdTenant `shouldNotMatch` bertIssuer

      -- Verify sso/get-by-email returns Ernie's IdP
      getSsoCodeByEmailWithZHost domain (Just ernieZHost) biboEmail `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 200
        ssoCodeStr <- resp.json %. "sso_code" >>= asString
        ssoCodeStr `shouldMatch` idpIdErnie

      -- Step 1.5: Bibo re-logs in on Ernie (should succeed - proves SSO works on same ingress)
      (mUserIdErnieAgain, _) <-
        loginWithSamlWithZHost
          (Just ernieZHost)
          domain
          True -- expect success
          tid
          biboNameId
          (idpIdErnie, (idpMetaErnie, pCredsErnie))

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
          (idpIdBert, (idpMetaBert, pCredsBert))

      -- Verify the same user ID is returned (cross-IdP SSO migration worked)
      case mUserIdBert of
        Just uid -> uid `shouldMatch` userIdErnie
        Nothing -> error "Expected user ID from cross-IdP SSO login on Bert domain"

      -- Verify user's SSO ID was migrated to Bert's issuer (not Ernie's anymore)
      getUsersId domain [userIdErnie] `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 200
        ssoId <- resp.json %. "0.sso_id"
        ssoIdTenant <- ssoId %. "tenant" >>= asString
        ernieIssuer <- idpErnie.json %. "metadata.issuer" >>= asString
        bertIssuer <- idpBert.json %. "metadata.issuer" >>= asString
        ssoIdTenant `shouldContain` bertIssuer
        ssoIdTenant `shouldNotMatch` ernieIssuer

      -- Verify sso/get-by-email returns Bert's IdP after migration
      getSsoCodeByEmailWithZHost domain (Just bertZHost) biboEmail `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 200
        ssoCodeStr <- resp.json %. "sso_code" >>= asString
        ssoCodeStr `shouldMatch` idpIdBert

      -- Step 3: Login on Ernie again to show back-and-forth migration works
      (mUserIdErnieFinal, _) <-
        loginWithSamlWithZHost
          (Just ernieZHost)
          domain
          True -- expect success
          tid
          biboNameId
          (idpIdErnie, (idpMetaErnie, pCredsErnie))

      case mUserIdErnieFinal of
        Just uid -> uid `shouldMatch` userIdErnie
        Nothing -> error "Expected user ID from final login on Ernie domain"

      -- Verify user's SSO ID was migrated back to Ernie's issuer
      getUsersId domain [userIdErnie] `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 200
        ssoId <- resp.json %. "0.sso_id"
        ssoIdTenant <- ssoId %. "tenant" >>= asString
        ernieIssuer <- idpErnie.json %. "metadata.issuer" >>= asString
        bertIssuer <- idpBert.json %. "metadata.issuer" >>= asString
        ssoIdTenant `shouldContain` ernieIssuer
        ssoIdTenant `shouldNotMatch` bertIssuer

      -- Verify sso/get-by-email returns Ernie's IdP after migration back
      getSsoCodeByEmailWithZHost domain (Just ernieZHost) biboEmail `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 200
        ssoCodeStr <- resp.json %. "sso_code" >>= asString
        ssoCodeStr `shouldMatch` idpIdErnie

-- | Test that demonstrates cross-IdP SSO migration when a SCIM user provisioned for one IdP
-- logs in for the first time via a different IdP.
--
-- Scenario:
-- 1. SCIM user is provisioned for Ernie's IdP (has SSO credentials for Ernie)
-- 2. User has NEVER logged in via SSO before (only provisioned via SCIM)
-- 3. User logs in for the FIRST time via Bert's IdP (different IdP)
-- 4. Expected: Cross-IdP SSO migration should work, user should be migrated to Bert's IdP
testScimUserLoginsDifferentIdP :: (HasCallStack) => App ()
testScimUserLoginsDifferentIdP = do
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

      -- Register IdP for Ernie domain with fixed issuer "ernie"
      SampleIdP idpMetaErnie pCredsErnie _ _ <- makeSampleIdPMetadataWithIssuer "ernie"
      idpErnie <- createIdpWithZHostV2 owner (Just ernieZHost) idpMetaErnie
      idpIdErnie <- asString $ idpErnie.json %. "id"

      -- Register IdP for Bert domain with fixed issuer "bert"
      SampleIdP idpMetaBert pCredsBert _ _ <- makeSampleIdPMetadataWithIssuer "bert"
      idpBert <- createIdpWithZHostV2 owner (Just bertZHost) idpMetaBert
      idpIdBert <- asString $ idpBert.json %. "id"

      -- Create email-based NameID for "charlie"
      charlieEmail <- randomEmail
      let charlieNameId =
            fromRight (error "could not create name id")
              $ SAML.emailNameID (pack charlieEmail)

      -- Provision SCIM user for Ernie's IdP
      scimTok <- createScimToken owner (def {idp = Just idpIdErnie})
      scimToken <- scimTok.json %. "token" & asString

      -- Create SCIM user with the email (associated with Ernie's IdP)
      scimUser <- randomScimUserWithEmail charlieEmail charlieEmail
      charlieUid <- bindResponse (createScimUser domain scimToken scimUser) $ \resp -> do
        resp.status `shouldMatchInt` 201
        resp.json %. "id" >>= asString

      -- Activate the email
      activateEmail domain charlieEmail

      -- Verify user was created with Ernie's SSO ID
      getUsersId domain [charlieUid] `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 200
        ssoId <- resp.json %. "0.sso_id"
        ssoIdTenant <- ssoId %. "tenant" >>= asString
        ernieIssuer <- idpErnie.json %. "metadata.issuer" >>= asString
        ssoIdTenant `shouldContain` ernieIssuer

      -- Step 1: Charlie logs in for the FIRST time on Bert's IdP (NOT Ernie!)
      -- This tests cross-IdP migration when user has never logged in before (only SCIM provisioned)
      userIdBert <- fst <$> loginWithSamlWithZHost
        (Just bertZHost)
        domain
        True -- expect success
        tid
        charlieNameId
        (idpIdBert, (idpMetaBert, pCredsBert))
        >>= maybe (error "Expected user ID from cross-IdP SSO login on Bert domain") pure

      -- Verify the same user ID is returned (cross-IdP SSO migration worked)
      userIdBert `shouldMatch` charlieUid

      -- Verify user's SSO ID was migrated to Bert's issuer
      getUsersId domain [userIdBert] `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 200
        ssoId <- resp.json %. "0.sso_id"
        ssoIdTenant <- ssoId %. "tenant" >>= asString
        ernieIssuer <- idpErnie.json %. "metadata.issuer" >>= asString
        bertIssuer <- idpBert.json %. "metadata.issuer" >>= asString
        ssoIdTenant `shouldContain` bertIssuer
        ssoIdTenant `shouldNotMatch` ernieIssuer

      -- Verify sso/get-by-email returns Bert's IdP after migration
      getSsoCodeByEmailWithZHost domain (Just bertZHost) charlieEmail `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 200
        ssoCodeStr <- resp.json %. "sso_code" >>= asString
        ssoCodeStr `shouldMatch` idpIdBert

      -- Step 2: Login on Ernie to verify back-migration also works
      (mUserIdErnie, _) <-
        loginWithSamlWithZHost
          (Just ernieZHost)
          domain
          True -- expect success
          tid
          charlieNameId
          (idpIdErnie, (idpMetaErnie, pCredsErnie))

      case mUserIdErnie of
        Just uid -> uid `shouldMatch` charlieUid
        Nothing -> error "Expected user ID from login on Ernie domain"

      -- Verify user's SSO ID was migrated back to Ernie's issuer
      getUsersId domain [charlieUid] `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 200
        ssoId <- resp.json %. "0.sso_id"
        ssoIdTenant <- ssoId %. "tenant" >>= asString
        ernieIssuer <- idpErnie.json %. "metadata.issuer" >>= asString
        bertIssuer <- idpBert.json %. "metadata.issuer" >>= asString
        ssoIdTenant `shouldContain` ernieIssuer
        ssoIdTenant `shouldNotMatch` bertIssuer

-- | Helper to create IdP metadata with a fixed issuer suffix for deterministic tests
makeSampleIdPMetadataWithIssuer :: (HasCallStack) => String -> App SampleIdP
makeSampleIdPMetadataWithIssuer suffix = do
  let issuerUri = pack $ "https://issuer.net/_" <> suffix
      requriUri = pack $ "https://requri.net/_req_" <> suffix
      issuer = SAML.Issuer . fromRight' $ SAML.parseURI' issuerUri
      requri = fromRight' $ SAML.parseURI' requriUri
  (privcreds, creds, cert) <- liftIO $ SAML.mkSignCredsWithCert Nothing 96
  pure $ SampleIdP (SAML.IdPMetadata issuer requri (cert :| [])) privcreds creds cert
