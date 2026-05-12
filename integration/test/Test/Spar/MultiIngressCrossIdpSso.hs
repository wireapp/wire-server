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
import API.Common (randomEmail, randomHandle)
import API.Spar
  ( CreateScimToken (..),
    createIdpWithZHostV2,
    createScimToken,
    createScimUser,
    finalizeSamlLoginWithZHost,
    getSPMetadataWithZHost,
    getSsoCodeByEmailWithZHost,
    initiateSamlLoginWithZHostAndLabel,
  )
import Control.Lens ((^.))
import Data.ByteString.Char8 (unpack)
import Data.Either.Extra
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.String.Conversions (cs)
import Data.Text (pack)
import qualified Data.UUID as UUID
import GHC.Stack
import qualified SAML2.WebSSO as SAML
import qualified SAML2.WebSSO.Test.MockResponse as SAML
import SAML2.WebSSO.Test.Util (SampleIdP (..))
import SetupHelpers
import Testlib.Prelude
import qualified Text.XML.DSig as SAML

ernieDomain, bertDomain, ernieZHost, bertZHost :: String
ernieDomain = "ernie.example.com"
bertDomain = "bert.example.com"
ernieZHost = "nginz-https." <> ernieDomain
bertZHost = "nginz-https." <> bertDomain

-- | Test that a user provisioned under one IdP can log in via another IdP,
-- with their SSO identity migrating to the new IdP transparently.
--
-- Covers both SCIM-provisioned and auto-provisioned users, and verifies
-- back-and-forth migration between IdPs.
testCrossIdpSsoMigration :: (HasCallStack) => Bool -> App ()
testCrossIdpSsoMigration useSCIM = do
  withMultiIngressBackend [ernieDomain, bertDomain] $ \domain -> do
    (owner, tid, _) <- createTeam domain 1

    -- Register IdP for Ernie's domain
    SampleIdP idpMetaErnie pCredsErnie _ _ <- makeSampleIdPMetadataWithIssuer "ernie"
    idpErnie <- createIdpWithZHostV2 owner (Just ernieZHost) idpMetaErnie
    idpIdErnie <- asString $ idpErnie.json %. "id"

    -- Register IdP for Bert's domain
    SampleIdP idpMetaBert pCredsBert _ _ <- makeSampleIdPMetadataWithIssuer "bert"
    idpBert <- createIdpWithZHostV2 owner (Just bertZHost) idpMetaBert
    idpIdBert <- asString $ idpBert.json %. "id"

    ernieIssuer <- idpErnie.json %. "metadata.issuer" >>= asString
    bertIssuer <- idpBert.json %. "metadata.issuer" >>= asString

    (biboEmail, biboNameId) <- randomEmailNameId

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

    -- Bibo logs in on Ernie ingress (should succeed)
    userIdErnie <-
      loginWithSamlWithZHost
        (Just ernieZHost)
        domain
        True -- expect success
        tid
        biboNameId
        (idpIdErnie, (idpMetaErnie, pCredsErnie))
        >>= maybe (error "Expected user ID from SSO login on Ernie domain") pure
        . fst

    case mScimUserId of
      Just scimUid ->
        -- Validate that SCIM-created user matches SSO login user
        scimUid `shouldMatch` userIdErnie
      Nothing ->
        -- Non-SCIM user was auto-provisioned. Activate them.
        activateEmail domain biboEmail

    -- Verify user's SSO ID has Ernie's issuer (not Bert's)
    getUsersId domain [userIdErnie] `bindResponse` \resp -> do
      resp.status `shouldMatchInt` 200
      ssoId <- resp.json %. "0.sso_id"
      ssoIdTenant <- ssoId %. "tenant" >>= asString
      ssoIdTenant `shouldContain` ernieIssuer
      ssoIdTenant `shouldNotMatch` bertIssuer

    -- Verify sso/get-by-email returns Ernie's IdP
    getSsoCodeByEmailWithZHost domain (Just ernieZHost) biboEmail `bindResponse` \resp -> do
      resp.status `shouldMatchInt` 200
      ssoCodeStr <- resp.json %. "sso_code" >>= asString
      ssoCodeStr `shouldMatch` idpIdErnie

    -- Bibo re-logs in on Ernie (should succeed - proves SSO works on same ingress)
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

    -- Same Bibo logs in on Bert ingress with SAME email
    -- This should SUCCEED because of cross-IdP SSO migration.
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
      ssoIdTenant `shouldContain` bertIssuer
      ssoIdTenant `shouldNotMatch` ernieIssuer

    -- Verify sso/get-by-email returns Bert's IdP for Bert's ingress after migration
    getSsoCodeByEmailWithZHost domain (Just bertZHost) biboEmail `bindResponse` \resp -> do
      resp.status `shouldMatchInt` 200
      ssoCodeStr <- resp.json %. "sso_code" >>= asString
      ssoCodeStr `shouldMatch` idpIdBert

    -- Verify sso/get-by-email returns Ernie's IdP for Ernie's ingress after migration
    getSsoCodeByEmailWithZHost domain (Just ernieZHost) biboEmail `bindResponse` \resp -> do
      resp.status `shouldMatchInt` 200
      ssoCodeStr <- resp.json %. "sso_code" >>= asString
      ssoCodeStr `shouldMatch` idpIdErnie

    -- Login on Ernie again to show back-and-forth migration works
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

    -- Verify user's SSO ID was migrated back to Ernie's IdP
    getUsersId domain [userIdErnie] `bindResponse` \resp -> do
      resp.status `shouldMatchInt` 200
      ssoId <- resp.json %. "0.sso_id"
      ssoIdTenant <- ssoId %. "tenant" >>= asString
      ssoIdTenant `shouldContain` ernieIssuer
      ssoIdTenant `shouldNotMatch` bertIssuer

    -- Verify sso/get-by-email returns correct IdP by ingress
    getSsoCodeByEmailWithZHost domain (Just ernieZHost) biboEmail `bindResponse` \resp -> do
      resp.status `shouldMatchInt` 200
      ssoCodeStr <- resp.json %. "sso_code" >>= asString
      ssoCodeStr `shouldMatch` idpIdErnie

    getSsoCodeByEmailWithZHost domain (Just bertZHost) biboEmail `bindResponse` \resp -> do
      resp.status `shouldMatchInt` 200
      ssoCodeStr <- resp.json %. "sso_code" >>= asString
      ssoCodeStr `shouldMatch` idpIdBert

-- | Cross-IdP migration works even when the user's first SSO login is on a different IdP
-- than the one they were SCIM-provisioned under.
testScimUserLoginsDifferentIdP :: (HasCallStack) => App ()
testScimUserLoginsDifferentIdP = do
  withMultiIngressBackend [ernieDomain, bertDomain] $ \domain -> do
    (owner, tid, _) <- createTeam domain 1

    -- Register IdP for Ernie's domain
    SampleIdP idpMetaErnie pCredsErnie _ _ <- makeSampleIdPMetadataWithIssuer "ernie"
    idpErnie <- createIdpWithZHostV2 owner (Just ernieZHost) idpMetaErnie
    idpIdErnie <- asString $ idpErnie.json %. "id"

    -- Register IdP for Bert's domain
    SampleIdP idpMetaBert pCredsBert _ _ <- makeSampleIdPMetadataWithIssuer "bert"
    idpBert <- createIdpWithZHostV2 owner (Just bertZHost) idpMetaBert
    idpIdBert <- asString $ idpBert.json %. "id"

    ernieIssuer <- idpErnie.json %. "metadata.issuer" >>= asString
    bertIssuer <- idpBert.json %. "metadata.issuer" >>= asString

    (biboEmail, biboNameId) <- randomEmailNameId

    -- Provision SCIM user for Ernie's IdP
    scimTok <- createScimToken owner (def {idp = Just idpIdErnie})
    scimToken <- scimTok.json %. "token" & asString

    scimUser <- randomScimUserWithEmail biboEmail biboEmail
    biboUid <- bindResponse (createScimUser domain scimToken scimUser) $ \resp -> do
      resp.status `shouldMatchInt` 201
      resp.json %. "id" >>= asString

    activateEmail domain biboEmail

    -- Verify user was created with Ernie's SSO ID
    getUsersId domain [biboUid] `bindResponse` \resp -> do
      resp.status `shouldMatchInt` 200
      ssoId <- resp.json %. "0.sso_id"
      ssoIdTenant <- ssoId %. "tenant" >>= asString
      ssoIdTenant `shouldContain` ernieIssuer

    -- Bibo logs in for the FIRST time on Bert's IdP (NOT Ernie!)
    -- This tests cross-IdP migration when user has never logged in before (only SCIM provisioned)
    userIdBert <-
      loginWithSamlWithZHost
        (Just bertZHost)
        domain
        True -- expect success
        tid
        biboNameId
        (idpIdBert, (idpMetaBert, pCredsBert))
        >>= maybe (error "Expected user ID from cross-IdP SSO login on Bert domain") pure
        . fst

    -- Verify the same user ID is returned (cross-IdP SSO migration worked)
    userIdBert `shouldMatch` biboUid

    -- Verify user's SSO ID was migrated to Bert's issuer
    getUsersId domain [userIdBert] `bindResponse` \resp -> do
      resp.status `shouldMatchInt` 200
      ssoId <- resp.json %. "0.sso_id"
      ssoIdTenant <- ssoId %. "tenant" >>= asString
      ssoIdTenant `shouldContain` bertIssuer
      ssoIdTenant `shouldNotMatch` ernieIssuer

    -- Login on Ernie to verify back-migration also works
    (mUserIdErnie, _) <-
      loginWithSamlWithZHost
        (Just ernieZHost)
        domain
        True -- expect success
        tid
        biboNameId
        (idpIdErnie, (idpMetaErnie, pCredsErnie))

    case mUserIdErnie of
      Just uid -> uid `shouldMatch` biboUid
      Nothing -> error "Expected user ID from login on Ernie domain"

    -- Verify user's SSO ID was migrated back to Ernie's issuer
    getUsersId domain [biboUid] `bindResponse` \resp -> do
      resp.status `shouldMatchInt` 200
      ssoId <- resp.json %. "0.sso_id"
      ssoIdTenant <- ssoId %. "tenant" >>= asString
      ssoIdTenant `shouldContain` ernieIssuer
      ssoIdTenant `shouldNotMatch` bertIssuer

-- | Test cross-domain login when team has a single IdP.
--
-- As IdPs cannot be deleted when users are bound to them, having a single IdP
-- implies that all SSO users are bound to it.
testSingletonIdpWorksOnAllDomains :: (HasCallStack) => App ()
testSingletonIdpWorksOnAllDomains = do
  withMultiIngressBackend [ernieDomain, bertDomain] $ \domain -> do
    (owner, tid, _) <- createTeam domain 1

    -- Register ONLY ONE IdP for Bert domain
    -- This is the key: there's only a single IdP for the team
    SampleIdP idpMetaBert pCredsBert _ _ <- makeSampleIdPMetadataWithIssuer "bert"
    idpBert <- createIdpWithZHostV2 owner (Just bertZHost) idpMetaBert
    idpIdBert <- asString $ idpBert.json %. "id"

    bertIssuer <- idpBert.json %. "metadata.issuer" >>= asString

    (biboEmail, biboNameId) <- randomEmailNameId

    -- Provision SCIM user for Bert's IdP
    scimTok <- createScimToken owner (def {idp = Just idpIdBert})
    scimToken <- scimTok.json %. "token" & asString

    scimUser <- randomScimUserWithEmail biboEmail biboEmail
    biboUid <- bindResponse (createScimUser domain scimToken scimUser) $ \resp -> do
      resp.status `shouldMatchInt` 201
      resp.json %. "id" >>= asString

    activateEmail domain biboEmail

    -- Verify user was created with Bert's SSO ID
    getUsersId domain [biboUid] `bindResponse` \resp -> do
      resp.status `shouldMatchInt` 200
      ssoId <- resp.json %. "0.sso_id"
      ssoIdTenant <- ssoId %. "tenant" >>= asString
      ssoIdTenant `shouldContain` bertIssuer

    -- User logs in via ERNIE ingress (different domain from IdP registration)
    -- Bert's singleton IdP is valid here are well.
    userIdFromErnie <-
      loginWithSamlWithZHost
        (Just ernieZHost)
        domain
        True -- expect success
        tid
        biboNameId
        (idpIdBert, (idpMetaBert, pCredsBert))
        >>= maybe (error "Expected user ID from cross-domain login") pure
        . fst

    userIdFromErnie `shouldMatch` biboUid

    -- Verify user's SSO ID is still Bert's issuer
    getUsersId domain [userIdFromErnie] `bindResponse` \resp -> do
      resp.status `shouldMatchInt` 200
      ssoId <- resp.json %. "0.sso_id"
      ssoIdTenant <- ssoId %. "tenant" >>= asString
      ssoIdTenant `shouldContain` bertIssuer

-- | Login fails when the authenticating IdP's issuer is not registered for the target domain.
-- (Multiple IdPs configured, so the singleton fallback does not apply.)
testIdpNotFoundError :: (HasCallStack) => App ()
testIdpNotFoundError = do
  withMultiIngressBackend [ernieDomain, bertDomain] $ \domain -> do
    (owner, tid, _) <- createTeam domain 1

    -- Register TWO IdPs: one for Ernie domain, one for Bert domain
    SampleIdP idpMetaErnie pCredsErnie _ _ <- makeSampleIdPMetadataWithIssuer "ernie"
    idpErnie <- createIdpWithZHostV2 owner (Just ernieZHost) idpMetaErnie
    idpIdErnie <- asString $ idpErnie.json %. "id"
    ernieIssuer <- idpErnie.json %. "metadata.issuer" >>= asString

    SampleIdP idpMetaBert _ _ _ <- makeSampleIdPMetadataWithIssuer "bert"
    _idpBert <- createIdpWithZHostV2 owner (Just bertZHost) idpMetaBert

    -- Ernie's IdP is registered for ernieZHost, so authenticating on bertZHost with Ernie's
    -- credentials triggers a "not found" error (multiple IdPs, no singleton fallback).
    (_biboEmail, biboNameId) <- randomEmailNameId

    authnReqResp <- buildSamlAuthnResponse domain bertZHost tid idpIdErnie idpMetaErnie pCredsErnie biboNameId

    bindResponse (finalizeSamlLoginWithZHost domain (Just bertZHost) tid authnReqResp) $ \resp -> do
      resp.status `shouldMatchInt` 200
      let bdy = unpack resp.body
      bdy `shouldContain` "wire:sso:error:"
      bdy `shouldContain` "\"type\":\"AUTH_ERROR\""
      bdy `shouldContain` "wire:sso:error:not-found"
      bdy `shouldContain` "\"label\":\"forbidden\""
      let expectedErrorMsg =
            "Could not find IdP: IdP with issuer '\\\""
              <> ernieIssuer
              <> "\\\"' for domain '"
              <> bertZHost
              <> "' is not configured for this team"
      bdy `shouldContain` expectedErrorMsg

-- | Test that a user of one team cannot log in using the IdP of a different team.
--
-- Team B's IdP must not grant access to Team A, even when the SAML response is otherwise
-- well-formed.
testCrossTeamIdpLoginRejected :: (HasCallStack) => App ()
testCrossTeamIdpLoginRejected = do
  withMultiIngressBackend [ernieDomain, bertDomain] $ \domain -> do
    -- Team A with IdP A on bert domain
    (ownerA, tidA, _) <- createTeam domain 1
    SampleIdP idpMetaA pCredsA _ _ <- makeSampleIdPMetadataWithIssuer "team-a"
    idpA <- createIdpWithZHostV2 ownerA (Just bertZHost) idpMetaA
    idpIdA <- asString $ idpA.json %. "id"

    -- Team B with IdP B on ernie domain
    (ownerB, _, _) <- createTeam domain 1
    SampleIdP idpMetaB pCredsB _ _ <- makeSampleIdPMetadataWithIssuer "team-b"
    idpB <- createIdpWithZHostV2 ownerB (Just ernieZHost) idpMetaB
    idpIdB <- asString $ idpB.json %. "id"

    -- Create Bibo as a user of Team A
    (biboEmail, biboNameId) <- randomEmailNameId
    _ <- loginWithSamlWithZHost (Just bertZHost) domain True tidA biboNameId (idpIdA, (idpMetaA, pCredsA))
    activateEmail domain biboEmail

    -- Team B's IdP issuer is not registered under Team A, so spar returns 404.
    authnReqRespBert <- buildSamlAuthnResponse domain bertZHost tidA idpIdB idpMetaB pCredsB biboNameId
    bindResponse (finalizeSamlLoginWithZHost domain (Just bertZHost) tidA authnReqRespBert) $ \resp ->
      resp.status `shouldMatchInt` 404

    authnReqRespErnie <- buildSamlAuthnResponse domain ernieZHost tidA idpIdB idpMetaB pCredsB biboNameId
    bindResponse (finalizeSamlLoginWithZHost domain (Just ernieZHost) tidA authnReqRespErnie) $ \resp ->
      resp.status `shouldMatchInt` 404

-- | Test that a new user is provisioned when the IdP is correct but the user doesn't exist.
--
-- This tests the case in multiIngressFlow (services/spar/src/Spar/App.hs:516) where:
-- 1. Multi-ingress SSO is enabled with multiple IdPs
-- 2. A user tries to authenticate with an email-based NameID
-- 3. The IdP matches (issuer + domain are correct)
-- 4. But the user doesn't exist in ANY IdP for this team yet
-- 5. Expected: A new user should be provisioned
testNewUserProvisioningWithMultipleIdPs :: (HasCallStack) => App ()
testNewUserProvisioningWithMultipleIdPs = do
  withMultiIngressBackend [ernieDomain, bertDomain] $ \domain -> do
    -- Create team and enable SSO
    (owner, tid, _) <- createTeam domain 1

    -- Register TWO IdPs: one for Ernie domain, one for Bert domain
    SampleIdP idpMetaErnie pCredsErnie _ _ <- makeSampleIdPMetadataWithIssuer "ernie"
    idpErnie <- createIdpWithZHostV2 owner (Just ernieZHost) idpMetaErnie
    idpIdErnie <- asString $ idpErnie.json %. "id"

    SampleIdP idpMetaBert pCredsBert _ _ <- makeSampleIdPMetadataWithIssuer "bert"
    idpBert <- createIdpWithZHostV2 owner (Just bertZHost) idpMetaBert
    idpIdBert <- asString $ idpBert.json %. "id"

    -- Create email-based NameID for NEW user Bibo (doesn't exist yet)
    (_biboEmail, biboNameId) <- randomEmailNameId

    -- First login: Bibo tries to log in on Bert domain with Bert's IdP
    -- This should SUCCEED and create a new user because:
    -- - The IdP matches (issuer=bert, domain=bert)
    -- - Bibo doesn't exist in any IdP yet
    -- - So multiIngressFlow provisions a new user
    (mUserIdBert, _) <-
      loginWithSamlWithZHost
        (Just bertZHost)
        domain
        True -- expect SUCCESS
        tid
        biboNameId
        (idpIdBert, (idpMetaBert, pCredsBert))

    -- Verify user was created
    biboId <- assertOne mUserIdBert

    -- Login on Ernie domain with same email -> cross-IdP migration
    (mUserIdErnie, _) <-
      loginWithSamlWithZHost
        (Just ernieZHost)
        domain
        True
        tid
        biboNameId -- Same email
        (idpIdErnie, (idpMetaErnie, pCredsErnie))

    -- Should return same user (migration, not new user)
    mUserIdErnie `shouldMatch` Just biboId

-- | Test that non-email NameIDs are rejected in multi-ingress mode.
--
-- Multi-ingress cross-IdP SSO requires email-based NameIDs to prevent ambiguities.
testNonEmailNameIdRejectedInMultiIngress :: (HasCallStack) => App ()
testNonEmailNameIdRejectedInMultiIngress = do
  withMultiIngressBackend [bertDomain] $ \domain -> do
    (owner, tid, _) <- createTeam domain 1

    -- Register IdP
    SampleIdP idpMetaBert pCredsBert _ _ <- makeSampleIdPMetadataWithIssuer "bert"
    idpBert <- createIdpWithZHostV2 owner (Just bertZHost) idpMetaBert
    idpIdBert <- asString $ idpBert.json %. "id"

    randomUsername <- randomHandle
    let usernameNameId =
          fromRight (error "could not create name id")
            $ SAML.mkNameID (SAML.mkUNameIDUnspecified (pack randomUsername)) Nothing Nothing Nothing

    authnReqResp <- buildSamlAuthnResponse domain bertZHost tid idpIdBert idpMetaBert pCredsBert usernameNameId
    bindResponse (finalizeSamlLoginWithZHost domain (Just bertZHost) tid authnReqResp) $ \resp -> do
      resp.status `shouldMatchInt` 200
      let bdy = unpack resp.body
      bdy `shouldContain` "wire:sso:error:multi-ingress-config-error"
      bdy `shouldContain` "Multi-ingress SSO requires email-based NameIDs: Multi-ingress SSO only supports email-based NameIDs for cross-IdP migration. Username-based NameIDs are not allowed."

-- | Test that SAML responses without a prior authentication request are rejected.
--
-- A response referencing a request Spar never stored results in a "bad InResponseTo" error.
testUnsolicitedSamlResponseRejected :: (HasCallStack) => App ()
testUnsolicitedSamlResponseRejected = do
  withMultiIngressBackend [bertDomain] $ \domain -> do
    (owner, tid, _) <- createTeam domain 1

    SampleIdP idpMetaBert pCredsBert _ _ <- makeSampleIdPMetadataWithIssuer "bert"
    idpBert <- createIdpWithZHostV2 owner (Just bertZHost) idpMetaBert
    idpIdBert <- asString $ idpBert.json %. "id"

    (_biboEmail, biboNameId) <- randomEmailNameId

    let idpConfig = SAML.IdPConfig (SAML.IdPId (fromMaybe (error "invalid idp id") (UUID.fromString idpIdBert))) idpMetaBert ()
    spmeta <- getSPMetadataWithZHost domain (Just bertZHost) tid
    let spMetaData = fromRight (error "could not decode spmetadata") $ SAML.decode $ cs spmeta.body
    -- Create a local authn request (stored in SimpleSP's in-memory store, not in Spar's Cassandra)
    localReq <- runSimpleSP $ SAML.createAuthnRequest 300 (idpMetaBert ^. SAML.edIssuer) (idpMetaBert ^. SAML.edIssuer)
    authnReqResp <- makeAuthnResponse biboNameId pCredsBert idpConfig spMetaData localReq

    -- Spar cannot find the request (no verdict format stored), so it rejects with server error.
    -- This is not a user flow, so we can accept any error - even 500 - here.
    bindResponse (finalizeSamlLoginWithZHost domain (Just bertZHost) tid authnReqResp) $ \resp -> do
      resp.status `shouldMatchInt` 500
      resp.json %. "label" `shouldMatch` "server-error"

-- | Test that SAML responses for one ingress are rejected when submitted to a different ingress.
--
-- A login request on the ernie ingress must be finalized on the ernie ingress.
-- Finalizing on the bert ingress should fail with a bad recipient error.
testCrossIngressRequestResponseMismatch :: (HasCallStack) => App ()
testCrossIngressRequestResponseMismatch = do
  withMultiIngressBackend [ernieDomain, bertDomain] $ \domain -> do
    (owner, tid, _) <- createTeam domain 1

    SampleIdP idpMetaErnie pCredsErnie _ _ <- makeSampleIdPMetadataWithIssuer "ernie"
    idpErnie <- createIdpWithZHostV2 owner (Just ernieZHost) idpMetaErnie
    idpIdErnie <- asString $ idpErnie.json %. "id"

    SampleIdP idpMetaBert _ _ _ <- makeSampleIdPMetadataWithIssuer "bert"
    void $ createIdpWithZHostV2 owner (Just bertZHost) idpMetaBert

    (_biboEmail, biboNameId) <- randomEmailNameId

    -- Response Destination is ernie's ACS URL; finalizing on bert causes a mismatch.
    authnReqResp <- buildSamlAuthnResponse domain ernieZHost tid idpIdErnie idpMetaErnie pCredsErnie biboNameId

    -- Finalize on bert ingress — Destination mismatch, bad recipient
    bindResponse (finalizeSamlLoginWithZHost domain (Just bertZHost) tid authnReqResp) $ \resp -> do
      resp.status `shouldMatchInt` 200
      let bdy = unpack resp.body
      bdy `shouldContain` "wire:sso:error:forbidden"
      bdy `shouldContain` "bad Recipient"

-- | Run a test with the standard multi-ingress backend configuration.
-- Takes base domain names (e.g. "ernie.example.com"); the ZHost and SSO/webapp URLs
-- are derived from each base domain.
withMultiIngressBackend :: (HasCallStack) => [String] -> (String -> App ()) -> App ()
withMultiIngressBackend baseDomains action =
  withModifiedBackend
    def
      { sparCfg =
          removeField "saml.spSsoUri"
            >=> removeField "saml.spAppUri"
            >=> removeField "saml.contacts"
            >=> setField "saml.spDomainConfigs" (object (map mkDomainEntry baseDomains))
            >=> setField "enableIdPByEmailDiscovery" True,
        galleyCfg = setField "settings.featureFlags.sso" "enabled-by-default"
      }
    action
  where
    mkDomainEntry base =
      ("nginz-https." <> base)
        .= object
          [ "spAppUri" .= ("https://webapp." <> base :: String),
            "spSsoUri" .= ("https://nginz-https." <> base <> "/sso" :: String),
            "contacts" .= [object ["type" .= ("ContactTechnical" :: String)]]
          ]

-- | Initiate a SAML login and build a signed authn response for the given NameID.
-- Use this when testing error cases that require manual control over the finalize step.
buildSamlAuthnResponse ::
  (HasCallStack, MakesValue domain) =>
  domain ->
  String ->
  String ->
  String ->
  SAML.IdPMetadata ->
  SAML.SignPrivCreds ->
  SAML.NameID ->
  App SAML.SignedAuthnResponse
buildSamlAuthnResponse domain mbZHost tid idpId idpMeta pcreds nameId = do
  let idpConfig = SAML.IdPConfig (SAML.IdPId (fromMaybe (error "invalid idp id") (UUID.fromString idpId))) idpMeta ()
  spmeta <- getSPMetadataWithZHost domain (Just mbZHost) tid
  authnreq <- initiateSamlLoginWithZHostAndLabel domain (Just mbZHost) Nothing idpId
  let spMetaData = fromRight (error "could not decode spmetadata") $ SAML.decode $ cs spmeta.body
      parsedAuthnReq = parseAuthnReqResp authnreq.body
  makeAuthnResponse nameId pcreds idpConfig spMetaData parsedAuthnReq

-- | Generate a random email address and the corresponding email-based SAML NameID.
randomEmailNameId :: (HasCallStack) => App (String, SAML.NameID)
randomEmailNameId = do
  email <- randomEmail
  let nameId = fromRight (error "could not create name id") $ SAML.emailNameID (pack email)
  pure (email, nameId)

-- | Helper to create IdP metadata with a fixed issuer suffix for deterministic tests
makeSampleIdPMetadataWithIssuer :: (HasCallStack) => String -> App SampleIdP
makeSampleIdPMetadataWithIssuer suffix = do
  let issuerUri = pack $ "https://issuer.net/_" <> suffix
      requriUri = pack $ "https://requri.net/_req_" <> suffix
      issuer = SAML.Issuer . fromRight' $ SAML.parseURI' issuerUri
      requri = fromRight' $ SAML.parseURI' requriUri
  (privcreds, creds, cert) <- liftIO $ SAML.mkSignCredsWithCert Nothing 96
  pure $ SampleIdP (SAML.IdPMetadata issuer requri (cert :| [])) privcreds creds cert
