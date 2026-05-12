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
import API.Common (randomHandle)
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
import Control.Lens ((.~), (^.))
import Data.ByteString.Char8 (unpack)
import Data.Either.Extra
import Data.String.Conversions (cs)
import Data.Text (pack)
import qualified Data.UUID as UUID
import qualified Data.X509 as X509
import GHC.Stack
import qualified SAML2.WebSSO as SAML
import qualified SAML2.WebSSO.Test.MockResponse as SAML
import SAML2.WebSSO.Test.Util
import SetupHelpers
import Testlib.Certs (fingerprintHex)
import Testlib.Prelude
import qualified Text.XML.DSig as SAML

ernieDomain, bertDomain, ernieZHost, bertZHost :: String
ernieDomain = "ernie.example.com"
bertDomain = "bert.example.com"
ernieZHost = "nginz-https." <> ernieDomain
bertZHost = "nginz-https." <> bertDomain

-- | Test that - in a multi-ingress scenario -  a user provisioned under one
-- IdP can log in via another IdP, with their SSO identity migrating to the new
-- IdP transparently.
--
-- Covers both SCIM-provisioned and auto-provisioned users, and verifies
-- back-and-forth migration between IdPs.
testCrossIdpSsoMigration :: (HasCallStack) => TaggedBool "useScim" -> App ()
testCrossIdpSsoMigration (TaggedBool useSCIM) = do
  ernieCredsWithCert@(_, _, ernieCert) <- liftIO $ SAML.mkSignCredsWithCert Nothing 96
  bertCredsWithCert@(_, _, bertCert) <- liftIO $ SAML.mkSignCredsWithCert Nothing 96

  withMultiIngressBackend [ernieDomain, bertDomain] [ernieCert, bertCert] $ \domain -> do
    (owner, tid, _) <- createTeam domain 1

    -- Register IdP for Ernie's domain
    SampleIdP idpMetaErnie pCredsErnie _ _ <- makeSampleIdPMetadataWithIssuer ernieCredsWithCert "ernie"
    idpErnie <- createIdpWithZHostV2 owner (Just ernieZHost) idpMetaErnie
    idpIdErnie <- asString $ idpErnie.json %. "id"

    -- Register IdP for Bert's domain
    SampleIdP idpMetaBert pCredsBert _ _ <- makeSampleIdPMetadataWithIssuer bertCredsWithCert "bert"
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
          scimToken <- createScimToken owner (def {idp = Just idpIdErnie})
          scimTokenStr <- scimToken.json %. "token" & asString

          -- Create SCIM user with the email
          scimUser <- randomScimUserWithEmail biboEmail biboEmail
          scimUid <- bindResponse (createScimUser domain scimTokenStr scimUser) $ \resp -> do
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

    userIdErnieAgain <- assertJust "Expected user ID from re-login on Ernie domain" mUserIdErnieAgain
    userIdErnieAgain `shouldMatch` userIdErnie

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
    userIdBert <- assertJust "Expected user ID from cross-IdP SSO login on Bert domain" mUserIdBert
    userIdBert `shouldMatch` userIdErnie

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

    userIdErnieFinal <- assertJust "Expected user ID from final login on Ernie domain" mUserIdErnieFinal
    userIdErnieFinal `shouldMatch` userIdErnie

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
  ernieCredsWithCert@(_, _, ernieCert) <- liftIO $ SAML.mkSignCredsWithCert Nothing 96
  bertCredsWithCert@(_, _, bertCert) <- liftIO $ SAML.mkSignCredsWithCert Nothing 96

  withMultiIngressBackend [ernieDomain, bertDomain] [ernieCert, bertCert] $ \domain -> do
    (owner, tid, _) <- createTeam domain 1

    -- Register IdP for Ernie's domain
    SampleIdP idpMetaErnie pCredsErnie _ _ <- makeSampleIdPMetadataWithIssuer ernieCredsWithCert "ernie"
    idpErnie <- createIdpWithZHostV2 owner (Just ernieZHost) idpMetaErnie
    idpIdErnie <- asString $ idpErnie.json %. "id"

    -- Register IdP for Bert's domain
    SampleIdP idpMetaBert pCredsBert _ _ <- makeSampleIdPMetadataWithIssuer bertCredsWithCert "bert"
    idpBert <- createIdpWithZHostV2 owner (Just bertZHost) idpMetaBert
    idpIdBert <- asString $ idpBert.json %. "id"

    ernieIssuer <- idpErnie.json %. "metadata.issuer" >>= asString
    bertIssuer <- idpBert.json %. "metadata.issuer" >>= asString

    (biboEmail, biboNameId) <- randomEmailNameId

    -- Provision SCIM user for Ernie's IdP
    scimToken <- createScimToken owner (def {idp = Just idpIdErnie})
    scimTokenStr <- scimToken.json %. "token" & asString

    scimUser <- randomScimUserWithEmail biboEmail biboEmail
    biboUid <- bindResponse (createScimUser domain scimTokenStr scimUser) $ \resp -> do
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

    userIdErnie <- assertJust "Expected user ID from login on Ernie domain" mUserIdErnie
    userIdErnie `shouldMatch` biboUid

    -- Verify user's SSO ID was migrated back to Ernie's issuer
    getUsersId domain [biboUid] `bindResponse` \resp -> do
      resp.status `shouldMatchInt` 200
      ssoId <- resp.json %. "0.sso_id"
      ssoIdTenant <- ssoId %. "tenant" >>= asString
      ssoIdTenant `shouldContain` ernieIssuer
      ssoIdTenant `shouldNotMatch` bertIssuer

-- | Login fails when the authenticating IdP's issuer is not registered for the target domain.
testIdpNotFoundError :: (HasCallStack) => App ()
testIdpNotFoundError = do
  ernieCredsWithCert@(_, _, ernieCert) <- liftIO $ SAML.mkSignCredsWithCert Nothing 96
  bertCredsWithCert@(_, _, bertCert) <- liftIO $ SAML.mkSignCredsWithCert Nothing 96

  withMultiIngressBackend [ernieDomain, bertDomain] [ernieCert, bertCert] $ \domain -> do
    (owner, tid, _) <- createTeam domain 1

    -- Register TWO IdPs: one for Ernie domain, one for Bert domain
    SampleIdP idpMetaErnie pCredsErnie _ _ <- makeSampleIdPMetadataWithIssuer ernieCredsWithCert "ernie"
    idpErnie <- createIdpWithZHostV2 owner (Just ernieZHost) idpMetaErnie
    idpIdErnie <- asString $ idpErnie.json %. "id"
    ernieIssuer <- idpErnie.json %. "metadata.issuer" >>= asString

    SampleIdP idpMetaBert _ _ _ <- makeSampleIdPMetadataWithIssuer bertCredsWithCert "bert"
    _idpBert <- createIdpWithZHostV2 owner (Just bertZHost) idpMetaBert

    (_biboEmail, biboNameId) <- randomEmailNameId

    -- Initiate on ernie, then replay the response against bert's endpoint (cross-domain).
    -- Spar rejects: ernie's issuer is not configured for bert's domain.
    spmetaBert <- getSPMetadataWithZHost domain (Just bertZHost) tid
    authnReqRaw <- initiateSamlLoginWithZHostAndLabel domain (Just ernieZHost) Nothing idpIdErnie
    let spMetaDataBert = fromRight (error "could not decode spmetadata") $ SAML.decode $ cs spmetaBert.body
        -- Manipulate: redirect the ernie authn request to bert's SP issuer so the audience
        -- restriction in the SAML response targets bert's ACS URL, not ernie's.
        parsedAuthnReqErnie =
          parseAuthnReqResp authnReqRaw.body
            & SAML.rqIssuer .~ SAML.Issuer (spMetaDataBert ^. SAML.spResponseURL)
        idpConfigErnie =
          SAML.IdPConfig
            (SAML.IdPId (fromMaybe (error "invalid idp id") (UUID.fromString idpIdErnie)))
            idpMetaErnie
            ()
    authnReqResp <-
      runSimpleSP
        $ SAML.mkAuthnResponseWithSubj
          biboNameId
          pCredsErnie
          idpConfigErnie
          spMetaDataBert
          (Just parsedAuthnReqErnie)
          True

    bindResponse (finalizeSamlLoginWithZHost domain (Just bertZHost) tid authnReqResp) $ \resp -> do
      resp.status `shouldMatchInt` 200
      let bdy = unpack resp.body
      bdy `shouldContain` "wire:sso:error:"
      bdy `shouldContain` "\"type\":\"AUTH_ERROR\""
      bdy `shouldContain` "wire:sso:error:not-found"
      bdy `shouldContain` "\"label\":\"forbidden\""
      let expectedErrorMsg =
            "Could not find IdP: IdP with issuer '"
              <> ernieIssuer
              <> "' for domain '"
              <> bertZHost
              <> "' is not configured for this team"
      bdy `shouldContain` expectedErrorMsg

-- | Test that a user of one team cannot log in using the IdP of a different team.
--
-- Team B's IdP must not grant access to Team A, even when the SAML response is otherwise
-- well-formed.
testCrossTeamIdpLoginRejected :: (HasCallStack) => App ()
testCrossTeamIdpLoginRejected = do
  credsA@(_, _, certA) <- liftIO $ SAML.mkSignCredsWithCert Nothing 96
  credsB@(_, _, certB) <- liftIO $ SAML.mkSignCredsWithCert Nothing 96

  withMultiIngressBackend [ernieDomain, bertDomain] [certA, certB] $ \domain -> do
    -- Team A with IdP A on bert domain
    (ownerA, tidA, _) <- createTeam domain 1
    SampleIdP idpMetaA pCredsA _ _ <- makeSampleIdPMetadataWithIssuer credsA "team-a"
    idpA <- createIdpWithZHostV2 ownerA (Just bertZHost) idpMetaA
    idpIdA <- asString $ idpA.json %. "id"

    -- Team B with IdP B on ernie domain
    (ownerB, _, _) <- createTeam domain 1
    SampleIdP idpMetaB pCredsB _ _ <- makeSampleIdPMetadataWithIssuer credsB "team-b"
    idpB <- createIdpWithZHostV2 ownerB (Just ernieZHost) idpMetaB
    idpIdB <- asString $ idpB.json %. "id"

    -- Create Bibo as a user of Team A
    (biboEmail, biboNameId) <- randomEmailNameId
    _ <- loginWithSamlWithZHost (Just bertZHost) domain True tidA biboNameId (idpIdA, (idpMetaA, pCredsA))
    activateEmail domain biboEmail

    -- IdP B lives on ernie and can be initiated there, but finalization fails because IdP B
    -- belongs to Team B, not Team A.
    authnReqRespErnie <- buildSamlAuthnResponse domain ernieZHost tidA idpIdB idpMetaB pCredsB biboNameId
    bindResponse (finalizeSamlLoginWithZHost domain (Just ernieZHost) tidA authnReqRespErnie) $ \resp -> do
      resp.status `shouldMatchInt` 404
      extractSAMLErrorPageContent resp.body `shouldContain` "IdpNotFound"

    -- IdP B lives on ernie, not bert: initiation on bert is rejected
    -- immediately independent of the team (domain mismatch).
    bindResponse (initiateSamlLoginWithZHostAndLabel domain (Just bertZHost) Nothing idpIdB) $ \resp ->
      resp.status `shouldMatchInt` 404

-- | Test that non-email NameIDs are rejected in multi-ingress mode.
--
-- Multi-ingress cross-IdP SSO requires email-based NameIDs to prevent ambiguities.
testNonEmailNameIdRejectedInMultiIngress :: (HasCallStack) => App ()
testNonEmailNameIdRejectedInMultiIngress = do
  bertCredsWithCert@(_, _, bertCert) <- liftIO $ SAML.mkSignCredsWithCert Nothing 96

  withMultiIngressBackend [bertDomain] [bertCert] $ \domain -> do
    (owner, tid, _) <- createTeam domain 1

    -- Register IdP
    SampleIdP idpMetaBert pCredsBert _ _ <- makeSampleIdPMetadataWithIssuer bertCredsWithCert "bert"
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
      bdy `shouldContain` "Multi-ingress SSO only supports email-based NameIDs for cross-IdP migration. Username-based NameIDs are not allowed."

-- | Test that SAML responses without a prior authentication request are rejected.
--
-- A response referencing a request Spar never stored results in a "bad InResponseTo" error.
testUnsolicitedSamlResponseRejected :: (HasCallStack) => App ()
testUnsolicitedSamlResponseRejected = do
  ernieCredsWithCert@(_, _, ernieCert) <- liftIO $ SAML.mkSignCredsWithCert Nothing 96
  bertCredsWithCert@(_, _, bertCert) <- liftIO $ SAML.mkSignCredsWithCert Nothing 96

  withMultiIngressBackend [ernieDomain, bertDomain] [ernieCert, bertCert] $ \domain -> do
    (owner, tid, _) <- createTeam domain 1

    SampleIdP idpMetaErnie _ _ _ <- makeSampleIdPMetadataWithIssuer ernieCredsWithCert "ernie"
    void $ createIdpWithZHostV2 owner (Just ernieZHost) idpMetaErnie

    SampleIdP idpMetaBert pCredsBert _ _ <- makeSampleIdPMetadataWithIssuer bertCredsWithCert "bert"
    idpBert <- createIdpWithZHostV2 owner (Just bertZHost) idpMetaBert
    idpIdBert <- asString $ idpBert.json %. "id"

    (_biboEmail, biboNameId) <- randomEmailNameId

    spmeta <- getSPMetadataWithZHost domain (Just bertZHost) tid
    let spMetaData = fromRight (error "could not decode spmetadata") $ SAML.decode $ cs spmeta.body
        idpConfig = SAML.IdPConfig (SAML.IdPId (fromMaybe (error "invalid idp id") (UUID.fromString idpIdBert))) idpMetaBert ()
    -- Create a local authn request (stored in SimpleSP's in-memory store, not in Spar's database)
    localReq <- runSimpleSP $ SAML.createAuthnRequest 300 (idpMetaBert ^. SAML.edIssuer) (idpMetaBert ^. SAML.edIssuer)
    authnReqResp <- makeAuthnResponse biboNameId pCredsBert idpConfig spMetaData localReq

    -- Spar cannot find the request (no verdict format stored), so it rejects with server error.
    -- This is not a user flow, so we can accept any error - even 500 - here.
    bindResponse (finalizeSamlLoginWithZHost domain (Just bertZHost) tid authnReqResp) $ \resp -> do
      resp.status `shouldMatchInt` 500
      resp.json %. "label" `shouldMatch` "server-error"

-- | Test that SAML responses for one ingress are rejected when submitted to a
-- different ingress.
--
-- A login request on the ernie ingress must be finalized on the ernie ingress.
-- Finalizing on the bert ingress should fail with a bad recipient error.
testCrossIngressRequestResponseMismatch :: (HasCallStack) => App ()
testCrossIngressRequestResponseMismatch = do
  ernieCredsWithCert@(_, _, ernieCert) <- liftIO $ SAML.mkSignCredsWithCert Nothing 96
  bertCredsWithCert@(_, _, bertCert) <- liftIO $ SAML.mkSignCredsWithCert Nothing 96

  withMultiIngressBackend [ernieDomain, bertDomain] [ernieCert, bertCert] $ \domain -> do
    (owner, tid, _) <- createTeam domain 1

    SampleIdP idpMetaErnie pCredsErnie _ _ <- makeSampleIdPMetadataWithIssuer ernieCredsWithCert "ernie"
    idpErnie <- createIdpWithZHostV2 owner (Just ernieZHost) idpMetaErnie
    idpIdErnie <- asString $ idpErnie.json %. "id"

    SampleIdP idpMetaBert _ _ _ <- makeSampleIdPMetadataWithIssuer bertCredsWithCert "bert"
    void $ createIdpWithZHostV2 owner (Just bertZHost) idpMetaBert

    (_biboEmail, biboNameId) <- randomEmailNameId

    -- The SAML response's Destination is ernie's ACS (Assertion Consumer Service) URL,
    -- i.e. ernie's /sso/finalize-login endpoint. Submitting it to bert's endpoint causes
    -- a Destination mismatch ("bad Recipient").
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
-- Optionally accepts IdP certificates to add to the allowlist.
withMultiIngressBackend :: (HasCallStack) => [String] -> [X509.SignedCertificate] -> (String -> App ()) -> App ()
withMultiIngressBackend baseDomains certs action =
  withModifiedBackend
    def
      { sparCfg =
          removeField "saml.spSsoUri"
            >=> removeField "saml.spAppUri"
            >=> removeField "saml.contacts"
            >=> setField "saml.spDomainConfigs" (object (map mkDomainEntry baseDomains))
            >=> setField "enableIdPByEmailDiscovery" True
            >=> if null certs
              then pure
              else setField "idpCertFingerprintAllowlist" (map fingerprintHex certs),
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
  spmeta <- getSPMetadataWithZHost domain (Just mbZHost) tid
  authnreq <- initiateSamlLoginWithZHostAndLabel domain (Just mbZHost) Nothing idpId
  let spMetaData = fromRight (error "could not decode spmetadata") $ SAML.decode $ cs spmeta.body
      parsedAuthnReq = parseAuthnReqResp authnreq.body
      idpConfig =
        SAML.IdPConfig
          (SAML.IdPId (fromMaybe (error "invalid idp id") (UUID.fromString idpId)))
          idpMeta
          ()
  makeAuthnResponse nameId pcreds idpConfig spMetaData parsedAuthnReq
