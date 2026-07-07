-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
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

module Test.Spar.MultiIngressSSO where

import API.BrigInternal
import API.Common
import API.GalleyInternal
import API.Spar
import Control.Arrow ((>>>))
import Data.ByteString.Base64
import Data.String.Conversions (cs)
import qualified Data.Text as T
import qualified Data.UUID as UUID
import GHC.Stack
import qualified SAML2.WebSSO as SAML
import SetupHelpers
import qualified Testlib.KleisliXML as KXML
import Testlib.Prelude
import qualified Text.XML as XML
import qualified Text.XML.Cursor as XML
import qualified Text.XML.DSig as SAML

-- | Test multi-ingress SSO with an IdP that is not bound to a domain.
--
-- In this case NO SSO login can happen as a redirect to a common IdP would
-- leak information about relationships to a common backend. Also, in reality
-- it is very hard (to impossible for some IdPs) to find a valid and sound
-- configuration for multiple domains at IdP SaaS.
testMultiIngressSSOGeneralIdp :: (HasCallStack) => App ()
testMultiIngressSSOGeneralIdp = do
  let ernieZHost = "nginz-https.ernie.example.com"
      bertZHost = "nginz-https.bert.example.com"
      kermitZHost = "nginz-https.kermit.example.com"

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
                        [ "spAppUri" .= "https://webapp.ernie.example.com",
                          "spSsoUri" .= "https://nginz-https.ernie.example.com/sso",
                          "contacts" .= [object ["type" .= "ContactTechnical"]]
                        ],
                    bertZHost
                      .= object
                        [ "spAppUri" .= "https://webapp.bert.example.com",
                          "spSsoUri" .= "https://nginz-https.bert.example.com/sso",
                          "contacts" .= [object ["type" .= "ContactTechnical"]]
                        ]
                  ]
              )
      }
    $ \domain -> do
      (owner, tid, _) <- createTeam domain 1
      void $ setTeamFeatureStatus owner tid "sso" "enabled"

      (idp, _idpMeta) <- registerTestIdPWithMetaWithPrivateCreds owner
      idpId <- asString $ idp.json %. "id"

      _ernieEmail <- ("ernie@" <>) <$> randomDomain
      checkSPMetadata domain ernieZHost tid

      -- Z-Host set, but IdP has no domain -> failure
      initiateSamlLoginWithZHost domain (Just ernieZHost) idpId >>= assertLabel 404 "not-found"

      precheckSamlLoginWithZHost domain (Just ernieZHost) idpId >>= assertStatus 404

      -- When multi-ingress is configured, domain match is mandatory (empty Z-Host is a no-match)
      initiateSamlLoginWithZHost domain Nothing idpId >>= assertLabel 404 "not-found"

      precheckSamlLoginWithZHost domain Nothing idpId >>= assertStatus 404

      -- Kermit's domain is not configured at all
      _kermitEmail <- ("kermit@" <>) <$> randomDomain
      getSPMetadataWithZHost domain (Just kermitZHost) tid >>= assertLabel 404 "not-found"

      initiateSamlLoginWithZHost domain (Just kermitZHost) idpId >>= assertLabel 404 "not-found"

      precheckSamlLoginWithZHost domain (Just kermitZHost) idpId >>= assertStatus 404

-- | Test multi-ingress SSO with an IdP that is bound to a domain.
--
-- The IdP is created on a multi-ingress domain. The details of managing
-- multi-ingress IdPs are covered in `Test.Spar.MultiIngressIdp`. Here we want
-- to test that logins are possible with such an IdP, but only if the request's
-- and IdP's domains match.
testMultiIngressSSODomainBoundIdp :: (HasCallStack) => App ()
testMultiIngressSSODomainBoundIdp = do
  let ernieZHost = "nginz-https.ernie.example.com"
      bertZHost = "nginz-https.bert.example.com"
      kermitZHost = "nginz-https.kermit.example.com"

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
                        [ "spAppUri" .= "https://webapp.ernie.example.com",
                          "spSsoUri" .= "https://nginz-https.ernie.example.com/sso",
                          "contacts" .= [object ["type" .= "ContactTechnical"]]
                        ],
                    bertZHost
                      .= object
                        [ "spAppUri" .= "https://webapp.bert.example.com",
                          "spSsoUri" .= "https://nginz-https.bert.example.com/sso",
                          "contacts" .= [object ["type" .= "ContactTechnical"]]
                        ]
                  ]
              )
      }
    $ \domain -> do
      (owner, tid, _) <- createTeam domain 1
      void $ setTeamFeatureStatus owner tid "sso" "enabled"

      (idp, idpMeta) <- registerTestIdPWithMetaWithPrivateCredsForZHost owner (Just ernieZHost)
      idpId <- asString $ idp.json %. "id"

      ernieEmail <- ("ernie@" <>) <$> randomDomain
      checkSPMetadata domain ernieZHost tid
      checkAuthnRequest domain ernieZHost idpId tid

      -- Ernie's precheck succeeds for the correct domain
      precheckSamlLoginWithZHost domain (Just ernieZHost) idpId >>= assertStatus 200

      makeSuccessfulSamlLogin domain ernieZHost tid ernieEmail idpId idpMeta

      -- SAML flow cannot be intercepted and redirected to another domain
      finalizeLoginWithWrongZHost ernieZHost bertZHost domain tid ernieEmail (idpId, idpMeta)
        `bindResponse` \resp -> do
          assertStatus 200 resp
          let titleName = XML.Name (cs "title") (Just (cs "http://www.w3.org/1999/xhtml")) Nothing
              getRoot :: ByteString -> Maybe XML.Cursor
              getRoot = pure . KXML.parseXml . cs
          ((getRoot >=> KXML.findElement titleName >=> KXML.getContent) resp.body)
            `shouldMatch` Just "wire:sso:error:forbidden"

      _bertEmail <- ("bert@" <>) <$> randomDomain
      checkSPMetadata domain bertZHost tid

      -- Bert cannot initiate a login with an Ernie IdP
      initiateSamlLoginWithZHost domain (Just bertZHost) idpId >>= assertLabel 404 "not-found"

      precheckSamlLoginWithZHost domain (Just bertZHost) idpId >>= assertStatus 404

      -- Kermit's domain is not configured at all
      _kermitEmail <- ("kermit@" <>) <$> randomDomain
      getSPMetadataWithZHost domain (Just kermitZHost) tid >>= assertLabel 404 "not-found"

      initiateSamlLoginWithZHost domain (Just kermitZHost) idpId >>= assertLabel 404 "not-found"

      precheckSamlLoginWithZHost domain (Just kermitZHost) idpId >>= assertStatus 404

-- | Test that without multi-ingress configuration, endpoints are domain-agnostic.
--
-- In the standard (non-multi-ingress) case, SAML endpoints work regardless of
-- which domain (Z-Host) is used or if no Z-Host is specified.
testSsoWithoutMultiIngress :: (HasCallStack) => App ()
testSsoWithoutMultiIngress = do
  let host1 = "nginz-https.host1.example.com"
      host2 = "nginz-https.host2.example.com"
      spHost = "nginz-https.example.com"

  withModifiedBackend
    def
      { sparCfg =
          setField "saml.spSsoUri" ("https://" <> spHost <> "/sso")
            >=> setField "saml.spAppUri" ("https://" <> spHost <> "/")
            >=> setField "saml.contacts" [object ["type" .= "ContactTechnical"]]
      }
    $ \domain -> do
      (owner, tid, _) <- createTeam domain 1
      void $ setTeamFeatureStatus owner tid "sso" "enabled"

      -- Create both types of IdPs
      idpGeneral <- registerTestIdPWithMetaWithPrivateCreds owner
      idpZHost1 <- registerTestIdPWithMetaWithPrivateCredsForZHost owner (Just host1)
      idpZHost2 <- registerTestIdPWithMetaWithPrivateCredsForZHost owner (Just host2)

      forM_
        [idpGeneral, idpZHost1, idpZHost2]
        $ \(idp, idpMeta) -> do
          idpId <- asString $ idp.json %. "id"

          -- Precheck succeeds from any domain
          precheckSamlLoginWithZHost domain (Just host1) idpId >>= assertStatus 200
          precheckSamlLoginWithZHost domain (Just host2) idpId >>= assertStatus 200
          precheckSamlLoginWithZHost domain Nothing idpId >>= assertStatus 200

          -- SP metadata is accessible from any domain
          getSPMetadataWithZHost domain (Just host1) tid >>= assertStatus 200
          getSPMetadataWithZHost domain (Just host2) tid >>= assertStatus 200
          getSPMetadataWithZHost domain Nothing tid >>= assertStatus 200

          -- AuthnRequest Issuer is same regardless of Z-Host (domain-agnostic)
          checkAuthnRequestBase domain (Just host1) spHost idpId tid
          checkAuthnRequestBase domain (Just host2) spHost idpId tid
          checkAuthnRequestBase domain Nothing spHost idpId tid

          -- Authentication request can be initiated from any domain
          initiateSamlLoginWithZHost domain (Just host1) idpId >>= assertStatus 200
          initiateSamlLoginWithZHost domain (Just host2) idpId >>= assertStatus 200
          initiateSamlLoginWithZHost domain Nothing idpId >>= assertStatus 200

          -- SAML login
          email1 <- ("user1@" <>) <$> randomDomain
          makeSuccessfulSamlLogin domain host1 tid email1 idpId idpMeta

          email2 <- ("user2@" <>) <$> randomDomain
          makeSuccessfulSamlLogin domain host2 tid email2 idpId idpMeta

          email3 <- ("user3@" <>) <$> randomDomain
          let nameId3 = fromRight (error "could not create name id") $ SAML.emailNameID (cs email3)
          void $ loginWithSamlWithZHost Nothing domain True tid nameId3 (idpId, idpMeta)

-- | Check the AuthnRequest by the SP (Wire backend) to be sent to the IdP
--
-- Most important: The @Issuer@ must fit to the multi-ingress domain (@host@).
checkAuthnRequest :: (HasCallStack, MakesValue domain) => domain -> String -> String -> String -> App ()
checkAuthnRequest domain host idpId tid = checkAuthnRequestBase domain (Just host) host idpId tid

-- | Check the AuthnRequest by the SP (Wire backend) to be sent to the IdP
--
-- Compares the Issuer in the request against an expected target host URL. This
-- allows testing that requests to different hosts produce different (or same)
-- Issuer URLs.
checkAuthnRequestBase :: (HasCallStack, MakesValue domain) => domain -> Maybe String -> String -> String -> String -> App ()
checkAuthnRequestBase domain mbRequestHost targetHost idpId tid =
  initiateSamlLoginWithZHost domain mbRequestHost idpId `bindResponse` \authnreq -> do
    assertStatus 200 authnreq

    let inputName = XML.Name (cs "input") (Just (cs "http://www.w3.org/1999/xhtml")) Nothing
        valueName = XML.Name (cs "value") Nothing Nothing
        issuerName = XML.Name (cs "Issuer") (Just (cs "urn:oasis:names:tc:SAML:2.0:assertion")) Nothing

        decodeBase64 :: T.Text -> Maybe ByteString
        decodeBase64 = either (const Nothing) Just . Data.ByteString.Base64.decode . cs

        targetSPUrl = T.pack ("https://" <> targetHost <> "/sso/finalize-login/" <> tid)

        getIssuerUrl :: ByteString -> Maybe T.Text
        getIssuerUrl =
          (pure . KXML.parseXml . cs)
            >=> KXML.findElement inputName
            >=> KXML.getAttribute valueName
            >=> (cs >>> decodeBase64)
            >=> (cs >>> (pure . KXML.parseXml))
            >=> KXML.findElement issuerName
            >=> KXML.getContent

    getIssuerUrl authnreq.body `shouldMatch` targetSPUrl

-- | Check the metadata of the ServiceProvider (i.e. of the Wire backend on multi-ingress domain @host@)
checkSPMetadata :: (HasCallStack, MakesValue domain) => domain -> String -> String -> App ()
checkSPMetadata domain host tid =
  getSPMetadataWithZHost domain (Just host) tid `bindResponse` \resp -> do
    assertStatus 200 resp

    let spSsoDescName = XML.Name (cs "SPSSODescriptor") (Just (cs "urn:oasis:names:tc:SAML:2.0:metadata")) (Just (cs "md"))
        acsName = XML.Name (cs "AssertionConsumerService") (Just (cs "urn:oasis:names:tc:SAML:2.0:metadata")) (Just (cs "md"))
        orgName = XML.Name (cs "Organization") (Just (cs "urn:oasis:names:tc:SAML:2.0:metadata")) (Just (cs "md"))
        orgUrlName = XML.Name (cs "OrganizationURL") (Just (cs "urn:oasis:names:tc:SAML:2.0:metadata")) (Just (cs "md"))
        entityIdName = XML.Name (cs "entityID") Nothing Nothing
        locationName = XML.Name (cs "Location") Nothing Nothing

        targetSPUrl = T.pack ("https://" <> host <> "/sso/finalize-login/" <> tid)

        root = (KXML.parseXml . cs) resp.body

        getLocation :: XML.Cursor -> Maybe T.Text
        getLocation =
          KXML.findElement spSsoDescName
            >=> KXML.findElement acsName
            >=> KXML.getAttribute locationName

        getOrgUrlContent :: XML.Cursor -> Maybe T.Text
        getOrgUrlContent =
          KXML.findElement spSsoDescName
            >=> KXML.findElement orgName
            >=> KXML.findElement orgUrlName
            >=> KXML.getContent

    KXML.getAttribute entityIdName root `shouldMatch` Just targetSPUrl
    getLocation root `shouldMatch` Just targetSPUrl
    getOrgUrlContent root `shouldMatch` Just targetSPUrl

makeSuccessfulSamlLogin ::
  (MakesValue domain) =>
  domain ->
  String ->
  String ->
  String ->
  String ->
  (SAML.IdPMetadata, SAML.SignPrivCreds) ->
  App ()
makeSuccessfulSamlLogin domain host tid email idpId idpMeta = do
  let nameId = fromRight (error "could not create name id") $ SAML.emailNameID (cs email)
  void $ loginWithSamlWithZHost (Just host) domain True tid nameId (idpId, idpMeta)
  activateEmail domain email
  getUsersByEmail domain [email] `bindResponse` \res -> do
    assertStatus 200 res
    user <- res.json & asList >>= assertOne
    user %. "status" `shouldMatch` "active"
    user %. "email" `shouldMatch` email

finalizeLoginWithWrongZHost ::
  (MakesValue domain, HasCallStack) =>
  String ->
  String ->
  domain ->
  String ->
  String ->
  (String, (SAML.IdPMetadata, SAML.SignPrivCreds)) ->
  App Response
finalizeLoginWithWrongZHost zHost1 zHost2 domain tid email (iid, (meta, privcreds)) = do
  let idpConfig = SAML.IdPConfig (SAML.IdPId (fromMaybe (error "invalid idp id") (UUID.fromString iid))) meta ()
  spmeta <- getSPMetadataWithZHost domain (Just zHost1) tid
  authnreq <- initiateSamlLoginWithZHost domain (Just zHost1) iid
  let nameId = fromRight (error "could not create name id") $ SAML.emailNameID (cs email)
      spMetaData = toSPMetaData spmeta.body
      parsedAuthnReq = parseAuthnReqResp authnreq.body
  authnReqResp <- makeAuthnResponse nameId privcreds idpConfig spMetaData parsedAuthnReq
  finalizeSamlLoginWithZHost domain (Just zHost2) tid authnReqResp
  where
    toSPMetaData :: ByteString -> SAML.SPMetadata
    toSPMetaData bs = fromRight (error "could not decode spmetatdata") $ SAML.decode $ cs bs
