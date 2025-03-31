module Test.Spar.MultiIngressSSO where

import API.BrigInternal
import API.Common
import API.GalleyInternal
import API.Spar
import Data.ByteString.Base64
import Data.String.Conversions (cs)
import qualified Data.Text as T
import GHC.Stack
import SetupHelpers
import Testlib.Prelude
import qualified Text.XML as XML
import qualified Text.XML.Cursor as XML

testMultiIngressSSO :: (HasCallStack) => App ()
testMultiIngressSSO = do
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
                        [ "spAppUri" .= "https://webapp.ernie.example.com",
                          "spSsoUri" .= "https://nginz-https.ernie.example.com/sso",
                          "contacts" .= [object ["type" .= "ContactTechnical"]]
                        ],
                    "nginz-https.bert.example.com"
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

      (idp, idpMeta) <- registerTestIdPWithMetaWithPrivateCreds owner
      idpId <- asString $ idp.json %. "id"

      ernieEmail <- ("ernie@" <>) <$> randomDomain
      checkSPIssuer domain ernieZHost tid

      initiateSamlLoginWithZHost domain (Just ernieZHost) idpId `bindResponse` \authnreq -> do
        authnreq.status `shouldMatchInt` 200
        let root = XML.fromDocument $ XML.parseText_ XML.def (cs authnreq.body)
            inputEl = head $ root XML.$// XML.element (XML.Name (cs "input") (Just (cs "http://www.w3.org/1999/xhtml")) Nothing)
            innerXml :: T.Text = head $ inputEl XML.$| XML.attribute (XML.Name (cs "value") Nothing Nothing)
            innerXml' = either (error . show) id $ Data.ByteString.Base64.decode (cs innerXml)
            innerRoot = XML.fromDocument $ XML.parseText_ XML.def (cs innerXml')
            issuer = head $ innerRoot XML.$// XML.element (XML.Name (cs "Issuer") (Just (cs "urn:oasis:names:tc:SAML:2.0:assertion")) Nothing)
            targetSPUrl = T.pack ("https://" <> ernieZHost <> "/sso/finalize-login/" <> tid)
        (issuer XML.$// XML.content) `shouldMatch` [targetSPUrl]

      void $ loginWithSamlWithZHost (Just ernieZHost) domain True tid ernieEmail (idpId, idpMeta)
      activateEmail domain ernieEmail
      getUsersByEmail domain [ernieEmail] `bindResponse` \res -> do
        res.status `shouldMatchInt` 200
        user <- res.json >>= asList >>= assertOne
        user %. "status" `shouldMatch` "active"
        user %. "email" `shouldMatch` ernieEmail

      bertEmail <- ("bert@" <>) <$> randomDomain
      checkSPIssuer domain bertZHost tid

      void $ loginWithSamlWithZHost (Just bertZHost) domain True tid bertEmail (idpId, idpMeta)
      activateEmail domain bertEmail
      getUsersByEmail domain [bertEmail] `bindResponse` \res -> do
        res.status `shouldMatchInt` 200
        user <- res.json >>= asList >>= assertOne
        user %. "status" `shouldMatch` "active"
        user %. "email" `shouldMatch` bertEmail

      -- kermitEmail <- ("kermit@" <>) <$> randomDomain
      getSPMetadataWithZHost domain (Just "nginz-https.kermit.example.com") tid `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 404
        resp.json %. "label" `shouldMatch` "not-found"

checkSPIssuer :: (HasCallStack) => String -> String -> String -> App ()
checkSPIssuer domain host tid =
  getSPMetadataWithZHost domain (Just host) tid `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 200
    let root = XML.fromDocument $ XML.parseText_ XML.def (cs resp.body)
        desc = head $ root XML.$// XML.element (XML.Name (cs "SPSSODescriptor") (Just (cs "urn:oasis:names:tc:SAML:2.0:metadata")) (Just (cs "md")))
        assertionConsumerService = head $ desc XML.$// XML.element (XML.Name (cs "AssertionConsumerService") (Just (cs "urn:oasis:names:tc:SAML:2.0:metadata")) (Just (cs "md")))
        organization = head $ desc XML.$// XML.element (XML.Name (cs "Organization") (Just (cs "urn:oasis:names:tc:SAML:2.0:metadata")) (Just (cs "md")))
        organizationUrl = head $ organization XML.$// XML.element (XML.Name (cs "OrganizationURL") (Just (cs "urn:oasis:names:tc:SAML:2.0:metadata")) (Just (cs "md")))
        entityID = XML.attribute (XML.Name (cs "entityID") Nothing Nothing) root
        location = XML.attribute (XML.Name (cs "Location") Nothing Nothing) assertionConsumerService
        targetSPUrl = T.pack ("https://" <> host <> "/sso/finalize-login/" <> tid)

    entityID `shouldMatch` [targetSPUrl]
    location `shouldMatch` [targetSPUrl]
    (organizationUrl XML.$// XML.content) `shouldMatch` [targetSPUrl]
