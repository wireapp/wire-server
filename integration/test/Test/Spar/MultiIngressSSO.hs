module Test.Spar.MultiIngressSSO where

import API.BrigInternal
import API.Common
import API.GalleyInternal
import API.Spar
import Control.Arrow ((>>>))
import Data.ByteString.Base64
import Data.String.Conversions (cs)
import qualified Data.Text as T
import GHC.Stack
import SetupHelpers
import qualified Testlib.KleisliXML as KXML
import Testlib.Prelude
import qualified Text.XML as XML
import qualified Text.XML.Cursor as XML

testMultiIngressSSO :: (HasCallStack) => App ()
testMultiIngressSSO = do
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
      checkMetadataSPIssuer domain ernieZHost tid
      checkAuthnSPIssuer domain ernieZHost idpId tid

      void $ loginWithSamlWithZHost (Just ernieZHost) domain True tid ernieEmail (idpId, idpMeta)
      activateEmail domain ernieEmail
      getUsersByEmail domain [ernieEmail] `bindResponse` \res -> do
        res.status `shouldMatchInt` 200
        user <- res.json >>= asList >>= assertOne
        user %. "status" `shouldMatch` "active"
        user %. "email" `shouldMatch` ernieEmail

      bertEmail <- ("bert@" <>) <$> randomDomain
      checkMetadataSPIssuer domain bertZHost tid
      checkAuthnSPIssuer domain bertZHost idpId tid

      void $ loginWithSamlWithZHost (Just bertZHost) domain True tid bertEmail (idpId, idpMeta)
      activateEmail domain bertEmail
      getUsersByEmail domain [bertEmail] `bindResponse` \res -> do
        res.status `shouldMatchInt` 200
        user <- res.json >>= asList >>= assertOne
        user %. "status" `shouldMatch` "active"
        user %. "email" `shouldMatch` bertEmail

      -- kermitEmail <- ("kermit@" <>) <$> randomDomain
      getSPMetadataWithZHost domain (Just kermitZHost) tid `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 404
        resp.json %. "label" `shouldMatch` "not-found"

      initiateSamlLoginWithZHost domain (Just kermitZHost) idpId `bindResponse` \authnreq -> do
        authnreq.status `shouldMatchInt` 404
        authnreq.json %. "label" `shouldMatch` "not-found"

checkAuthnSPIssuer :: (HasCallStack) => String -> String -> String -> String -> App ()
checkAuthnSPIssuer domain host idpId tid =
  initiateSamlLoginWithZHost domain (Just host) idpId `bindResponse` \authnreq -> do
    authnreq.status `shouldMatchInt` 200

    let inputName = XML.Name (cs "input") (Just (cs "http://www.w3.org/1999/xhtml")) Nothing
        valueName = XML.Name (cs "value") Nothing Nothing
        issuerName = XML.Name (cs "Issuer") (Just (cs "urn:oasis:names:tc:SAML:2.0:assertion")) Nothing

    let decodeBase64 :: T.Text -> Maybe ByteString
        decodeBase64 = either (const Nothing) Just . Data.ByteString.Base64.decode . cs

    let targetSPUrl = T.pack ("https://" <> host <> "/sso/finalize-login/" <> tid)

    let getIssuerUrl :: ByteString -> Maybe T.Text
        getIssuerUrl =
          (pure . KXML.parseXml . cs)
            >=> KXML.findElement inputName
            >=> KXML.getAttribute valueName
            >=> (cs >>> decodeBase64)
            >=> (cs >>> (pure . KXML.parseXml))
            >=> KXML.findElement issuerName
            >=> KXML.getContent

    getIssuerUrl authnreq.body `shouldMatch` targetSPUrl

checkMetadataSPIssuer :: (HasCallStack) => String -> String -> String -> App ()
checkMetadataSPIssuer domain host tid =
  getSPMetadataWithZHost domain (Just host) tid `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 200

    let spSsoDescName = XML.Name (cs "SPSSODescriptor") (Just (cs "urn:oasis:names:tc:SAML:2.0:metadata")) (Just (cs "md"))
        acsName = XML.Name (cs "AssertionConsumerService") (Just (cs "urn:oasis:names:tc:SAML:2.0:metadata")) (Just (cs "md"))
        orgName = XML.Name (cs "Organization") (Just (cs "urn:oasis:names:tc:SAML:2.0:metadata")) (Just (cs "md"))
        orgUrlName = XML.Name (cs "OrganizationURL") (Just (cs "urn:oasis:names:tc:SAML:2.0:metadata")) (Just (cs "md"))
        entityIdName = XML.Name (cs "entityID") Nothing Nothing
        locationName = XML.Name (cs "Location") Nothing Nothing

    let targetSPUrl = T.pack ("https://" <> host <> "/sso/finalize-login/" <> tid)

    let root = (KXML.parseXml . cs) resp.body

        locationPipeline :: XML.Cursor -> Maybe T.Text
        locationPipeline =
          KXML.findElement spSsoDescName
            >=> KXML.findElement acsName
            >=> KXML.getAttribute locationName

        orgUrlContentPipeline :: XML.Cursor -> Maybe T.Text
        orgUrlContentPipeline =
          KXML.findElement spSsoDescName
            >=> KXML.findElement orgName
            >=> KXML.findElement orgUrlName
            >=> KXML.getContent

    KXML.getAttribute entityIdName root `shouldMatch` Just targetSPUrl
    locationPipeline root `shouldMatch` Just targetSPUrl
    orgUrlContentPipeline root `shouldMatch` Just targetSPUrl
