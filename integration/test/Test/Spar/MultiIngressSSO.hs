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
      getSPMetadataWithZHost domain (Just "nginz-https.kermit.example.com") tid `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 404
        resp.json %. "label" `shouldMatch` "not-found"

checkAuthnSPIssuer :: (HasCallStack) => String -> String -> String -> String -> App ()
checkAuthnSPIssuer domain host idpId tid =
  initiateSamlLoginWithZHost domain (Just host) idpId `bindResponse` \authnreq -> do
    authnreq.status `shouldMatchInt` 200

    let inputName = XML.Name (cs "input") (Just (cs "http://www.w3.org/1999/xhtml")) Nothing
        valueName = XML.Name (cs "value") Nothing Nothing
        issuerName = XML.Name (cs "Issuer") (Just (cs "urn:oasis:names:tc:SAML:2.0:assertion")) Nothing

    -- Kleisli components
    let findElement :: XML.Name -> XML.Cursor -> Maybe XML.Cursor
        findElement name = listToMaybe . (XML.$// XML.element name)

        getAttribute :: XML.Name -> XML.Cursor -> Maybe T.Text
        getAttribute name = listToMaybe . (XML.$| XML.attribute name)

        getContent :: XML.Cursor -> Maybe T.Text
        getContent = listToMaybe . (XML.$// XML.content)

        parseXml :: ByteString -> XML.Cursor
        parseXml = XML.fromDocument . XML.parseText_ XML.def . cs

        decodeBase64 :: T.Text -> Maybe ByteString
        decodeBase64 = either (const Nothing) Just . Data.ByteString.Base64.decode . cs

    let targetSPUrl = T.pack ("https://" <> host <> "/sso/finalize-login/" <> tid)

    let getIssuerUrl :: ByteString -> Maybe T.Text
        getIssuerUrl =
          (pure . parseXml)
            >=> findElement inputName
            >=> getAttribute valueName
            >=> (cs >>> decodeBase64)
            >=> (cs >>> (pure . parseXml))
            >=> findElement issuerName
            >=> getContent

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

    -- Kleisli components
    let parseXml :: ByteString -> XML.Cursor
        parseXml = XML.fromDocument . XML.parseText_ XML.def . cs

        findElement :: XML.Name -> XML.Cursor -> Maybe XML.Cursor
        findElement name = listToMaybe . (XML.$// XML.element name)

        getAttribute :: XML.Name -> XML.Cursor -> Maybe T.Text
        getAttribute name = listToMaybe . (XML.$| (XML.attribute name))

        getContent :: XML.Cursor -> Maybe T.Text
        getContent = listToMaybe . (XML.$// XML.content)

    let targetSPUrl = T.pack ("https://" <> host <> "/sso/finalize-login/" <> tid)

    let root = parseXml resp.body

        locationPipeline :: XML.Cursor -> Maybe T.Text
        locationPipeline =
          findElement spSsoDescName
            >=> findElement acsName
            >=> pure
            >=> getAttribute locationName

        orgUrlContentPipeline :: XML.Cursor -> Maybe T.Text
        orgUrlContentPipeline =
          findElement spSsoDescName
            >=> findElement orgName
            >=> findElement orgUrlName
            >=> pure
            >=> getContent

    getAttribute entityIdName root `shouldMatch` Just targetSPUrl
    locationPipeline root `shouldMatch` Just targetSPUrl
    orgUrlContentPipeline root `shouldMatch` Just targetSPUrl
