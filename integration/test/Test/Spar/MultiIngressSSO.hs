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

      finalizeLoginWithWrongZHost bertZHost ernieZHost domain tid ernieEmail (idpId, idpMeta) `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 200

        let titleName = XML.Name (cs "title") (Just (cs "http://www.w3.org/1999/xhtml")) Nothing
            getRoot :: ByteString -> Maybe XML.Cursor
            getRoot = pure . KXML.parseXml . cs

        ((getRoot >=> KXML.findElement titleName >=> KXML.getContent) resp.body)
          `shouldMatch` (Just "wire:sso:error:forbidden")

      makeSuccessfulSamlLogin domain ernieZHost tid ernieEmail idpId idpMeta

      bertEmail <- ("bert@" <>) <$> randomDomain
      checkMetadataSPIssuer domain bertZHost tid
      checkAuthnSPIssuer domain bertZHost idpId tid

      makeSuccessfulSamlLogin domain bertZHost tid bertEmail idpId idpMeta

      -- Kermit's domain is not configured
      kermitEmail <- ("kermit@" <>) <$> randomDomain
      getSPMetadataWithZHost domain (Just kermitZHost) tid `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 404
        resp.json %. "label" `shouldMatch` "not-found"

      initiateSamlLoginWithZHost domain (Just kermitZHost) idpId `bindResponse` \authnreq -> do
        authnreq.status `shouldMatchInt` 404
        authnreq.json %. "label" `shouldMatch` "not-found"

      finalizeLoginWithWrongZHost bertZHost kermitZHost domain tid kermitEmail (idpId, idpMeta) `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 404

checkAuthnSPIssuer :: (HasCallStack) => String -> String -> String -> String -> App ()
checkAuthnSPIssuer domain host idpId tid =
  initiateSamlLoginWithZHost domain (Just host) idpId `bindResponse` \authnreq -> do
    authnreq.status `shouldMatchInt` 200

    let inputName = XML.Name (cs "input") (Just (cs "http://www.w3.org/1999/xhtml")) Nothing
        valueName = XML.Name (cs "value") Nothing Nothing
        issuerName = XML.Name (cs "Issuer") (Just (cs "urn:oasis:names:tc:SAML:2.0:assertion")) Nothing

        decodeBase64 :: T.Text -> Maybe ByteString
        decodeBase64 = either (const Nothing) Just . Data.ByteString.Base64.decode . cs

        targetSPUrl = T.pack ("https://" <> host <> "/sso/finalize-login/" <> tid)

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
  void $ loginWithSamlWithZHost (Just host) domain True tid email (idpId, idpMeta)
  activateEmail domain email
  getUsersByEmail domain [email] `bindResponse` \res -> do
    res.status `shouldMatchInt` 200
    user <- res.json >>= asList >>= assertOne
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
