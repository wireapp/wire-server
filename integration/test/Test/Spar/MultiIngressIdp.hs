module Test.Spar.MultiIngressIdp where

import API.GalleyInternal
import API.Spar
import SAML2.WebSSO.Test.Util
import SetupHelpers
import Testlib.Prelude

ernieZHost :: String
ernieZHost = "nginz-https.ernie.example.com"

bertZHost :: String
bertZHost = "nginz-https.bert.example.com"

kermitZHost :: String
kermitZHost = "nginz-https.kermit.example.com"

testMultiIngressIdp :: (HasCallStack) => App ()
testMultiIngressIdp = do
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

      -- Requests with configured multi-ingress domains
      SampleIdP idpmeta _pCreds _ _ <- makeSampleIdPMetadata
      idpId <-
        createIdpWithZHost owner (Just ernieZHost) idpmeta `bindResponse` \resp -> do
          resp.status `shouldMatchInt` 201
          resp.jsonBody %. "extraInfo.domain" `shouldMatch` ernieZHost
          resp.jsonBody %. "id" >>= asString

      getIdp owner idpId `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 200
        resp.jsonBody %. "extraInfo.domain" `shouldMatch` ernieZHost

      updateIdpWithZHost owner (Just bertZHost) idpId idpmeta `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 200
        resp.jsonBody %. "extraInfo.domain" `shouldMatch` bertZHost

      getIdp owner idpId `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 200
        resp.jsonBody %. "extraInfo.domain" `shouldMatch` bertZHost

      -- Requests with an unconfigured domain
      SampleIdP idpmeta2 _pCreds _ _ <- makeSampleIdPMetadata
      idpId2 <-
        createIdpWithZHost owner (Just kermitZHost) idpmeta2 `bindResponse` \resp -> do
          resp.status `shouldMatchInt` 201
          resp.jsonBody %. "extraInfo.domain" `shouldMatch` Null
          resp.jsonBody %. "id" >>= asString

      getIdp owner idpId2 `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 200
        resp.jsonBody %. "extraInfo.domain" `shouldMatch` Null

      -- From unconfigured to configured domain
      updateIdpWithZHost owner (Just bertZHost) idpId2 idpmeta2 `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 409
        resp.jsonBody %. "label" `shouldMatch` "idp-duplicate-domain-for-team"

      deleteIdp owner idpId `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 204

      updateIdpWithZHost owner (Just bertZHost) idpId2 idpmeta2 `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 200
        resp.jsonBody %. "extraInfo.domain" `shouldMatch` bertZHost

      getIdp owner idpId2 `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 200
        resp.jsonBody %. "extraInfo.domain" `shouldMatch` bertZHost

      -- From configured domain to unconfigured
      updateIdpWithZHost owner (Just kermitZHost) idpId2 idpmeta2 `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 200
        resp.jsonBody %. "extraInfo.domain" `shouldMatch` Null

      getIdp owner idpId2 `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 200
        resp.jsonBody %. "extraInfo.domain" `shouldMatch` Null

testMultiIngressAtMostOneIdPPerDomain :: (HasCallStack) => App ()
testMultiIngressAtMostOneIdPPerDomain = do
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

      SampleIdP idpmeta1 _pCreds _ _ <- makeSampleIdPMetadata
      idpId1 <-
        createIdpWithZHost owner (Just ernieZHost) idpmeta1 `bindResponse` \resp -> do
          resp.status `shouldMatchInt` 201
          resp.jsonBody %. "id" >>= asString

      SampleIdP idpmeta2 _pCreds _ _ <- makeSampleIdPMetadata
      void $ createIdpWithZHost owner (Just ernieZHost) idpmeta2 `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 409
        resp.jsonBody %. "label" `shouldMatch` "idp-duplicate-domain-for-team"

      updateIdpWithZHost owner (Just ernieZHost) idpId1 idpmeta2 `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 200
        resp.jsonBody %. "extraInfo.domain" `shouldMatch` ernieZHost

-- TODO: Test updating existing IDP such that two with the same domain would exist -> should fail

testNonMultiIngressSetupsCanHaveMoreIdPsPerDomain :: (HasCallStack) => App ()
testNonMultiIngressSetupsCanHaveMoreIdPsPerDomain = do
  (owner, tid, _) <- createTeam OwnDomain 1
  void $ setTeamFeatureStatus owner tid "sso" "enabled"

  -- With Z-Host header
  SampleIdP idpmeta1 _pCreds _ _ <- makeSampleIdPMetadata
  idpId1 <-
    createIdpWithZHost owner (Just ernieZHost) idpmeta1 `bindResponse` \resp -> do
      resp.status `shouldMatchInt` 201
      resp.jsonBody %. "extraInfo.domain" `shouldMatch` Null
      resp.jsonBody %. "id" >>= asString

  SampleIdP idpmeta2 _pCreds _ _ <- makeSampleIdPMetadata
  idpId2 <-
    createIdpWithZHost owner (Just ernieZHost) idpmeta2 `bindResponse` \resp -> do
      resp.status `shouldMatchInt` 201
      resp.jsonBody %. "extraInfo.domain" `shouldMatch` Null
      resp.jsonBody %. "id" >>= asString

  SampleIdP idpmeta3 _pCreds _ _ <- makeSampleIdPMetadata
  updateIdpWithZHost owner (Just ernieZHost) idpId1 idpmeta3 `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 200
    resp.jsonBody %. "extraInfo.domain" `shouldMatch` Null

  SampleIdP idpmeta4 _pCreds _ _ <- makeSampleIdPMetadata
  updateIdpWithZHost owner (Just ernieZHost) idpId2 idpmeta4 `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 200
    resp.jsonBody %. "extraInfo.domain" `shouldMatch` Null

  -- Without Z-Host header
  SampleIdP idpmeta5 _pCreds _ _ <- makeSampleIdPMetadata
  idpId5 <-
    createIdpWithZHost owner Nothing idpmeta5 `bindResponse` \resp -> do
      resp.status `shouldMatchInt` 201
      resp.jsonBody %. "extraInfo.domain" `shouldMatch` Null
      resp.jsonBody %. "id" >>= asString

  SampleIdP idpmeta6 _pCreds _ _ <- makeSampleIdPMetadata
  idpId6 <-
    createIdpWithZHost owner Nothing idpmeta6 `bindResponse` \resp -> do
      resp.status `shouldMatchInt` 201
      resp.jsonBody %. "extraInfo.domain" `shouldMatch` Null
      resp.jsonBody %. "id" >>= asString

  SampleIdP idpmeta7 _pCreds _ _ <- makeSampleIdPMetadata
  updateIdpWithZHost owner Nothing idpId5 idpmeta7 `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 200
    resp.jsonBody %. "extraInfo.domain" `shouldMatch` Null

  SampleIdP idpmeta8 _pCreds _ _ <- makeSampleIdPMetadata
  updateIdpWithZHost owner Nothing idpId6 idpmeta8 `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 200
    resp.jsonBody %. "extraInfo.domain" `shouldMatch` Null
