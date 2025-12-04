module Test.Spar.MultiIngressIdp where

import API.GalleyInternal
import API.Spar
import Control.Lens ((.~), (^.))
import qualified SAML2.WebSSO.Test.Util as SAML
import qualified SAML2.WebSSO.Types as SAML
import SetupHelpers
import Testlib.Prelude

ernieZHost :: String
ernieZHost = "nginz-https.ernie.example.com"

bertZHost :: String
bertZHost = "nginz-https.bert.example.com"

kermitZHost :: String
kermitZHost = "nginz-https.kermit.example.com"

-- | Create a `MultiIngressDomainConfig` JSON object with the given @zhost@
makeSpDomainConfig :: String -> Value
makeSpDomainConfig zhost =
  object
    [ "spAppUri" .= ("https://webapp." ++ zhost),
      "spSsoUri" .= ("https://nginz-https." ++ zhost ++ "/sso"),
      "contacts" .= [object ["type" .= ("ContactTechnical" :: String)]]
    ]

testMultiIngressIdpSimpleCase :: (HasCallStack) => App ()
testMultiIngressIdpSimpleCase = do
  withModifiedBackend
    def
      { sparCfg =
          removeField "saml.spSsoUri"
            >=> removeField "saml.spAppUri"
            >=> removeField "saml.contacts"
            >=> setField
              "saml.spDomainConfigs"
              ( object
                  [ ernieZHost .= makeSpDomainConfig ernieZHost,
                    bertZHost .= makeSpDomainConfig bertZHost
                  ]
              )
      }
    $ \domain -> do
      (owner, tid, _) <- createTeam domain 1
      void $ setTeamFeatureStatus owner tid "sso" "enabled"

      -- Create IdP for one domain
      SAML.SampleIdP idpmeta _ _ _ <- SAML.makeSampleIdPMetadata
      idpId <-
        createIdpWithZHost owner (Just ernieZHost) idpmeta `bindResponse` \resp -> do
          resp.status `shouldMatchInt` 201
          resp.jsonBody %. "extraInfo.domain" `shouldMatch` ernieZHost
          resp.jsonBody %. "id" >>= asString

      getIdp owner idpId `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 200
        resp.jsonBody %. "extraInfo.domain" `shouldMatch` ernieZHost

      -- Update IdP for another domain
      updateIdpWithZHost owner (Just bertZHost) idpId idpmeta `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 200
        resp.jsonBody %. "extraInfo.domain" `shouldMatch` bertZHost

      getIdp owner idpId `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 200
        resp.jsonBody %. "extraInfo.domain" `shouldMatch` bertZHost

-- We must guard against domains being filled up with multiple IdPs and then
-- being configured as multi-ingress domains. Then, we'd have multiple IdPs for
-- a multi-ingress domain and cannot decide which one to choose. The solution
-- to this is that unconfigured domains' IdPs store no domain. I.e. the
-- assignment of domains to IdPs begins when the domain is configured as
-- multi-ingress domain.
testUnconfiguredDomain :: (HasCallStack) => App ()
testUnconfiguredDomain = forM_ [Nothing, Just kermitZHost] $ \unconfiguredZHost -> do
  withModifiedBackend
    def
      { sparCfg =
          removeField "saml.spSsoUri"
            >=> removeField "saml.spAppUri"
            >=> removeField "saml.contacts"
            >=> setField
              "saml.spDomainConfigs"
              (object [ernieZHost .= makeSpDomainConfig ernieZHost])
      }
    $ \domain -> do
      (owner, tid, _) <- createTeam domain 1
      void $ setTeamFeatureStatus owner tid "sso" "enabled"

      SAML.SampleIdP idpmeta1 _ _ _ <- SAML.makeSampleIdPMetadata
      idpId1 <-
        createIdpWithZHost owner (Just ernieZHost) idpmeta1 `bindResponse` \resp -> do
          resp.status `shouldMatchInt` 201
          resp.jsonBody %. "extraInfo.domain" `shouldMatch` ernieZHost
          resp.jsonBody %. "id" >>= asString

      -- From configured domain to unconfigured -> no multi-ingress domain
      updateIdpWithZHost owner (unconfiguredZHost) idpId1 idpmeta1 `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 200
        resp.jsonBody %. "extraInfo.domain" `shouldMatch` Null

      getIdp owner idpId1 `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 200
        resp.jsonBody %. "extraInfo.domain" `shouldMatch` Null

      -- From unconfigured back to configured -> add multi-ingress domain
      updateIdpWithZHost owner (Just ernieZHost) idpId1 idpmeta1 `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 200
        resp.jsonBody %. "extraInfo.domain" `shouldMatch` ernieZHost

      getIdp owner idpId1 `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 200
        resp.jsonBody %. "extraInfo.domain" `shouldMatch` ernieZHost

      -- Create unconfigured -> no multi-ingress domain
      SAML.SampleIdP idpmeta2 _ _ _ <- SAML.makeSampleIdPMetadata
      idpId2 <-
        createIdpWithZHost owner (unconfiguredZHost) idpmeta2 `bindResponse` \resp -> do
          resp.status `shouldMatchInt` 201
          resp.jsonBody %. "extraInfo.domain" `shouldMatch` Null
          resp.jsonBody %. "id" >>= asString

      getIdp owner idpId2 `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 200
        resp.jsonBody %. "extraInfo.domain" `shouldMatch` Null

      -- Create a second unconfigured -> no multi-ingress domain
      SAML.SampleIdP idpmeta3 _ _ _ <- SAML.makeSampleIdPMetadata
      idpId3 <-
        createIdpWithZHost owner (unconfiguredZHost) idpmeta3 `bindResponse` \resp -> do
          resp.status `shouldMatchInt` 201
          resp.jsonBody %. "extraInfo.domain" `shouldMatch` Null
          resp.jsonBody %. "id" >>= asString

      getIdp owner idpId3 `bindResponse` \resp -> do
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
                  [ ernieZHost .= makeSpDomainConfig ernieZHost,
                    bertZHost .= makeSpDomainConfig bertZHost
                  ]
              )
      }
    $ \domain -> do
      (owner, tid, _) <- createTeam domain 1
      void $ setTeamFeatureStatus owner tid "sso" "enabled"

      SAML.SampleIdP idpmeta1 _ _ _ <- SAML.makeSampleIdPMetadata
      idpId1 <-
        createIdpWithZHost owner (Just ernieZHost) idpmeta1 `bindResponse` \resp -> do
          resp.status `shouldMatchInt` 201
          resp.jsonBody %. "id" >>= asString

      -- Creating a second IdP for the same domain -> failure
      SAML.SampleIdP idpmeta2 _ _ _ <- SAML.makeSampleIdPMetadata
      _idpId2 <-
        createIdpWithZHost owner (Just ernieZHost) idpmeta2 `bindResponse` \resp -> do
          resp.status `shouldMatchInt` 409
          resp.jsonBody %. "label" `shouldMatch` "idp-duplicate-domain-for-team"

      -- Create an IdP for one domain and update it to another that already has one -> failure
      SAML.SampleIdP idpmeta3 _ _ _ <- SAML.makeSampleIdPMetadata
      idpId3 <-
        createIdpWithZHost owner (Just bertZHost) idpmeta2 `bindResponse` \resp -> do
          resp.status `shouldMatchInt` 201
          resp.jsonBody %. "id" >>= asString

      updateIdpWithZHost owner (Just ernieZHost) idpId3 idpmeta3
        `bindResponse` \resp -> do
          resp.status `shouldMatchInt` 409
          resp.jsonBody %. "label" `shouldMatch` "idp-duplicate-domain-for-team"

      -- Create an IdP with no domain and update it to a domain that already has one -> failure
      SAML.SampleIdP idpmeta4 _ _ _ <- SAML.makeSampleIdPMetadata
      idpId4 <-
        createIdpWithZHost owner Nothing idpmeta4 `bindResponse` \resp -> do
          resp.status `shouldMatchInt` 201
          resp.jsonBody %. "id" >>= asString

      updateIdpWithZHost owner (Just ernieZHost) idpId4 idpmeta4
        `bindResponse` \resp -> do
          resp.status `shouldMatchInt` 409
          resp.jsonBody %. "label" `shouldMatch` "idp-duplicate-domain-for-team"

      -- Updating an IdP itself should still work
      updateIdpWithZHost
        owner
        (Just ernieZHost)
        idpId1
        -- The edIssuer needs to stay unchanged. Otherwise, deletion will fail
        -- with a 404 (see bug https://wearezeta.atlassian.net/browse/WPB-20407)
        (idpmeta2 & SAML.edIssuer .~ (idpmeta1 ^. SAML.edIssuer))
        `bindResponse` \resp -> do
          resp.status `shouldMatchInt` 200
          resp.jsonBody %. "extraInfo.domain" `shouldMatch` ernieZHost

      -- After deletion of the IdP of a domain, a new one can be created
      deleteIdp owner idpId1 `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 204

      SAML.SampleIdP idpmeta5 _ _ _ <- SAML.makeSampleIdPMetadata
      idpId5 <-
        createIdpWithZHost owner (Just ernieZHost) idpmeta5 `bindResponse` \resp -> do
          resp.status `shouldMatchInt` 201
          resp.jsonBody %. "extraInfo.domain" `shouldMatch` ernieZHost
          resp.jsonBody %. "id" >>= asString

      -- After deletion of the IdP of a domain, one can be moved from another domain
      SAML.SampleIdP idpmeta6 _ _ _ <- SAML.makeSampleIdPMetadata
      createIdpWithZHost owner (Just bertZHost) idpmeta6 `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 409
        resp.jsonBody %. "label" `shouldMatch` "idp-duplicate-domain-for-team"

      deleteIdp owner idpId3 `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 204

      idpId6 <-
        createIdpWithZHost owner (Just bertZHost) idpmeta6 `bindResponse` \resp -> do
          resp.status `shouldMatchInt` 201
          resp.jsonBody %. "extraInfo.domain" `shouldMatch` bertZHost
          resp.jsonBody %. "id" >>= asString

      updateIdpWithZHost owner (Just ernieZHost) idpId6 idpmeta6 `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 409
        resp.jsonBody %. "label" `shouldMatch` "idp-duplicate-domain-for-team"

      deleteIdp owner idpId5 `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 204

      updateIdpWithZHost owner (Just ernieZHost) idpId6 idpmeta6
        `bindResponse` \resp -> do
          resp.status `shouldMatchInt` 200
          resp.jsonBody %. "extraInfo.domain" `shouldMatch` ernieZHost

-- We only record the domain for multi-ingress setups.
testNonMultiIngressSetupsCanHaveMoreIdPsPerDomain :: (HasCallStack) => App ()
testNonMultiIngressSetupsCanHaveMoreIdPsPerDomain = do
  (owner, tid, _) <- createTeam OwnDomain 1
  void $ setTeamFeatureStatus owner tid "sso" "enabled"

  -- With Z-Host header
  SAML.SampleIdP idpmeta1 _ _ _ <- SAML.makeSampleIdPMetadata
  idpId1 <-
    createIdpWithZHost owner (Just ernieZHost) idpmeta1 `bindResponse` \resp -> do
      resp.status `shouldMatchInt` 201
      resp.jsonBody %. "extraInfo.domain" `shouldMatch` Null
      resp.jsonBody %. "id" >>= asString

  SAML.SampleIdP idpmeta2 _ _ _ <- SAML.makeSampleIdPMetadata
  idpId2 <-
    createIdpWithZHost owner (Just ernieZHost) idpmeta2 `bindResponse` \resp -> do
      resp.status `shouldMatchInt` 201
      resp.jsonBody %. "extraInfo.domain" `shouldMatch` Null
      resp.jsonBody %. "id" >>= asString

  SAML.SampleIdP idpmeta3 _ _ _ <- SAML.makeSampleIdPMetadata
  updateIdpWithZHost owner (Just ernieZHost) idpId1 idpmeta3 `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 200
    resp.jsonBody %. "extraInfo.domain" `shouldMatch` Null

  SAML.SampleIdP idpmeta4 _ _ _ <- SAML.makeSampleIdPMetadata
  updateIdpWithZHost owner (Just ernieZHost) idpId2 idpmeta4 `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 200
    resp.jsonBody %. "extraInfo.domain" `shouldMatch` Null

  -- Without Z-Host header
  SAML.SampleIdP idpmeta5 _ _ _ <- SAML.makeSampleIdPMetadata
  idpId5 <-
    createIdpWithZHost owner Nothing idpmeta5 `bindResponse` \resp -> do
      resp.status `shouldMatchInt` 201
      resp.jsonBody %. "extraInfo.domain" `shouldMatch` Null
      resp.jsonBody %. "id" >>= asString

  SAML.SampleIdP idpmeta6 _ _ _ <- SAML.makeSampleIdPMetadata
  idpId6 <-
    createIdpWithZHost owner Nothing idpmeta6 `bindResponse` \resp -> do
      resp.status `shouldMatchInt` 201
      resp.jsonBody %. "extraInfo.domain" `shouldMatch` Null
      resp.jsonBody %. "id" >>= asString

  SAML.SampleIdP idpmeta7 _ _ _ <- SAML.makeSampleIdPMetadata
  updateIdpWithZHost owner Nothing idpId5 idpmeta7 `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 200
    resp.jsonBody %. "extraInfo.domain" `shouldMatch` Null

  SAML.SampleIdP idpmeta8 _ _ _ <- SAML.makeSampleIdPMetadata
  updateIdpWithZHost owner Nothing idpId6 idpmeta8 `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 200
    resp.jsonBody %. "extraInfo.domain" `shouldMatch` Null
