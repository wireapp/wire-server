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
                    bertZHost .= makeSpDomainConfig bertZHost,
                    kermitZHost .= makeSpDomainConfig kermitZHost
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
          resp.json %. "extraInfo.domain" `shouldMatch` ernieZHost
          resp.json %. "id" >>= asString

      getIdp owner idpId `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 200
        resp.json %. "extraInfo.domain" `shouldMatch` ernieZHost

      -- Update IdP for another domain
      updateIdpWithZHost owner (Just bertZHost) idpId idpmeta `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 200
        resp.json %. "extraInfo.domain" `shouldMatch` bertZHost

      getIdp owner idpId `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 200
        resp.json %. "extraInfo.domain" `shouldMatch` bertZHost

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
          resp.json %. "extraInfo.domain" `shouldMatch` ernieZHost
          resp.json %. "id" >>= asString

      -- From configured domain to unconfigured -> no multi-ingress domain
      updateIdpWithZHost owner (unconfiguredZHost) idpId1 idpmeta1 `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 200
        resp.json %. "extraInfo.domain" `shouldMatch` Null

      getIdp owner idpId1 `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 200
        resp.json %. "extraInfo.domain" `shouldMatch` Null

      -- From unconfigured back to configured -> add multi-ingress domain
      updateIdpWithZHost owner (Just ernieZHost) idpId1 idpmeta1 `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 200
        resp.json %. "extraInfo.domain" `shouldMatch` ernieZHost

      getIdp owner idpId1 `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 200
        resp.json %. "extraInfo.domain" `shouldMatch` ernieZHost

      -- Create unconfigured -> no multi-ingress domain
      SAML.SampleIdP idpmeta2 _ _ _ <- SAML.makeSampleIdPMetadata
      idpId2 <-
        createIdpWithZHost owner (unconfiguredZHost) idpmeta2 `bindResponse` \resp -> do
          resp.status `shouldMatchInt` 201
          resp.json %. "extraInfo.domain" `shouldMatch` Null
          resp.json %. "id" >>= asString

      getIdp owner idpId2 `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 200
        resp.json %. "extraInfo.domain" `shouldMatch` Null

      -- Create a second unconfigured -> no multi-ingress domain
      SAML.SampleIdP idpmeta3 _ _ _ <- SAML.makeSampleIdPMetadata
      idpId3 <-
        createIdpWithZHost owner (unconfiguredZHost) idpmeta3 `bindResponse` \resp -> do
          resp.status `shouldMatchInt` 201
          resp.json %. "extraInfo.domain" `shouldMatch` Null
          resp.json %. "id" >>= asString

      getIdp owner idpId3 `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 200
        resp.json %. "extraInfo.domain" `shouldMatch` Null

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
                    bertZHost .= makeSpDomainConfig bertZHost,
                    kermitZHost .= makeSpDomainConfig kermitZHost
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
          resp.json %. "id" >>= asString

      -- Creating a second IdP for the same domain -> failure
      SAML.SampleIdP idpmeta2 _ _ _ <- SAML.makeSampleIdPMetadata
      _idpId2 <-
        createIdpWithZHost owner (Just ernieZHost) idpmeta2 `bindResponse` \resp -> do
          resp.status `shouldMatchInt` 409
          resp.json %. "label" `shouldMatch` "idp-duplicate-domain-for-team"

      -- Create an IdP for one domain and update it to another that already has one -> failure
      SAML.SampleIdP idpmeta3 _ _ _ <- SAML.makeSampleIdPMetadata
      idpId3 <-
        createIdpWithZHost owner (Just bertZHost) idpmeta2 `bindResponse` \resp -> do
          resp.status `shouldMatchInt` 201
          resp.json %. "id" >>= asString

      updateIdpWithZHost owner (Just ernieZHost) idpId3 idpmeta3
        `bindResponse` \resp -> do
          resp.status `shouldMatchInt` 409
          resp.json %. "label" `shouldMatch` "idp-duplicate-domain-for-team"

      -- Create an IdP with no domain and update it to a domain that already has one -> failure
      SAML.SampleIdP idpmeta4 _ _ _ <- SAML.makeSampleIdPMetadata
      idpId4 <-
        createIdpWithZHost owner Nothing idpmeta4 `bindResponse` \resp -> do
          resp.status `shouldMatchInt` 201
          resp.json %. "id" >>= asString

      updateIdpWithZHost owner (Just ernieZHost) idpId4 idpmeta4
        `bindResponse` \resp -> do
          resp.status `shouldMatchInt` 409
          resp.json %. "label" `shouldMatch` "idp-duplicate-domain-for-team"

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
          resp.json %. "extraInfo.domain" `shouldMatch` ernieZHost

      -- After deletion of the IdP of a domain, a new one can be created
      deleteIdp owner idpId1 `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 204

      SAML.SampleIdP idpmeta5 _ _ _ <- SAML.makeSampleIdPMetadata
      idpId5 <-
        createIdpWithZHost owner (Just ernieZHost) idpmeta5 `bindResponse` \resp -> do
          resp.status `shouldMatchInt` 201
          resp.json %. "extraInfo.domain" `shouldMatch` ernieZHost
          resp.json %. "id" >>= asString

      -- After deletion of the IdP of a domain, one can be moved from another domain
      SAML.SampleIdP idpmeta6 _ _ _ <- SAML.makeSampleIdPMetadata
      createIdpWithZHost owner (Just bertZHost) idpmeta6 `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 409
        resp.json %. "label" `shouldMatch` "idp-duplicate-domain-for-team"

      deleteIdp owner idpId3 `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 204

      idpId6 <-
        createIdpWithZHost owner (Just bertZHost) idpmeta6 `bindResponse` \resp -> do
          resp.status `shouldMatchInt` 201
          resp.json %. "extraInfo.domain" `shouldMatch` bertZHost
          resp.json %. "id" >>= asString

      updateIdpWithZHost owner (Just ernieZHost) idpId6 idpmeta6 `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 409
        resp.json %. "label" `shouldMatch` "idp-duplicate-domain-for-team"

      deleteIdp owner idpId5 `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 204

      updateIdpWithZHost owner (Just ernieZHost) idpId6 idpmeta6
        `bindResponse` \resp -> do
          resp.status `shouldMatchInt` 200
          resp.json %. "extraInfo.domain" `shouldMatch` ernieZHost

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
      resp.json %. "extraInfo.domain" `shouldMatch` Null
      resp.json %. "id" >>= asString

  SAML.SampleIdP idpmeta2 _ _ _ <- SAML.makeSampleIdPMetadata
  idpId2 <-
    createIdpWithZHost owner (Just ernieZHost) idpmeta2 `bindResponse` \resp -> do
      resp.status `shouldMatchInt` 201
      resp.json %. "extraInfo.domain" `shouldMatch` Null
      resp.json %. "id" >>= asString

  SAML.SampleIdP idpmeta3 _ _ _ <- SAML.makeSampleIdPMetadata
  updateIdpWithZHost owner (Just ernieZHost) idpId1 idpmeta3 `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "extraInfo.domain" `shouldMatch` Null

  SAML.SampleIdP idpmeta4 _ _ _ <- SAML.makeSampleIdPMetadata
  updateIdpWithZHost owner (Just ernieZHost) idpId2 idpmeta4 `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "extraInfo.domain" `shouldMatch` Null

  -- Without Z-Host header
  SAML.SampleIdP idpmeta5 _ _ _ <- SAML.makeSampleIdPMetadata
  idpId5 <-
    createIdpWithZHost owner Nothing idpmeta5 `bindResponse` \resp -> do
      resp.status `shouldMatchInt` 201
      resp.json %. "extraInfo.domain" `shouldMatch` Null
      resp.json %. "id" >>= asString

  SAML.SampleIdP idpmeta6 _ _ _ <- SAML.makeSampleIdPMetadata
  idpId6 <-
    createIdpWithZHost owner Nothing idpmeta6 `bindResponse` \resp -> do
      resp.status `shouldMatchInt` 201
      resp.json %. "extraInfo.domain" `shouldMatch` Null
      resp.json %. "id" >>= asString

  SAML.SampleIdP idpmeta7 _ _ _ <- SAML.makeSampleIdPMetadata
  updateIdpWithZHost owner Nothing idpId5 idpmeta7 `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "extraInfo.domain" `shouldMatch` Null

  SAML.SampleIdP idpmeta8 _ _ _ <- SAML.makeSampleIdPMetadata
  updateIdpWithZHost owner Nothing idpId6 idpmeta8 `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "extraInfo.domain" `shouldMatch` Null

-- * IdP API V1/V2 constraints

-- | The `validateNewIdP` rules for IdP creation apply to multi-ingress setups as
-- well. Depending on the IdP API version, IdP issuers have to be either unique
-- per backend (V1) or per team (V2).
--
-- As issuers correspond to realms in Keycloak and realms are return-URL
-- specific, this is probably fine for now. Otherwise, we would have to
-- redesign spar's database schema. E.g there would be a race-condition on the
-- `spar.issuer_idp_v2` table.
testMultiIngressSameIdPDifferentDomains :: (HasCallStack) => App ()
testMultiIngressSameIdPDifferentDomains = do
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
      -- V1 API: Issuers must be unique per backend (across all teams)
      (owner1, tid1, _) <- createTeam domain 1
      void $ setTeamFeatureStatus owner1 tid1 "sso" "enabled"

      SAML.SampleIdP idpmetaV1 _ _ _ <- SAML.makeSampleIdPMetadata
      _idpId1 <-
        createIdpWithZHostV1 owner1 (Just ernieZHost) idpmetaV1 `bindResponse` \resp -> do
          resp.status `shouldMatchInt` 201
          resp.json %. "extraInfo.domain" `shouldMatch` ernieZHost
          resp.json %. "id" >>= asString

      -- Try to create the same V1 IdP on a different team -> failure
      -- Test with different domains to show constraint is domain-independent
      (owner2, tid2, _) <- createTeam domain 1
      void $ setTeamFeatureStatus owner2 tid2 "sso" "enabled"

      -- Try with same domain as original -> should fail (V1 global uniqueness)
      createIdpWithZHostV1 owner2 (Just ernieZHost) idpmetaV1 `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 400
        resp.json %. "label" `shouldMatch` "idp-already-in-use"

      -- Try with different domain -> should also fail (V1 global uniqueness)
      createIdpWithZHostV1 owner2 (Just bertZHost) idpmetaV1 `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 400
        resp.json %. "label" `shouldMatch` "idp-already-in-use"

      -- Try with no domain -> should also fail (V1 global uniqueness)
      createIdpWithZHostV1 owner2 Nothing idpmetaV1 `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 400
        resp.json %. "label" `shouldMatch` "idp-already-in-use"

      -- Counter-example: V1 IdP with different issuer -> success
      SAML.SampleIdP idpmetaV1_different _ _ _ <- SAML.makeSampleIdPMetadata
      void
        $ createIdpWithZHostV1 owner2 (Just ernieZHost) idpmetaV1_different
        `bindResponse` \resp -> do
          resp.status `shouldMatchInt` 201

      -- V2 API: Issuers must be unique per team (but can be reused across teams)
      -- Use a different issuer than V1 to avoid API version mixing errors
      (owner3, tid3, _) <- createTeam domain 1
      void $ setTeamFeatureStatus owner3 tid3 "sso" "enabled"

      SAML.SampleIdP idpmetaV2 _ _ _ <- SAML.makeSampleIdPMetadata
      _idpId3 <-
        createIdpWithZHost owner3 (Just ernieZHost) idpmetaV2 `bindResponse` \resp -> do
          resp.status `shouldMatchInt` 201
          resp.json %. "extraInfo.domain" `shouldMatch` ernieZHost
          resp.json %. "id" >>= asString

      -- Try to create the same IdP again on same team -> failure
      -- First, try with the same domain -> hits domain constraint (409)
      createIdpWithZHost owner3 (Just ernieZHost) idpmetaV2 `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 409
        resp.json %. "label" `shouldMatch` "idp-duplicate-domain-for-team"

      -- Now try with a different domain -> hits issuer constraint (400)
      createIdpWithZHost owner3 (Just bertZHost) idpmetaV2 `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 400
        resp.json %. "label" `shouldMatch` "idp-already-in-use"

      -- Now try with a different domain -> hits issuer constraint (400)
      createIdpWithZHost owner3 Nothing idpmetaV2 `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 400
        resp.json %. "label" `shouldMatch` "idp-already-in-use"

      -- Creating V2 IdP on team 4 with same issuer -> success (different team)
      (owner4, tid4, _) <- createTeam domain 1
      void $ setTeamFeatureStatus owner4 tid4 "sso" "enabled"

      void
        $ createIdpWithZHost owner4 (Just ernieZHost) idpmetaV2
        `bindResponse` \resp -> do
          resp.status `shouldMatchInt` 201
          resp.json %. "extraInfo.domain" `shouldMatch` ernieZHost

testMultiIngressSameIdPsSameIssuerDifferentDomains :: (HasCallStack) => App ()
testMultiIngressSameIdPsSameIssuerDifferentDomains = do
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
                    bertZHost .= makeSpDomainConfig bertZHost,
                    kermitZHost .= makeSpDomainConfig kermitZHost
                  ]
              )
      }
    $ \domain -> do
      -- V1 API: Issuers must be unique per backend (across all teams)
      (owner1, tid1, _) <- createTeam domain 1
      void $ setTeamFeatureStatus owner1 tid1 "sso" "enabled"

      -- Create first IdP metadata for V1
      SAML.SampleIdP idpmetaV1 _ _ _ <- SAML.makeSampleIdPMetadata
      _idpId1 <-
        createIdpWithZHostV1 owner1 (Just ernieZHost) idpmetaV1 `bindResponse` \resp -> do
          resp.status `shouldMatchInt` 201
          resp.json %. "extraInfo.domain" `shouldMatch` ernieZHost
          resp.json %. "id" >>= asString

      -- Try to create V1 IdP on a different team with different metadata but same issuer -> failure
      -- Test with different domains to show constraint is domain-independent
      (owner2, tid2, _) <- createTeam domain 1
      void $ setTeamFeatureStatus owner2 tid2 "sso" "enabled"

      -- Try with same domain as original -> should fail (V1 global uniqueness)
      SAML.SampleIdP idpmetaV1_alt _ _ _ <- SAML.makeSampleIdPMetadata
      let idpmetaV1_alt_sameIssuer = idpmetaV1_alt & SAML.edIssuer .~ (idpmetaV1 ^. SAML.edIssuer)

      createIdpWithZHostV1 owner2 (Just ernieZHost) idpmetaV1_alt_sameIssuer `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 400
        resp.json %. "label" `shouldMatch` "idp-already-in-use"

      -- Try with different domain -> should also fail (V1 global uniqueness)
      createIdpWithZHostV1 owner2 (Just bertZHost) idpmetaV1_alt_sameIssuer `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 400
        resp.json %. "label" `shouldMatch` "idp-already-in-use"

      -- Try with no domain -> should also fail (V1 global uniqueness)
      createIdpWithZHostV1 owner2 Nothing idpmetaV1_alt_sameIssuer `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 400
        resp.json %. "label" `shouldMatch` "idp-already-in-use"

      -- Counter-example: V1 IdP with different issuer -> success
      SAML.SampleIdP idpmetaV1_differentIssuer _ _ _ <- SAML.makeSampleIdPMetadata
      void
        $ createIdpWithZHostV1 owner2 (Just ernieZHost) idpmetaV1_differentIssuer
        `bindResponse` \resp -> do
          resp.status `shouldMatchInt` 201

      -- V2 API: Issuers must be unique per team (but can be reused across teams)
      -- Use a different issuer than V1 to avoid API version mixing errors
      (owner3, tid3, _) <- createTeam domain 1
      void $ setTeamFeatureStatus owner3 tid3 "sso" "enabled"

      -- Create V2 IdP on team 3 with new issuer
      SAML.SampleIdP idpmetaV2 _ _ _ <- SAML.makeSampleIdPMetadata

      _idpId3 <-
        createIdpWithZHost owner3 (Just ernieZHost) idpmetaV2 `bindResponse` \resp -> do
          resp.status `shouldMatchInt` 201
          resp.json %. "extraInfo.domain" `shouldMatch` ernieZHost
          resp.json %. "id" >>= asString

      -- Try to create another V2 IdP on same team with different metadata but same issuer -> failure
      -- First, try with the same domain -> hits domain constraint (409)
      SAML.SampleIdP idpmetaV2_alt _ _ _ <- SAML.makeSampleIdPMetadata
      let idpmetaV2_alt_sameIssuer = idpmetaV2_alt & SAML.edIssuer .~ (idpmetaV2 ^. SAML.edIssuer)

      createIdpWithZHost owner3 (Just ernieZHost) idpmetaV2_alt_sameIssuer `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 409
        resp.json %. "label" `shouldMatch` "idp-duplicate-domain-for-team"

      -- Try with a different domain -> hits issuer constraint (400)
      createIdpWithZHost owner3 (Just bertZHost) idpmetaV2_alt_sameIssuer `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 400
        resp.json %. "label" `shouldMatch` "idp-already-in-use"

      -- Try with no domain -> hits issuer constraint (400)
      createIdpWithZHost owner3 Nothing idpmetaV2_alt_sameIssuer `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 400
        resp.json %. "label" `shouldMatch` "idp-already-in-use"

      -- Counter-example: V2 IdP with same issuer on different team -> success (different team)
      (owner4, tid4, _) <- createTeam domain 1
      void $ setTeamFeatureStatus owner4 tid4 "sso" "enabled"

      void
        $ createIdpWithZHost owner4 (Just ernieZHost) idpmetaV2_alt_sameIssuer
        `bindResponse` \resp -> do
          resp.status `shouldMatchInt` 201
          resp.json %. "extraInfo.domain" `shouldMatch` ernieZHost
