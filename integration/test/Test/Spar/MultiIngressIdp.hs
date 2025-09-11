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

testMultiIngressIdp :: (HasCallStack) => App ()
testMultiIngressIdp = do
  (owner, tid, _) <- createTeam OwnDomain 1
  void $ setTeamFeatureStatus owner tid "sso" "enabled"

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

-- TODO: Test creation of two IDPs for the same domain -> should fail
-- TODO: Test updating existing IDP such that two with the same domain would exist -> should fail
