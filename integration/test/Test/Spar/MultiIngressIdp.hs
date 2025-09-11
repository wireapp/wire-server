module Test.Spar.MultiIngressIdp where

import API.GalleyInternal
import API.Spar
import SAML2.WebSSO.Test.Util
import SetupHelpers
import Testlib.Prelude

ernieZHost :: String
ernieZHost = "nginz-https.ernie.example.com"

testMultiIngressIdp :: (HasCallStack) => App ()
testMultiIngressIdp = do
  (owner, tid, _) <- createTeam OwnDomain 1
  void $ setTeamFeatureStatus owner tid "sso" "enabled"

  SampleIdP idpmeta _pCreds _ _ <- makeSampleIdPMetadata
  createIdpWithZHost owner (Just ernieZHost) idpmeta `bindResponse` \resp -> do
    printJSON resp.jsonBody
    resp.status `shouldMatchInt` 201
    resp.jsonBody %. "extraInfo.domain" `shouldMatch` ernieZHost
