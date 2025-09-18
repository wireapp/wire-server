module Test.Spar.ScimIssuerUpdateDeletionFailureReproducer where

import API.GalleyInternal
import API.Spar
import Control.Lens
import SAML2.WebSSO
import SAML2.WebSSO.Test.Util
import SetupHelpers
import Testlib.Prelude

testRepoduceSCIMUpdateIssuerDeletionBug :: (HasCallStack) => App ()
testRepoduceSCIMUpdateIssuerDeletionBug = do
  (owner, tid, _) <- createTeam OwnDomain 1
  void $ setTeamFeatureStatus owner tid "sso" "enabled"

  -- This create->update->delete flow works, because the issuer doesn't change
  SampleIdP idpmeta1 _ _ _ <- makeSampleIdPMetadata
  idpId1 <-
    createIdp owner idpmeta1 `bindResponse` \resp -> do
      resp.status `shouldMatchInt` 201
      resp.jsonBody %. "id" >>= asString

  SampleIdP (edIssuer .~ (idpmeta1 ^. edIssuer) -> idpmeta2) _ _ _ <- makeSampleIdPMetadata
  idpId2 <-
    updateIdp owner idpId1 idpmeta2 `bindResponse` \resp -> do
      resp.status `shouldMatchInt` 200
      resp.jsonBody %. "id" >>= asString

  idpId1 `shouldMatch` idpId2
  (idpmeta1 ^. edIssuer) `shouldMatch` (idpmeta2 ^. edIssuer)

  deleteIdp owner idpId2 `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 204

  -- This create->update->delete flow fails, because the issuer is updated as well
  SampleIdP idpmeta3 _ _ _ <- makeSampleIdPMetadata
  idpId3 <-
    createIdp owner idpmeta3 `bindResponse` \resp -> do
      resp.status `shouldMatchInt` 201
      resp.jsonBody %. "id" >>= asString

  SampleIdP idpmeta4 _ _ _ <- makeSampleIdPMetadata
  idpId4 <-
    updateIdp owner idpId3 idpmeta4 `bindResponse` \resp -> do
      resp.status `shouldMatchInt` 200
      resp.jsonBody %. "id" >>= asString

  idpId3 `shouldMatch` idpId4
  (idpmeta3 ^. edIssuer) `shouldNotMatch` (idpmeta4 ^. edIssuer)

  deleteIdp owner idpId4 `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 404 -- XXX this SHOULD BE 204
