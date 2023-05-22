-- | This module is meant to show how Testlib can be used
module Test.Demo where

import qualified API.Brig as Public
import qualified API.GalleyInternal as Internal
import GHC.Stack
import SetupHelpers
import Testlib.Prelude

-- | Legalhold clients cannot be deleted.
testCantDeleteLHClient :: HasCallStack => App ()
testCantDeleteLHClient = do
  user <- randomUser OwnDomain def
  client <-
    Public.addClient user def {Public.ctype = "legalhold", Public.internal = True}
      >>= getJSON 201

  bindResponse (Public.deleteClient user client) $ \resp -> do
    resp.status `shouldMatchInt` 400

-- | Deleting unknown clients should fail with 404.
testDeleteUnknownClient :: HasCallStack => App ()
testDeleteUnknownClient = do
  user <- randomUser OwnDomain def
  let fakeClientId = "deadbeefdeadbeef"
  bindResponse (Public.deleteClient user fakeClientId) $ \resp -> do
    resp.status `shouldMatchInt` 404
    resp.json %. "label" `shouldMatch` "client-not-found"

testModifiedBrig :: HasCallStack => App ()
testModifiedBrig = do
  withModifiedService
    Brig
    (setField "optSettings.setFederationDomain" "overridden.example.com")
    $ bindResponse (Public.getAPIVersion OwnDomain)
    $ \resp -> do
      resp.status `shouldMatchInt` 200
      (resp.json %. "domain") `shouldMatch` "overridden.example.com"

testModifiedGalley :: HasCallStack => App ()
testModifiedGalley = do
  (_user, tid) <- createTeam OwnDomain

  let getFeatureStatus = do
        bindResponse (Internal.getTeamFeature "searchVisibility" tid) $ \res -> do
          res.status `shouldMatchInt` 200
          res.json %. "status"

  do
    getFeatureStatus `shouldMatch` "disabled"

  withModifiedService
    Galley
    (setField "settings.featureFlags.teamSearchVisibility" "enabled-by-default")
    $ do
      getFeatureStatus `shouldMatch` "enabled"

testWebSockets :: HasCallStack => App ()
testWebSockets = do
  user <- randomUser OwnDomain def
  withWebSocket user $ \ws -> do
    client <- Public.addClient user def >>= getJSON 201
    n <- awaitMatch 3 (\n -> nPayload n %. "type" `isEqual` "user.client-add") ws
    nPayload n %. "client.id" `shouldMatch` (client %. "id")

testMultipleBackends :: App ()
testMultipleBackends = do
  ownDomainRes <- (Public.getAPIVersion OwnDomain >>= getJSON 200) %. "domain"
  otherDomainRes <- (Public.getAPIVersion OtherDomain >>= getJSON 200) %. "domain"
  ownDomainRes `shouldMatch` OwnDomain
  otherDomainRes `shouldMatch` OtherDomain
  OwnDomain `shouldNotMatch` OtherDomain

testUnrace :: App ()
testUnrace = do
  {-
  -- the following would retry for ~30s and only then fail
  unrace $ do
    True `shouldMatch` True
    True `shouldMatch` False
  -}
  unrace $ True `shouldMatch` True
