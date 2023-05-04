-- | This module is meant to show how Testlib can be used
module Test.Demo where

import qualified API.Brig as Public
import qualified API.GalleyInternal as Internal
import GHC.Stack
import SetupHelpers
import Testlib.Prelude

testCantDeleteLHClient :: HasCallStack => App ()
testCantDeleteLHClient = do
  user <- randomUser ownDomain def
  client <- bindResponseR (Public.addClient user def {Public.ctype = "legalhold", Public.internal = True}) $ \resp -> do
    resp.status `shouldMatchInt` 201

  bindResponse (Public.deleteClient user Nothing client) $ \resp -> do
    resp.status `shouldMatchInt` 400

testDeleteUnknownClient :: HasCallStack => App ()
testDeleteUnknownClient = do
  user <- randomUser ownDomain def
  let fakeClientId = "deadbeefdeadbeef"
  bindResponse (Public.deleteClient user Nothing fakeClientId) $ \resp -> do
    resp.status `shouldMatchInt` 404
    resp %. "label" `shouldMatch` "client-not-found"

testModifiedBrig :: HasCallStack => App ()
testModifiedBrig = do
  withModifiedService
    Brig
    (setField "optSettings.setFederationDomain" "overridden.example.com")
    $ bindResponse (Public.getAPIVersion ownDomain)
    $ ( \resp ->
          (resp %. "domain") `shouldMatch` "overridden.example.com"
      )

testModifiedGalley :: HasCallStack => App ()
testModifiedGalley = do
  (_user, tid) <- createTeam ownDomain

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
  user <- randomUser ownDomain def
  withWebSocket user $ \ws -> do
    client <- bindResponseR (Public.addClient user def) $ \resp -> do
      resp.status `shouldMatchInt` 201
    n <- awaitMatch 3 (\n -> nPayload n %. "type" `isEqual` "user.client-add") ws
    nPayload n %. "client.id" `shouldMatch` (client %. "id")

testMultipleBackends :: App ()
testMultipleBackends = do
  ownDomainRes <- bindResponse (Public.getAPIVersion ownDomain) (%. "domain")
  otherDomainRes <- bindResponse (Public.getAPIVersion otherDomain) (%. "domain")
  ownDomainRes `shouldMatch` ownDomain
  otherDomainRes `shouldMatch` otherDomain
  ownDomain `shouldNotMatch` otherDomain
