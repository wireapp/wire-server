{-# LANGUAGE FlexibleContexts #-}

-- | This module is meant to show how TestLib can be used
module Test.Demo where

import qualified API
import Imports
import TestLib.Prelude

testCantDeleteLHClient :: HasCallStack => App ()
testCantDeleteLHClient = do
  user <- randomUser def
  lhClientId <- bindResponse (API.addClient user def {API.ctype = "legalhold", API.internal = True}) $ \resp -> do
    resp.status `shouldMatchInt` 201
    resp.json %. "id"

  bindResponse (API.deleteClient user Nothing lhClientId) $ \resp -> do
    resp.status `shouldMatchInt` 400

testDeleteUnknownClient :: HasCallStack => App ()
testDeleteUnknownClient = do
  user <- randomUser def
  let fakeClientId = "deadbeefdeadbeef"
  bindResponse (API.deleteClient user Nothing fakeClientId) $ \resp -> do
    resp.status `shouldMatchInt` 404
    resp.json %. "label" `shouldMatch` "client-not-found"

testModifiedBrig :: HasCallStack => App ()
testModifiedBrig = do
  withModifiedService
    Brig
    (setField "optSettings.setFederationDomain" "overridden.example.com")
    $ bindResponse getAPIVersion
    $ ( \resp ->
          (resp.json %. "domain") `shouldMatch` "overridden.example.com"
      )
  where
    getAPIVersion :: App Response
    getAPIVersion = do
      req <- baseRequest Brig Unversioned $ "/api-version"
      submit "GET" req

testModifiedGalley :: HasCallStack => App ()
testModifiedGalley = do
  (_user, tid) <- createTeam

  let getFeatureStatus = do
        bindResponse (API.getTeamFeatureInternal "searchVisibility" tid) $ \res -> do
          res.status `shouldMatchInt` 200
          res.json %. "status"

  do
    status <- getFeatureStatus
    status `shouldMatch` "disabled"

  withModifiedService
    Galley
    (setField "settings.featureFlags.teamSearchVisibility" "enabled-by-default")
    $ do
      status <- getFeatureStatus
      status `shouldMatch` "enabled"

testWebSockets :: HasCallStack => App ()
testWebSockets = do
  user <- randomUser def
  withWebSocket user $ \ws -> do
    client <- bindResponse (API.addClient user def) $ \resp -> do
      resp.status `shouldMatchInt` 201
      resp.json
    n <- awaitMatch 3 (\n -> nPayload n %. "type" `isEqual` "user.client-add") ws
    nPayload n %. "client.id" `shouldMatch` (client %. "id")
