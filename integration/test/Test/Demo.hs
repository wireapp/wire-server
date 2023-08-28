{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

-- | This module is meant to show how Testlib can be used
module Test.Demo where

import API.Brig qualified as Public
import API.BrigInternal qualified as Internal
import API.GalleyInternal qualified as Internal
import API.Nginz qualified as Nginz
import Control.Monad.Cont
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
  withModifiedBackend
    (def {brigCfg = setField "optSettings.setFederationDomain" "overridden.example.com"})
    $ \domain -> do
      bindResponse (Public.getAPIVersion domain)
      $ \resp -> do
        resp.status `shouldMatchInt` 200
        (resp.json %. "domain") `shouldMatch` "overridden.example.com"

testModifiedGalley :: HasCallStack => App ()
testModifiedGalley = do
  (_user, tid) <- createTeam OwnDomain

  let getFeatureStatus :: (MakesValue domain) => domain -> String -> App Value
      getFeatureStatus domain team = do
        bindResponse (Internal.getTeamFeature domain "searchVisibility" team) $ \res -> do
          res.status `shouldMatchInt` 200
          res.json %. "status"

  getFeatureStatus OwnDomain tid `shouldMatch` "disabled"

  withModifiedBackend
    def {galleyCfg = setField "settings.featureFlags.teamSearchVisibility" "enabled-by-default"}
    $ \domain -> do
      (_user, tid') <- createTeam domain
      getFeatureStatus domain tid' `shouldMatch` "enabled"

testModifiedCannon :: HasCallStack => App ()
testModifiedCannon = do
  withModifiedBackend def $ \_ -> pure ()

testModifiedGundeck :: HasCallStack => App ()
testModifiedGundeck = do
  withModifiedBackend def $ \_ -> pure ()

testModifiedCargohold :: HasCallStack => App ()
testModifiedCargohold = do
  withModifiedBackend def $ \_ -> pure ()

testModifiedSpar :: HasCallStack => App ()
testModifiedSpar = do
  withModifiedBackend def $ \_ -> pure ()

testModifiedServices :: HasCallStack => App ()
testModifiedServices = do
  let serviceMap =
        def
          { brigCfg = setField "optSettings.setFederationDomain" "overridden.example.com",
            galleyCfg = setField "settings.featureFlags.teamSearchVisibility" "enabled-by-default"
          }

  withModifiedBackend serviceMap $ \domain -> do
    (_user, tid) <- createTeam domain
    bindResponse (Internal.getTeamFeature domain "searchVisibility" tid) $ \res -> do
      res.status `shouldMatchInt` 200
      res.json %. "status" `shouldMatch` "enabled"

    bindResponse (Public.getAPIVersion domain) $
      \resp -> do
        resp.status `shouldMatchInt` 200
        (resp.json %. "domain") `shouldMatch` "overridden.example.com"

    bindResponse (Nginz.getSystemSettingsUnAuthorized domain) $
      \resp -> do
        resp.status `shouldMatchInt` 200
        resp.json %. "setRestrictUserCreation" `shouldMatch` False

testDynamicBackend :: HasCallStack => App ()
testDynamicBackend = do
  ownDomain <- objDomain OwnDomain
  user <- randomUser OwnDomain def
  uid <- objId user
  bindResponse (Public.getSelf ownDomain uid) $ \resp -> do
    resp.status `shouldMatchInt` 200
    (resp.json %. "id") `shouldMatch` objId user

  startDynamicBackends [def] $ \dynDomains -> do
    [dynDomain] <- pure dynDomains
    bindResponse (Nginz.getSystemSettingsUnAuthorized dynDomain) $
      \resp -> do
        resp.status `shouldMatchInt` 200
        resp.json %. "setRestrictUserCreation" `shouldMatch` False

    -- user created in own domain should not be found in dynamic backend
    bindResponse (Public.getSelf dynDomain uid) $ \resp -> do
      resp.status `shouldMatchInt` 404

    -- now create a user in the dynamic backend
    userD1 <- randomUser dynDomain def
    uidD1 <- objId userD1
    bindResponse (Public.getSelf dynDomain uidD1) $ \resp -> do
      resp.status `shouldMatchInt` 200
      (resp.json %. "id") `shouldMatch` objId userD1

    -- the d1 user should not be found in the own domain
    bindResponse (Public.getSelf ownDomain uidD1) $ \resp -> do
      resp.status `shouldMatchInt` 404

testStartMultipleDynamicBackends :: HasCallStack => App ()
testStartMultipleDynamicBackends = do
  let assertCorrectDomain domain =
        bindResponse (Public.getAPIVersion domain) $
          \resp -> do
            resp.status `shouldMatchInt` 200
            (resp.json %. "domain") `shouldMatch` domain
  startDynamicBackends [def, def, def] $ mapM_ assertCorrectDomain

testIndependentESIndices :: HasCallStack => App ()
testIndependentESIndices = do
  u1 <- randomUser OwnDomain def
  u2 <- randomUser OwnDomain def
  uid2 <- objId u2
  connectUsers u1 u2
  Internal.refreshIndex OwnDomain
  bindResponse (Public.searchContacts u1 (u2 %. "name") OwnDomain) $ \resp -> do
    resp.status `shouldMatchInt` 200
    docs <- resp.json %. "documents" >>= asList
    case docs of
      [] -> assertFailure "Expected a non empty result, but got an empty one"
      doc : _ -> doc %. "id" `shouldMatch` uid2
  startDynamicBackends [def] $ \dynDomains -> do
    [dynDomain] <- pure dynDomains
    uD1 <- randomUser dynDomain def
    -- searching for u1 on the dyn backend should yield no result
    bindResponse (Public.searchContacts uD1 (u2 %. "name") dynDomain) $ \resp -> do
      resp.status `shouldMatchInt` 200
      docs <- resp.json %. "documents" >>= asList
      null docs `shouldMatch` True
    uD2 <- randomUser dynDomain def
    uidD2 <- objId uD2
    connectUsers uD1 uD2
    Internal.refreshIndex dynDomain
    -- searching for uD2 on the dyn backend should yield a result
    bindResponse (Public.searchContacts uD1 (uD2 %. "name") dynDomain) $ \resp -> do
      resp.status `shouldMatchInt` 200
      docs <- resp.json %. "documents" >>= asList
      case docs of
        [] -> assertFailure "Expected a non empty result, but got an empty one"
        doc : _ -> doc %. "id" `shouldMatch` uidD2

testDynamicBackendsFederation :: HasCallStack => App ()
testDynamicBackendsFederation = do
  startDynamicBackends [def, def] $ \[aDynDomain, anotherDynDomain] -> do
    u1 <- randomUser aDynDomain def
    u2 <- randomUser anotherDynDomain def
    uid2 <- objId u2
    Internal.refreshIndex anotherDynDomain
    bindResponse (Public.searchContacts u1 (u2 %. "name") anotherDynDomain) $ \resp -> do
      resp.status `shouldMatchInt` 200
      docs <- resp.json %. "documents" >>= asList
      case docs of
        [] -> assertFailure "Expected a non empty result, but got an empty one"
        doc : _ -> doc %. "id" `shouldMatch` uid2

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
  retryT $ do
    True `shouldMatch` True
    True `shouldMatch` False
  -}
  retryT $ True `shouldMatch` True
