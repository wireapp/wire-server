{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

-- | This module is meant to show how Testlib can be used
module Test.Demo where

import qualified API.Brig as BrigP
import qualified API.BrigInternal as BrigI
import qualified API.GalleyInternal as GalleyI
import qualified API.Nginz as Nginz
import Control.Monad.Cont
import GHC.Stack
import SetupHelpers
import Testlib.Prelude

-- | Deleting unknown clients should fail with 404.
testDeleteUnknownClient :: HasCallStack => App ()
testDeleteUnknownClient = do
  user <- randomUser OwnDomain def
  let fakeClientId = "deadbeefdeadbeef"
  bindResponse (BrigP.deleteClient user fakeClientId) $ \resp -> do
    resp.status `shouldMatchInt` 404
    resp.json %. "label" `shouldMatch` "client-not-found"

testModifiedBrig :: HasCallStack => App ()
testModifiedBrig = do
  withModifiedBackend
    (def {brigCfg = setField "optSettings.setFederationDomain" "overridden.example.com"})
    $ \domain -> do
      bindResponse (BrigP.getAPIVersion domain)
      $ \resp -> do
        resp.status `shouldMatchInt` 200
        (resp.json %. "domain") `shouldMatch` "overridden.example.com"

testModifiedGalley :: HasCallStack => App ()
testModifiedGalley = do
  (_user, tid, _) <- createTeam OwnDomain 1

  let getFeatureStatus :: (MakesValue domain) => domain -> String -> App Value
      getFeatureStatus domain team = do
        bindResponse (GalleyI.getTeamFeature domain "searchVisibility" team) $ \res -> do
          res.status `shouldMatchInt` 200
          res.json %. "status"

  getFeatureStatus OwnDomain tid `shouldMatch` "disabled"

  withModifiedBackend
    def {galleyCfg = setField "settings.featureFlags.teamSearchVisibility" "enabled-by-default"}
    $ \domain -> do
      (_user, tid', _) <- createTeam domain 1
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
    (_user, tid, _) <- createTeam domain 1
    bindResponse (GalleyI.getTeamFeature domain "searchVisibility" tid) $ \res -> do
      res.status `shouldMatchInt` 200
      res.json %. "status" `shouldMatch` "enabled"

    bindResponse (BrigP.getAPIVersion domain) $
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
  bindResponse (BrigP.getSelf user) $ \resp -> do
    resp.status `shouldMatchInt` 200
    (resp.json %. "id") `shouldMatch` objId user

  startDynamicBackends [def] $ \dynDomains -> do
    [dynDomain] <- pure dynDomains
    bindResponse (Nginz.getSystemSettingsUnAuthorized dynDomain) $
      \resp -> do
        resp.status `shouldMatchInt` 200
        resp.json %. "setRestrictUserCreation" `shouldMatch` False

    -- user created in own domain should not be found in dynamic backend
    bindResponse (BrigP.getSelf' dynDomain uid) $ \resp -> do
      resp.status `shouldMatchInt` 404

    -- now create a user in the dynamic backend
    userD1 <- randomUser dynDomain def
    uidD1 <- objId userD1
    bindResponse (BrigP.getSelf userD1) $ \resp -> do
      resp.status `shouldMatchInt` 200
      (resp.json %. "id") `shouldMatch` objId userD1

    -- the d1 user should not be found in the own domain
    bindResponse (BrigP.getSelf' ownDomain uidD1) $ \resp -> do
      resp.status `shouldMatchInt` 404

testStartMultipleDynamicBackends :: HasCallStack => App ()
testStartMultipleDynamicBackends = do
  let assertCorrectDomain domain =
        bindResponse (BrigP.getAPIVersion domain) $
          \resp -> do
            resp.status `shouldMatchInt` 200
            (resp.json %. "domain") `shouldMatch` domain
  startDynamicBackends [def, def, def] $ mapM_ assertCorrectDomain

testIndependentESIndices :: HasCallStack => App ()
testIndependentESIndices = do
  u1 <- randomUser OwnDomain def
  u2 <- randomUser OwnDomain def
  uid2 <- objId u2
  connectTwoUsers u1 u2
  BrigI.refreshIndex OwnDomain
  bindResponse (BrigP.searchContacts u1 (u2 %. "name") OwnDomain) $ \resp -> do
    resp.status `shouldMatchInt` 200
    docs <- resp.json %. "documents" >>= asList
    case docs of
      [] -> assertFailure "Expected a non empty result, but got an empty one"
      doc : _ -> doc %. "id" `shouldMatch` uid2
  startDynamicBackends [def] $ \dynDomains -> do
    [dynDomain] <- pure dynDomains
    uD1 <- randomUser dynDomain def
    -- searching for u1 on the dyn backend should yield no result
    bindResponse (BrigP.searchContacts uD1 (u2 %. "name") dynDomain) $ \resp -> do
      resp.status `shouldMatchInt` 200
      docs <- resp.json %. "documents" >>= asList
      null docs `shouldMatch` True
    uD2 <- randomUser dynDomain def
    uidD2 <- objId uD2
    connectTwoUsers uD1 uD2
    BrigI.refreshIndex dynDomain
    -- searching for uD2 on the dyn backend should yield a result
    bindResponse (BrigP.searchContacts uD1 (uD2 %. "name") dynDomain) $ \resp -> do
      resp.status `shouldMatchInt` 200
      docs <- resp.json %. "documents" >>= asList
      case docs of
        [] -> assertFailure "Expected a non empty result, but got an empty one"
        doc : _ -> doc %. "id" `shouldMatch` uidD2

testDynamicBackendsFederation :: HasCallStack => App ()
testDynamicBackendsFederation = do
  startDynamicBackends [def, def] $ \[aDynDomain, anotherDynDomain] -> do
    [u1, u2] <- createAndConnectUsers [aDynDomain, anotherDynDomain]
    bindResponse (BrigP.getConnection u1 u2) assertSuccess
    bindResponse (BrigP.getConnection u2 u1) assertSuccess

testWebSockets :: HasCallStack => App ()
testWebSockets = do
  user <- randomUser OwnDomain def
  withWebSocket user $ \ws -> do
    client <- BrigP.addClient user def >>= getJSON 201
    n <- awaitMatch (\n -> nPayload n %. "type" `isEqual` "user.client-add") ws
    nPayload n %. "client.id" `shouldMatch` (client %. "id")

testMultipleBackends :: App ()
testMultipleBackends = do
  ownDomainRes <- (BrigP.getAPIVersion OwnDomain >>= getJSON 200) %. "domain"
  otherDomainRes <- (BrigP.getAPIVersion OtherDomain >>= getJSON 200) %. "domain"
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

testFedV0Instance :: HasCallStack => App ()
testFedV0Instance = do
  res <- BrigP.getAPIVersion FedV0Domain >>= getJSON 200
  res %. "domain" `shouldMatch` FedV0Domain

testFedV0Federation :: HasCallStack => App ()
testFedV0Federation = do
  alice <- randomUser OwnDomain def
  bob <- randomUser FedV0Domain def

  bob' <- BrigP.getUser alice bob >>= getJSON 200
  bob' %. "qualified_id" `shouldMatch` (bob %. "qualified_id")
