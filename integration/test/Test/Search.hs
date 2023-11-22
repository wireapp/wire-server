module Test.Search where

import API.Brig qualified as BrigP
import API.BrigInternal qualified as BrigI
import API.Common qualified as API
import API.GalleyInternal qualified as GalleyI
import GHC.Stack
import SetupHelpers
import Testlib.Assertions
import Testlib.Prelude

--------------------------------------------------------------------------------
-- LOCAL SEARCH

testSearchContactForExternalUsers :: HasCallStack => App ()
testSearchContactForExternalUsers = do
  owner <- randomUser OwnDomain def {BrigI.team = True}
  partner <- randomUser OwnDomain def {BrigI.team = True}

  bindResponse (GalleyI.putTeamMember partner (partner %. "team") (API.teamRole "partner")) $ \resp ->
    resp.status `shouldMatchInt` 200

  bindResponse (BrigP.searchContacts partner (owner %. "name") OwnDomain) $ \resp ->
    resp.status `shouldMatchInt` 403

--------------------------------------------------------------------------------
-- FEDERATION SEARCH

testRemoteUserSearch :: HasCallStack => App ()
testRemoteUserSearch = do
  startDynamicBackends [def, def] $ \[d1, d2] -> do
    void $ BrigI.createFedConn d2 (BrigI.FedConn d1 "full_search" "allow_all")
    void $ BrigI.createFedConn d1 (BrigI.FedConn d2 "full_search" "allow_all")

    u1 <- randomUser d1 def
    u2 <- randomUser d2 def
    BrigI.refreshIndex d2
    uidD2 <- objId u2

    bindResponse (BrigP.searchContacts u1 (u2 %. "name") d2) $ \resp -> do
      resp.status `shouldMatchInt` 200
      docs <- resp.json %. "documents" >>= asList
      case docs of
        [] -> assertFailure "Expected a non empty result, but got an empty one"
        doc : _ -> doc %. "id" `shouldMatch` uidD2

testRemoteUserSearchExactHandle :: HasCallStack => App ()
testRemoteUserSearchExactHandle = do
  startDynamicBackends [def, def] $ \[d1, d2] -> do
    void $ BrigI.createFedConn d2 (BrigI.FedConn d1 "exact_handle_search" "allow_all")
    void $ BrigI.createFedConn d1 (BrigI.FedConn d2 "exact_handle_search" "allow_all")

    u1 <- randomUser d1 def
    u2 <- randomUser d2 def
    u2Handle <- API.randomHandle
    bindResponse (BrigP.putHandle u2 u2Handle) $ assertSuccess
    BrigI.refreshIndex d2

    bindResponse (BrigP.searchContacts u1 u2Handle d2) $ \resp -> do
      resp.status `shouldMatchInt` 200
      docs <- resp.json %. "documents" >>= asList
      case docs of
        [] -> assertFailure "Expected a non empty result, but got an empty one"
        doc : _ -> objQid doc `shouldMatch` objQid u2

--------------------------------------------------------------------------------
-- FEDERATION SEARCH WITH TEAM RESTRICTIONS

data Restriction = AllowAll | TeamAllowed | TeamNotAllowed
  deriving (Eq, Ord, Show)

data FedUserSearchTestCase = FedUserSearchTestCase
  { searchPolicy :: String,
    restrictionD1D2 :: Restriction,
    restrictionD2D1 :: Restriction,
    exactHandleSearchExpectFound :: Bool,
    fullSearchExpectFound :: Bool
  }
  deriving (Eq, Ord, Show)

testFederatedUserSearchNoSearch :: HasCallStack => App ()
testFederatedUserSearchNoSearch = do
  let testCases =
        [ FedUserSearchTestCase "no_search" AllowAll AllowAll False False,
          FedUserSearchTestCase "no_search" TeamAllowed TeamAllowed False False
        ]
  forM_ testCases federatedUserSearch

testFederatedUserSearchExactHandleSearchAllowAll :: HasCallStack => App ()
testFederatedUserSearchExactHandleSearchAllowAll = do
  let testCases =
        [ FedUserSearchTestCase "exact_handle_search" AllowAll AllowAll True False,
          FedUserSearchTestCase "exact_handle_search" TeamAllowed TeamAllowed True False,
          FedUserSearchTestCase "exact_handle_search" AllowAll TeamAllowed True False,
          FedUserSearchTestCase "exact_handle_search" TeamAllowed AllowAll True False
        ]
  forM_ testCases federatedUserSearch

testFederatedUserSearchExactHandleSearchTeamNotAllowed :: HasCallStack => App ()
testFederatedUserSearchExactHandleSearchTeamNotAllowed = do
  let testCases =
        [ FedUserSearchTestCase "exact_handle_search" TeamNotAllowed AllowAll False False,
          FedUserSearchTestCase "exact_handle_search" AllowAll TeamNotAllowed False False
        ]
  forM_ testCases federatedUserSearch

testFederatedUserSearchFullSearchAllowAll :: HasCallStack => App ()
testFederatedUserSearchFullSearchAllowAll = do
  let testCases =
        [ FedUserSearchTestCase "full_search" AllowAll AllowAll True True,
          FedUserSearchTestCase "full_search" TeamAllowed TeamAllowed True True,
          FedUserSearchTestCase "full_search" TeamAllowed AllowAll True True,
          FedUserSearchTestCase "full_search" AllowAll TeamAllowed True True
        ]
  forM_ testCases federatedUserSearch

testFederatedUserSearchFullSearchTeamNotAllowed :: HasCallStack => App ()
testFederatedUserSearchFullSearchTeamNotAllowed = do
  let testCases =
        [ FedUserSearchTestCase "full_search" TeamNotAllowed AllowAll False False,
          FedUserSearchTestCase "full_search" AllowAll TeamNotAllowed False False
        ]
  forM_ testCases federatedUserSearch

federatedUserSearch :: HasCallStack => FedUserSearchTestCase -> App ()
federatedUserSearch test = do
  startDynamicBackends [def, def] $ \[d1, d2] -> do
    void $ BrigI.createFedConn d2 (BrigI.FedConn d1 test.searchPolicy (restriction test.restrictionD2D1))
    void $ BrigI.createFedConn d1 (BrigI.FedConn d2 test.searchPolicy (restriction test.restrictionD1D2))

    u1 <- randomUser d1 def {BrigI.team = True}
    teamU1 <- u1 %. "team"
    u2 <- randomUser d2 def {BrigI.team = True}
    uidD2 <- objId u2
    team2 <- u2 %. "team"
    GalleyI.setTeamFeatureStatus d2 team2 "searchVisibilityInbound" "enabled"

    addTeamRestriction d1 d2 team2 test.restrictionD1D2
    addTeamRestriction d2 d1 teamU1 test.restrictionD2D1

    u2Handle <- API.randomHandle
    bindResponse (BrigP.putHandle u2 u2Handle) $ assertSuccess
    BrigI.refreshIndex d2

    bindResponse (BrigP.searchContacts u1 u2Handle d2) $ \resp -> do
      resp.status `shouldMatchInt` 200
      docs <- resp.json %. "documents" >>= asList
      case docs of
        [] ->
          when (test.exactHandleSearchExpectFound) $ assertFailure $ "Expected a non empty result, but got an empty one, for test case " <> show test
        doc : _ ->
          if test.exactHandleSearchExpectFound
            then doc %. "id" `shouldMatch` uidD2
            else assertFailure $ "Expected an empty result, but got " <> show doc <> " for test case " <> show test

    bindResponse (BrigP.searchContacts u1 (u2 %. "name") d2) $ \resp -> do
      resp.status `shouldMatchInt` 200
      docs <- resp.json %. "documents" >>= asList
      case docs of
        [] -> when (test.fullSearchExpectFound) $ assertFailure $ "Expected a non empty result, but got an empty one, for test case " <> show test
        doc : _ ->
          if test.fullSearchExpectFound
            then doc %. "id" `shouldMatch` uidD2
            else assertFailure $ "Expected an empty result, but got " <> show doc <> " for test case " <> show test
  where
    restriction :: Restriction -> String
    restriction = \case
      AllowAll -> "allow_all"
      TeamAllowed -> "restrict_by_team"
      TeamNotAllowed -> "restrict_by_team"

    addTeamRestriction :: (MakesValue ownDomain, MakesValue remoteDomain, MakesValue remoteTeam) => ownDomain -> remoteDomain -> remoteTeam -> Restriction -> App ()
    addTeamRestriction ownDomain remoteDomain remoteTeam = \case
      AllowAll ->
        pure ()
      TeamNotAllowed ->
        pure ()
      TeamAllowed -> do
        BrigI.addFederationRemoteTeam ownDomain remoteDomain remoteTeam

testFederatedUserSearchNonTeamSearcher :: HasCallStack => App ()
testFederatedUserSearchNonTeamSearcher = do
  startDynamicBackends [def, def] $ \[d1, d2] -> do
    void $ BrigI.createFedConn d2 (BrigI.FedConn d1 "full_search" "restrict_by_team")
    void $ BrigI.createFedConn d1 (BrigI.FedConn d2 "full_search" "allow_all")

    u1 <- randomUser d1 def
    u2 <- randomUser d2 def {BrigI.team = True}
    team2 <- u2 %. "team"
    GalleyI.setTeamFeatureStatus d2 team2 "searchVisibilityInbound" "enabled"

    u2Handle <- API.randomHandle
    bindResponse (BrigP.putHandle u2 u2Handle) $ assertSuccess
    BrigI.refreshIndex d2

    bindResponse (BrigP.searchContacts u1 u2Handle d2) $ \resp -> do
      resp.status `shouldMatchInt` 200
      docs <- resp.json %. "documents" >>= asList
      case docs of
        [] -> pure ()
        doc : _ ->
          assertFailure $ "Expected an empty result, but got " <> show doc

    bindResponse (BrigP.searchContacts u1 (u2 %. "name") d2) $ \resp -> do
      resp.status `shouldMatchInt` 200
      docs <- resp.json %. "documents" >>= asList
      case docs of
        [] -> pure ()
        doc : _ ->
          assertFailure $ "Expected an empty result, but got " <> show doc <> " for test case "

testFederatedSearchForNonTeamUser :: HasCallStack => App ()
testFederatedSearchForNonTeamUser = do
  startDynamicBackends [def, def] $ \[d1, d2] -> do
    void $ BrigI.createFedConn d2 (BrigI.FedConn d1 "full_search" "allow_all")
    void $ BrigI.createFedConn d1 (BrigI.FedConn d2 "full_search" "restrict_by_team")

    u1 <- randomUser d1 def {BrigI.team = True}
    u2 <- randomUser d2 def

    u2Handle <- API.randomHandle
    bindResponse (BrigP.putHandle u2 u2Handle) $ assertSuccess
    BrigI.refreshIndex d2

    bindResponse (BrigP.searchContacts u1 u2Handle d2) $ \resp -> do
      resp.status `shouldMatchInt` 200
      docs <- resp.json %. "documents" >>= asList
      case docs of
        [] -> pure ()
        doc : _ ->
          assertFailure $ "Expected an empty result, but got " <> show doc

    bindResponse (BrigP.searchContacts u1 (u2 %. "name") d2) $ \resp -> do
      resp.status `shouldMatchInt` 200
      docs <- resp.json %. "documents" >>= asList
      case docs of
        [] -> pure ()
        doc : _ ->
          assertFailure $ "Expected an empty result, but got " <> show doc <> " for test case "
