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

-- | Enumeration of the possible restrictions that can be applied to a federated user search
data Restriction = AllowAll | TeamAllowed | TeamNotAllowed
  deriving (Eq, Ord, Show)

data FedUserSearchTestCase = FedUserSearchTestCase
  { searchPolicy :: String,
    -- restriction settings of the calling backend
    restrictionD1D2 :: Restriction,
    -- restriction settings of the remote backend
    restrictionD2D1 :: Restriction,
    exactHandleSearchExpectFound :: Bool,
    fullSearchExpectFound :: Bool
  }
  deriving (Eq, Ord, Show)

testFederatedUserSearch :: HasCallStack => App ()
testFederatedUserSearch = do
  let testCases =
        [ -- no search
          FedUserSearchTestCase "no_search" AllowAll AllowAll False False,
          FedUserSearchTestCase "no_search" TeamAllowed TeamAllowed False False,
          -- exact handle search allow all/team allowed
          FedUserSearchTestCase "exact_handle_search" AllowAll AllowAll True False,
          FedUserSearchTestCase "exact_handle_search" TeamAllowed TeamAllowed True False,
          FedUserSearchTestCase "exact_handle_search" AllowAll TeamAllowed True False,
          FedUserSearchTestCase "exact_handle_search" TeamAllowed AllowAll True False,
          -- exact handle search team not allowed
          FedUserSearchTestCase "exact_handle_search" TeamNotAllowed AllowAll False False,
          FedUserSearchTestCase "exact_handle_search" AllowAll TeamNotAllowed False False,
          -- full search allow all/team allowed
          FedUserSearchTestCase "full_search" AllowAll AllowAll True True,
          FedUserSearchTestCase "full_search" TeamAllowed TeamAllowed True True,
          FedUserSearchTestCase "full_search" TeamAllowed AllowAll True True,
          FedUserSearchTestCase "full_search" AllowAll TeamAllowed True True,
          -- full search team not allowed
          FedUserSearchTestCase "full_search" TeamNotAllowed AllowAll False False,
          FedUserSearchTestCase "full_search" AllowAll TeamNotAllowed False False
        ]
  startDynamicBackends [def, def] $ \[d1, d2] -> do
    void $ BrigI.createFedConn d2 (BrigI.FedConn d1 "full_search" Nothing)
    void $ BrigI.createFedConn d1 (BrigI.FedConn d2 "full_search" Nothing)
    forM_ testCases (federatedUserSearch d1 d2)

federatedUserSearch :: HasCallStack => String -> String -> FedUserSearchTestCase -> App ()
federatedUserSearch d1 d2 test = do
  void $ BrigI.updateFedConn d2 d1 (BrigI.FedConn d1 test.searchPolicy (restriction test.restrictionD2D1))
  void $ BrigI.updateFedConn d1 d2 (BrigI.FedConn d2 test.searchPolicy (restriction test.restrictionD1D2))

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
    restriction :: Restriction -> Maybe [String]
    restriction = \case
      AllowAll -> Nothing
      TeamAllowed -> Just []
      TeamNotAllowed -> Just []

    addTeamRestriction :: (MakesValue ownDomain, MakesValue remoteDomain, MakesValue remoteTeam) => ownDomain -> remoteDomain -> remoteTeam -> Restriction -> App ()
    addTeamRestriction ownDomain remoteDomain remoteTeam = \case
      AllowAll ->
        pure ()
      TeamNotAllowed ->
        -- if the team is not allowed, the restriction was set to by team earlier and we do not need to do anything
        pure ()
      TeamAllowed -> do
        BrigI.addFederationRemoteTeam ownDomain remoteDomain remoteTeam

testFederatedUserSearchNonTeamSearcher :: HasCallStack => App ()
testFederatedUserSearchNonTeamSearcher = do
  startDynamicBackends [def, def] $ \[d1, d2] -> do
    void $ BrigI.createFedConn d2 (BrigI.FedConn d1 "full_search" (Just []))
    void $ BrigI.createFedConn d1 (BrigI.FedConn d2 "full_search" Nothing)

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

testFederatedUserSearchForNonTeamUser :: HasCallStack => App ()
testFederatedUserSearchForNonTeamUser = do
  startDynamicBackends [def, def] $ \[d1, d2] -> do
    void $ BrigI.createFedConn d2 (BrigI.FedConn d1 "full_search" Nothing)
    void $ BrigI.createFedConn d1 (BrigI.FedConn d2 "full_search" (Just []))

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
