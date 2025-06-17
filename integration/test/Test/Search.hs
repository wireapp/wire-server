{-# OPTIONS -Wno-ambiguous-fields #-}
module Test.Search where

import qualified API.Brig as BrigP
import qualified API.BrigInternal as BrigI
import qualified API.Common as API
import API.Galley
import qualified API.Galley as Galley
import qualified API.GalleyInternal as GalleyI
import GHC.Stack
import SetupHelpers
import Testlib.Assertions
import Testlib.Prelude

--------------------------------------------------------------------------------
-- LOCAL SEARCH

testSearchContactForExternalUsers :: (HasCallStack) => App ()
testSearchContactForExternalUsers = do
  owner <- randomUser OwnDomain def {BrigI.team = True}
  tid <- owner %. "team" & asString

  partner <- createTeamMember owner def {role = "partner"}
  tm1 <- createTeamMember owner def
  tm2 <- createTeamMember owner def

  -- a team member can search for contacts
  bindResponse (BrigP.searchContacts tm1 (owner %. "name") OwnDomain) $ \resp ->
    resp.status `shouldMatchInt` 200

  -- a partner is not allowed to search for contacts
  bindResponse (BrigP.searchContacts partner (owner %. "name") OwnDomain) $ \resp ->
    resp.status `shouldMatchInt` 403

  -- a team member can see all other team members
  bindResponse (Galley.getTeamMembers tm1 tid) $ \resp -> do
    resp.status `shouldMatchInt` 200
    assertContainsUserIds resp [owner, tm1, tm2, partner]

  -- an external partner should see the person who invited them
  bindResponse (Galley.getTeamMembers partner tid) $ \resp -> do
    resp.status `shouldMatchInt` 200
    assertContainsUserIds resp [owner, partner]

  -- the team owner creates a conversation with the partner and another team member
  void $ postConversation owner (defProteus {qualifiedUsers = [tm1, partner], team = Just tid}) >>= getJSON 201

  -- now the external partner should still only see the person who invited them
  bindResponse (Galley.getTeamMembers partner tid) $ \resp -> do
    resp.status `shouldMatchInt` 200
    assertContainsUserIds resp [owner, partner]
  where
    assertContainsUserIds :: Response -> [Value] -> App ()
    assertContainsUserIds resp users = do
      members <- resp.json %. "members" & asList
      userIds <- for members (\m -> m %. "user")
      expected <- for users objId
      userIds `shouldMatchSet` expected

testEphemeralUsersSearch :: (HasCallStack) => App ()
testEphemeralUsersSearch = do
  userEphemeral <- ephemeralUser OwnDomain
  [user1, user2] <- replicateM 2 $ randomUser OwnDomain def
  BrigI.refreshIndex OwnDomain

  -- user1 can find user2
  BrigP.searchContacts user1 (user2 %. "name") OwnDomain >>= \resp -> do
    resp.status `shouldMatchInt` 200
    docs <- resp.json %. "documents" >>= asList
    quids <- for docs objId
    expected <- objId user2
    quids `shouldMatchSet` [expected]

  -- ephemeral user is not allowed to search for contacts
  BrigP.searchContacts userEphemeral (user2 %. "name") OwnDomain >>= \resp -> do
    resp.status `shouldMatchInt` 403
    resp.json %. "label" `shouldMatch` "insufficient-permissions"

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

testFederatedUserSearch :: (HasCallStack) => App ()
testFederatedUserSearch = do
  let tcs =
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
    forM_ tcs (federatedUserSearch d1 d2)

federatedUserSearch :: (HasCallStack) => String -> String -> FedUserSearchTestCase -> App ()
federatedUserSearch d1 d2 test = do
  void $ BrigI.updateFedConn d2 d1 (BrigI.FedConn d1 test.searchPolicy (restriction test.restrictionD2D1))
  void $ BrigI.updateFedConn d1 d2 (BrigI.FedConn d2 test.searchPolicy (restriction test.restrictionD1D2))

  u1 <- randomUser d1 def {BrigI.team = True}
  teamU1 <- u1 %. "team"
  u2 <- randomUser d2 def {BrigI.team = True}
  uidD2 <- objId u2
  team2 <- u2 %. "team"
  assertSuccess =<< GalleyI.setTeamFeatureStatus d2 team2 "searchVisibilityInbound" "enabled"

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

testFederatedUserSearchNonTeamSearcher :: (HasCallStack) => App ()
testFederatedUserSearchNonTeamSearcher = do
  startDynamicBackends [def, def] $ \[d1, d2] -> do
    void $ BrigI.createFedConn d2 (BrigI.FedConn d1 "full_search" (Just []))
    void $ BrigI.createFedConn d1 (BrigI.FedConn d2 "full_search" Nothing)

    u1 <- randomUser d1 def
    u2 <- randomUser d2 def {BrigI.team = True}
    team2 <- u2 %. "team"
    assertSuccess =<< GalleyI.setTeamFeatureStatus d2 team2 "searchVisibilityInbound" "enabled"

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

testFederatedUserSearchForNonTeamUser :: (HasCallStack) => App ()
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
