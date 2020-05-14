-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

module API.Search
  ( tests,
  )
where

import API.Search.Util
import API.Team.Util
import API.User.Util
import Bilge
import qualified Brig.Options as Opt
import Brig.Types
import Control.Lens ((.~))
import Data.Handle (fromHandle)
import Data.Id
import Data.List (elemIndex)
import qualified Data.Text as Text
import qualified Galley.Types.Teams.SearchVisibility as Team
import Imports
import Network.HTTP.Client (Manager)
import Test.Tasty
import Test.Tasty.HUnit
import UnliftIO (Concurrently (..), runConcurrently)
import Util

tests :: Opt.Opts -> Manager -> Galley -> Brig -> IO TestTree
tests opts mgr galley brig = do
  testSetupOutboundOnly <- runHttpT mgr $ prepareUsersForSearchVisibilityNoNameOutsideTeamTests
  return $
    testGroup
      "search"
      [ test mgr "by-name" $ testSearchByName brig,
        test mgr "by-handle" $ testSearchByHandle brig,
        test mgr "reindex" $ testReindex brig,
        test mgr "no match" $ testSearchNoMatch brig,
        test mgr "no extra results" $ testSearchNoExtraResults brig,
        test mgr "order-name" $ testOrderName brig,
        test mgr "order-handle" $ testOrderHandle brig,
        test mgr "by-first/middle/last name" $ testSearchByLastOrMiddleName brig,
        test mgr "Non ascii names" $ testSearchNonAsciiNames brig,
        testGroup "team-search-visibility disabled OR SearchVisibilityStandard" $
          [ test mgr "team member cannot be found by non-team user" $ testSearchTeamMemberAsNonMember galley brig,
            test mgr "team A member cannot be found by team B member" $ testSearchTeamMemberAsOtherMember galley brig,
            test mgr "team A member *can* be found by other team A member" $ testSearchTeamMemberAsSameMember galley brig,
            test mgr "non team user can be found by a team member" $ testSeachNonMemberAsTeamMember galley brig,
            testGroup "order" $
              [ test mgr "team-mates are listed before team-outsiders (exact match)" $ testSearchOrderingAsTeamMemberExactMatch galley brig,
                test mgr "team-mates are listed before team-outsiders (prefix match)" $ testSearchOrderingAsTeamMemberPrefixMatch galley brig,
                test mgr "team-mates are listed before team-outsiders (worse match)" $ testSearchOrderingAsTeamMemberWorseMatch galley brig
              ]
          ],
        testGroup "searchSameTeamOnly" $
          [ test mgr "when searchSameTeamOnly flag is set, non team user cannot be found by a team member" $ testSearchSameTeamOnly opts galley brig
          ],
        testGroup
          "team-search-visibility SearchVisibilityNoNameOutsideTeam"
          [ test mgr "team member cannot be found by non-team user" $ testSearchTeamMemberAsNonMemberOutboundOnly brig testSetupOutboundOnly,
            test mgr "team A member cannot be found by team B member" $ testSearchTeamMemberAsOtherMemberOutboundOnly brig testSetupOutboundOnly,
            test mgr "team A member *can* be found by other team A member" $ testSearchTeamMemberAsSameMemberOutboundOnly brig testSetupOutboundOnly,
            test mgr "non team user cannot be found by a team member A" $ testSeachNonMemberAsTeamMemberOutboundOnly brig testSetupOutboundOnly
          ]
      ]
  where
    -- Since the tests are about querying only, we only need 1 creation
    -- FUTUREWORK: this should probably be used for all tests in this module, not just some.
    prepareUsersForSearchVisibilityNoNameOutsideTeamTests :: Http ((TeamId, User, User), (TeamId, User, User), User)
    prepareUsersForSearchVisibilityNoNameOutsideTeamTests = do
      (tidA, ownerA, (memberA : _)) <- createPopulatedBindingTeamWithNamesAndHandles brig galley 1
      setTeamTeamSearchVisibilityAvailable galley tidA Team.TeamSearchVisibilityEnabled
      setTeamSearchVisibility galley tidA Team.SearchVisibilityNoNameOutsideTeam
      (tidB, ownerB, (memberB : _)) <- createPopulatedBindingTeamWithNamesAndHandles brig galley 1
      regularUser <- randomUserWithHandle brig
      refreshIndex brig
      return ((tidA, ownerA, memberA), (tidB, ownerB, memberB), regularUser)

testSearchByName :: Brig -> Http ()
testSearchByName brig = do
  u1 <- randomUser brig
  u2 <- randomUser brig
  refreshIndex brig
  let uid1 = userId u1
      uid2 = userId u2
  assertCanFind brig uid1 uid2 (fromName (userDisplayName u2))
  assertCanFind brig uid2 uid1 (fromName (userDisplayName u1))
  -- Users cannot find themselves
  assertCan'tFind brig uid1 uid1 (fromName (userDisplayName u1))
  assertCan'tFind brig uid2 uid2 (fromName (userDisplayName u2))

testSearchByLastOrMiddleName :: Brig -> Http ()
testSearchByLastOrMiddleName brig = do
  searcher <- userId <$> randomUser brig
  firstName <- randomHandle
  middleName <- randomHandle
  lastName <- randomHandle
  searched <- userId <$> createUser' True (firstName <> " " <> middleName <> " " <> lastName) brig
  refreshIndex brig
  assertCanFind brig searcher searched firstName
  assertCanFind brig searcher searched middleName
  assertCanFind brig searcher searched lastName
  assertCanFind brig searcher searched (firstName <> " " <> lastName)

testSearchNonAsciiNames :: Brig -> Http ()
testSearchNonAsciiNames brig = do
  searcher <- userId <$> randomUser brig
  suffix <- randomHandle
  searched <- userId <$> createUser' True ("शक्तिमान" <> suffix) brig
  refreshIndex brig
  assertCanFind brig searcher searched ("शक्तिमान" <> suffix)
  -- This is pathetic transliteration, but it is what we have.
  assertCanFind brig searcher searched ("saktimana" <> suffix)

testSearchByHandle :: Brig -> Http ()
testSearchByHandle brig = do
  u1 <- randomUserWithHandle brig
  u2 <- randomUser brig
  refreshIndex brig
  let uid1 = userId u1
      uid2 = userId u2
      Just h = fromHandle <$> userHandle u1
  assertCanFind brig uid2 uid1 h

testSearchNoMatch :: Brig -> Http ()
testSearchNoMatch brig = do
  u1 <- randomUser brig
  _ <- randomUser brig
  let uid1 = userId u1
  -- _uid2 = userId u2
  refreshIndex brig
  (Just result) <- fmap searchResults <$> executeSearch brig uid1 "nomatch"
  liftIO $ assertEqual "Expected 0 results" 0 (length result)

testSearchNoExtraResults :: Brig -> Http ()
testSearchNoExtraResults brig = do
  u1 <- randomUser brig
  u2 <- randomUser brig
  let uid1 = userId u1
      uid2 = userId u2
  refreshIndex brig
  (Just resultUIds) <- fmap (map contactUserId . searchResults) <$> executeSearch brig uid1 (fromName $ userDisplayName u2)
  liftIO $ do
    assertEqual "Expected search returns only the searched" [uid2] resultUIds

testReindex :: Brig -> Http ()
testReindex brig = do
  u <- randomUser brig
  ((), regular) <-
    runConcurrently $
      (,)
        <$> Concurrently (reindex brig)
        <*> Concurrently (replicateM 5 $ delayed *> mkRegularUser)
  refreshIndex brig
  for_ regular $ \u' -> do
    let Just h = fromHandle <$> userHandle u'
    assertCanFind brig (userId u) (userId u') h
    Just (found : _) <- fmap searchResults <$> executeSearch brig (userId u) h
    liftIO $ do
      assertEqual "Unexpected UserId" (contactUserId found) (userId u')
      assertEqual "Unexpected Name" (contactName found) (fromName $ userDisplayName u')
      assertEqual "Unexpected Colour" (contactColorId found) (Just . fromIntegral . fromColourId $ userAccentId u')
      assertEqual "Unexpected Handle" (contactHandle found) (fromHandle <$> userHandle u')
  where
    -- note: delaying user creation a bit to increase the chance of actually
    -- happen concurrently to the reindex on a small test database
    delayed = liftIO $ threadDelay 10000
    mkRegularUser = randomUserWithHandle brig

testOrderName :: Brig -> Http ()
testOrderName brig = do
  searcher <- userId <$> randomUser brig
  Name searchedWord <- randomNameWithMaxLen 122
  nameMatch <- userId <$> createUser' True searchedWord brig
  namePrefixMatch <- userId <$> createUser' True (searchedWord <> "suffix") brig
  refreshIndex brig
  (Just resultUIds) <- fmap (map contactUserId . searchResults) <$> executeSearch brig searcher searchedWord
  let expectedOrder = [nameMatch, namePrefixMatch]
  liftIO $ do
    assertEqual
      "Expected order: name match, name prefix match"
      expectedOrder
      resultUIds

testOrderHandle :: Brig -> Http ()
testOrderHandle brig = do
  searcher <- userId <$> randomUser brig
  searchedWord <- randomHandle
  handleMatch <- userId <$> createUser' True "handle match" brig
  void $ putHandle brig handleMatch searchedWord
  handlePrefixMatch <- userId <$> createUser' True "handle prefix match" brig
  void $ putHandle brig handlePrefixMatch (searchedWord <> "suffix")
  refreshIndex brig
  (Just resultUIds) <- fmap (map contactUserId . searchResults) <$> executeSearch brig searcher searchedWord
  let expectedOrder = [handleMatch, handlePrefixMatch]
  liftIO $ do
    assertEqual
      "Expected order: handle match, handle prefix match"
      expectedOrder
      resultUIds

testSearchTeamMemberAsNonMember :: Galley -> Brig -> Http ()
testSearchTeamMemberAsNonMember galley brig = do
  nonTeamMember <- randomUser brig
  (_, _, [teamMember]) <- createPopulatedBindingTeamWithNamesAndHandles brig galley 1
  refreshIndex brig
  let teamMemberHandle = fromMaybe (error "teamBMember must have a handle") (userHandle teamMember)
  assertCan'tFind brig (userId nonTeamMember) (userId teamMember) (fromName (userDisplayName teamMember))
  assertCan'tFind brig (userId nonTeamMember) (userId teamMember) (fromHandle teamMemberHandle)

testSearchTeamMemberAsOtherMember :: Galley -> Brig -> Http ()
testSearchTeamMemberAsOtherMember galley brig = do
  (_, _, [teamAMember]) <- createPopulatedBindingTeamWithNamesAndHandles brig galley 1
  (_, _, [teamBMember]) <- createPopulatedBindingTeamWithNamesAndHandles brig galley 1
  refreshIndex brig
  assertCan'tFind brig (userId teamAMember) (userId teamBMember) (fromName (userDisplayName teamBMember))
  let teamBMemberHandle = fromMaybe (error "teamBMember must have a handle") (userHandle teamBMember)
  assertCan'tFind brig (userId teamAMember) (userId teamBMember) (fromHandle teamBMemberHandle)

testSearchTeamMemberAsSameMember :: Galley -> Brig -> Http ()
testSearchTeamMemberAsSameMember galley brig = do
  (_, _, [teamAMember, teamAMember']) <- createPopulatedBindingTeam brig galley 2
  refreshIndex brig
  assertCanFind brig (userId teamAMember) (userId teamAMember') (fromName (userDisplayName teamAMember'))

testSeachNonMemberAsTeamMember :: Galley -> Brig -> Http ()
testSeachNonMemberAsTeamMember galley brig = do
  nonTeamMember <- randomUser brig
  (_, _, [teamMember]) <- createPopulatedBindingTeam brig galley 1
  refreshIndex brig
  assertCanFind brig (userId teamMember) (userId nonTeamMember) (fromName (userDisplayName nonTeamMember))

testSearchOrderingAsTeamMemberExactMatch :: Galley -> Brig -> Http ()
testSearchOrderingAsTeamMemberExactMatch galley brig = do
  searchedName <- randomName
  mapM_ (\(_ :: Int) -> createUser' True (fromName searchedName) brig) [0 .. 99]
  (_, _, [searcher, teamSearchee]) <- createPopulatedBindingTeamWithNames brig galley [Name "Searcher", searchedName]
  refreshIndex brig
  (Just result) <- executeSearch brig (userId searcher) (fromName searchedName)
  let resultUserIds = contactUserId <$> searchResults result
  liftIO $ do
    case elemIndex (userId teamSearchee) resultUserIds of
      Nothing -> assertFailure "team mate not found in search"
      Just teamSearcheeIndex -> assertEqual "teammate is not the first result" 0 teamSearcheeIndex

testSearchOrderingAsTeamMemberPrefixMatch :: Galley -> Brig -> Http ()
testSearchOrderingAsTeamMemberPrefixMatch galley brig = do
  searchedName <- randomNameWithMaxLen 122 -- 6 characters for "suffix"
  mapM_ (\(i :: Int) -> createUser' True (fromName searchedName <> Text.pack (show i)) brig) [0 .. 99]
  (_, _, [searcher, teamSearchee]) <- createPopulatedBindingTeamWithNames brig galley [Name "Searcher", Name $ fromName searchedName <> "suffix"]
  refreshIndex brig
  (Just result) <- executeSearch brig (userId searcher) (fromName searchedName)
  let resultUserIds = contactUserId <$> searchResults result
  liftIO $ do
    case elemIndex (userId teamSearchee) resultUserIds of
      Nothing -> assertFailure "team mate not found in search"
      Just teamSearcheeIndex -> assertEqual "teammate is not the first result" 0 teamSearcheeIndex

testSearchOrderingAsTeamMemberWorseMatch :: Galley -> Brig -> Http ()
testSearchOrderingAsTeamMemberWorseMatch galley brig = do
  searchedName <- randomHandle
  user <- createUser' True searchedName brig
  void $ putHandle brig (userId user) searchedName
  (_, _, [searcher, teamSearchee]) <- createPopulatedBindingTeamWithNames brig galley [Name "Searcher", Name (searchedName <> "Suffix")]
  refreshIndex brig
  (Just result) <- executeSearch brig (userId searcher) searchedName
  let resultUserIds = contactUserId <$> searchResults result
  liftIO $ do
    case elemIndex (userId teamSearchee) resultUserIds of
      Nothing -> assertFailure "team mate not found in search"
      Just teamSearcheeIndex -> assertEqual "teammate is not the first result" 0 teamSearcheeIndex

testSearchSameTeamOnly :: Opt.Opts -> Galley -> Brig -> Http ()
testSearchSameTeamOnly opts galley brig = do
  nonTeamMember <- randomUser brig
  (_, _, [teamMember]) <- createPopulatedBindingTeam brig galley 1
  refreshIndex brig
  let newOpts = opts & Opt.optionSettings . Opt.searchSameTeamOnly .~ Just True
  withSettingsOverrides newOpts $ do
    assertCan'tFind brig (userId teamMember) (userId nonTeamMember) (fromName (userDisplayName nonTeamMember))

testSearchTeamMemberAsNonMemberOutboundOnly :: Brig -> ((TeamId, User, User), (TeamId, User, User), User) -> Http ()
testSearchTeamMemberAsNonMemberOutboundOnly brig ((_, _, teamAMember), (_, _, _), nonTeamMember) = do
  let teamAMemberHandle = fromMaybe (error "teamAMember must have a handle") (userHandle teamAMember)
  assertCan'tFind brig (userId nonTeamMember) (userId teamAMember) (fromName (userDisplayName teamAMember))
  assertCan'tFind brig (userId nonTeamMember) (userId teamAMember) (fromHandle teamAMemberHandle)

testSearchTeamMemberAsOtherMemberOutboundOnly :: Brig -> ((TeamId, User, User), (TeamId, User, User), User) -> Http ()
testSearchTeamMemberAsOtherMemberOutboundOnly brig ((_, _, teamAMember), (_, _, teamBMember), _) = do
  let teamBMemberHandle = fromMaybe (error "teamBMember must have a handle") (userHandle teamBMember)
  assertCan'tFind brig (userId teamAMember) (userId teamBMember) (fromName (userDisplayName teamBMember))
  assertCan'tFind brig (userId teamAMember) (userId teamBMember) (fromHandle teamBMemberHandle)

testSearchTeamMemberAsSameMemberOutboundOnly :: Brig -> ((TeamId, User, User), (TeamId, User, User), User) -> Http ()
testSearchTeamMemberAsSameMemberOutboundOnly brig ((_, teamAOwner, teamAMember), (_, _, _), _) = do
  let teamAMemberHandle = fromMaybe (error "teamAMember must have a handle") (userHandle teamAMember)
  assertCanFind brig (userId teamAOwner) (userId teamAMember) (fromName (userDisplayName teamAMember))
  assertCanFind brig (userId teamAOwner) (userId teamAMember) (fromHandle teamAMemberHandle)
  let teamAOwnerHandle = fromMaybe (error "teamAMember must have a handle") (userHandle teamAOwner)
  assertCanFind brig (userId teamAMember) (userId teamAOwner) (fromName (userDisplayName teamAOwner))
  assertCanFind brig (userId teamAMember) (userId teamAOwner) (fromHandle teamAOwnerHandle)

testSeachNonMemberAsTeamMemberOutboundOnly :: Brig -> ((TeamId, User, User), (TeamId, User, User), User) -> Http ()
testSeachNonMemberAsTeamMemberOutboundOnly brig ((_, _, teamAMember), (_, _, _), nonTeamMember) = do
  let teamMemberAHandle = fromMaybe (error "nonTeamMember must have a handle") (userHandle nonTeamMember)
  assertCan'tFind brig (userId teamAMember) (userId nonTeamMember) (fromName (userDisplayName nonTeamMember))
  assertCan'tFind brig (userId teamAMember) (userId nonTeamMember) (fromHandle teamMemberAHandle)
