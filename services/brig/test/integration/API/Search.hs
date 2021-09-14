{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

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
import Control.Lens ((.~), (?~), (^.))
import Control.Monad.Catch (MonadCatch, MonadThrow)
import Control.Retry
import Data.Aeson (FromJSON, Value, decode)
import qualified Data.Aeson as Aeson
import Data.Domain (Domain (Domain))
import Data.Handle (fromHandle)
import Data.Id
import qualified Data.Map.Strict as Map
import Data.Qualified (Qualified (qDomain, qUnqualified))
import Data.String.Conversions (cs)
import qualified Data.Text as Text
import qualified Database.Bloodhound as ES
import Federation.Util
import qualified Galley.Types.Teams.SearchVisibility as Team
import Imports
import qualified Network.HTTP.Client as HTTP
import qualified Network.Wai.Test as WaiTest
import Test.QuickCheck (Arbitrary (arbitrary), generate)
import Test.Tasty
import Test.Tasty.HUnit
import Text.RawString.QQ (r)
import UnliftIO (Concurrently (..), runConcurrently)
import Util
import Wire.API.Federation.GRPC.Types
import Wire.API.Team.Feature (TeamFeatureStatusValue (..))

tests :: Opt.Opts -> Manager -> Galley -> Brig -> IO TestTree
tests opts mgr galley brig = do
  testSetupOutboundOnly <- runHttpT mgr prepareUsersForSearchVisibilityNoNameOutsideTeamTests
  return $
    testGroup "search" $
      [ testWithBothIndices opts mgr "by-name" $ testSearchByName brig,
        testWithBothIndices opts mgr "by-handle" $ testSearchByHandle brig,
        testWithBothIndices opts mgr "size - when exact handle matches a team user" $ testSearchSize brig True,
        testWithBothIndices opts mgr "size - when exact handle matches a non team user" $ testSearchSize brig False,
        test mgr "empty query" $ testSearchEmpty brig,
        test mgr "reindex" $ testReindex brig,
        testWithBothIndices opts mgr "no match" $ testSearchNoMatch brig,
        testWithBothIndices opts mgr "no extra results" $ testSearchNoExtraResults brig,
        testWithBothIndices opts mgr "order-name (prefix match)" $ testOrderName brig,
        testWithBothIndices opts mgr "order-handle (prefix match)" $ testOrderHandle brig,
        testWithBothIndices opts mgr "by-first/middle/last name" $ testSearchByLastOrMiddleName brig,
        testWithBothIndices opts mgr "Non ascii names" $ testSearchNonAsciiNames brig,
        test mgr "migration to new index" $ testMigrationToNewIndex opts brig,
        testGroup "team-search-visibility disabled OR SearchVisibilityStandard" $
          [ testWithBothIndices opts mgr "team member cannot be found by non-team user with display name" $ testSearchTeamMemberAsNonMemberDisplayName brig,
            testWithBothIndices opts mgr "team member can be found by non-team user with exact handle" $ testSearchTeamMemeberAsNonMemberExactHandle brig,
            testWithBothIndices opts mgr "team A member cannot be found by team B member with display name" $ testSearchTeamMemberAsOtherMemberDisplayName brig,
            testWithBothIndices opts mgr "team A member can be found by team B member with exact handle" $ testSearchTeamMemberAsOtherMemberExactHandle brig,
            testWithBothIndices opts mgr "team A member can be found by other team A member" $ testSearchTeamMemberAsSameMember brig,
            testWithBothIndices opts mgr "non team user can be found by a team member" $ testSeachNonMemberAsTeamMember brig,
            testGroup "order" $
              [ test mgr "team-mates are listed before team-outsiders (exact match)" $ testSearchOrderingAsTeamMemberExactMatch brig,
                test mgr "team-mates are listed before team-outsiders (prefix match)" $ testSearchOrderingAsTeamMemberPrefixMatch brig,
                test mgr "team-mates are listed before team-outsiders (worse name match)" $ testSearchOrderingAsTeamMemberWorseNameMatch brig,
                test mgr "team-mates are listed after team-outsiders (worse handle match)" $ testSearchOrderingAsTeamMemberWorseHandleMatch brig
              ]
          ],
        testGroup "searchSameTeamOnly" $
          [ testWithBothIndicesAndOpts opts mgr "when searchSameTeamOnly flag is set, non team user cannot be found by a team member" $ testSearchSameTeamOnly brig
          ],
        testGroup "team-search-visibility SearchVisibilityNoNameOutsideTeam" $
          [ test mgr "team member cannot be found by non-team user" $ testSearchTeamMemberAsNonMemberOutboundOnly brig testSetupOutboundOnly,
            test mgr "team A member cannot be found by team B member" $ testSearchTeamMemberAsOtherMemberOutboundOnly brig testSetupOutboundOnly,
            test mgr "team A member *can* be found by other team A member" $ testSearchTeamMemberAsSameMemberOutboundOnly brig testSetupOutboundOnly,
            test mgr "non team user cannot be found by a team member A" $ testSeachNonMemberAsTeamMemberOutboundOnly brig testSetupOutboundOnly
          ],
        testGroup "federated" $
          [ test mgr "search passing own domain" $ testSearchWithDomain brig,
            test mgr "remote lookup should call remote code path" $ testSearchOtherDomain opts brig
            -- FUTUREWORK(federation): we need tests for:
            -- failure/error cases on search (augment the federatorMock?)
            -- wire-api-federation Servant-Api vs protobuf-client interactions
          ]
      ]
  where
    -- Since the tests are about querying only, we only need 1 creation
    -- FUTUREWORK: this should probably be used for all tests in this module, not just some.
    prepareUsersForSearchVisibilityNoNameOutsideTeamTests :: Http ((TeamId, User, User), (TeamId, User, User), User)
    prepareUsersForSearchVisibilityNoNameOutsideTeamTests = do
      (tidA, ownerA, (memberA : _)) <- createPopulatedBindingTeamWithNamesAndHandles brig 1
      setTeamTeamSearchVisibilityAvailable galley tidA TeamFeatureEnabled
      setTeamSearchVisibility galley tidA Team.SearchVisibilityNoNameOutsideTeam
      (tidB, ownerB, (memberB : _)) <- createPopulatedBindingTeamWithNamesAndHandles brig 1
      regularUser <- randomUserWithHandle brig
      refreshIndex brig
      return ((tidA, ownerA, memberA), (tidB, ownerB, memberB), regularUser)

type TestConstraints m = (MonadFail m, MonadCatch m, MonadIO m, MonadHttp m)

testSearchByName :: TestConstraints m => Brig -> m ()
testSearchByName brig = do
  u1 <- randomUser brig
  u2 <- randomUser brig
  refreshIndex brig
  let uid1 = userId u1
      quid1 = userQualifiedId u1
      uid2 = userId u2
      quid2 = userQualifiedId u2
  assertCanFind brig uid1 quid2 (fromName (userDisplayName u2))
  assertCanFind brig uid2 quid1 (fromName (userDisplayName u1))
  -- Users cannot find themselves
  assertCan'tFind brig uid1 quid1 (fromName (userDisplayName u1))
  assertCan'tFind brig uid2 quid2 (fromName (userDisplayName u2))

testSearchByLastOrMiddleName :: TestConstraints m => Brig -> m ()
testSearchByLastOrMiddleName brig = do
  searcher <- userId <$> randomUser brig
  firstName <- randomHandle
  middleName <- randomHandle
  lastName <- randomHandle
  searchedUser <- createUser' True (firstName <> " " <> middleName <> " " <> lastName) brig
  let searched = userQualifiedId searchedUser
  refreshIndex brig
  assertCanFind brig searcher searched firstName
  assertCanFind brig searcher searched middleName
  assertCanFind brig searcher searched lastName
  assertCanFind brig searcher searched (firstName <> " " <> lastName)

testSearchNonAsciiNames :: TestConstraints m => Brig -> m ()
testSearchNonAsciiNames brig = do
  searcher <- userId <$> randomUser brig
  suffix <- randomHandle
  searchedUser <- createUser' True ("शक्तिमान" <> suffix) brig
  let searched = userQualifiedId searchedUser
  refreshIndex brig
  assertCanFind brig searcher searched ("शक्तिमान" <> suffix)
  -- This is pathetic transliteration, but it is what we have.
  assertCanFind brig searcher searched ("saktimana" <> suffix)

testSearchByHandle :: TestConstraints m => Brig -> m ()
testSearchByHandle brig = do
  u1 <- randomUserWithHandle brig
  u2 <- randomUser brig
  refreshIndex brig
  let quid1 = userQualifiedId u1
      uid2 = userId u2
      Just h = fromHandle <$> userHandle u1
  assertCanFind brig uid2 quid1 h

testSearchEmpty :: TestConstraints m => Brig -> m ()
testSearchEmpty brig = do
  -- This user exists just in case empty string starts matching everything
  _someUser <- randomUserWithHandle brig
  searcher <- randomUser brig
  refreshIndex brig
  res <- searchResults <$> executeSearch brig (userId searcher) ""
  liftIO $ assertEqual "nothing should be returned" [] res

testSearchSize :: TestConstraints m => Brig -> Bool -> m ()
testSearchSize brig exactHandleInTeam = do
  (handleMatch, searchTerm) <-
    if exactHandleInTeam
      then do
        (_, _, teamHandleMatch : _) <- createPopulatedBindingTeamWithNamesAndHandles brig 1
        let handle = fromHandle . fromMaybe (error "impossible") $ userHandle teamHandleMatch
        pure (teamHandleMatch, handle)
      else do
        nonTeamHandleMatch <- randomUserWithHandle brig
        let handle = fromHandle . fromMaybe (error "impossible") $ userHandle nonTeamHandleMatch
        pure (nonTeamHandleMatch, handle)
  replicateM_ 6 $ createUser' True searchTerm brig
  refreshIndex brig

  self <- userId <$> randomUser brig
  res <- searchResults <$> executeSearch' brig self searchTerm Nothing (Just 5)

  liftIO $ do
    assertEqual "expected exactly 10 results" 5 (length res)
    assertEqual
      ("first match should be exact handle: " <> show searchTerm <> ", but got: \n" <> show res)
      (userQualifiedId handleMatch)
      (contactQualifiedId $ Imports.head res)
    assertEqual
      ("exact handle: " <> show searchTerm <> " should only be present once, but got \n" <> show res)
      Nothing
      (find ((userQualifiedId handleMatch ==) . contactQualifiedId) (tail res))

testSearchNoMatch :: TestConstraints m => Brig -> m ()
testSearchNoMatch brig = do
  u1 <- randomUser brig
  _ <- randomUser brig
  let uid1 = userId u1
  -- _uid2 = userId u2
  refreshIndex brig
  result <- searchResults <$> executeSearch brig uid1 "nomatch"
  liftIO $ assertEqual "Expected 0 results" 0 (length result)

testSearchNoExtraResults :: TestConstraints m => Brig -> m ()
testSearchNoExtraResults brig = do
  u1 <- randomUser brig
  u2 <- randomUser brig
  let uid1 = userId u1
      quid2 = userQualifiedId u2
  refreshIndex brig
  resultUIds <- map contactQualifiedId . searchResults <$> executeSearch brig uid1 (fromName $ userDisplayName u2)
  liftIO $
    assertEqual "Expected search returns only the searched" [quid2] resultUIds

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
    assertCanFind brig (userId u) (userQualifiedId u') h
    (found : _) <- searchResults <$> executeSearch brig (userId u) h
    liftIO $ do
      assertEqual "Unexpected UserId" (contactQualifiedId found) (userQualifiedId u')
      assertEqual "Unexpected Name" (contactName found) (fromName $ userDisplayName u')
      assertEqual "Unexpected Colour" (contactColorId found) (Just . fromIntegral . fromColourId $ userAccentId u')
      assertEqual "Unexpected Handle" (contactHandle found) (fromHandle <$> userHandle u')
  where
    -- note: delaying user creation a bit to increase the chance of actually
    -- happen concurrently to the reindex on a small test database
    delayed = liftIO $ threadDelay 10000
    mkRegularUser = randomUserWithHandle brig

testOrderName :: TestConstraints m => Brig -> m ()
testOrderName brig = do
  searcher <- userId <$> randomUser brig
  Name searchedWord <- randomNameWithMaxLen 122
  nameMatch <- userQualifiedId <$> createUser' True searchedWord brig
  namePrefixMatch <- userQualifiedId <$> createUser' True (searchedWord <> "suffix") brig
  refreshIndex brig
  results <- searchResults <$> executeSearch brig searcher searchedWord
  let resultUIds = map contactQualifiedId results
  let expectedOrder = [nameMatch, namePrefixMatch]
  let dbg = "results: " <> show results <> "\nsearchedWord: " <> cs searchedWord
  liftIO $
    assertEqual
      ("Expected order: name match, name prefix match.\n\nSince this test fails sporadically for unknown reasons here is some debug info:\n" <> dbg)
      expectedOrder
      resultUIds

testOrderHandle :: TestConstraints m => Brig -> m ()
testOrderHandle brig = do
  searcher <- userId <$> randomUser brig
  searchedWord <- randomHandle
  handleMatch <- userQualifiedId <$> createUser' True "handle match" brig
  void $ putHandle brig (qUnqualified handleMatch) searchedWord
  handlePrefixMatch <- userQualifiedId <$> createUser' True "handle prefix match" brig
  void $ putHandle brig (qUnqualified handlePrefixMatch) (searchedWord <> "suffix")
  refreshIndex brig
  results <- searchResults <$> executeSearch brig searcher searchedWord
  let resultUIds = map contactQualifiedId results
  let expectedOrder = [handleMatch, handlePrefixMatch]
  let dbg = "results: " <> show results <> "\nsearchedWord: " <> cs searchedWord
  liftIO $
    assertEqual
      ("Expected order: handle match, handle prefix match.\n\nSince this test fails sporadically for unknown reasons here here is some debug info:\n" <> dbg)
      expectedOrder
      resultUIds

testSearchTeamMemberAsNonMemberDisplayName :: TestConstraints m => Brig -> m ()
testSearchTeamMemberAsNonMemberDisplayName brig = do
  nonTeamMember <- randomUser brig
  (_, _, [teamMember]) <- createPopulatedBindingTeamWithNamesAndHandles brig 1
  refreshIndex brig
  assertCan'tFind brig (userId nonTeamMember) (userQualifiedId teamMember) (fromName (userDisplayName teamMember))

testSearchTeamMemeberAsNonMemberExactHandle :: TestConstraints m => Brig -> m ()
testSearchTeamMemeberAsNonMemberExactHandle brig = do
  nonTeamMember <- randomUser brig
  (_, _, [teamMember]) <- createPopulatedBindingTeamWithNamesAndHandles brig 1
  refreshIndex brig
  let teamMemberHandle = fromMaybe (error "teamBMember must have a handle") (userHandle teamMember)
  assertCanFind brig (userId nonTeamMember) (userQualifiedId teamMember) (fromHandle teamMemberHandle)

testSearchTeamMemberAsOtherMemberDisplayName :: TestConstraints m => Brig -> m ()
testSearchTeamMemberAsOtherMemberDisplayName brig = do
  (_, _, [teamAMember]) <- createPopulatedBindingTeamWithNamesAndHandles brig 1
  (_, _, [teamBMember]) <- createPopulatedBindingTeamWithNamesAndHandles brig 1
  refreshIndex brig
  assertCan'tFind brig (userId teamAMember) (userQualifiedId teamBMember) (fromName (userDisplayName teamBMember))

testSearchTeamMemberAsOtherMemberExactHandle :: TestConstraints m => Brig -> m ()
testSearchTeamMemberAsOtherMemberExactHandle brig = do
  (_, _, [teamAMember]) <- createPopulatedBindingTeamWithNamesAndHandles brig 1
  (_, _, [teamBMember]) <- createPopulatedBindingTeamWithNamesAndHandles brig 1
  refreshIndex brig
  let teamBMemberHandle = fromMaybe (error "teamBMember must have a handle") (userHandle teamBMember)
  assertCanFind brig (userId teamAMember) (userQualifiedId teamBMember) (fromHandle teamBMemberHandle)

testSearchTeamMemberAsSameMember :: TestConstraints m => Brig -> m ()
testSearchTeamMemberAsSameMember brig = do
  (_, _, [teamAMember, teamAMember']) <- createPopulatedBindingTeam brig 2
  refreshIndex brig
  assertCanFind brig (userId teamAMember) (userQualifiedId teamAMember') (fromName (userDisplayName teamAMember'))

testSeachNonMemberAsTeamMember :: TestConstraints m => Brig -> m ()
testSeachNonMemberAsTeamMember brig = do
  nonTeamMember <- randomUser brig
  (_, _, [teamMember]) <- createPopulatedBindingTeam brig 1
  refreshIndex brig
  assertCanFind brig (userId teamMember) (userQualifiedId nonTeamMember) (fromName (userDisplayName nonTeamMember))

testSearchOrderingAsTeamMemberExactMatch :: TestConstraints m => Brig -> m ()
testSearchOrderingAsTeamMemberExactMatch brig = do
  searchedName <- randomName
  mapM_ (\(_ :: Int) -> createUser' True (fromName searchedName) brig) [0 .. 99]
  (_, _, [searcher, teamSearchee]) <- createPopulatedBindingTeamWithNames brig [Name "Searcher", searchedName]
  refreshIndex brig
  result <- executeSearch brig (userId searcher) (fromName searchedName)
  let resultUserIds = contactQualifiedId <$> searchResults result
  liftIO $
    case elemIndex (userQualifiedId teamSearchee) resultUserIds of
      Nothing -> assertFailure "team mate not found in search"
      Just teamSearcheeIndex -> assertEqual "teammate is not the first result" 0 teamSearcheeIndex

testSearchOrderingAsTeamMemberPrefixMatch :: TestConstraints m => Brig -> m ()
testSearchOrderingAsTeamMemberPrefixMatch brig = do
  searchedName <- randomNameWithMaxLen 122 -- 6 characters for "suffix"
  mapM_ (\(i :: Int) -> createUser' True (fromName searchedName <> Text.pack (show i)) brig) [0 .. 99]
  (_, _, [searcher, teamSearchee]) <- createPopulatedBindingTeamWithNames brig [Name "Searcher", Name $ fromName searchedName <> "suffix"]
  refreshIndex brig
  result <- executeSearch brig (userId searcher) (fromName searchedName)
  let resultUserIds = contactQualifiedId <$> searchResults result
  liftIO $
    case elemIndex (userQualifiedId teamSearchee) resultUserIds of
      Nothing -> assertFailure "team mate not found in search"
      Just teamSearcheeIndex -> assertEqual "teammate is not the first result" 0 teamSearcheeIndex

testSearchOrderingAsTeamMemberWorseNameMatch :: TestConstraints m => Brig -> m ()
testSearchOrderingAsTeamMemberWorseNameMatch brig = do
  searchedTerm <- randomHandle
  _ <- createUser' True searchedTerm brig
  (_, _, [searcher, teamSearchee]) <- createPopulatedBindingTeamWithNames brig [Name "Searcher", Name (searchedTerm <> "Suffix")]
  refreshIndex brig
  result <- executeSearch brig (userId searcher) searchedTerm
  let resultUserIds = contactQualifiedId <$> searchResults result
  liftIO $
    case elemIndex (userQualifiedId teamSearchee) resultUserIds of
      Nothing -> assertFailure "team mate not found in search"
      Just teamSearcheeIndex -> assertEqual "teammate is not the first result" 0 teamSearcheeIndex

testSearchOrderingAsTeamMemberWorseHandleMatch :: TestConstraints m => Brig -> m ()
testSearchOrderingAsTeamMemberWorseHandleMatch brig = do
  searchedTerm <- randomHandle
  nonTeamSearchee <- createUser' True searchedTerm brig
  void $ putHandle brig (userId nonTeamSearchee) searchedTerm
  (_, _, [searcher, teamSearchee]) <- createPopulatedBindingTeamWithNames brig [Name "Searcher", Name (searchedTerm <> "Suffix")]
  refreshIndex brig
  result <- executeSearch brig (userId searcher) searchedTerm
  let resultUserIds = contactQualifiedId <$> searchResults result
  liftIO $ do
    case elemIndex (userQualifiedId nonTeamSearchee) resultUserIds of
      Nothing -> assertFailure "non team mate user not found in search"
      Just teamSearcheeIndex -> assertEqual "non team mate is not the first result" 0 teamSearcheeIndex
    case elemIndex (userQualifiedId teamSearchee) resultUserIds of
      Nothing -> assertFailure "team mate not found in search"
      Just teamSearcheeIndex -> assertEqual "teammate is not the second result" 1 teamSearcheeIndex

testSearchSameTeamOnly :: TestConstraints m => Brig -> Opt.Opts -> m ()
testSearchSameTeamOnly brig opts = do
  nonTeamMember <- randomUser brig
  (_, _, [teamMember]) <- createPopulatedBindingTeam brig 1
  refreshIndex brig
  let newOpts = opts & Opt.optionSettings . Opt.searchSameTeamOnly .~ Just True
  withSettingsOverrides newOpts $
    assertCan'tFind brig (userId teamMember) (userQualifiedId nonTeamMember) (fromName (userDisplayName nonTeamMember))

testSearchTeamMemberAsNonMemberOutboundOnly :: Brig -> ((TeamId, User, User), (TeamId, User, User), User) -> Http ()
testSearchTeamMemberAsNonMemberOutboundOnly brig ((_, _, teamAMember), (_, _, _), nonTeamMember) = do
  assertCan'tFind brig (userId nonTeamMember) (userQualifiedId teamAMember) (fromName (userDisplayName teamAMember))

testSearchTeamMemberAsOtherMemberOutboundOnly :: Brig -> ((TeamId, User, User), (TeamId, User, User), User) -> Http ()
testSearchTeamMemberAsOtherMemberOutboundOnly brig ((_, _, teamAMember), (_, _, teamBMember), _) = do
  let teamBMemberHandle = fromMaybe (error "teamBMember must have a handle") (userHandle teamBMember)
  assertCan'tFind brig (userId teamAMember) (userQualifiedId teamBMember) (fromName (userDisplayName teamBMember))
  assertCan'tFind brig (userId teamAMember) (userQualifiedId teamBMember) (fromHandle teamBMemberHandle)

testSearchTeamMemberAsSameMemberOutboundOnly :: Brig -> ((TeamId, User, User), (TeamId, User, User), User) -> Http ()
testSearchTeamMemberAsSameMemberOutboundOnly brig ((_, teamAOwner, teamAMember), (_, _, _), _) = do
  let teamAMemberHandle = fromMaybe (error "teamAMember must have a handle") (userHandle teamAMember)
  assertCanFind brig (userId teamAOwner) (userQualifiedId teamAMember) (fromName (userDisplayName teamAMember))
  assertCanFind brig (userId teamAOwner) (userQualifiedId teamAMember) (fromHandle teamAMemberHandle)
  let teamAOwnerHandle = fromMaybe (error "teamAMember must have a handle") (userHandle teamAOwner)
  assertCanFind brig (userId teamAMember) (userQualifiedId teamAOwner) (fromName (userDisplayName teamAOwner))
  assertCanFind brig (userId teamAMember) (userQualifiedId teamAOwner) (fromHandle teamAOwnerHandle)

testSeachNonMemberAsTeamMemberOutboundOnly :: Brig -> ((TeamId, User, User), (TeamId, User, User), User) -> Http ()
testSeachNonMemberAsTeamMemberOutboundOnly brig ((_, _, teamAMember), (_, _, _), nonTeamMember) = do
  let teamMemberAHandle = fromMaybe (error "nonTeamMember must have a handle") (userHandle nonTeamMember)
  assertCan'tFind brig (userId teamAMember) (userQualifiedId nonTeamMember) (fromName (userDisplayName nonTeamMember))
  assertCan'tFind brig (userId teamAMember) (userQualifiedId nonTeamMember) (fromHandle teamMemberAHandle)

testSearchWithDomain :: TestConstraints m => Brig -> m ()
testSearchWithDomain brig = do
  searcher <- randomUser brig
  searchee <- randomUser brig
  refreshIndex brig
  let searcherId = userId searcher
      searcheeQid = userQualifiedId searchee
      searcheeName = fromName (userDisplayName searchee)
      searcheeDomain = qDomain searcheeQid
  assertCanFindWithDomain brig searcherId searcheeQid searcheeName searcheeDomain

-- | WARNING: this test only tests that brig will indeed make a call to federator
-- (i.e. does the correct if/else branching based on the domain),
-- it does not test any of the federation API details. This needs to be tested separately.
testSearchOtherDomain :: TestConstraints m => Opt.Opts -> Brig -> m ()
testSearchOtherDomain opts brig = do
  user <- randomUser brig
  -- We cannot assert on a real federated request here, so we make a request to
  -- a mocked federator started and stopped during this test
  otherSearchResult :: [Contact] <- liftIO $ generate arbitrary
  let mockResponse = OutwardResponseBody (cs $ Aeson.encode otherSearchResult)
  (results, _) <- liftIO . withTempMockFederator opts (Domain "non-existent.example.com") mockResponse $ do
    executeSearchWithDomain brig (userId user) "someSearchText" (Domain "non-existent.example.com")
  let expectedResult =
        SearchResult
          { searchResults = otherSearchResult,
            searchFound = length otherSearchResult,
            searchReturned = length otherSearchResult,
            searchTook = 0
          }
  liftIO $ do
    assertEqual "The search request should get its result from federator" expectedResult results

-- | Migration sequence:
-- 1. A migration is planned, in this time brig writes to two indices
-- 2. A migration is triggered, users are copied from old index to new index
-- 3. Brig is configured to write to only the new index
--
-- So, we have four time frames ("phases") in which a user could be created/updated:
-- 1. Before migration is even planned
-- 2. When brig is writing to both indices
-- 3. While/After reindexing is done form old index to new index
-- 4. After brig is writing to only the new index
testMigrationToNewIndex :: TestConstraints m => Opt.Opts -> Brig -> m ()
testMigrationToNewIndex opts brig = do
  (optsOldIndex, ES.IndexName -> oldIndexName) <- optsForOldIndex opts
  -- Phase 1: Using old index only
  (phase1NonTeamUser, teamOwner, phase1TeamUser1, phase1TeamUser2, tid) <- withSettingsOverrides optsOldIndex $ do
    nonTeamUser <- randomUser brig
    (tid, teamOwner, [teamUser1, teamUser2]) <- createPopulatedBindingTeam brig 2
    pure (nonTeamUser, teamOwner, teamUser1, teamUser2, tid)

  -- Phase 2: Using old index for search, writing to both indices, migrations have not run
  let phase2OptsWhile = optsOldIndex & Opt.elasticsearchL . Opt.additionalWriteIndexL ?~ (opts ^. Opt.elasticsearchL . Opt.indexL)
  (phase2NonTeamUser, phase2TeamUser) <- withSettingsOverrides phase2OptsWhile $ do
    phase2NonTeamUser <- randomUser brig
    phase2TeamUser <- inviteAndRegisterUser teamOwner tid brig
    refreshIndex brig

    -- searching phase1 users should work
    assertCanFindByName brig phase1TeamUser1 phase1TeamUser2
    assertCanFindByName brig phase1TeamUser1 phase1NonTeamUser

    -- searching phase2 users should work
    assertCanFindByName brig phase1TeamUser1 phase2NonTeamUser
    assertCanFindByName brig phase1TeamUser1 phase2TeamUser
    pure (phase2NonTeamUser, phase2TeamUser)

  refreshIndex brig
  -- Before migration the phase1 users shouldn't be found in the new index
  assertCan'tFindByName brig phase1TeamUser1 phase1TeamUser2
  assertCan'tFindByName brig phase1TeamUser1 phase1NonTeamUser

  -- Before migration the phase2 users should be found in the new index
  assertCanFindByName brig phase1TeamUser1 phase2NonTeamUser
  assertCanFindByName brig phase1TeamUser1 phase2TeamUser

  -- Run Migrations
  let newIndexName = ES.IndexName $ opts ^. Opt.elasticsearchL . Opt.indexL
  taskNodeId <- assertRight =<< (runBH opts $ ES.reindexAsync $ ES.mkReindexRequest oldIndexName newIndexName)
  runBH opts $ waitForTaskToComplete @ES.ReindexResponse taskNodeId

  -- Phase 3: Using old index for search, writing to both indices, migrations have run
  refreshIndex brig
  (phase3NonTeamUser, phase3TeamUser) <- withSettingsOverrides phase2OptsWhile $ do
    phase3NonTeamUser <- randomUser brig
    phase3TeamUser <- inviteAndRegisterUser teamOwner tid brig
    refreshIndex brig

    -- searching phase1/2 users should work
    assertCanFindByName brig phase1TeamUser1 phase1TeamUser2
    assertCanFindByName brig phase1TeamUser1 phase1NonTeamUser
    assertCanFindByName brig phase1TeamUser1 phase2TeamUser
    assertCanFindByName brig phase1TeamUser1 phase2NonTeamUser

    -- searching new phase3 should also work
    assertCanFindByName brig phase1TeamUser1 phase3NonTeamUser
    assertCanFindByName brig phase1TeamUser1 phase3TeamUser
    pure (phase3NonTeamUser, phase3TeamUser)

  -- Phase 4: Using only new index
  refreshIndex brig
  -- Searching should work for phase1 users
  assertCanFindByName brig phase1TeamUser1 phase1TeamUser2
  assertCanFindByName brig phase1TeamUser1 phase1NonTeamUser

  -- Searching should work for phase2 users
  assertCanFindByName brig phase1TeamUser1 phase2TeamUser
  assertCanFindByName brig phase1TeamUser1 phase2NonTeamUser

  -- Searching should work for phase3 users
  assertCanFindByName brig phase1TeamUser1 phase3NonTeamUser
  assertCanFindByName brig phase1TeamUser1 phase3TeamUser

waitForTaskToComplete :: forall a m. (ES.MonadBH m, MonadIO m, MonadThrow m, FromJSON a) => ES.TaskNodeId -> m ()
waitForTaskToComplete taskNodeId = do
  let policy = constantDelay 100000 <> limitRetries 30
  let retryCondition _ = fmap not . isTaskComplete
  task <- retrying policy retryCondition (const $ ES.getTask @m @a taskNodeId)
  taskCompleted <- isTaskComplete task
  liftIO $ assertBool "Timed out waiting for task" taskCompleted
  where
    isTaskComplete :: Either ES.EsError (ES.TaskResponse a) -> m Bool
    isTaskComplete (Left e) = liftIO $ assertFailure $ "Expected Right, got Left: " <> show e
    isTaskComplete (Right taskRes) = pure $ ES.taskResponseCompleted taskRes

testWithBothIndices :: Opt.Opts -> Manager -> TestName -> WaiTest.Session a -> TestTree
testWithBothIndices opts mgr name f = do
  testGroup
    name
    [ test mgr "new-index" $ withSettingsOverrides opts f,
      test mgr "old-index" $ withOldIndex opts f
    ]

testWithBothIndicesAndOpts :: Opt.Opts -> Manager -> TestName -> (Opt.Opts -> Http ()) -> TestTree
testWithBothIndicesAndOpts opts mgr name f =
  testGroup
    name
    [ test mgr "new-index" (f opts),
      test mgr "old-index" $ do
        (newOpts, indexName) <- optsForOldIndex opts
        f newOpts <* deleteIndex opts indexName
    ]

withOldIndex :: MonadIO m => Opt.Opts -> WaiTest.Session a -> m a
withOldIndex opts f = do
  indexName <- randomHandle
  createIndexWithMapping opts indexName oldMapping
  let newOpts = opts & Opt.elasticsearchL . Opt.indexL .~ indexName
  withSettingsOverrides newOpts f <* deleteIndex opts indexName

optsForOldIndex :: MonadIO m => Opt.Opts -> m (Opt.Opts, Text)
optsForOldIndex opts = do
  indexName <- randomHandle
  createIndexWithMapping opts indexName oldMapping
  pure (opts & Opt.elasticsearchL . Opt.indexL .~ indexName, indexName)

createIndexWithMapping :: MonadIO m => Opt.Opts -> Text -> Value -> m ()
createIndexWithMapping opts name val = do
  let indexName = (ES.IndexName name)
  createReply <- runBH opts $ ES.createIndexWith [ES.AnalysisSetting analysisSettings] 1 indexName
  unless (ES.isCreated createReply || ES.isSuccess createReply) $ do
    liftIO $ assertFailure $ "failed to create index: " <> show name <> " with error: " <> show createReply
  mappingReply <- runBH opts $ ES.putMapping indexName (ES.MappingName "user") val
  unless (ES.isCreated mappingReply || ES.isSuccess createReply) $ do
    liftIO $ assertFailure $ "failed to create mapping: " <> show name

-- | This doesn't fail if ES returns error because we don't really want to fail the tests for this
deleteIndex :: MonadIO m => Opt.Opts -> Text -> m ()
deleteIndex opts name = do
  let indexName = ES.IndexName name
  void $ runBH opts $ ES.deleteIndex indexName

runBH :: MonadIO m => Opt.Opts -> ES.BH IO a -> m a
runBH opts =
  let esURL = opts ^. Opt.elasticsearchL . Opt.urlL
   in liftIO . ES.withBH HTTP.defaultManagerSettings (ES.Server esURL)

-- | This was copied from at Brig.User.Search.Index at commit 3242aa26
analysisSettings :: ES.Analysis
analysisSettings =
  let analyzerDef =
        Map.fromList
          [ ("prefix_index", ES.AnalyzerDefinition (Just $ ES.Tokenizer "whitespace") [ES.TokenFilter "edge_ngram_1_30"] []),
            ("prefix_search", ES.AnalyzerDefinition (Just $ ES.Tokenizer "whitespace") [ES.TokenFilter "truncate_30"] [])
          ]
      filterDef =
        Map.fromList
          [ ("edge_ngram_1_30", ES.TokenFilterDefinitionEdgeNgram (ES.NgramFilter 1 30) Nothing),
            ("truncate_30", ES.TokenFilterTruncate 30)
          ]
   in ES.Analysis analyzerDef mempty filterDef mempty

--- | This was copied from at Brig.User.Search.Index.indexMapping at commit 3242aa26
oldMapping :: Value
oldMapping =
  fromJust $
    decode
      [r|
{
  "user": {
    "dynamic": false,
    "properties": {
      "account_status": {
        "store": false,
        "type": "keyword",
        "index": true
      },
      "handle": {
        "store": false,
        "type": "text",
        "index": true,
        "fields": {
          "prefix": {
            "search_analyzer": "prefix_search",
            "type": "text",
            "analyzer": "prefix_index"
          }
        }
      },
      "accent_id": {
        "store": false,
        "type": "byte",
        "index": false
      },
      "name": {
        "store": false,
        "type": "keyword",
        "index": false
      },
      "team": {
        "store": false,
        "type": "keyword",
        "index": true
      },
      "normalized": {
        "store": false,
        "type": "text",
        "index": true,
        "fields": {
          "prefix": {
            "search_analyzer": "prefix_search",
            "type": "text",
            "analyzer": "prefix_index"
          }
        }
      }
    }
  }
}
|]
