{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
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
import Bilge.Assert
import Brig.Options qualified as Opt
import Control.Lens ((?~))
import Control.Monad.Catch (MonadCatch)
import Data.Aeson qualified as Aeson
import Data.Domain (Domain (Domain))
import Data.Handle (fromHandle)
import Data.Id
import Data.Qualified (Qualified (qDomain, qUnqualified))
import Data.Text qualified as Text
import Federation.Util
import Imports
import Test.QuickCheck (Arbitrary (arbitrary), generate)
import Test.Tasty
import Test.Tasty.HUnit
import Util
import Wire.API.Federation.API.Brig (SearchResponse (SearchResponse))
import Wire.API.Team.Feature
import Wire.API.Team.SearchVisibility
import Wire.API.User as User
import Wire.API.User.Search
import Wire.API.User.Search qualified as Search

tests :: Opt.Opts -> Manager -> Galley -> Brig -> IO TestTree
tests opts mgr galley brig = do
  testSetupOutboundOnly <- runHttpT mgr prepareUsersForSearchVisibilityNoNameOutsideTeamTests
  pure $
    testGroup "search" $
      [ test mgr "by-name" $ testSearchByName brig,
        test mgr "by-handle" $ testSearchByHandle brig,
        test mgr "size - when exact handle matches a team user" $ testSearchSize brig True,
        test mgr "size - when exact handle matches a non team user" $ testSearchSize brig False,
        test mgr "empty query" $ testSearchEmpty brig,
        test mgr "no match" $ testSearchNoMatch brig,
        test mgr "no extra results" $ testSearchNoExtraResults brig,
        test mgr "order-handle (prefix match)" $ testOrderHandle brig,
        test mgr "by-first/middle/last name" $ testSearchByLastOrMiddleName brig,
        test mgr "Non ascii names" $ testSearchNonAsciiNames brig,
        test mgr "user with umlaut" $ testSearchWithUmlaut brig,
        test mgr "user with japanese name" $ testSearchCJK brig,
        testGroup "team A: SearchVisibilityStandard (= unrestricted outbound search)" $
          [ testGroup "team A: SearchableByOwnTeam (= restricted inbound search)" $
              [ test mgr "  I. non-team user cannot find team A member by display name" $ testSearchTeamMemberAsNonMemberDisplayName mgr brig galley FeatureStatusDisabled,
                test mgr " II. non-team user can find team A member by exact handle" $ testSearchTeamMemberAsNonMemberExactHandle mgr brig galley FeatureStatusDisabled,
                test mgr "III. team B member cannot find team A member by display name" $ testSearchTeamMemberAsOtherMemberDisplayName mgr brig galley FeatureStatusDisabled,
                test mgr " IV. team B member can find team A member by exact handle" $ testSearchTeamMemberAsOtherMemberExactHandle mgr brig galley FeatureStatusDisabled,
                test mgr "  V. team A member can find team A member by display name" $ testSearchTeamMemberAsSameMember mgr brig galley FeatureStatusDisabled,
                test mgr " VI. team A member can find non-team user by display name" $ testSeachNonMemberAsTeamMember brig,
                testGroup "order" $
                  [ test mgr "team-mates are listed before team-outsiders (exact match)" $ testSearchOrderingAsTeamMemberExactMatch brig,
                    test mgr "team-mates are listed before team-outsiders (prefix match)" $ testSearchOrderingAsTeamMemberPrefixMatch brig,
                    test mgr "team-mates are listed before team-outsiders (worse name match)" $ testSearchOrderingAsTeamMemberWorseNameMatch brig,
                    test mgr "team-mates are listed after team-outsiders (worse handle match)" $ testSearchOrderingAsTeamMemberWorseHandleMatch brig
                  ]
              ],
            testGroup "team A: SearchableByAllTeams (= unrestricted inbound search)" $
              [ test mgr "   I.  non-team user cannot find team A member via display name" $ testSearchTeamMemberAsNonMemberDisplayName mgr brig galley FeatureStatusEnabled,
                test mgr "  II.  non-team user can find team A member by exact handle" $ testSearchTeamMemberAsNonMemberExactHandle mgr brig galley FeatureStatusEnabled,
                test mgr "III*.  team B member can find team A member by display name" $ testSearchTeamMemberAsOtherMemberDisplayName mgr brig galley FeatureStatusEnabled,
                test mgr "  IV.  team B member can find team A member by exact handle" $ testSearchTeamMemberAsOtherMemberExactHandle mgr brig galley FeatureStatusEnabled,
                test mgr "   V.  team A member can find team A member by display name" $ testSearchTeamMemberAsSameMember mgr brig galley FeatureStatusEnabled
              ]
          ],
        testGroup "searchSameTeamOnly == true (server setting)" $
          [ test mgr "any team user cannot find any non-team user by display name or exact handle" $ testSearchSameTeamOnly brig opts
          ],
        testGroup "team A: SearchVisibilityNoNameOutsideTeam (restricted outbound search)" $
          [ testGroup "team A: SearchableByOwnTeam (= restricted inbound search)" $
              [ test mgr "I. non-team user cannot find team A member by display name" $ testSearchTeamMemberAsNonMemberOutboundOnly brig testSetupOutboundOnly,
                test mgr "team A member cannot find team B member by display name" $ testSearchTeamMemberAsOtherMemberOutboundOnly brig testSetupOutboundOnly,
                test mgr "team A member can find team B member by by exact handle" $ testSearchOutboundOnlyCanFindOtherByHandle brig testSetupOutboundOnly,
                test mgr "V. team A member can find other team A member by display name or exact handle" $ testSearchTeamMemberAsSameMemberOutboundOnly brig testSetupOutboundOnly,
                test mgr "team A member cannot find non-team user by display name" $ testSearchNonMemberOutboundOnlyByDisplay brig testSetupOutboundOnly,
                test mgr "team A member can find non-team user by exact handle" $ testSearchNonMemberOutboundOnlyByHandle brig testSetupOutboundOnly
              ]
          ],
        testGroup "federated" $
          [ test mgr "search passing own domain" $ testSearchWithDomain brig,
            test mgr "remote lookup should call remote code path" $ testSearchOtherDomain opts brig
            -- FUTUREWORK(federation): we need tests for:
            -- failure/error cases on search (augment the federatorMock?)
            -- wire-api-federation Servant-Api vs protobuf-client interactions
          ],
        test mgr "user with unvalidated email" $ testSearchWithUnvalidatedEmail brig
      ]
  where
    -- Since the tests are about querying only, we only need 1 creation
    -- FUTUREWORK: this should probably be used for all tests in this module, not just some.
    prepareUsersForSearchVisibilityNoNameOutsideTeamTests :: Http ((TeamId, User, User), (TeamId, User, User), User)
    prepareUsersForSearchVisibilityNoNameOutsideTeamTests = do
      (tidA, ownerA, memberA : _) <- createPopulatedBindingTeamWithNamesAndHandles brig 1
      setTeamTeamSearchVisibilityAvailable galley tidA FeatureStatusEnabled
      setTeamSearchVisibility galley tidA SearchVisibilityNoNameOutsideTeam
      (tidB, ownerB, memberB : _) <- createPopulatedBindingTeamWithNamesAndHandles brig 1
      regularUser <- randomUserWithHandle brig
      pure ((tidA, ownerA, memberA), (tidB, ownerB, memberB), regularUser)

type TestConstraints m = (MonadFail m, MonadCatch m, MonadIO m, MonadHttp m)

testSearchWithUnvalidatedEmail :: (TestConstraints m) => Brig -> m ()
testSearchWithUnvalidatedEmail brig = do
  (tid, owner, user : _) <- createPopulatedBindingTeamWithNamesAndHandles brig 1
  let uid = userId user
      Just oldEmail = userEmail user
      ownerId = userId owner
  let searchForUserAndCheckThat = searchAndCheckResult brig tid ownerId uid
  email <- randomEmail
  searchForUserAndCheckThat
    ( \tc -> do
        Search.teamContactEmail tc @?= Just oldEmail
        assertBool "unvalidated email should be null" (isNothing . Search.teamContactEmailUnvalidated $ tc)
    )
  initiateEmailUpdateLogin brig email (emailLogin oldEmail defPassword Nothing) uid !!! const 202 === statusCode
  searchForUserAndCheckThat
    ( \tc -> do
        Search.teamContactEmail tc @?= Just oldEmail
        Search.teamContactEmailUnvalidated tc @?= Just email
    )
  activateEmail brig email
  searchForUserAndCheckThat
    ( \tc -> do
        Search.teamContactEmail tc @?= Just email
        assertBool "unvalidated email should be null" (isNothing . Search.teamContactEmailUnvalidated $ tc)
    )
  where
    searchAndCheckResult :: (TestConstraints m) => Brig -> TeamId -> UserId -> UserId -> (Search.TeamContact -> Assertion) -> m ()
    searchAndCheckResult b tid ownerId userToSearchFor assertion =
      executeTeamUserSearch b tid ownerId Nothing Nothing Nothing Nothing >>= checkResult userToSearchFor assertion . searchResults

    checkResult :: (TestConstraints m) => UserId -> (Search.TeamContact -> Assertion) -> [Search.TeamContact] -> m ()
    checkResult userToSearchFor assertion results = liftIO $ do
      let mbTeamContact = find ((==) userToSearchFor . Search.teamContactUserId) results
      case mbTeamContact of
        Nothing -> fail "no team contact found"
        Just teamContact -> assertion teamContact

testSearchByName :: (TestConstraints m) => Brig -> m ()
testSearchByName brig = do
  u1 <- randomUser brig
  u2 <- randomUser brig
  let uid1 = userId u1
      quid1 = userQualifiedId u1
      uid2 = userId u2
      quid2 = userQualifiedId u2
  assertCanFind brig uid1 quid2 (fromName (userDisplayName u2))
  assertCanFind brig uid2 quid1 (fromName (userDisplayName u1))
  -- Users cannot find themselves
  assertCan'tFind brig uid1 quid1 (fromName (userDisplayName u1))
  assertCan'tFind brig uid2 quid2 (fromName (userDisplayName u2))

testSearchByLastOrMiddleName :: (TestConstraints m) => Brig -> m ()
testSearchByLastOrMiddleName brig = do
  searcher <- userId <$> randomUser brig
  firstName <- randomHandle
  middleName <- randomHandle
  lastName <- randomHandle
  searchedUser <- createUser' True (firstName <> " " <> middleName <> " " <> lastName) brig
  let searched = userQualifiedId searchedUser
  assertCanFind brig searcher searched firstName
  assertCanFind brig searcher searched middleName
  assertCanFind brig searcher searched lastName
  assertCanFind brig searcher searched (firstName <> " " <> lastName)

testSearchNonAsciiNames :: (TestConstraints m) => Brig -> m ()
testSearchNonAsciiNames brig = do
  searcher <- userId <$> randomUser brig
  suffix <- randomHandle
  searchedUser <- createUser' True ("शक्तिमान" <> suffix) brig
  let searched = userQualifiedId searchedUser
  assertCanFind brig searcher searched ("शक्तिमान" <> suffix)
  -- This is pathetic transliteration, but it is what we have.
  assertCanFind brig searcher searched ("saktimana" <> suffix)

testSearchCJK :: (TestConstraints m) => Brig -> m ()
testSearchCJK brig = do
  searcher <- randomUser brig
  user <- createUser' True "藤崎詩織" brig
  user' <- createUser' True "さおり" brig
  user'' <- createUser' True "ジョン" brig
  assertCanFind brig (User.userId searcher) user.userQualifiedId "藤崎詩織"

  assertCanFind brig (User.userId searcher) user'.userQualifiedId "saori"
  assertCanFind brig (User.userId searcher) user'.userQualifiedId "さおり"
  assertCanFind brig (User.userId searcher) user'.userQualifiedId "サオリ"

  assertCanFind brig (User.userId searcher) user''.userQualifiedId "jon"
  assertCanFind brig (User.userId searcher) user''.userQualifiedId "ジョン"
  assertCanFind brig (User.userId searcher) user''.userQualifiedId "じょん"

testSearchWithUmlaut :: (TestConstraints m) => Brig -> m ()
testSearchWithUmlaut brig = do
  searcher <- randomUser brig
  user <- createUser' True "Özi Müller" brig
  assertCanFind brig (User.userId searcher) user.userQualifiedId "ozi muller"
  assertCanFind brig (User.userId searcher) user.userQualifiedId "Özi Müller"

testSearchByHandle :: (TestConstraints m) => Brig -> m ()
testSearchByHandle brig = do
  u1 <- randomUserWithHandle brig
  u2 <- randomUser brig
  let quid1 = userQualifiedId u1
      uid2 = userId u2
      Just h = fromHandle <$> userHandle u1
  assertCanFind brig uid2 quid1 h

testSearchEmpty :: (TestConstraints m) => Brig -> m ()
testSearchEmpty brig = do
  -- This user exists just in case empty string starts matching everything
  _someUser <- randomUserWithHandle brig
  searcher <- randomUser brig
  res <- searchResults <$> executeSearch brig (userId searcher) ""
  liftIO $ assertEqual "nothing should be returned" [] res

testSearchSize :: (TestConstraints m) => Brig -> Bool -> m ()
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

testSearchNoMatch :: (TestConstraints m) => Brig -> m ()
testSearchNoMatch brig = do
  u1 <- randomUser brig
  _ <- randomUser brig
  let uid1 = userId u1
  -- _uid2 = userId u2
  result <- searchResults <$> executeSearch brig uid1 "nomatch"
  liftIO $ assertEqual "Expected 0 results" 0 (length result)

testSearchNoExtraResults :: (TestConstraints m) => Brig -> m ()
testSearchNoExtraResults brig = do
  u1Handle <- ("zqnoextra1-" <>) <$> randomHandle
  u1 <- createUser' True u1Handle brig
  u2Handle <- ("zqnoextra2-" <>) <$> randomHandle
  u2 <- createUser' True u2Handle brig
  let uid1 = userId u1
      quid2 = userQualifiedId u2
  resultUIds <- map contactQualifiedId . searchResults <$> executeSearch brig uid1 u2Handle
  liftIO $
    assertEqual "Expected search returns only the searched" [quid2] resultUIds

testOrderHandle :: (TestConstraints m) => Brig -> m ()
testOrderHandle brig = do
  searcher <- userId <$> randomUser brig
  searchedWord <- randomHandle
  handleMatch <- userQualifiedId <$> createUser' True "handle match" brig
  void $ putHandle brig (qUnqualified handleMatch) searchedWord
  handlePrefixMatch <- userQualifiedId <$> createUser' True "handle prefix match" brig
  void $ putHandle brig (qUnqualified handlePrefixMatch) (searchedWord <> "suffix")
  results <- searchResults <$> executeSearch brig searcher searchedWord
  let resultUIds = map contactQualifiedId results
  let expectedOrder = [handleMatch, handlePrefixMatch]
  liftIO $
    assertEqual
      "Expected order: handle match, handle prefix match."
      expectedOrder
      resultUIds

testSearchTeamMemberAsNonMemberDisplayName :: (TestConstraints m) => Manager -> Brig -> Galley -> FeatureStatus -> m ()
testSearchTeamMemberAsNonMemberDisplayName mgr brig galley inboundVisibility = do
  nonTeamMember <- randomUser brig
  (tid, _, [teamMember, teamBTargetReindexedAfter]) <- createPopulatedBindingTeamWithNamesAndHandles brig 2
  circumventSettingsOverride mgr $ setTeamSearchVisibilityInboundAvailable galley tid inboundVisibility
  void $ setRandomHandle brig teamBTargetReindexedAfter
  assertCan'tFind brig (userId nonTeamMember) (userQualifiedId teamMember) (fromName (userDisplayName teamMember))
  assertCan'tFind brig (userId nonTeamMember) (userQualifiedId teamBTargetReindexedAfter) (fromName (userDisplayName teamBTargetReindexedAfter))

testSearchTeamMemberAsNonMemberExactHandle :: (TestConstraints m) => Manager -> Brig -> Galley -> FeatureStatus -> m ()
testSearchTeamMemberAsNonMemberExactHandle mgr brig galley inboundVisibility = do
  nonTeamMember <- randomUser brig
  (tid, _, [teamMember, teamMemberReindexedAfter]) <- createPopulatedBindingTeamWithNamesAndHandles brig 2
  circumventSettingsOverride mgr $ setTeamSearchVisibilityInboundAvailable galley tid inboundVisibility
  teamMemberReindexedAfterHandle <- do
    teamMemberReindexedAfter' <- setRandomHandle brig teamMemberReindexedAfter
    pure $ fromMaybe (error "teamATargetReindexedAfter must have a handle") (userHandle teamMemberReindexedAfter')
  let teamMemberHandle = fromMaybe (error "teamMember must have a handle") (userHandle teamMember)
  assertCanFind brig (userId nonTeamMember) (userQualifiedId teamMember) (fromHandle teamMemberHandle)
  assertCanFind brig (userId nonTeamMember) (userQualifiedId teamMemberReindexedAfter) (fromHandle teamMemberReindexedAfterHandle)

testSearchTeamMemberAsOtherMemberDisplayName :: (TestConstraints m) => Manager -> Brig -> Galley -> FeatureStatus -> m ()
testSearchTeamMemberAsOtherMemberDisplayName mgr brig galley inboundVisibility = do
  (_, _, [teamBSearcher]) <- createPopulatedBindingTeamWithNamesAndHandles brig 1
  (tidA, _, [teamATarget, teamATargetReindexedAfter]) <- createPopulatedBindingTeamWithNamesAndHandles brig 2
  circumventSettingsOverride mgr $ setTeamSearchVisibilityInboundAvailable galley tidA inboundVisibility
  void $ setRandomHandle brig teamATargetReindexedAfter
  assertion brig (userId teamBSearcher) (userQualifiedId teamATarget) (fromName (userDisplayName teamATarget))
  assertion brig (userId teamBSearcher) (userQualifiedId teamATargetReindexedAfter) (fromName (userDisplayName teamATargetReindexedAfter))
  where
    assertion :: (MonadCatch m, MonadIO m, MonadHttp m, HasCallStack) => Brig -> UserId -> Qualified UserId -> Text -> m ()
    assertion =
      case inboundVisibility of
        FeatureStatusEnabled -> assertCanFind
        FeatureStatusDisabled -> assertCan'tFind

testSearchTeamMemberAsOtherMemberExactHandle :: (TestConstraints m) => Manager -> Brig -> Galley -> FeatureStatus -> m ()
testSearchTeamMemberAsOtherMemberExactHandle mgr brig galley inboundVisibility = do
  (_, _, [teamASearcher]) <- createPopulatedBindingTeamWithNamesAndHandles brig 1
  (tidA, _, [teamATarget, teamATargetReindexedAfter]) <- createPopulatedBindingTeamWithNamesAndHandles brig 2
  circumventSettingsOverride mgr $ setTeamSearchVisibilityInboundAvailable galley tidA inboundVisibility
  teamATargetReindexedAfter' <- setRandomHandle brig teamATargetReindexedAfter
  let teamATargetHandle = fromMaybe (error "teamATarget must have a handle") (userHandle teamATarget)
  assertCanFind brig (userId teamASearcher) (userQualifiedId teamATarget) (fromHandle teamATargetHandle)
  assertCanFind brig (userId teamASearcher) (userQualifiedId teamATargetReindexedAfter) (fromHandle (fromJust (userHandle teamATargetReindexedAfter')))

testSearchTeamMemberAsSameMember :: (TestConstraints m) => Manager -> Brig -> Galley -> FeatureStatus -> m ()
testSearchTeamMemberAsSameMember mgr brig galley inboundVisibility = do
  (tid, _, [teamASearcher, teamATarget]) <- createPopulatedBindingTeam brig 2
  circumventSettingsOverride mgr $ setTeamSearchVisibilityInboundAvailable galley tid inboundVisibility
  assertCanFind brig (userId teamASearcher) (userQualifiedId teamATarget) (fromName (userDisplayName teamATarget))

testSeachNonMemberAsTeamMember :: (TestConstraints m) => Brig -> m ()
testSeachNonMemberAsTeamMember brig = do
  nonTeamMember <- randomUser brig
  (_, _, [teamMember]) <- createPopulatedBindingTeam brig 1
  assertCanFind brig (userId teamMember) (userQualifiedId nonTeamMember) (fromName (userDisplayName nonTeamMember))

testSearchOrderingAsTeamMemberExactMatch :: (TestConstraints m) => Brig -> m ()
testSearchOrderingAsTeamMemberExactMatch brig = do
  searchedName <- randomName
  mapM_ (\(_ :: Int) -> createUser' True (fromName searchedName) brig) [0 .. 99]
  (_, _, [searcher, teamSearchee]) <- createPopulatedBindingTeamWithNames brig [Name "Searcher", searchedName]
  result <- executeSearch brig (userId searcher) (fromName searchedName)
  let resultUserIds = contactQualifiedId <$> searchResults result
  liftIO $
    case elemIndex (userQualifiedId teamSearchee) resultUserIds of
      Nothing -> assertFailure "team mate not found in search"
      Just teamSearcheeIndex -> assertEqual "teammate is not the first result" 0 teamSearcheeIndex

testSearchOrderingAsTeamMemberPrefixMatch :: (TestConstraints m) => Brig -> m ()
testSearchOrderingAsTeamMemberPrefixMatch brig = do
  searchedName <- randomNameWithMaxLen 122 -- 6 characters for "suffix"
  mapM_ (\(i :: Int) -> createUser' True (fromName searchedName <> Text.pack (show i)) brig) [0 .. 99]
  (_, _, [searcher, teamSearchee]) <- createPopulatedBindingTeamWithNames brig [Name "Searcher", Name $ fromName searchedName <> "suffix"]
  result <- executeSearch brig (userId searcher) (fromName searchedName)
  let resultUserIds = contactQualifiedId <$> searchResults result
  liftIO $
    case elemIndex (userQualifiedId teamSearchee) resultUserIds of
      Nothing -> assertFailure "team mate not found in search"
      Just teamSearcheeIndex -> assertEqual "teammate is not the first result" 0 teamSearcheeIndex

testSearchOrderingAsTeamMemberWorseNameMatch :: (TestConstraints m) => Brig -> m ()
testSearchOrderingAsTeamMemberWorseNameMatch brig = do
  searchedTerm <- randomHandle
  _ <- createUser' True searchedTerm brig
  (_, _, [searcher, teamSearchee]) <- createPopulatedBindingTeamWithNames brig [Name "Searcher", Name (searchedTerm <> "Suffix")]
  result <- executeSearch brig (userId searcher) searchedTerm
  let resultUserIds = contactQualifiedId <$> searchResults result
  liftIO $
    case elemIndex (userQualifiedId teamSearchee) resultUserIds of
      Nothing -> assertFailure "team mate not found in search"
      Just teamSearcheeIndex -> assertEqual "teammate is not the first result" 0 teamSearcheeIndex

testSearchOrderingAsTeamMemberWorseHandleMatch :: (TestConstraints m) => Brig -> m ()
testSearchOrderingAsTeamMemberWorseHandleMatch brig = do
  searchedTerm <- randomHandle
  nonTeamSearchee <- createUser' True searchedTerm brig
  void $ putHandle brig (userId nonTeamSearchee) searchedTerm
  (_, _, [searcher, teamSearchee]) <- createPopulatedBindingTeamWithNames brig [Name "Searcher", Name (searchedTerm <> "Suffix")]
  result <- executeSearch brig (userId searcher) searchedTerm
  let resultUserIds = contactQualifiedId <$> searchResults result
  liftIO $ do
    case elemIndex (userQualifiedId nonTeamSearchee) resultUserIds of
      Nothing -> assertFailure "non team mate user not found in search"
      Just teamSearcheeIndex -> assertEqual "non team mate is not the first result" 0 teamSearcheeIndex
    case elemIndex (userQualifiedId teamSearchee) resultUserIds of
      Nothing -> assertFailure "team mate not found in search"
      Just teamSearcheeIndex -> assertEqual "teammate is not the second result" 1 teamSearcheeIndex

testSearchSameTeamOnly :: (TestConstraints m) => Brig -> Opt.Opts -> m ()
testSearchSameTeamOnly brig opts = do
  nonTeamMember' <- randomUser brig
  nonTeamMember <- setRandomHandle brig nonTeamMember'
  (_, _, [teamMember]) <- createPopulatedBindingTeam brig 1
  let newOpts = opts & Opt.settingsLens . Opt.searchSameTeamOnlyLens ?~ True
  withSettingsOverrides newOpts $ do
    assertCan'tFind brig (userId teamMember) (userQualifiedId nonTeamMember) (fromName (userDisplayName nonTeamMember))
    let nonTeamMemberHandle = fromMaybe (error "nonTeamMember must have a handle") (userHandle nonTeamMember)
    assertCan'tFind brig (userId teamMember) (userQualifiedId nonTeamMember) (fromHandle nonTeamMemberHandle)

testSearchTeamMemberAsNonMemberOutboundOnly :: Brig -> ((TeamId, User, User), (TeamId, User, User), User) -> Http ()
testSearchTeamMemberAsNonMemberOutboundOnly brig ((_, _, teamAMember), (_, _, _), nonTeamMember) = do
  assertCan'tFind brig (userId nonTeamMember) (userQualifiedId teamAMember) (fromName (userDisplayName teamAMember))

testSearchTeamMemberAsOtherMemberOutboundOnly :: Brig -> ((TeamId, User, User), (TeamId, User, User), User) -> Http ()
testSearchTeamMemberAsOtherMemberOutboundOnly brig ((_, _, teamAMember), (_, _, teamBMember), _) = do
  assertCan'tFind brig (userId teamAMember) (userQualifiedId teamBMember) (fromName (userDisplayName teamBMember))

testSearchOutboundOnlyCanFindOtherByHandle :: Brig -> ((TeamId, User, User), (TeamId, User, User), User) -> Http ()
testSearchOutboundOnlyCanFindOtherByHandle brig ((_, _, teamAMember), (_, _, teamBMember), _) = do
  let teamBMemberHandle = fromMaybe (error "teamBMember must have a handle") (userHandle teamBMember)
  assertCanFind brig (userId teamAMember) (userQualifiedId teamBMember) (fromHandle teamBMemberHandle)

testSearchTeamMemberAsSameMemberOutboundOnly :: Brig -> ((TeamId, User, User), (TeamId, User, User), User) -> Http ()
testSearchTeamMemberAsSameMemberOutboundOnly brig ((_, teamAOwner, teamAMember), (_, _, _), _) = do
  let teamAMemberHandle = fromMaybe (error "teamAMember must have a handle") (userHandle teamAMember)
  assertCanFind brig (userId teamAOwner) (userQualifiedId teamAMember) (fromName (userDisplayName teamAMember))
  assertCanFind brig (userId teamAOwner) (userQualifiedId teamAMember) (fromHandle teamAMemberHandle)
  let teamAOwnerHandle = fromMaybe (error "teamAOwner must have a handle") (userHandle teamAOwner)
  assertCanFind brig (userId teamAMember) (userQualifiedId teamAOwner) (fromName (userDisplayName teamAOwner))
  assertCanFind brig (userId teamAMember) (userQualifiedId teamAOwner) (fromHandle teamAOwnerHandle)

testSearchNonMemberOutboundOnlyByDisplay :: Brig -> ((TeamId, User, User), (TeamId, User, User), User) -> Http ()
testSearchNonMemberOutboundOnlyByDisplay brig ((_, _, teamAMember), (_, _, _), nonTeamMember) = do
  assertCan'tFind brig (userId teamAMember) (userQualifiedId nonTeamMember) (fromName (userDisplayName nonTeamMember))

testSearchNonMemberOutboundOnlyByHandle :: Brig -> ((TeamId, User, User), (TeamId, User, User), User) -> Http ()
testSearchNonMemberOutboundOnlyByHandle brig ((_, _, teamAMember), (_, _, _), nonTeamMember) = do
  let teamMemberAHandle = fromMaybe (error "nonTeamMember must have a handle") (userHandle nonTeamMember)
  assertCanFind brig (userId teamAMember) (userQualifiedId nonTeamMember) (fromHandle teamMemberAHandle)

testSearchWithDomain :: (TestConstraints m) => Brig -> m ()
testSearchWithDomain brig = do
  searcher <- randomUser brig
  searchee <- randomUser brig
  let searcherId = userId searcher
      searcheeQid = userQualifiedId searchee
      searcheeName = fromName (userDisplayName searchee)
      searcheeDomain = qDomain searcheeQid
  assertCanFindWithDomain brig searcherId searcheeQid searcheeName searcheeDomain

-- | WARNING: this test only tests that brig will indeed make a call to federator
-- (i.e. does the correct if/else branching based on the domain),
-- it does not test any of the federation API details. This needs to be tested separately.
testSearchOtherDomain :: (TestConstraints m) => Opt.Opts -> Brig -> m ()
testSearchOtherDomain opts brig = do
  user <- randomUser brig
  -- We cannot assert on a real federated request here, so we make a request to
  -- a mocked federator started and stopped during this test
  otherSearchResult :: [Contact] <- liftIO $ generate arbitrary
  let mockResponse = Aeson.encode (SearchResponse otherSearchResult ExactHandleSearch)
  (searchResult, _) <- liftIO . withTempMockFederator opts mockResponse $ do
    executeSearchWithDomain brig (userId user) "someSearchText" (Domain "non-existent.example.com")
  let expectedResult =
        SearchResult
          { searchResults = otherSearchResult,
            searchFound = length otherSearchResult,
            searchReturned = length otherSearchResult,
            searchTook = 0,
            searchPolicy = ExactHandleSearch,
            searchPagingState = Nothing,
            searchHasMore = Nothing
          }
  liftIO $ do
    assertEqual "The search request should get its result from federator" expectedResult searchResult
