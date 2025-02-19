{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
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
import Brig.App (initHttpManagerWithTLSConfig)
import Brig.Index.Eval (runCommand)
import Brig.Index.Options
import Brig.Index.Options qualified as IndexOpts
import Brig.Options (ElasticSearchOpts)
import Brig.Options qualified as Opt
import Brig.Options qualified as Opts
import Cassandra qualified as C
import Cassandra.Options qualified as CassOpts
import Control.Lens ((.~), (?~), (^.))
import Control.Monad.Catch (MonadCatch)
import Data.Aeson (Value, decode)
import Data.Aeson qualified as Aeson
import Data.Domain (Domain (Domain))
import Data.Handle (fromHandle)
import Data.Id
import Data.Qualified (Qualified (qDomain, qUnqualified))
import Data.String.Conversions
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Database.Bloodhound qualified as ES
import Federation.Util
import Imports
import Network.HTTP.ReverseProxy (waiProxyTo)
import Network.HTTP.ReverseProxy qualified as Wai
import Network.HTTP.Types qualified as HTTP
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Test qualified as WaiTest
import Safe (headMay)
import System.Logger qualified as Log
import Test.QuickCheck (Arbitrary (arbitrary), generate)
import Test.Tasty
import Test.Tasty.HUnit
import Text.RawString.QQ (r)
import URI.ByteString qualified as URI
import UnliftIO (Concurrently (..), async, bracket, cancel, runConcurrently)
import Util
import Util.Options (Endpoint)
import Wire.API.Federation.API.Brig (SearchResponse (SearchResponse))
import Wire.API.Team.Feature
import Wire.API.Team.SearchVisibility
import Wire.API.User as User
import Wire.API.User.Search
import Wire.API.User.Search qualified as Search
import Wire.IndexedUserStore.ElasticSearch (mappingName)
import Wire.IndexedUserStore.MigrationStore.ElasticSearch (defaultMigrationIndexName)

tests :: Opt.Opts -> ES.Server -> Manager -> Galley -> Brig -> IO TestTree
tests opts additionalElasticSearch mgr galley brig = do
  testSetupOutboundOnly <- runHttpT mgr prepareUsersForSearchVisibilityNoNameOutsideTeamTests
  pure $
    testGroup "search" $
      [ testWithBothIndices opts mgr "by-name" $ testSearchByName brig,
        testWithBothIndices opts mgr "by-handle" $ testSearchByHandle brig,
        testWithBothIndices opts mgr "size - when exact handle matches a team user" $ testSearchSize brig True,
        testWithBothIndices opts mgr "size - when exact handle matches a non team user" $ testSearchSize brig False,
        test mgr "empty query" $ testSearchEmpty brig,
        flakyTest mgr "reindex" $ testReindex brig,
        testWithBothIndices opts mgr "no match" $ testSearchNoMatch brig,
        testWithBothIndices opts mgr "no extra results" $ testSearchNoExtraResults brig,
        testWithBothIndices opts mgr "order-handle (prefix match)" $ testOrderHandle brig,
        testWithBothIndices opts mgr "by-first/middle/last name" $ testSearchByLastOrMiddleName brig,
        testWithBothIndices opts mgr "Non ascii names" $ testSearchNonAsciiNames brig,
        testWithBothIndices opts mgr "user with umlaut" $ testSearchWithUmlaut brig,
        testWithBothIndices opts mgr "user with japanese name" $ testSearchCJK brig,
        testGroup "index migration" $
          [ testGroup "same ElasticSearch instance" $
              let esServer = (opts ^. Opt.elasticsearchLens . Opt.urlLens)
               in [ test mgr "migration to new index from existing index" $ testMigrationToNewIndex opts brig esServer runReindexFromAnotherIndex,
                    test mgr "migration to new index from database" $ testMigrationToNewIndex opts brig esServer (runReindexFromDatabase Reindex),
                    test mgr "migration to new index from database (force sync)" $ testMigrationToNewIndex opts brig esServer (runReindexFromDatabase ReindexSameOrNewer)
                  ],
            testGroup "different ElasticSearch instance" $
              [ test mgr "migration to new index from database" $
                  testMigrationToNewIndex opts brig additionalElasticSearch (runReindexFromDatabase Migrate)
              ]
          ],
        testGroup "team A: SearchVisibilityStandard (= unrestricted outbound search)" $
          [ testGroup "team A: SearchableByOwnTeam (= restricted inbound search)" $
              [ testWithBothIndices opts mgr "  I. non-team user cannot find team A member by display name" $ testSearchTeamMemberAsNonMemberDisplayName mgr brig galley FeatureStatusDisabled,
                testWithBothIndices opts mgr " II. non-team user can find team A member by exact handle" $ testSearchTeamMemberAsNonMemberExactHandle mgr brig galley FeatureStatusDisabled,
                testWithBothIndices opts mgr "III. team B member cannot find team A member by display name" $ testSearchTeamMemberAsOtherMemberDisplayName mgr brig galley FeatureStatusDisabled,
                testWithBothIndices opts mgr " IV. team B member can find team A member by exact handle" $ testSearchTeamMemberAsOtherMemberExactHandle mgr brig galley FeatureStatusDisabled,
                testWithBothIndices opts mgr "  V. team A member can find team A member by display name" $ testSearchTeamMemberAsSameMember mgr brig galley FeatureStatusDisabled,
                testWithBothIndices opts mgr " VI. team A member can find non-team user by display name" $ testSeachNonMemberAsTeamMember brig,
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
          [ testWithBothIndicesAndOpts opts mgr "any team user cannot find any non-team user by display name or exact handle" $ testSearchSameTeamOnly brig
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
      refreshIndex brig
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
  refreshIndex brig
  searchForUserAndCheckThat
    ( \tc -> do
        Search.teamContactEmail tc @?= Just oldEmail
        assertBool "unvalidated email should be null" (isNothing . Search.teamContactEmailUnvalidated $ tc)
    )
  initiateEmailUpdateLogin brig email (emailLogin oldEmail defPassword Nothing) uid !!! const 202 === statusCode
  refreshIndex brig
  searchForUserAndCheckThat
    ( \tc -> do
        Search.teamContactEmail tc @?= Just oldEmail
        Search.teamContactEmailUnvalidated tc @?= Just email
    )
  activateEmail brig email
  refreshIndex brig
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

testSearchByLastOrMiddleName :: (TestConstraints m) => Brig -> m ()
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

testSearchNonAsciiNames :: (TestConstraints m) => Brig -> m ()
testSearchNonAsciiNames brig = do
  searcher <- userId <$> randomUser brig
  suffix <- randomHandle
  searchedUser <- createUser' True ("शक्तिमान" <> suffix) brig
  let searched = userQualifiedId searchedUser
  refreshIndex brig
  assertCanFind brig searcher searched ("शक्तिमान" <> suffix)
  -- This is pathetic transliteration, but it is what we have.
  assertCanFind brig searcher searched ("saktimana" <> suffix)

testSearchCJK :: (TestConstraints m) => Brig -> m ()
testSearchCJK brig = do
  searcher <- randomUser brig
  user <- createUser' True "藤崎詩織" brig
  user' <- createUser' True "さおり" brig
  user'' <- createUser' True "ジョン" brig
  refreshIndex brig
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
  refreshIndex brig
  assertCanFind brig (User.userId searcher) user.userQualifiedId "ozi muller"
  assertCanFind brig (User.userId searcher) user.userQualifiedId "Özi Müller"

testSearchByHandle :: (TestConstraints m) => Brig -> m ()
testSearchByHandle brig = do
  u1 <- randomUserWithHandle brig
  u2 <- randomUser brig
  refreshIndex brig
  let quid1 = userQualifiedId u1
      uid2 = userId u2
      Just h = fromHandle <$> userHandle u1
  assertCanFind brig uid2 quid1 h

testSearchEmpty :: (TestConstraints m) => Brig -> m ()
testSearchEmpty brig = do
  -- This user exists just in case empty string starts matching everything
  _someUser <- randomUserWithHandle brig
  searcher <- randomUser brig
  refreshIndex brig
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

testSearchNoMatch :: (TestConstraints m) => Brig -> m ()
testSearchNoMatch brig = do
  u1 <- randomUser brig
  _ <- randomUser brig
  let uid1 = userId u1
  -- _uid2 = userId u2
  refreshIndex brig
  result <- searchResults <$> executeSearch brig uid1 "nomatch"
  liftIO $ assertEqual "Expected 0 results" 0 (length result)

testSearchNoExtraResults :: (TestConstraints m) => Brig -> m ()
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

-- This test is currently disabled, because it fails sporadically, probably due
-- to imprecisions in ES exact match scoring.
-- FUTUREWORK: Find the reason for the failures and fix ES behaviour.
-- See also the "cassandra writetime hypothesis":
--   https://wearezeta.atlassian.net/browse/BE-523
--   https://github.com/wireapp/wire-server/pull/1798#issuecomment-933174913
_testOrderName :: (TestConstraints m) => Brig -> m ()
_testOrderName brig = do
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

testOrderHandle :: (TestConstraints m) => Brig -> m ()
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
  -- we set a random handle here to force a reindexing of that user
  void $ setRandomHandle brig teamBTargetReindexedAfter
  refreshIndex brig
  assertCan'tFind brig (userId nonTeamMember) (userQualifiedId teamMember) (fromName (userDisplayName teamMember))
  assertCan'tFind brig (userId nonTeamMember) (userQualifiedId teamBTargetReindexedAfter) (fromName (userDisplayName teamBTargetReindexedAfter))

testSearchTeamMemberAsNonMemberExactHandle :: (TestConstraints m) => Manager -> Brig -> Galley -> FeatureStatus -> m ()
testSearchTeamMemberAsNonMemberExactHandle mgr brig galley inboundVisibility = do
  nonTeamMember <- randomUser brig
  (tid, _, [teamMember, teamMemberReindexedAfter]) <- createPopulatedBindingTeamWithNamesAndHandles brig 2
  circumventSettingsOverride mgr $ setTeamSearchVisibilityInboundAvailable galley tid inboundVisibility
  -- we set a random handle here to force a reindexing of that user
  teamMemberReindexedAfterHandle <- do
    teamMemberReindexedAfter' <- setRandomHandle brig teamMemberReindexedAfter
    pure $ fromMaybe (error "teamATargetReindexedAfter must have a handle") (userHandle teamMemberReindexedAfter')
  refreshIndex brig
  let teamMemberHandle = fromMaybe (error "teamMember must have a handle") (userHandle teamMember)
  assertCanFind brig (userId nonTeamMember) (userQualifiedId teamMember) (fromHandle teamMemberHandle)
  assertCanFind brig (userId nonTeamMember) (userQualifiedId teamMemberReindexedAfter) (fromHandle teamMemberReindexedAfterHandle)

testSearchTeamMemberAsOtherMemberDisplayName :: (TestConstraints m) => Manager -> Brig -> Galley -> FeatureStatus -> m ()
testSearchTeamMemberAsOtherMemberDisplayName mgr brig galley inboundVisibility = do
  (_, _, [teamBSearcher]) <- createPopulatedBindingTeamWithNamesAndHandles brig 1
  (tidA, _, [teamATarget, teamATargetReindexedAfter]) <- createPopulatedBindingTeamWithNamesAndHandles brig 2
  refreshIndex brig
  circumventSettingsOverride mgr $ setTeamSearchVisibilityInboundAvailable galley tidA inboundVisibility
  void $ setRandomHandle brig teamATargetReindexedAfter
  hFlush stdout
  refreshIndex brig
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
  refreshIndex brig
  let teamATargetHandle = fromMaybe (error "teamATarget must have a handle") (userHandle teamATarget)
  assertCanFind brig (userId teamASearcher) (userQualifiedId teamATarget) (fromHandle teamATargetHandle)
  assertCanFind brig (userId teamASearcher) (userQualifiedId teamATargetReindexedAfter) (fromHandle (fromJust (userHandle teamATargetReindexedAfter')))

testSearchTeamMemberAsSameMember :: (TestConstraints m) => Manager -> Brig -> Galley -> FeatureStatus -> m ()
testSearchTeamMemberAsSameMember mgr brig galley inboundVisibility = do
  (tid, _, [teamASearcher, teamATarget]) <- createPopulatedBindingTeam brig 2
  circumventSettingsOverride mgr $ setTeamSearchVisibilityInboundAvailable galley tid inboundVisibility
  refreshIndex brig
  assertCanFind brig (userId teamASearcher) (userQualifiedId teamATarget) (fromName (userDisplayName teamATarget))

testSeachNonMemberAsTeamMember :: (TestConstraints m) => Brig -> m ()
testSeachNonMemberAsTeamMember brig = do
  nonTeamMember <- randomUser brig
  (_, _, [teamMember]) <- createPopulatedBindingTeam brig 1
  refreshIndex brig
  assertCanFind brig (userId teamMember) (userQualifiedId nonTeamMember) (fromName (userDisplayName nonTeamMember))

testSearchOrderingAsTeamMemberExactMatch :: (TestConstraints m) => Brig -> m ()
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

testSearchOrderingAsTeamMemberPrefixMatch :: (TestConstraints m) => Brig -> m ()
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

testSearchOrderingAsTeamMemberWorseNameMatch :: (TestConstraints m) => Brig -> m ()
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

testSearchOrderingAsTeamMemberWorseHandleMatch :: (TestConstraints m) => Brig -> m ()
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

testSearchSameTeamOnly :: (TestConstraints m) => Brig -> Opt.Opts -> m ()
testSearchSameTeamOnly brig opts = do
  nonTeamMember' <- randomUser brig
  nonTeamMember <- setRandomHandle brig nonTeamMember'
  (_, _, [teamMember]) <- createPopulatedBindingTeam brig 1
  refreshIndex brig
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
  let teamAOwnerHandle = fromMaybe (error "teamAMember must have a handle") (userHandle teamAOwner)
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
  refreshIndex brig
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

-- | Migration sequence:
-- 1. A migration is planned, in this time brig writes to two indices
-- 2. A migration is triggered, users are copied from old index to new index
-- 3. Brig is configured to write to only the new index
--
-- So, we have four time frames ("phases") in which a user could be created/updated:
-- 1. Before migration is even planned
-- 2. When brig is writing to both indices
-- 3. While/After reindexing is done from old index to new index
-- 4. After brig is writing to only the new index
--
-- Note: The new index can be on another cluster of ES, but we have only one ES
-- cluster. This test spins up a proxy server to pass requests to our only ES
-- server. The proxy server ensures that only requests to the 'old' index go
-- through.
testMigrationToNewIndex ::
  (TestConstraints m, MonadUnliftIO m) =>
  Opt.Opts ->
  Brig ->
  ES.Server ->
  (Log.Logger -> Opt.Opts -> ES.IndexName -> ES.IndexName -> IO ()) ->
  m ()
testMigrationToNewIndex opts brig additionalIndexServer migrateIndexCommand = do
  logger <- Log.create Log.StdOut
  migrationIndexName <- ES.IndexName <$> randomHandle
  -- running brig with `withSettingsOverride` to direct it to the expected index(es).  it's
  -- important to make both old and new index name/url explicit via `withESProxy`, or the
  -- calls to `refreshIndex` in this test will interfere with parallel test runs of this test.
  withESProxy logger opts migrationIndexName $ \oldESUrl oldESIndex ->
    withESProxy logger (opts & Opt.elasticsearchLens . Opt.urlLens .~ additionalIndexServer) migrationIndexName $
      \newESUrl newESIndex -> do
        let optsWithIndex :: Text -> Opt.Opts
            optsWithIndex "old" =
              opts
                & Opt.elasticsearchLens . Opt.indexLens .~ oldESIndex
                & Opt.elasticsearchLens . Opt.urlLens .~ oldESUrl
            optsWithIndex "new" =
              opts
                & Opt.elasticsearchLens . Opt.indexLens .~ newESIndex
                & Opt.elasticsearchLens . Opt.urlLens .~ newESUrl
            optsWithIndex "both" =
              optsWithIndex "old"
                & Opt.elasticsearchLens . Opt.additionalWriteIndexLens ?~ newESIndex
                & Opt.elasticsearchLens . Opt.additionalWriteIndexUrlLens ?~ newESUrl
                -- 'additionalCaCertLens' needs to be added in order for brig to be able to reach both indices.
                & Opt.elasticsearchLens . Opt.additionalCaCertLens .~ (opts ^. Opt.elasticsearchLens . Opt.caCertLens)
                & Opt.elasticsearchLens . Opt.additionalInsecureSkipVerifyTlsLens .~ (opts ^. Opt.elasticsearchLens . Opt.insecureSkipVerifyTlsLens)
            optsWithIndex "oldAccessToBoth" =
              -- Configure only the old index. However, allow HTTP access to both
              -- (such that jobs can create and fill the new one).
              optsWithIndex "old" & Opt.elasticsearchLens . Opt.urlLens .~ additionalIndexServer

        -- Phase 1: Using old index only
        (phase1NonTeamUser, teamOwner, phase1TeamUser1, phase1TeamUser2, tid) <- withSettingsOverrides (optsWithIndex "old") $ do
          nonTeamUser <- randomUser brig
          (tid, teamOwner, [teamUser1, teamUser2]) <- createPopulatedBindingTeam brig 2
          pure (nonTeamUser, teamOwner, teamUser1, teamUser2, tid)

        -- Phase 2: Using old index for search, writing to both indices, migrations have not run
        (phase2NonTeamUser, phase2TeamUser) <- withSettingsOverrides (optsWithIndex "both") $ do
          phase2NonTeamUser <- randomUser brig
          phase2TeamUser <- inviteAndRegisterUser teamOwner tid brig
          refreshIndex brig

          -- searching phase1 users should work
          assertEventuallyCanFindByName brig phase1TeamUser1 phase1TeamUser2
          assertEventuallyCanFindByName brig phase1TeamUser1 phase1NonTeamUser

          -- searching phase2 users should work
          assertEventuallyCanFindByName brig phase1TeamUser1 phase2NonTeamUser
          assertEventuallyCanFindByName brig phase1TeamUser1 phase2TeamUser

          pure (phase2NonTeamUser, phase2TeamUser)

        withSettingsOverrides (optsWithIndex "new") $ do
          -- Before migration the phase1 users shouldn't be found in the new index
          assertEventuallyCan'tFindByName brig phase1TeamUser1 phase1TeamUser2
          assertEventuallyCan'tFindByName brig phase1TeamUser1 phase1NonTeamUser

          -- Before migration the phase2 users should be found in the new index
          assertEventuallyCanFindByName brig phase1TeamUser1 phase2NonTeamUser
          assertEventuallyCanFindByName brig phase1TeamUser1 phase2TeamUser

        -- Run Migrations
        liftIO $ migrateIndexCommand logger (optsWithIndex "oldAccessToBoth") newESIndex migrationIndexName

        -- Phase 3: Using old index for search, writing to both indices, migrations have run
        (phase3NonTeamUser, phase3TeamUser) <- withSettingsOverrides (optsWithIndex "both") $ do
          refreshIndex brig
          phase3NonTeamUser <- randomUser brig
          phase3TeamUser <- inviteAndRegisterUser teamOwner tid brig
          refreshIndex brig

          -- searching phase1/2 users should work
          assertEventuallyCanFindByName brig phase1TeamUser1 phase1TeamUser2
          assertEventuallyCanFindByName brig phase1TeamUser1 phase1NonTeamUser
          assertEventuallyCanFindByName brig phase1TeamUser1 phase2TeamUser
          assertEventuallyCanFindByName brig phase1TeamUser1 phase2NonTeamUser

          -- searching new phase3 should also work
          assertEventuallyCanFindByName brig phase1TeamUser1 phase3NonTeamUser
          assertEventuallyCanFindByName brig phase1TeamUser1 phase3TeamUser
          pure (phase3NonTeamUser, phase3TeamUser)

        -- Phase 4: Using only new index
        withSettingsOverrides (optsWithIndex "new") $ do
          refreshIndex brig
          -- Searching should work for phase1 users
          assertEventuallyCanFindByName brig phase1TeamUser1 phase1TeamUser2
          assertEventuallyCanFindByName brig phase1TeamUser1 phase1NonTeamUser

          -- Searching should work for phase2 users
          assertEventuallyCanFindByName brig phase1TeamUser1 phase2TeamUser
          assertEventuallyCanFindByName brig phase1TeamUser1 phase2NonTeamUser

          -- Searching should work for phase3 users
          assertEventuallyCanFindByName brig phase1TeamUser1 phase3NonTeamUser
          assertEventuallyCanFindByName brig phase1TeamUser1 phase3TeamUser

runReindexFromAnotherIndex :: Log.Logger -> Opt.Opts -> ES.IndexName -> ES.IndexName -> IO ()
runReindexFromAnotherIndex logger opts newIndexName migrationIndexName =
  let esOldOpts :: Opt.ElasticSearchOpts = opts ^. Opt.elasticsearchLens
      esOldConnectionSettings :: ESConnectionSettings = toESConnectionSettings esOldOpts migrationIndexName
      reindexSettings = ReindexFromAnotherIndexSettings esOldConnectionSettings newIndexName 5
   in runCommand logger $ ReindexFromAnotherIndex reindexSettings

runReindexFromDatabase ::
  (ElasticSettings -> CassandraSettings -> Endpoint -> Command) ->
  Log.Logger ->
  Opt.Opts ->
  ES.IndexName ->
  ES.IndexName ->
  IO ()
runReindexFromDatabase syncCommand logger opts newIndexName migrationIndexName =
  let esNewOpts :: Opt.ElasticSearchOpts = (opts ^. Opt.elasticsearchLens) & (Opt.indexLens .~ newIndexName)
      esNewConnectionSettings :: ESConnectionSettings = toESConnectionSettings esNewOpts migrationIndexName
      replicas = 2
      shards = 2
      refreshInterval = 5
      elasticSettings :: ElasticSettings =
        IndexOpts.localElasticSettings
          & IndexOpts.esConnection .~ esNewConnectionSettings
          & IndexOpts.esIndexReplicas .~ ES.ReplicaCount replicas
          & IndexOpts.esIndexShardCount .~ shards
          & IndexOpts.esIndexRefreshInterval .~ refreshInterval
      cassandraSettings :: CassandraSettings =
        ( localCassandraSettings
            & IndexOpts.cHost .~ (Text.unpack opts.cassandra.endpoint.host)
            & IndexOpts.cPort .~ (opts.cassandra.endpoint.port)
            & IndexOpts.cKeyspace .~ (C.Keyspace opts.cassandra.keyspace)
        )

      endpoint :: Endpoint = opts.galley
   in runCommand logger $ syncCommand elasticSettings cassandraSettings endpoint

toESConnectionSettings :: ElasticSearchOpts -> ES.IndexName -> ESConnectionSettings
toESConnectionSettings opts migrationIndexName = ESConnectionSettings {..}
  where
    toText (ES.Server url) = url
    esServer = (fromRight undefined . URI.parseURI URI.strictURIParserOptions . Text.encodeUtf8 . toText) opts.url
    esIndex = opts.index
    esCaCert = opts.caCert
    esInsecureSkipVerifyTls = opts.insecureSkipVerifyTls
    esCredentials = opts.credentials
    esMigrationIndexName = Just migrationIndexName

withESProxy ::
  (TestConstraints m, MonadUnliftIO m, HasCallStack) =>
  Log.Logger ->
  Opt.Opts ->
  ES.IndexName ->
  (ES.Server -> ES.IndexName -> m a) ->
  m a
withESProxy lg opts migrationIndexName f = do
  indexName <- ES.IndexName <$> randomHandle
  liftIO $ createEsIndexCommand lg opts indexName migrationIndexName
  withESProxyOnly [indexName] opts $ flip f indexName

createEsIndexCommand :: Log.Logger -> Opt.Opts -> ES.IndexName -> ES.IndexName -> IO ()
createEsIndexCommand logger opts newIndexName migrationIndexName =
  let esNewOpts = (opts ^. Opt.elasticsearchLens) & (Opt.indexLens .~ newIndexName)
      replicas = 2
      shards = 2
      refreshInterval = 5
      esSettings =
        IndexOpts.localElasticSettings
          & IndexOpts.esConnection .~ toESConnectionSettings esNewOpts migrationIndexName
          & IndexOpts.esIndexReplicas .~ ES.ReplicaCount replicas
          & IndexOpts.esIndexShardCount .~ shards
          & IndexOpts.esIndexRefreshInterval .~ refreshInterval
   in runCommand logger $ Create esSettings opts.galley

-- | Gives a URL to a HTTP proxy server to the continuation. The proxy is only
-- configured for ES calls for the given @indexNames@ (and some other ES
-- specific endpoints.)
withESProxyOnly :: (TestConstraints m, MonadUnliftIO m, HasCallStack) => [ES.IndexName] -> Opt.Opts -> (ES.Server -> m a) -> m a
withESProxyOnly indexNames opts f = do
  mgr <- liftIO $ initHttpManagerWithTLSConfig opts.elasticsearch.insecureSkipVerifyTls opts.elasticsearch.caCert
  (proxyPort, sock) <- liftIO Warp.openFreePort
  bracket
    (async $ liftIO $ Warp.runSettingsSocket Warp.defaultSettings sock $ indexProxyServer indexNames opts mgr)
    cancel
    (\_ -> f (ES.Server ("http://localhost:" <> Text.pack (show proxyPort))))

-- | Create a `Wai.Application` that acts as a proxy to ElasticSearch. Requests
-- are only forwarded for specified index names (and some technical endpoints.)
indexProxyServer :: [ES.IndexName] -> Opt.Opts -> Manager -> Wai.Application
indexProxyServer idxs opts mgr =
  let toUri (ES.Server url) = either (error . show) id $ URI.parseURI URI.strictURIParserOptions (Text.encodeUtf8 url)
      proxyURI = toUri (Opts.url (Opts.elasticsearch opts))
      proxyToHost = URI.hostBS . URI.authorityHost . fromMaybe (error "No Host") . URI.uriAuthority $ proxyURI
      proxyToPort = URI.portNumber . fromMaybe (URI.Port 9200) . URI.authorityPort . fromMaybe (error "No Host") . URI.uriAuthority $ proxyURI
      forwardRequest = Wai.WPRProxyDestSecure (Wai.ProxyDest proxyToHost proxyToPort)
      denyRequest req =
        Wai.WPRResponse
          ( Wai.responseLBS HTTP.status400 [] $
              "Refusing to proxy to path=" <> cs (Wai.rawPathInfo req) <> ". Proxy configured for indices: " <> cs (show idxs)
          )
      proxyApp req
        | (headMay (Wai.pathInfo req)) `elem` [Just "_reindex", Just "_tasks"] =
            forwardRequest
        | (any (\(ES.IndexName idx) -> (headMay (Wai.pathInfo req) == Just idx)) idxs) =
            forwardRequest
        | otherwise =
            denyRequest req
   in waiProxyTo (pure . proxyApp) Wai.defaultOnExc mgr

testWithBothIndices :: Opt.Opts -> Manager -> TestName -> WaiTest.Session a -> TestTree
testWithBothIndices opts mgr name f = do
  testGroup
    name
    [ test mgr "new-index" $ withSettingsOverrides opts f,
      test mgr "old-index" $ withOldIndex opts defaultMigrationIndexName f
    ]

testWithBothIndicesAndOpts :: Opt.Opts -> Manager -> TestName -> ((HasCallStack) => Opt.Opts -> Http ()) -> TestTree
testWithBothIndicesAndOpts opts mgr name f =
  testGroup
    name
    [ test mgr "new-index" (f opts),
      test mgr "old-index" $ do
        (newOpts, indexName) <- optsForOldIndex opts defaultMigrationIndexName
        f newOpts <* deleteIndex opts indexName
    ]

withOldIndex :: (MonadIO m, HasCallStack) => Opt.Opts -> ES.IndexName -> WaiTest.Session a -> m a
withOldIndex opts migrationIndexName f = do
  lg <- Log.create Log.StdOut
  indexName <- randomHandle
  createIndexWithMapping lg opts migrationIndexName indexName oldMapping
  let newOpts = opts & Opt.elasticsearchLens . Opt.indexLens .~ (ES.IndexName indexName)
  withSettingsOverrides newOpts f <* deleteIndex opts indexName

optsForOldIndex :: (MonadIO m, HasCallStack) => Opt.Opts -> ES.IndexName -> m (Opt.Opts, Text)
optsForOldIndex opts migrationIndexName = do
  lg <- Log.create Log.StdOut
  indexName <- randomHandle
  createIndexWithMapping lg opts migrationIndexName indexName oldMapping
  pure (opts & Opt.elasticsearchLens . Opt.indexLens .~ (ES.IndexName indexName), indexName)

createIndexWithMapping :: (MonadIO m, HasCallStack) => Log.Logger -> Opt.Opts -> ES.IndexName -> Text -> Value -> m ()
createIndexWithMapping lg opts migrationIndexName name val = do
  let indexName = ES.IndexName name
  liftIO $ createEsIndexCommand lg opts indexName migrationIndexName
  mappingReply <- runBH opts $ ES.putNamedMapping indexName mappingName val
  unless (ES.isCreated mappingReply || ES.isSuccess mappingReply) $ do
    liftIO $ assertFailure $ "failed to create mapping: " <> show name <> ", error: " <> show mappingReply

-- | This doesn't fail if ES returns error because we don't really want to fail the tests for this
deleteIndex :: (MonadIO m, HasCallStack) => Opt.Opts -> Text -> m ()
deleteIndex opts name = do
  let indexName = ES.IndexName name
  void $ runBH opts $ ES.deleteIndex indexName

runBH :: (MonadIO m, HasCallStack) => Opt.Opts -> ES.BH m a -> m a
runBH opts action = do
  let (ES.Server esURL) = opts ^. Opt.elasticsearchLens . Opt.urlLens
  mgr <- liftIO $ initHttpManagerWithTLSConfig opts.elasticsearch.insecureSkipVerifyTls opts.elasticsearch.caCert
  let bEnv = mkBHEnv esURL mgr
  ES.runBH bEnv action

--- | This was copied from at Brig.User.Search.Index.indexMapping at commit 75e6f6e
oldMapping :: Value
oldMapping =
  fromJust $
    decode
      [r|
{
  "dynamic": false,
  "properties": {
    "accent_id": {
      "index": false,
      "store": false,
      "type": "byte"
    },
    "account_status": {
      "index": true,
      "store": false,
      "type": "keyword"
    },
    "created_at": {
      "index": false,
      "store": false,
      "type": "date"
    },
    "email": {
      "fields": {
        "keyword": {
          "type": "keyword"
        },
        "prefix": {
          "analyzer": "prefix_index",
          "search_analyzer": "prefix_search",
          "type": "text"
        }
      },
      "index": true,
      "store": false,
      "type": "text"
    },
    "handle": {
      "fields": {
        "keyword": {
          "type": "keyword"
        },
        "prefix": {
          "analyzer": "prefix_index",
          "search_analyzer": "prefix_search",
          "type": "text"
        }
      },
      "index": true,
      "store": false,
      "type": "text"
    },
    "managed_by": {
      "index": true,
      "store": false,
      "type": "keyword"
    },
    "name": {
      "index": false,
      "store": false,
      "type": "keyword"
    },
    "normalized": {
      "fields": {
        "prefix": {
          "analyzer": "prefix_index",
          "search_analyzer": "prefix_search",
          "type": "text"
        }
      },
      "index": true,
      "store": false,
      "type": "text"
    },
    "role": {
      "index": true,
      "store": false,
      "type": "keyword"
    },
    "saml_idp": {
      "index": false,
      "store": false,
      "type": "keyword"
    },
    "scim_external_id": {
      "index": false,
      "store": false,
      "type": "keyword"
    },
    "search_visibility_inbound": {
      "index": true,
      "store": false,
      "type": "keyword"
    },
    "sso": {
      "properties": {
        "issuer": {
          "index": false,
          "store": false,
          "type": "keyword"
        },
        "nameid": {
          "index": false,
          "store": false,
          "type": "keyword"
        }
      },
      "type": "nested"
    },
    "team": {
      "index": true,
      "store": false,
      "type": "keyword"
    }
  }
}
|]
