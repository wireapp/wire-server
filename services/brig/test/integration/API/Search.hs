{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

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
import Control.Monad.Fail (MonadFail)
import Control.Retry
import Data.Aeson (FromJSON, Value, (.=))
import qualified Data.Aeson as Aeson
import Data.Handle (fromHandle)
import Data.Id
import Data.List (elemIndex)
import qualified Data.Text as Text
import qualified Database.Bloodhound as ES
import qualified Galley.Types.Teams.SearchVisibility as Team
import Imports
import Network.HTTP.Client (Manager)
import qualified Network.HTTP.Client as HTTP
import qualified Network.Wai.Test as WaiTest
import Test.Tasty
import Test.Tasty.HUnit
import UnliftIO (Concurrently (..), runConcurrently)
import Util
import Wire.API.Team.Feature (TeamFeatureStatusValue (..))

tests :: Opt.Opts -> Manager -> Galley -> Brig -> IO TestTree
tests opts mgr galley brig = do
  testSetupOutboundOnly <- runHttpT mgr prepareUsersForSearchVisibilityNoNameOutsideTeamTests
  return $
    testGroup "search" $
      [ testWithBothIndices opts mgr "by-name" $ testSearchByName brig,
        testWithBothIndices opts mgr "by-handle" $ testSearchByHandle brig,
        test mgr "reindex" $ testReindex brig,
        testWithBothIndices opts mgr "no match" $ testSearchNoMatch brig,
        testWithBothIndices opts mgr "no extra results" $ testSearchNoExtraResults brig,
        testWithBothIndices opts mgr "order-name (prefix match)" $ testOrderName brig,
        testWithBothIndices opts mgr "order-handle (prefix match)" $ testOrderHandle brig,
        testWithBothIndices opts mgr "by-first/middle/last name" $ testSearchByLastOrMiddleName brig,
        testWithBothIndices opts mgr "Non ascii names" $ testSearchNonAsciiNames brig,
        test mgr "migration to new index" $ testMigrationToNewIndex opts brig,
        testGroup "team-search-visibility disabled OR SearchVisibilityStandard" $
          [ testWithBothIndices opts mgr "team member cannot be found by non-team user" $ testSearchTeamMemberAsNonMember brig,
            testWithBothIndices opts mgr "team A member cannot be found by team B member" $ testSearchTeamMemberAsOtherMember brig,
            testWithBothIndices opts mgr "team A member can be found by other team A member" $ testSearchTeamMemberAsSameMember brig,
            testWithBothIndices opts mgr "non team user can be found by a team member" $ testSeachNonMemberAsTeamMember brig,
            testGroup "order" $
              [ test mgr "team-mates are listed before team-outsiders (exact match)" $ testSearchOrderingAsTeamMemberExactMatch brig,
                test mgr "team-mates are listed before team-outsiders (prefix match)" $ testSearchOrderingAsTeamMemberPrefixMatch brig,
                test mgr "team-mates are listed before team-outsiders (worse match)" $ testSearchOrderingAsTeamMemberWorseMatch brig
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
      uid2 = userId u2
  assertCanFind brig uid1 uid2 (fromName (userDisplayName u2))
  assertCanFind brig uid2 uid1 (fromName (userDisplayName u1))
  -- Users cannot find themselves
  assertCan'tFind brig uid1 uid1 (fromName (userDisplayName u1))
  assertCan'tFind brig uid2 uid2 (fromName (userDisplayName u2))

testSearchByLastOrMiddleName :: TestConstraints m => Brig -> m ()
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

testSearchNonAsciiNames :: TestConstraints m => Brig -> m ()
testSearchNonAsciiNames brig = do
  searcher <- userId <$> randomUser brig
  suffix <- randomHandle
  searched <- userId <$> createUser' True ("शक्तिमान" <> suffix) brig
  refreshIndex brig
  assertCanFind brig searcher searched ("शक्तिमान" <> suffix)
  -- This is pathetic transliteration, but it is what we have.
  assertCanFind brig searcher searched ("saktimana" <> suffix)

testSearchByHandle :: TestConstraints m => Brig -> m ()
testSearchByHandle brig = do
  u1 <- randomUserWithHandle brig
  u2 <- randomUser brig
  refreshIndex brig
  let uid1 = userId u1
      uid2 = userId u2
      Just h = fromHandle <$> userHandle u1
  assertCanFind brig uid2 uid1 h

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
      uid2 = userId u2
  refreshIndex brig
  resultUIds <- map contactUserId . searchResults <$> executeSearch brig uid1 (fromName $ userDisplayName u2)
  liftIO $
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
    (found : _) <- searchResults <$> executeSearch brig (userId u) h
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

testOrderName :: TestConstraints m => Brig -> m ()
testOrderName brig = do
  searcher <- userId <$> randomUser brig
  Name searchedWord <- randomNameWithMaxLen 122
  nameMatch <- userId <$> createUser' True searchedWord brig
  namePrefixMatch <- userId <$> createUser' True (searchedWord <> "suffix") brig
  refreshIndex brig
  resultUIds <- map contactUserId . searchResults <$> executeSearch brig searcher searchedWord
  let expectedOrder = [nameMatch, namePrefixMatch]
  liftIO $
    assertEqual
      "Expected order: name match, name prefix match"
      expectedOrder
      resultUIds

testOrderHandle :: TestConstraints m => Brig -> m ()
testOrderHandle brig = do
  searcher <- userId <$> randomUser brig
  searchedWord <- randomHandle
  handleMatch <- userId <$> createUser' True "handle match" brig
  void $ putHandle brig handleMatch searchedWord
  handlePrefixMatch <- userId <$> createUser' True "handle prefix match" brig
  void $ putHandle brig handlePrefixMatch (searchedWord <> "suffix")
  refreshIndex brig
  resultUIds <- map contactUserId . searchResults <$> executeSearch brig searcher searchedWord
  let expectedOrder = [handleMatch, handlePrefixMatch]
  liftIO $
    assertEqual
      "Expected order: handle match, handle prefix match"
      expectedOrder
      resultUIds

testSearchTeamMemberAsNonMember :: TestConstraints m => Brig -> m ()
testSearchTeamMemberAsNonMember brig = do
  nonTeamMember <- randomUser brig
  (_, _, [teamMember]) <- createPopulatedBindingTeamWithNamesAndHandles brig 1
  refreshIndex brig
  let teamMemberHandle = fromMaybe (error "teamBMember must have a handle") (userHandle teamMember)
  assertCan'tFind brig (userId nonTeamMember) (userId teamMember) (fromName (userDisplayName teamMember))
  assertCan'tFind brig (userId nonTeamMember) (userId teamMember) (fromHandle teamMemberHandle)

testSearchTeamMemberAsOtherMember :: TestConstraints m => Brig -> m ()
testSearchTeamMemberAsOtherMember brig = do
  (_, _, [teamAMember]) <- createPopulatedBindingTeamWithNamesAndHandles brig 1
  (_, _, [teamBMember]) <- createPopulatedBindingTeamWithNamesAndHandles brig 1
  refreshIndex brig
  assertCan'tFind brig (userId teamAMember) (userId teamBMember) (fromName (userDisplayName teamBMember))
  let teamBMemberHandle = fromMaybe (error "teamBMember must have a handle") (userHandle teamBMember)
  assertCan'tFind brig (userId teamAMember) (userId teamBMember) (fromHandle teamBMemberHandle)

testSearchTeamMemberAsSameMember :: TestConstraints m => Brig -> m ()
testSearchTeamMemberAsSameMember brig = do
  (_, _, [teamAMember, teamAMember']) <- createPopulatedBindingTeam brig 2
  refreshIndex brig
  assertCanFind brig (userId teamAMember) (userId teamAMember') (fromName (userDisplayName teamAMember'))

testSeachNonMemberAsTeamMember :: TestConstraints m => Brig -> m ()
testSeachNonMemberAsTeamMember brig = do
  nonTeamMember <- randomUser brig
  (_, _, [teamMember]) <- createPopulatedBindingTeam brig 1
  refreshIndex brig
  assertCanFind brig (userId teamMember) (userId nonTeamMember) (fromName (userDisplayName nonTeamMember))

testSearchOrderingAsTeamMemberExactMatch :: TestConstraints m => Brig -> m ()
testSearchOrderingAsTeamMemberExactMatch brig = do
  searchedName <- randomName
  mapM_ (\(_ :: Int) -> createUser' True (fromName searchedName) brig) [0 .. 99]
  (_, _, [searcher, teamSearchee]) <- createPopulatedBindingTeamWithNames brig [Name "Searcher", searchedName]
  refreshIndex brig
  result <- executeSearch brig (userId searcher) (fromName searchedName)
  let resultUserIds = contactUserId <$> searchResults result
  liftIO $
    case elemIndex (userId teamSearchee) resultUserIds of
      Nothing -> assertFailure "team mate not found in search"
      Just teamSearcheeIndex -> assertEqual "teammate is not the first result" 0 teamSearcheeIndex

testSearchOrderingAsTeamMemberPrefixMatch :: TestConstraints m => Brig -> m ()
testSearchOrderingAsTeamMemberPrefixMatch brig = do
  searchedName <- randomNameWithMaxLen 122 -- 6 characters for "suffix"
  mapM_ (\(i :: Int) -> createUser' True (fromName searchedName <> Text.pack (show i)) brig) [0 .. 99]
  (_, _, [searcher, teamSearchee]) <- createPopulatedBindingTeamWithNames brig [Name "Searcher", Name $ fromName searchedName <> "suffix"]
  refreshIndex brig
  result <- executeSearch brig (userId searcher) (fromName searchedName)
  let resultUserIds = contactUserId <$> searchResults result
  liftIO $
    case elemIndex (userId teamSearchee) resultUserIds of
      Nothing -> assertFailure "team mate not found in search"
      Just teamSearcheeIndex -> assertEqual "teammate is not the first result" 0 teamSearcheeIndex

testSearchOrderingAsTeamMemberWorseMatch :: TestConstraints m => Brig -> m ()
testSearchOrderingAsTeamMemberWorseMatch brig = do
  searchedName <- randomHandle
  user <- createUser' True searchedName brig
  void $ putHandle brig (userId user) searchedName
  (_, _, [searcher, teamSearchee]) <- createPopulatedBindingTeamWithNames brig [Name "Searcher", Name (searchedName <> "Suffix")]
  refreshIndex brig
  result <- executeSearch brig (userId searcher) searchedName
  let resultUserIds = contactUserId <$> searchResults result
  liftIO $
    case elemIndex (userId teamSearchee) resultUserIds of
      Nothing -> assertFailure "team mate not found in search"
      Just teamSearcheeIndex -> assertEqual "teammate is not the first result" 0 teamSearcheeIndex

testSearchSameTeamOnly :: TestConstraints m => Brig -> Opt.Opts -> m ()
testSearchSameTeamOnly brig opts = do
  nonTeamMember <- randomUser brig
  (_, _, [teamMember]) <- createPopulatedBindingTeam brig 1
  refreshIndex brig
  let newOpts = opts & Opt.optionSettings . Opt.searchSameTeamOnly .~ Just True
  withSettingsOverrides newOpts $
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

assertRight :: (MonadIO m, Show a, HasCallStack) => Either a b -> m b
assertRight = \case
  Left e -> liftIO $ assertFailure $ "Expected Right, got Left: " <> show e
  Right x -> pure x

testWithBothIndices :: Opt.Opts -> Manager -> TestName -> WaiTest.Session a -> TestTree
testWithBothIndices opts mgr name f =
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
  createReply <- runBH opts $ ES.createIndexWith [] 1 indexName
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

-- | This was copied from staging before the migration
oldMapping :: Value
oldMapping =
  Aeson.object
    [ "user"
        .= Aeson.object
          [ "properties"
              .= Aeson.object
                [ "accent_id" .= Aeson.object ["type" .= ("byte" :: Text), "index" .= False],
                  "handle" .= Aeson.object ["type" .= ("text" :: Text)],
                  "name" .= Aeson.object ["type" .= ("keyword" :: Text), "index" .= False],
                  "normalized" .= Aeson.object ["type" .= ("text" :: Text)],
                  "team" .= Aeson.object ["type" .= ("keyword" :: Text)]
                ]
          ]
    ]
