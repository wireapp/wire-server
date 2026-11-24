-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2026 Wire Swiss GmbH <opensource@wire.com>
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
{-# LANGUAGE OverloadedRecordDot #-}

module Wire.BackupSubsystem.InterpreterSpec (spec) where

import Conduit (ConduitT, ResourceT, yield)
import Data.ByteString.Char8 qualified as BS
import Data.Default (def)
import Data.Domain (Domain (..))
import Data.Id
import Data.LegalHold (UserLegalHoldStatus (..))
import Data.Map qualified as Map
import Data.Qualified
import Data.Set qualified as Set
import Data.Time.Clock (UTCTime)
import Data.UUID qualified as UUID
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.State
import Test.Hspec
import Wire.API.Backup
import Wire.API.Team.Feature
import Wire.API.Team.Member (mkTeamMember)
import Wire.API.Team.Permission (fullPermissions)
import Wire.API.User.Client (UserClients (..))
import Wire.BackupObjectStore
import Wire.BackupStore
import Wire.BackupSubsystem
import Wire.BackupSubsystem.Interpreter
import Wire.BrigAPIAccess (BrigAPIAccess (..))
import Wire.GalleyAPIAccess (GalleyAPIAccess)
import Wire.MockInterpreters
import Wire.Sem.Now (Now)
import Wire.Sem.Random (Random)

type TestStack =
  '[ BackupSubsystem,
     BackupStore,
     BackupObjectStore,
     BrigAPIAccess,
     GalleyAPIAccess,
     Input BackupConfig,
     Random,
     Now,
     Error BackupError,
     State BackupState,
     State (Map BackupFileId ByteString),
     State UTCTime,
     Embed IO
   ]

data Fixture = Fixture
  { inTeam :: Bool,
    featureEnabled :: Bool,
    -- | clients that exist in brig
    existingClients :: Set ClientId
  }

defaultFixture :: Fixture
defaultFixture =
  Fixture
    { inTeam = True,
      featureEnabled = True,
      existingClients = Set.fromList [clientA, clientB]
    }

testUser :: UserId
testUser = Id (fromJust (UUID.fromString "11111111-1111-4111-8111-111111111111"))

teamId :: TeamId
teamId = Id (fromJust (UUID.fromString "33333333-3333-4333-8333-333333333333"))

clientA, clientB :: ClientId
clientA = ClientId 0xa
clientB = ClientId 0xb

caller :: Local UserId
caller = toLocalUnsafe (Domain "example.com") testUser

testConfig :: BackupConfig
testConfig = BackupConfig {maxFullBytes = 100, uploadTimeout = 60}

-- | Runs the action; an error that escapes it fails the test.
runBackup ::
  Fixture ->
  PutBehaviour ->
  Sem TestStack a ->
  IO (a, BackupState, Map BackupFileId ByteString)
runBackup fx behaviour action = do
  (_, (objects, (state, result))) <-
    runM
      . runState defaultTime
      . runState Map.empty
      . runState emptyBackupState
      . fmap (either (error . ("unexpected error: " <>) . show) Imports.id)
      . runError @BackupError
      . interpretNowAsState
      . runRandomPure
      . runInputConst testConfig
      . miniGalleyAPIAccess teams features
      . brigStub
      . inMemoryBackupObjectStore behaviour
      . inMemoryBackupStore
      . interpretBackupSubsystem
      $ action
  pure (result, state, objects)
  where
    teams
      | fx.inTeam = Map.singleton teamId [mkTeamMember testUser fullPermissions Nothing UserLegalHoldDisabled]
      | otherwise = mempty
    features
      | fx.featureEnabled =
          npUpdate @AutomaticBackupsConfig (LockableFeature FeatureStatusEnabled LockStatusUnlocked def) def
      | otherwise = def
    brigStub = interpret $ \case
      LookupClients _ -> pure (UserClients (Map.singleton testUser fx.existingClients))
      _ -> error "unexpected BrigAPIAccess call"

body :: ByteString -> ConduitT () ByteString (ResourceT IO) ()
body = yield

type Upload r = (Member BackupSubsystem r, Member (Error BackupError) r)

upload :: (Upload r) => ClientId -> ClientId -> Word32 -> ByteString -> Sem r (Either BackupError BackupFile)
upload zClient cid epoch bytes =
  try (uploadFull caller zClient cid (BackupEpoch epoch) (fromIntegral (BS.length bytes)) (body bytes))

uploadA :: (Upload r) => Word32 -> Sem r (Either BackupError BackupFile)
uploadA epoch = upload clientA clientA epoch "hello"

readEpoch :: (Upload r) => ClientId -> Word32 -> Sem r (Either BackupError BackupFiles)
readEpoch zClient epoch = try (getEpochFiles caller zClient clientA (BackupEpoch epoch))

spec :: Spec
spec = describe "BackupSubsystem" $ do
  it "uploadFullStoresEpoch" $ do
    ((up, files), state, objects) <- runBackup defaultFixture PutSucceeds ((,) <$> uploadA 1 <*> readEpoch clientA 1)
    file <- either (fail . show) pure up
    file.size `shouldBe` 5
    fmap (.full.id) files `shouldBe` Right file.id
    fmap (.incrementals) files `shouldBe` Right []
    Map.lookup file.id objects `shouldBe` Just "hello"
    state.orphans `shouldBe` mempty

  it "forbiddenForOtherClient" $ do
    (r, state, _) <- runBackup defaultFixture PutSucceeds (upload clientA clientB 1 "hello")
    r `shouldBe` Left BackupErrorForbidden
    state.orphans `shouldBe` mempty

  it "forbiddenForRemovedClient" $ do
    (r, _, _) <- runBackup defaultFixture {existingClients = Set.singleton clientB} PutSucceeds (uploadA 1)
    r `shouldBe` Left BackupErrorForbidden

  it "disabledWithoutTeam" $ do
    (r, _, _) <- runBackup defaultFixture {inTeam = False} PutSucceeds (uploadA 1)
    r `shouldBe` Left BackupErrorDisabled

  it "disabledWhenFeatureOff" $ do
    (r, _, _) <- runBackup defaultFixture {featureEnabled = False} PutSucceeds (uploadA 1)
    r `shouldBe` Left BackupErrorDisabled

  it "tooLarge" $ do
    ((large, empty), _, _) <-
      runBackup defaultFixture PutSucceeds $
        (,) <$> upload clientA clientA 1 (BS.replicate 101 'x') <*> upload clientA clientA 1 ""
    large `shouldBe` Left BackupErrorTooLarge
    empty `shouldBe` Left BackupErrorInvalidLength

  it "epochRules" $ do
    ((r2, r1, r1', r2', orphansAfterReplace, r3, e1, e2, r5), state, _) <-
      runBackup defaultFixture PutSucceeds $ do
        r2 <- uploadA 2
        r1 <- uploadA 1
        r1' <- uploadA 1
        orphansAfterReplace <- gets @BackupState (.orphans)
        r2' <- uploadA 2
        r3 <- uploadA 3
        e1 <- readEpoch clientA 1
        e2 <- readEpoch clientA 2
        r5 <- uploadA 5
        pure (r2, r1, r1', r2', orphansAfterReplace, r3, e1, e2, r5)
    r2 `shouldBe` Left BackupErrorEpochInvalid
    for_ [r1, r1', r2', r3] $ \r -> r `shouldSatisfy` isRight
    -- re-uploading epoch 1 orphaned the first full, keeping its creation time
    fmap (\f -> Map.lookup f.id orphansAfterReplace) r1 `shouldBe` Right (Just (Just defaultTime))
    -- epoch 3 removed epoch 1 and orphaned its (replacing) full
    e1 `shouldBe` Left BackupErrorNotFound
    fmap (\f -> Map.member f.id state.orphans) r1' `shouldBe` Right True
    e2 `shouldSatisfy` isRight
    r5 `shouldBe` Left BackupErrorEpochInvalid

  it "putFailureKeepsReservation" $ do
    (r, state, _) <- runBackup defaultFixture (PutFailsWith PutIncompleteBody) (uploadA 1)
    r `shouldBe` Left BackupErrorIncompleteBody
    Map.elems state.orphans `shouldBe` [Nothing]
    state.epochs `shouldBe` mempty

  it "deadlineExceededDoesNotCommit" $ do
    (r, state, _) <- runBackup defaultFixture (PutTakes (testConfig.uploadTimeout + 1)) (uploadA 1)
    r `shouldBe` Left BackupErrorUploadFailed
    state.epochs `shouldBe` mempty
    Map.elems state.orphans `shouldBe` [Nothing]

  it "readOtherClientOfSameUser" $ do
    (r, _, _) <- runBackup defaultFixture PutSucceeds (uploadA 1 *> readEpoch clientB 1)
    r `shouldSatisfy` isRight
