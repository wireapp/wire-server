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
import Data.Json.Util (fromUTCTimeMillis)
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
import Polysemy.TinyLog (TinyLog)
import Test.Hspec
import URI.ByteString (URI, serializeURIRef')
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
import Wire.Sem.Logger.TinyLog (discardTinyLogs)
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
     TinyLog,
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
testConfig =
  BackupConfig
    { maxFullBytes = 100,
      maxIncrementalBytes = 10,
      maxFilesPerEpoch = 3,
      uploadTimeout = 60
    }

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
      . discardTinyLogs
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
      | fx.inTeam =
          Map.singleton
            teamId
            [ mkTeamMember u fullPermissions Nothing UserLegalHoldDisabled
            | u <- [testUser, otherUser]
            ]
      | otherwise = mempty
    features
      | fx.featureEnabled =
          npUpdate @AutomaticBackupsConfig (LockableFeature FeatureStatusEnabled LockStatusUnlocked def) def
      | otherwise = def
    brigStub = interpret $ \case
      LookupClients uids -> pure (UserClients (Map.fromList [(u, fx.existingClients) | u <- uids]))
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

otherUser :: UserId
otherUser = Id (fromJust (UUID.fromString "22222222-2222-4222-8222-222222222222"))

-- | Download as the given user; the fixture gives every user the same clients and feature.
download ::
  (Upload r) =>
  UserId ->
  ClientId ->
  Word32 ->
  BackupFileId ->
  Sem r (Either BackupError URI)
download uid cid epoch fid =
  try (downloadFull (toLocalUnsafe (Domain "example.com") uid) clientA Nothing cid (BackupEpoch epoch) fid)

uploadInc ::
  (Upload r) =>
  ClientId ->
  ClientId ->
  Word32 ->
  BackupFileId ->
  ByteString ->
  Sem r (Either BackupError BackupFile)
uploadInc zClient cid epoch full bytes =
  try
    ( uploadIncremental
        caller
        zClient
        cid
        (BackupEpoch epoch)
        full
        (fromIntegral (BS.length bytes))
        (body bytes)
    )

-- | Upload an incremental of epoch 1 under the given full as client A.
uploadIncA :: (Upload r) => BackupFileId -> ByteString -> Sem r (Either BackupError BackupFile)
uploadIncA = uploadInc clientA clientA 1

-- | Fails the test on 'Left'.
orFail :: (Show e) => Either e a -> Sem r a
orFail = either (error . ("unexpected error: " <>) . show) pure

downloadInc ::
  (Upload r) =>
  UserId ->
  Word32 ->
  BackupFileId ->
  BackupFileId ->
  Sem r (Either BackupError URI)
downloadInc uid epoch full inc =
  try
    ( downloadIncremental
        (toLocalUnsafe (Domain "example.com") uid)
        clientA
        Nothing
        clientA
        (BackupEpoch epoch)
        full
        inc
    )

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

  it "downloadFullReturnsPresignedUrl" $ do
    ((up, dl), _, _) <- runBackup defaultFixture PutSucceeds $ do
      up <- uploadA 1
      dl <- either (pure . Left) (download testUser clientA 1 . (.id)) up
      pure (up, dl)
    file <- either (fail . show) pure up
    url <- either (fail . show) pure dl
    let BackupFileId fileUuid = file.id
    BS.unpack (serializeURIRef' url) `shouldSatisfy` isSuffixOf (UUID.toString fileUuid)

  it "downloadReplacedFullNotFound" $ do
    ((old, dl), _, _) <- runBackup defaultFixture PutSucceeds $ do
      old <- uploadA 1
      _ <- uploadA 1
      dl <- either (pure . Left) (download testUser clientA 1 . (.id)) old
      pure (old, dl)
    old `shouldSatisfy` isRight
    dl `shouldBe` Left BackupErrorNotFound

  it "downloadOtherUserNotFound" $ do
    ((up, dl), _, _) <- runBackup defaultFixture PutSucceeds $ do
      up <- uploadA 1
      dl <- either (pure . Left) (download otherUser clientA 1 . (.id)) up
      pure (up, dl)
    up `shouldSatisfy` isRight
    dl `shouldBe` Left BackupErrorNotFound

  describe "incrementals" $ do
    it "uploadIncrementalListed" $ do
      ((files, i1, i2), _, objects) <- runBackup defaultFixture PutSucceeds $ do
        full <- uploadA 1 >>= orFail
        passTime 1
        i1 <- uploadIncA full.id "abc" >>= orFail
        passTime 1
        i2 <- uploadIncA full.id "defgh" >>= orFail
        files <- readEpoch clientA 1 >>= orFail
        pure (files, i1, i2)
      map (.id) files.incrementals `shouldBe` [i1.id, i2.id]
      map (.size) files.incrementals `shouldBe` [3, 5]
      Map.lookup i1.id objects `shouldBe` Just "abc"
      Map.lookup i2.id objects `shouldBe` Just "defgh"

    it "incrementalToReplacedFull" $ do
      ((r, orphansBefore, orphansAfter), state, _) <- runBackup defaultFixture PutSucceeds $ do
        a <- uploadA 1 >>= orFail
        _ <- uploadA 1
        pre <- gets @BackupState (.orphans)
        r <- uploadIncA a.id "abc"
        post <- gets @BackupState (.orphans)
        pure (r, pre, post)
      r `shouldBe` Left BackupErrorFullMissing
      -- pre-check: no reservation was made
      orphansAfter `shouldBe` orphansBefore
      Map.elems state.orphans `shouldNotContain` [Nothing]

    it "incrementalToStaleEpochFull" $ do
      (r, _, _) <- runBackup defaultFixture PutSucceeds $ do
        e1 <- uploadA 1 >>= orFail
        _ <- uploadA 2
        _ <- uploadA 3
        uploadIncA e1.id "abc"
      r `shouldBe` Left BackupErrorFullMissing

    it "incrementalUnknownFull" $ do
      (r, _, _) <- runBackup defaultFixture PutSucceeds $ do
        _ <- uploadA 1
        uploadIncA (BackupFileId (UUID.fromWords 1 2 3 4)) "abc"
      r `shouldBe` Left BackupErrorFullMissing

    it "commitIncrementalAfterReplacement" $ do
      (r, state, _) <- runBackup defaultFixture PutSucceeds $ do
        a <- uploadA 1 >>= orFail
        let inc = BackupFileId (UUID.fromWords 5 6 7 8)
            b = BackupFileId (UUID.fromWords 9 10 11 12)
        reserveUpload testUser clientA (BackupEpoch 1) inc
        reserveUpload testUser clientA (BackupEpoch 1) b
        _ <- commitFull testUser clientA (BackupEpoch 1) b 5
        r <- commitIncremental 3 testUser clientA (BackupEpoch 1) a.id inc 3
        pure (r, inc)
      fst r `shouldBe` Left CommitFullMissing
      Map.lookup (snd r) state.orphans `shouldBe` Just Nothing
      state.incrementals `shouldBe` mempty

    it "maxFilesPerEpochBoundary" $ do
      ((third, storeLevel), state, _) <- runBackup defaultFixture PutSucceeds $ do
        full <- uploadA 1 >>= orFail
        passTime 1
        _ <- uploadIncA full.id "a" >>= orFail
        passTime 1
        _ <- uploadIncA full.id "b" >>= orFail
        third <- uploadIncA full.id "c"
        let inc = BackupFileId (UUID.fromWords 5 6 7 8)
        reserveUpload testUser clientA (BackupEpoch 1) inc
        storeLevel <- commitIncremental 3 testUser clientA (BackupEpoch 1) full.id inc 1
        pure (third, (storeLevel, inc))
      -- full + 2 incrementals are within the limit of 3
      third `shouldBe` Left BackupErrorEpochFull
      fst storeLevel `shouldBe` Left CommitEpochFull
      Map.lookup (snd storeLevel) state.orphans `shouldBe` Just Nothing
      Map.size state.incrementals `shouldBe` 2

    it "incrementalTooLarge" $ do
      ((large, ok, empty), _, _) <- runBackup defaultFixture PutSucceeds $ do
        full <- uploadA 1 >>= orFail
        large <- uploadIncA full.id (BS.replicate 11 'x')
        ok <- uploadIncA full.id (BS.replicate 10 'x')
        empty <- uploadIncA full.id ""
        pure (large, ok, empty)
      large `shouldBe` Left BackupErrorTooLarge
      ok `shouldSatisfy` isRight
      empty `shouldBe` Left BackupErrorInvalidLength

    it "incrementalForbiddenOtherClient" $ do
      (r, _, _) <- runBackup defaultFixture PutSucceeds $ do
        full <- uploadA 1 >>= orFail
        uploadInc clientA clientB 1 full.id "abc"
      r `shouldBe` Left BackupErrorForbidden

    it "replaceFullOrphansIncrementals" $ do
      ((a, i1, i2, files), state, _) <- runBackup defaultFixture PutSucceeds $ do
        a <- uploadA 1 >>= orFail
        passTime 1
        i1 <- uploadIncA a.id "abc" >>= orFail
        passTime 1
        i2 <- uploadIncA a.id "def" >>= orFail
        passTime 1
        _ <- uploadA 1
        files <- readEpoch clientA 1 >>= orFail
        pure (a, i1, i2, files)
      files.incrementals `shouldBe` []
      state.incrementals `shouldBe` mempty
      for_ [a, i1, i2] $ \f ->
        Map.lookup f.id state.orphans `shouldBe` Just (Just (fromUTCTimeMillis f.createdAt))

    it "staleEpochOrphansIncrementals" $ do
      ((e1, i1), state, _) <- runBackup defaultFixture PutSucceeds $ do
        e1 <- uploadA 1 >>= orFail
        passTime 1
        i1 <- uploadIncA e1.id "abc" >>= orFail
        _ <- uploadA 2
        _ <- uploadA 3
        pure (e1, i1)
      state.incrementals `shouldBe` mempty
      for_ [e1, i1] $ \f ->
        Map.lookup f.id state.orphans `shouldBe` Just (Just (fromUTCTimeMillis f.createdAt))

    it "downloadIncremental" $ do
      ((inc, ok, wrongFull, otherUser'), _, _) <- runBackup defaultFixture PutSucceeds $ do
        e1 <- uploadA 1 >>= orFail
        inc <- uploadIncA e1.id "abc" >>= orFail
        e2 <- uploadA 2 >>= orFail
        ok <- downloadInc testUser 1 e1.id inc.id
        wrongFull <- downloadInc testUser 1 e2.id inc.id
        otherUser' <- downloadInc otherUser 1 e1.id inc.id
        pure (inc, ok, wrongFull, otherUser')
      url <- either (fail . show) pure ok
      let BackupFileId incUuid = inc.id
      BS.unpack (serializeURIRef' url) `shouldSatisfy` isSuffixOf (UUID.toString incUuid)
      wrongFull `shouldBe` Left BackupErrorNotFound
      otherUser' `shouldBe` Left BackupErrorNotFound
