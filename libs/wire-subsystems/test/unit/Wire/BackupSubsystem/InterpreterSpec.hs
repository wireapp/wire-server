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
import Data.Aeson (eitherDecode)
import Data.ByteString.Char8 qualified as BS
import Data.Default (def)
import Data.Domain (Domain (..))
import Data.Id
import Data.Json.Util (fromUTCTimeMillis)
import Data.LegalHold (UserLegalHoldStatus (..))
import Data.Map qualified as Map
import Data.Qualified
import Data.Set qualified as Set
import Data.Time.Clock (UTCTime, addUTCTime)
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
      uploadTimeout = 60,
      orphanGracePeriod = 120,
      cleanupBatchSize = 100
    }

-- | Runs the action; an error that escapes it fails the test.
runBackup ::
  Fixture ->
  PutBehaviour ->
  Sem TestStack a ->
  IO (a, BackupState, Map BackupFileId ByteString)
runBackup fx behaviour = runBackupWith fx behaviour DeleteSucceeds

runBackupWith ::
  Fixture ->
  PutBehaviour ->
  DeleteBehaviour ->
  Sem TestStack a ->
  IO (a, BackupState, Map BackupFileId ByteString)
runBackupWith fx behaviour deleteBehaviour =
  runBackupFrom testConfig fx behaviour deleteBehaviour (emptyBackupState, Map.empty)

-- | Starts from the given store and object store.
runBackupFrom ::
  BackupConfig ->
  Fixture ->
  PutBehaviour ->
  DeleteBehaviour ->
  (BackupState, Map BackupFileId ByteString) ->
  Sem TestStack a ->
  IO (a, BackupState, Map BackupFileId ByteString)
runBackupFrom config fx behaviour deleteBehaviour (initialState, initialObjects) action = do
  (_, (objects, (state, result))) <-
    runM
      . runState defaultTime
      . runState initialObjects
      . runState initialState
      . fmap (either (error . ("unexpected error: " <>) . show) Imports.id)
      . runError @BackupError
      . interpretNowAsState
      . discardTinyLogs
      . runRandomPure
      . runInputConst config
      . miniGalleyAPIAccess teams features
      . brigStub
      . inMemoryBackupObjectStore behaviour deleteBehaviour
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

-- | A file id for store-level tests.
fileN :: Word32 -> BackupFileId
fileN n = BackupFileId (UUID.fromWords 0 0 0 n)

-- | Commits a full of client A at store level (no object): epoch, id. Returns the file and the
-- ids orphaned by the commit.
storeFull :: (Member BackupStore r) => Word32 -> Word32 -> Sem r (BackupFile, [BackupFileId])
storeFull epoch n = do
  reserveUpload testUser clientA (BackupEpoch epoch) (fileN n)
  commitFull testUser clientA (BackupEpoch epoch) (fileN n) 5 >>= orFail

-- | Commits an incremental of client A at store level (no object): epoch, full, id.
storeInc :: (Member BackupStore r) => Word32 -> BackupFileId -> Word32 -> Sem r BackupFile
storeInc epoch full n = do
  reserveUpload testUser clientA (BackupEpoch epoch) (fileN n)
  commitIncremental 10 testUser clientA (BackupEpoch epoch) full (fileN n) 3 >>= orFail

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
    ((r2, r1, r1', r2', r3, e1, e2, r5), state, objects) <-
      runBackup defaultFixture PutSucceeds $ do
        r2 <- uploadA 2
        r1 <- uploadA 1
        r1' <- uploadA 1
        r2' <- uploadA 2
        r3 <- uploadA 3
        e1 <- readEpoch clientA 1
        e2 <- readEpoch clientA 2
        r5 <- uploadA 5
        pure (r2, r1, r1', r2', r3, e1, e2, r5)
    r2 `shouldBe` Left BackupErrorEpochInvalid
    for_ [r1, r1', r2', r3] $ \r -> r `shouldSatisfy` isRight
    -- the replaced full and the full of the removed epoch 1 are deleted right away
    e1 `shouldBe` Left BackupErrorNotFound
    for_ [r1, r1'] $ \r -> fmap (\f -> Map.member f.id objects) r `shouldBe` Right False
    state.orphans `shouldBe` mempty
    e2 `shouldSatisfy` isRight
    r5 `shouldBe` Left BackupErrorEpochInvalid

  it "putFailureKeepsReservation" $ do
    (r, state, _) <- runBackup defaultFixture (PutFailsWith PutIncompleteBody) (uploadA 1)
    r `shouldBe` Left BackupErrorIncompleteBody
    map (.fileCreatedAt) (Map.elems state.orphans) `shouldBe` [Nothing]
    state.epochs `shouldBe` mempty

  it "deadlineExceededDoesNotCommit" $ do
    (r, state, _) <- runBackup defaultFixture (PutTakes (testConfig.uploadTimeout + 1)) (uploadA 1)
    r `shouldBe` Left BackupErrorUploadFailed
    state.epochs `shouldBe` mempty
    map (.fileCreatedAt) (Map.elems state.orphans) `shouldBe` [Nothing]

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
      map (.fileCreatedAt) (Map.elems state.orphans) `shouldNotContain` [Nothing]

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
      (.fileCreatedAt) <$> Map.lookup (snd r) state.orphans `shouldBe` Just Nothing
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
      (.fileCreatedAt) <$> Map.lookup (snd storeLevel) state.orphans `shouldBe` Just Nothing
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
      ((a, i1, i2, orphaned, files), state, _) <- runBackup defaultFixture PutSucceeds $ do
        (a, _) <- storeFull 1 1
        passTime 1
        i1 <- storeInc 1 a.id 2
        passTime 1
        i2 <- storeInc 1 a.id 3
        passTime 1
        (_, orphaned) <- storeFull 1 4
        files <- lookupEpochFiles testUser clientA (BackupEpoch 1)
        pure (a, i1, i2, orphaned, files)
      fmap (.incrementals) files `shouldBe` Just []
      state.incrementals `shouldBe` mempty
      sort orphaned `shouldBe` sort [a.id, i1.id, i2.id]
      for_ [a, i1, i2] $ \f ->
        (.fileCreatedAt) <$> Map.lookup f.id state.orphans
          `shouldBe` Just (Just (fromUTCTimeMillis f.createdAt))

    it "staleEpochOrphansIncrementals" $ do
      ((e1, i1, orphaned), state, _) <- runBackup defaultFixture PutSucceeds $ do
        (e1, _) <- storeFull 1 1
        passTime 1
        i1 <- storeInc 1 e1.id 2
        _ <- storeFull 2 3
        (_, orphaned) <- storeFull 3 4
        pure (e1, i1, orphaned)
      state.incrementals `shouldBe` mempty
      sort orphaned `shouldBe` sort [e1.id, i1.id]
      for_ [e1, i1] $ \f ->
        (.fileCreatedAt) <$> Map.lookup f.id state.orphans
          `shouldBe` Just (Just (fromUTCTimeMillis f.createdAt))

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

  describe "listings" $ do
    it "listAggregates" $ do
      (r, _, _) <- runBackup defaultFixture PutSucceeds $ do
        full1 <- uploadA 1 >>= orFail
        passTime 1
        _ <- uploadIncA full1.id "abc" >>= orFail
        passTime 1
        inc2 <- uploadIncA full1.id "defg" >>= orFail
        passTime 1
        full2 <- upload clientA clientA 2 "sixsix" >>= orFail
        l <- listAs caller
        pure (l, inc2, full2)
      let (l, inc2, full2) = r
      fmap (.clients) l
        `shouldBe` Right
          [ ClientBackups
              { client = clientA,
                epochs =
                  [ EpochSummary
                      { epoch = BackupEpoch 1,
                        createdAt = inc2.createdAt,
                        fileCount = 3,
                        size = 12
                      },
                    EpochSummary
                      { epoch = BackupEpoch 2,
                        createdAt = full2.createdAt,
                        fileCount = 1,
                        size = 6
                      }
                  ]
              }
          ]

    it "listMultipleClients" $ do
      ((l, one), _, _) <- runBackup defaultFixture PutSucceeds $ do
        _ <- upload clientB clientB 1 "bbb" >>= orFail
        _ <- uploadA 1 >>= orFail
        l <- listAs caller
        one <- try (getClientBackups caller clientA clientA)
        pure (l, one)
      fmap (map (.client) . (.clients)) l `shouldBe` Right [clientA, clientB]
      fmap (.clients) l `shouldSatisfy` either (const False) ((== 2) . length)
      let entryA = either (error . show) (take 1 . (.clients)) l
      fmap (: []) one `shouldBe` Right entryA

    it "listNeverShowsOtherUsers" $ do
      ((l, one), _, _) <- runBackup defaultFixture PutSucceeds $ do
        let other = toLocalUnsafe (Domain "example.com") otherUser
        _ <- try @BackupError (uploadFull other clientB clientB (BackupEpoch 1) 3 (body "abc")) >>= orFail
        l <- listAs caller
        one <- try (getClientBackups caller clientA clientB)
        pure (l, one)
      l `shouldBe` Right (BackupList [])
      one `shouldBe` Right (ClientBackups clientB [])

    it "getClientBackupsEmpty" $ do
      (r, _, _) <-
        runBackup defaultFixture PutSucceeds $
          try (getClientBackups caller clientA clientA)
      r `shouldBe` Right (ClientBackups clientA [])

    it "listExcludesOrphans" $ do
      (r, _, _) <- runBackup defaultFixture PutSucceeds $ do
        full <- uploadA 1 >>= orFail
        passTime 1
        _ <- uploadIncA full.id "abc" >>= orFail
        passTime 1
        full' <- upload clientA clientA 1 "sevenby" >>= orFail
        reserveUpload testUser clientA (BackupEpoch 1) (BackupFileId (UUID.fromWords 9 9 9 9))
        l <- listAs caller
        pure (l, full')
      let (l, full') = r
      fmap (.clients) l
        `shouldBe` Right
          [ ClientBackups
              clientA
              [EpochSummary {epoch = BackupEpoch 1, createdAt = full'.createdAt, fileCount = 1, size = 7}]
          ]

    it "listChecks" $ do
      let both = do
            a <- listAs caller
            b <- try (getClientBackups caller clientA clientA)
            pure (a, b)
      (gone, _, _) <-
        runBackup defaultFixture {existingClients = Set.singleton clientB} PutSucceeds both
      fst gone `shouldBe` Left BackupErrorForbidden
      snd gone `shouldBe` Left BackupErrorForbidden
      (off, _, _) <- runBackup defaultFixture {featureEnabled = False} PutSucceeds both
      fst off `shouldBe` Left BackupErrorDisabled
      snd off `shouldBe` Left BackupErrorDisabled
      (noTeam, _, _) <- runBackup defaultFixture {inTeam = False} PutSucceeds both
      fst noTeam `shouldBe` Left BackupErrorDisabled
      snd noTeam `shouldBe` Left BackupErrorDisabled

  describe "deleteClientBackups" $ do
    let deleteA zClient cid = try (deleteClientBackups caller zClient cid)
        deleteOwn = deleteA clientA clientA

    it "deleteClientBackupsOrphansAndDeletes" $ do
      ((r, e1, e2), state, objects) <- runBackup defaultFixture PutSucceeds $ do
        a <- uploadA 1 >>= orFail
        passTime 1
        _ <- uploadIncA a.id "abc" >>= orFail
        _ <- uploadA 2 >>= orFail
        r <- deleteOwn
        e1 <- readEpoch clientA 1
        e2 <- readEpoch clientA 2
        pure (r, e1, e2)
      r `shouldBe` Right ()
      e1 `shouldBe` Left BackupErrorNotFound
      e2 `shouldBe` Left BackupErrorNotFound
      objects `shouldBe` mempty
      state.orphans `shouldBe` mempty

    it "deleteForbiddenForOtherClient" $ do
      ((r, e1), _, _) <- runBackup defaultFixture PutSucceeds $ do
        _ <- uploadA 1 >>= orFail
        r <- deleteA clientA clientB
        e1 <- readEpoch clientA 1
        pure (r, e1)
      r `shouldBe` Left BackupErrorForbidden
      e1 `shouldSatisfy` isRight

    it "deleteForbiddenForRemovedClient" $ do
      (r, _, _) <-
        runBackup defaultFixture {existingClients = Set.singleton clientB} PutSucceeds deleteOwn
      r `shouldBe` Left BackupErrorForbidden

    it "deleteAllowedWhenFeatureOff" $ do
      -- the feature is switched off after the upload: the stub reads the fixture, so the
      -- upload is committed at store level
      (r, state, objects) <- runBackup defaultFixture {featureEnabled = False} PutSucceeds $ do
        (full, _) <- storeFull 1 1
        putObject full.id 5 (body "hello") >>= orFail
        deleteOwn
      r `shouldBe` Right ()
      state.epochs `shouldBe` mempty
      state.orphans `shouldBe` mempty
      objects `shouldBe` mempty

    it "deleteAllowedWithoutTeam" $ do
      (r, _, _) <- runBackup defaultFixture {inTeam = False} PutSucceeds deleteOwn
      r `shouldBe` Right ()

    it "deleteKeepsOtherClients" $ do
      ((r, b1, b), _, objects) <- runBackup defaultFixture PutSucceeds $ do
        _ <- uploadA 1 >>= orFail
        b <- upload clientB clientB 1 "other" >>= orFail
        r <- deleteOwn
        b1 <- try (getEpochFiles caller clientB clientB (BackupEpoch 1))
        pure (r, b1, b)
      r `shouldBe` Right ()
      fmap (.full.id) b1 `shouldBe` Right b.id
      Map.lookup b.id objects `shouldBe` Just "other"

    it "deleteBlocksReservationsOfClientOnly" $ do
      (r, state, _) <- runBackup defaultFixture PutSucceeds $ do
        _ <- uploadA 1 >>= orFail
        passTime 1
        reserveUpload testUser clientA (BackupEpoch 2) (fileN 9)
        reserveUpload testUser clientB (BackupEpoch 1) (fileN 8)
        deleteOwn
      r `shouldBe` Right ()
      -- reservations stay (the cleanup collects them), those of the client are blocked
      map (.fileCreatedAt) (Map.elems state.orphans) `shouldBe` [Nothing, Nothing]
      (.deletionLastTried) <$> Map.lookup (fileN 9) state.orphans
        `shouldBe` Just (Just (addUTCTime 1 defaultTime))
      (.deletionLastTried) <$> Map.lookup (fileN 8) state.orphans `shouldBe` Just Nothing

    it "deleteObjectFailureMarksTried" $ do
      let failing = fileN 1
      ((r, others), state, objects) <-
        runBackupWith defaultFixture PutSucceeds (DeleteFailsFor (Set.singleton failing)) $ do
          (e1, _) <- storeFull 1 1
          i1 <- storeInc 1 e1.id 2
          (e2, _) <- storeFull 2 3
          for_ [e1.id, i1.id, e2.id] $ \f -> putObject f 5 (body "hello") >>= orFail
          passTime 5
          r <- deleteOwn
          pure (r, [i1.id, e2.id])
      r `shouldBe` Right ()
      Map.keys state.orphans `shouldBe` [failing]
      (.deletionLastTried) <$> Map.lookup failing state.orphans `shouldBe` Just (Just (addUTCTime 5 defaultTime))
      (.fileCreatedAt) <$> Map.lookup failing state.orphans `shouldBe` Just (Just defaultTime)
      Map.keys objects `shouldBe` [failing]
      for_ others $ \f -> Map.member f objects `shouldBe` False

    it "commitAfterOrphanClientFails" $ do
      ((next, replace, inc), _, _) <- runBackup defaultFixture PutSucceeds $ do
        (e1, _) <- storeFull 1 1
        -- in-flight uploads: the next epoch, a replacement of epoch 1, an incremental
        reserveUpload testUser clientA (BackupEpoch 2) (fileN 2)
        reserveUpload testUser clientA (BackupEpoch 1) (fileN 3)
        reserveUpload testUser clientA (BackupEpoch 1) (fileN 4)
        _ <- orphanClient testUser clientA
        next <- commitFull testUser clientA (BackupEpoch 2) (fileN 2) 5
        replace <- commitFull testUser clientA (BackupEpoch 1) (fileN 3) 5
        inc <- commitIncremental 10 testUser clientA (BackupEpoch 1) e1.id (fileN 4) 3
        pure (void next, void replace, void inc)
      next `shouldBe` Left CommitEpochInvalid
      -- epoch 1 is accepted again without epochs, so only the blocked reservation stops it
      replace `shouldBe` Left CommitReservationGone
      inc `shouldBe` Left CommitFullMissing

    it "commitFirstUploadAfterOrphanClientFails" $ do
      (r, state, _) <- runBackup defaultFixture PutSucceeds $ do
        reserveUpload testUser clientA (BackupEpoch 1) (fileN 1)
        _ <- orphanClient testUser clientA
        commitFull testUser clientA (BackupEpoch 1) (fileN 1) 5
      void r `shouldBe` Left CommitReservationGone
      state.epochs `shouldBe` mempty

    it "commitIncrementalAfterOrphanClientFails" $ do
      (r, state, _) <- runBackup defaultFixture PutSucceeds $ do
        _ <- storeFull 1 1
        reserveUpload testUser clientA (BackupEpoch 1) (fileN 2)
        _ <- orphanClient testUser clientA
        -- a new epoch 1 exists, but the reservation was made before the removal
        _ <- storeFull 1 3
        commitIncremental 10 testUser clientA (BackupEpoch 1) (fileN 3) (fileN 2) 3
      r `shouldBe` Left CommitIncrementalReservationGone
      state.incrementals `shouldBe` mempty

    it "replaceFullDeletesOldObjectInline" $ do
      ((first, second), state, objects) <- runBackup defaultFixture PutSucceeds $ do
        a <- uploadA 1 >>= orFail
        b <- uploadA 1 >>= orFail
        pure (a, b)
      Map.keys objects `shouldBe` [second.id]
      Map.member first.id objects `shouldBe` False
      state.orphans `shouldBe` mempty

    it "staleEpochDeletedInline" $ do
      ((e1, i1, others), state, objects) <- runBackup defaultFixture PutSucceeds $ do
        e1 <- uploadA 1 >>= orFail
        passTime 1
        i1 <- uploadIncA e1.id "abc" >>= orFail
        e2 <- uploadA 2 >>= orFail
        e3 <- uploadA 3 >>= orFail
        pure (e1, i1, [e2.id, e3.id])
      for_ [e1.id, i1.id] $ \f -> Map.member f objects `shouldBe` False
      Map.keys objects `shouldMatchList` others
      state.orphans `shouldBe` mempty

  describe "cleanup" $ do
    let grace = testConfig.orphanGracePeriod
        expire = passTime (grace + 1)
        runClean = runBackup defaultFixture PutSucceeds
        stored fid = putObject fid 5 (body "hello") >>= orFail
        reserved n = reserveUpload testUser clientA (BackupEpoch 1) (fileN n)
        withBatchSize n = runBackupFrom testConfig {cleanupBatchSize = n} defaultFixture PutSucceeds DeleteSucceeds
        configJson timeout' extra =
          "{\"maxFullBytes\": 1, \"maxIncrementalBytes\": 1, \"maxFilesPerEpoch\": 1, \"uploadTimeout\": "
            <> timeout'
            <> extra
            <> "}"

    it "backupConfigGraceMustExceedTimeout" $ do
      let decode' = eitherDecode @BackupConfig
          grace' = fmap (.orphanGracePeriod)
      isLeft (decode' (configJson "1800" ", \"orphanGracePeriod\": 1800")) `shouldBe` True
      grace' (decode' (configJson "1800" ", \"orphanGracePeriod\": 1801")) `shouldBe` Right 1801
      let omitted = decode' (configJson "1800" "")
      grace' omitted `shouldBe` Right 3600
      fmap (.cleanupBatchSize) omitted `shouldBe` Right 100
      isLeft (decode' (configJson "1800" ", \"cleanupBatchSize\": 0")) `shouldBe` True
      isLeft (decode' (configJson "0" "")) `shouldBe` True
      isLeft (decode' (configJson "-1" "")) `shouldBe` True

    it "cleanupSkipsFreshReservation" $ do
      (n, state, _) <- runClean $ reserved 1 *> passTime 1 *> cleanupOrphans
      n `shouldBe` 0
      Map.keys state.orphans `shouldBe` [fileN 1]

    it "cleanupDeletesExpiredReservation" $ do
      (n, state, objects) <- runClean $ do
        reserved 1
        stored (fileN 1)
        expire
        cleanupOrphans
      n `shouldBe` 1
      state.orphans `shouldBe` mempty
      objects `shouldBe` mempty

    it "cleanupCollectsReservationsBlockedByClientDeletion" $ do
      (n, state, objects) <- runClean $ do
        reserved 1
        stored (fileN 1)
        _ <- orphanClient testUser clientA
        expire
        cleanupOrphans
      n `shouldBe` 1
      state.orphans `shouldBe` mempty
      objects `shouldBe` mempty

    it "cleanupDeletesFailedDeletions" $ do
      let failing = fileN 1
      (_, state1, objects1) <-
        runBackupWith defaultFixture PutSucceeds (DeleteFailsFor (Set.singleton failing)) $ do
          (e1, _) <- storeFull 1 1
          stored e1.id
          (_, orphaned) <- storeFull 1 2
          deleteOrphansNow orphaned
      Map.keys state1.orphans `shouldBe` [failing]
      Map.keys objects1 `shouldBe` [failing]
      -- no waiting for the grace period: the file was created
      (n, state2, objects2) <-
        runBackupFrom testConfig defaultFixture PutSucceeds DeleteSucceeds (state1, objects1) cleanupOrphans
      n `shouldBe` 1
      state2.orphans `shouldBe` mempty
      objects2 `shouldBe` mempty

    it "cleanupKeepsFailedRows" $ do
      let failing = fileN 1
      (n, state, objects) <-
        runBackupWith defaultFixture PutSucceeds (DeleteFailsFor (Set.singleton failing)) $ do
          (e1, _) <- storeFull 1 1
          stored e1.id
          (_, orphaned) <- storeFull 1 2
          deleteOrphansNow orphaned
          passTime 5
          cleanupOrphans
      n `shouldBe` 0
      Map.keys objects `shouldBe` [failing]
      (.deletionLastTried) <$> Map.lookup failing state.orphans
        `shouldBe` Just (Just (addUTCTime 5 defaultTime))

    it "cleanupPrefersNeverTried" $ do
      let orphan n tried =
            MockOrphan
              { userId = testUser,
                clientId = clientA,
                fileCreatedAt = Just defaultTime,
                orphanedAt = addUTCTime n defaultTime,
                deletionLastTried = tried
              }
          initial =
            emptyBackupState
              { orphans =
                  Map.fromList
                    [ (fileN 1, orphan 0 (Just defaultTime)),
                      (fileN 2, orphan 1 Nothing)
                    ]
              }
      (n, state, _) <- withBatchSize 1 (initial, Map.empty) cleanupOrphans
      n `shouldBe` 1
      Map.keys state.orphans `shouldBe` [fileN 1]

    it "commitFailsAfterCleanupSelected" $ do
      ((full, inc, picked), _, _) <- runClean $ do
        _ <- storeFull 1 1
        reserved 2
        reserved 3
        expire
        picked <- selectCleanupBatch grace 100
        full <- commitFull testUser clientA (BackupEpoch 2) (fileN 2) 5
        inc <- commitIncremental 10 testUser clientA (BackupEpoch 1) (fileN 1) (fileN 3) 3
        pure (void full, void inc, picked)
      full `shouldBe` Left CommitReservationGone
      inc `shouldBe` Left CommitIncrementalReservationGone
      sort picked `shouldBe` [fileN 2, fileN 3]

    it "cleanupRespectsBatchSize" $ do
      (ns, _, _) <- withBatchSize 2 (emptyBackupState, Map.empty) $ do
        for_ [1, 2, 3] reserved
        expire
        replicateM 3 cleanupOrphans
      ns `shouldBe` [2, 1, 0]

listAs :: (Upload r) => Local UserId -> Sem r (Either BackupError BackupList)
listAs lusr = try (listBackups lusr clientA)
