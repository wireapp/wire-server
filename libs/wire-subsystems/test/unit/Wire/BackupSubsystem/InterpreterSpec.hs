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

module Wire.BackupSubsystem.InterpreterSpec (spec) where

import Conduit (yield)
import Data.ByteString qualified as BS
import Data.Default (def)
import Data.Domain
import Data.Function qualified as Function
import Data.Id
import Data.Json.Util (toUTCTimeMillis)
import Data.Map qualified as Map
import Data.Qualified
import Data.Set qualified as Set
import Data.Time (UTCTime (..), addUTCTime, fromGregorian)
import Data.Typeable (cast)
import Data.UUID qualified as UUID
import Imports hiding (id)
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.Resource (Resource, runResource)
import Polysemy.State
import System.Timeout (timeout)
import Test.Hspec
import Test.QuickCheck (arbitrary, generate)
import URI.ByteString (serializeURIRef')
import Wire.API.Backup
import Wire.API.Team.Feature (AutomaticBackupsConfig, FeatureStatus (..), LockableFeature (..))
import Wire.API.User (User (userTeam))
import Wire.API.User.Client (UserClients (..))
import Wire.BackupBlobStore
import Wire.BackupBlobStore.Key
import Wire.BackupSubsystem
import Wire.BackupSubsystem.Interpreter
import Wire.BackupSubsystem.Rules
import Wire.BrigAPIAccess
import Wire.GalleyAPIAccess
import Wire.MockInterpreters
import Wire.Sem.Now (Now)
import Wire.Sem.Random (Random)

data World = World
  { clients :: Map UserId (Set ClientId),
    feature :: FeatureStatus,
    -- | 'LookupClients' calls from this one on (1-based) no longer list @a1@.
    dropA1From :: Maybe Int,
    lookups :: Int,
    -- | 'LookupClients' calls from this one on (1-based) throw 'BrigDown'.
    failLookupFrom :: Maybe Int,
    -- | 'DeleteBlob' throws 'StoreDown'.
    failDelete :: Bool,
    -- | 'PutBlob' stores the object, then throws 'StoreDown'.
    failAfterPut :: Bool
  }

-- | Failures of brig and the store other than 'BackupError', like RPC errors in cargohold.
data MockFailure = BrigDown | StoreDown
  deriving (Eq, Show)

type Effs =
  '[ BackupSubsystem,
     BackupBlobStore,
     BrigAPIAccess,
     GalleyAPIAccess,
     Input BackupLimits,
     Random,
     Now,
     Error BackupError,
     Error MockFailure,
     Error BlobUploadError,
     Resource,
     State World,
     State UTCTime,
     State BlobVersions,
     Embed IO
   ]

uid :: Word64 -> UserId
uid n = Id (UUID.fromWords64 0 n)

uA, uB, uC :: UserId
uA = uid 1
uB = uid 2
uC = uid 3

a1, a2, b1, c1 :: ClientId
a1 = ClientId 0xa1
a2 = ClientId 0xa2
b1 = ClientId 0xb1
c1 = ClientId 0xc1

teamT :: TeamId
teamT = Id (UUID.fromWords64 1 1)

loc :: UserId -> Local UserId
loc = toLocalUnsafe (Domain "example.com")

t0 :: UTCTime
t0 = UTCTime (fromGregorian 2026 11 12) 0

limits :: BackupLimits
limits =
  BackupLimits
    { maxFullBytes = 1024 * 1024 * 1024,
      maxIncrementalBytes = 10 * 1024 * 1024,
      maxFilesPerEpoch = 3
    }

world0 :: World
world0 =
  World
    { clients =
        Map.fromList
          [(uA, Set.fromList [a1, a2]), (uB, Set.singleton b1), (uC, Set.singleton c1)],
      feature = FeatureStatusEnabled,
      dropA1From = Nothing,
      lookups = 0,
      failLookupFrom = Nothing,
      failDelete = False,
      failAfterPut = False
    }

runBackup ::
  World ->
  Maybe (TVar Word64) ->
  Sem Effs a ->
  IO (BlobVersions, Either BackupError a)
runBackup w counter prog =
  fmap (either (error . show) Function.id) <$> runBackupWithFailures w counter prog

-- | Like 'runBackup', but returns 'MockFailure's.
runBackupWithFailures ::
  World ->
  Maybe (TVar Word64) ->
  Sem Effs a ->
  IO (BlobVersions, Either MockFailure (Either BackupError a))
runBackupWithFailures w counter prog = do
  base <- generate arbitrary
  fmap (either (error . show) Function.id)
    <$> ( runM
            . runState mempty
            . evalState t0
            . evalState w
            . runResource
            . runError @BlobUploadError
            . runError @MockFailure
            . runError @BackupError
            . interpretNowAsState
            . runRandomPure
            . runInputConst limits
            . mockGalley
            . mockBrig base
            . ( \p -> case counter of
                  Nothing -> inMemoryBackupBlobStoreInterpreter p
                  Just t -> countingBackupBlobStoreInterpreter t p
              )
            . failingStore
            . interpretBackupSubsystem
            $ prog
        )

-- | Also fails 'DeleteBlob' and 'PutBlob' (after storing) if asked.
failingStore ::
  (Member BackupBlobStore r, Member (State World) r, Member (Error MockFailure) r) =>
  Sem r a ->
  Sem r a
failingStore = intercept $ \case
  PutBlob l n s -> do
    v <- putBlob l n s
    whenM (gets (.failAfterPut)) $ throw StoreDown
    pure v
  ListBlobs u c e -> listBlobs u c e
  BlobExists l -> blobExists l
  SignedDownloadUrl l -> signedDownloadUrl l
  ListBlobEpochs u c -> listBlobEpochs u c
  DeleteBlob l -> do
    whenM (gets (.failDelete)) $ throw StoreDown
    deleteBlob l
  DeleteBlobs p -> deleteBlobs p

mockBrig :: (Member (State World) r, Member (Error MockFailure) r) => User -> InterpreterFor BrigAPIAccess r
mockBrig base = interpret $ \case
  GetUsers uids ->
    pure [base {userTeam = if u == uC then Nothing else Just teamT} | u <- uids]
  LookupClients uids -> do
    modify (\w -> w {lookups = w.lookups + 1})
    w <- get
    when (maybe False (w.lookups >=) w.failLookupFrom) $ throw BrigDown
    let drop' = maybe False (w.lookups >=) w.dropA1From
        cs = if drop' then Map.adjust (Set.delete a1) uA w.clients else w.clients
    pure . UserClients $ Map.restrictKeys cs (Set.fromList uids)
  _ -> error "mockBrig: unexpected call"

mockGalley :: (Member (State World) r) => InterpreterFor GalleyAPIAccess r
mockGalley = interpret $ \case
  GetFeatureConfigForTeam tid | tid == teamT -> do
    s <- gets (.feature)
    let feat :: LockableFeature AutomaticBackupsConfig = def {status = s}
    pure . fromMaybe (error "mockGalley: unexpected feature") $ cast feat
  _ -> error "mockGalley: unexpected call"

uploadKind :: BackupKind -> UserId -> ClientId -> ClientId -> Word32 -> ByteString -> Sem Effs BackupFile
uploadKind kind u z c e body =
  uploadBackup (loc u) z c (BackupEpoch e) kind (fromIntegral (BS.length body)) (yield body)

-- | Uploads a full file if the epoch has no files yet, an incremental one otherwise.
upload :: UserId -> ClientId -> ClientId -> Word32 -> ByteString -> Sem Effs BackupFile
upload u z c e body = do
  existing <- attempt (filesOf u z c e)
  uploadKind (either (const BackupFull) (const BackupIncremental) existing) u z c e body

uploadSizedKind :: BackupKind -> UserId -> ClientId -> Word32 -> Word64 -> Sem Effs BackupFile
uploadSizedKind kind u c e n =
  uploadBackup (loc u) c c (BackupEpoch e) kind n (yield (BS.replicate (fromIntegral n) 0))

uploadSized :: UserId -> ClientId -> Word32 -> Word64 -> Sem Effs BackupFile
uploadSized = uploadSizedKind BackupIncremental

attempt :: Sem Effs a -> Sem Effs (Either BackupError a)
attempt = try

epochsOf :: UserId -> ClientId -> ClientId -> Sem Effs [Word32]
epochsOf u z c = map (.unBackupEpoch) . (.epochs) <$> listEpochs (loc u) z c

filesOf :: UserId -> ClientId -> ClientId -> Word32 -> Sem Effs [BackupFile]
filesOf u z c e = (.files) <$> listFiles (loc u) z c (BackupEpoch e)

locationOf :: UserId -> ClientId -> Word32 -> BackupFile -> BackupLocation
locationOf u c e f = BackupLocation {user = u, client = c, epoch = BackupEpoch e, kind = f.kind, fileId = f.id}

spec :: Spec
spec = describe "BackupSubsystem" $ do
  it "upload then list then download" $ do
    (_, res) <- runBackup world0 Nothing $ do
      f <- upload uA a1 a1 1 "hello"
      es <- epochsOf uA a2 a1
      fs <- filesOf uA a2 a1 1
      url <- downloadUrl (loc uA) a2 a1 (BackupEpoch 1) f.kind f.id
      pure (f, es, fs, serializeURIRef' url)
    Right (f, es, fs, url) <- pure res
    f.kind `shouldBe` BackupFull
    f.size `shouldBe` 5
    f.createdAt `shouldBe` toUTCTimeMillis t0
    es `shouldBe` [1]
    fs `shouldBe` [f]
    url `shouldBe` "https://blob.test/" <> UUID.toASCIIBytes f.id.unBackupFileId

  it "epochs listed ascending" $ do
    (_, res) <- runBackup world0 Nothing $ do
      _ <- upload uA a1 a1 1 "x"
      _ <- upload uA a1 a1 2 "y"
      epochsOf uA a1 a1
    res `shouldBe` Right [1, 2]

  it "list files orders full first, then by id" $ do
    (_, res) <- runBackup world0 Nothing $ do
      full <- upload uA a1 a1 1 "full"
      passTime 1
      inc1 <- upload uA a1 a1 1 "inc1"
      passTime 1
      inc2 <- upload uA a1 a1 1 "inc2"
      fs <- filesOf uA a1 a1 1
      pure ([full, inc1, inc2], fs)
    Right (uploaded, fs) <- pure res
    fs `shouldBe` uploaded
    map (.kind) fs `shouldBe` [BackupFull, BackupIncremental, BackupIncremental]
    map (.createdAt) fs `shouldBe` map toUTCTimeMillis [t0, addUTCTime 1 t0, addUTCTime 2 t0]

  it "list of empty epoch is not found" $ do
    (_, res) <- runBackup world0 Nothing $ filesOf uA a1 a1 1
    void res `shouldBe` Left BackupErrNotFound

  it "second file is incremental and capped at 10 MiB" $ do
    (_, res) <- runBackup world0 Nothing $ do
      _ <- upload uA a1 a1 1 "full"
      f <- upload uA a1 a1 1 "incr"
      tooLarge <- attempt $ uploadSized uA a1 1 (10 * 1024 * 1024 + 1)
      pure (f.kind, tooLarge)
    fmap (fmap void) res `shouldBe` Right (BackupIncremental, Left BackupErrTooLarge)

  it "file count per epoch capped" $ do
    (_, res) <- runBackup world0 Nothing $ do
      replicateM_ 3 (upload uA a1 a1 1 "x" <* passTime 1)
      attempt (upload uA a1 a1 1 "y")
    void <$> res `shouldBe` Right (Left BackupErrTooManyFiles)

  it "epoch rule enforced" $ do
    (_, res) <-
      runBackup world0 Nothing $ upload uA a1 a1 1 "x" >> attempt (upload uA a1 a1 3 "y")
    void <$> res `shouldBe` Right (Left BackupErrEpochNotAllowed)

  it "restart at high epoch when empty" $ do
    (_, res) <- runBackup world0 Nothing $ upload uA a1 a1 6 "x" >> epochsOf uA a1 a1
    res `shouldBe` Right [6]

  it "download needs matching kind" $ do
    (_, res) <- runBackup world0 Nothing $ do
      f <- upload uA a1 a1 1 "x"
      attempt (downloadUrl (loc uA) a1 a1 (BackupEpoch 1) BackupIncremental f.id)
    void <$> res `shouldBe` Right (Left BackupErrNotFound)

  it "download of missing file is not found" $ do
    (_, res) <- runBackup world0 Nothing $ do
      f <- upload uA a1 a1 1 "x"
      attempt (downloadUrl (loc uA) a1 a1 (BackupEpoch 2) f.kind f.id)
    void <$> res `shouldBe` Right (Left BackupErrNotFound)

  it "mutating other device forbidden" $ do
    (blobs, res) <- runBackup world0 Nothing $ do
      up <- attempt (upload uA a1 a2 1 "x")
      pure (void up)
    res `shouldBe` Right (Left BackupErrForbidden)
    blobs `shouldBe` mempty

  it "foreign client is not found" $ do
    (_, res) <- runBackup world0 Nothing $ do
      _ <- upload uA a1 a1 1 "x"
      (,) <$> attempt (epochsOf uB b1 a1) <*> attempt (epochsOf uA a1 b1)
    res `shouldBe` Right (Left BackupErrNotFound, Left BackupErrNotFound)

  it "non-team user cannot upload" $ do
    (_, res) <- runBackup world0 Nothing $ upload uC c1 c1 1 "x"
    void res `shouldBe` Left BackupErrFeatureDisabled

  it "feature disabled blocks upload but not read" $ do
    (blobs, res) <- runBackup world0 Nothing $ do
      f <- upload uA a1 a1 1 "x"
      modify (\w -> w {feature = FeatureStatusDisabled})
      blocked <- attempt $ upload uA a1 a1 1 "y"
      es <- epochsOf uA a2 a1
      _ <- downloadUrl (loc uA) a2 a1 (BackupEpoch 1) f.kind f.id
      pure (void blocked, es)
    res `shouldBe` Right (Left BackupErrFeatureDisabled, [1])
    Map.size blobs `shouldBe` 1

  it "client gone after PUT: rolled back, no version left" $ do
    (blobs, res) <- runBackup world0 {dropA1From = Just 2} Nothing $ uploadKind BackupFull uA a1 a1 1 "x"
    void res `shouldBe` Left BackupErrForbidden
    blobs `shouldBe` mempty

  it "failing client re-check (non-BackupError) rolls back, original error surfaced" $ do
    (blobs, res) <- runBackupWithFailures world0 {failLookupFrom = Just 2} Nothing $ uploadKind BackupFull uA a1 a1 1 "x"
    void <$> res `shouldBe` Left BrigDown
    blobs `shouldBe` mempty

  it "failing rollback does not mask the original error" $ do
    let w = world0 {failLookupFrom = Just 2, failDelete = True}
    (blobs, res) <- runBackupWithFailures w Nothing $ uploadKind BackupFull uA a1 a1 1 "x"
    void <$> res `shouldBe` Left BrigDown
    Map.size blobs `shouldBe` 1

  it "failing rollback after client gone surfaces forbidden" $ do
    let w = world0 {dropA1From = Just 2, failDelete = True}
    (_, res) <- runBackupWithFailures w Nothing $ uploadKind BackupFull uA a1 a1 1 "x"
    void <$> res `shouldBe` Right (Left BackupErrForbidden)

  it "incomplete body leaves no version" $ do
    (blobs, res) <-
      runBackup world0 Nothing $
        uploadBackup (loc uA) a1 a1 (BackupEpoch 1) BackupFull 10 (yield "abc")
    void res `shouldBe` Left BackupErrIncompleteBody
    blobs `shouldBe` mempty

  it "internal client delete removes only that device" $ do
    (blobs, res) <- runBackup world0 Nothing $ do
      _ <- upload uA a1 a1 1 "x"
      g <- upload uA a2 a2 1 "y"
      internalDeleteClientBackups uA a1
      pure (locationOf uA a2 1 g)
    Right kept <- pure res
    Map.keys blobs `shouldBe` [kept]

  it "internal user delete removes all devices" $ do
    (blobs, res) <- runBackup world0 Nothing $ do
      _ <- upload uA a1 a1 1 "x"
      _ <- upload uA a2 a2 1 "y"
      g <- upload uB b1 b1 1 "z"
      internalDeleteUserBackups uA
      pure (locationOf uB b1 1 g)
    Right kept <- pure res
    Map.keys blobs `shouldBe` [kept]

  it "upload streams chunks to blob store" $ do
    consumed <- newTVarIO 0
    let n = 64
        size = 16 * 1024
    out <-
      timeout 10_000_000 . runBackup world0 (Just consumed) $
        let len = fromIntegral (n * size)
         in uploadBackup (loc uA) a1 a1 (BackupEpoch 1) BackupFull len (lockstepSource consumed n size)
    Just (blobs, res) <- pure out
    void res `shouldBe` Right ()
    map (map snd) (Map.elems blobs)
      `shouldBe` [[Just (BS.concat [BS.replicate size (fromIntegral i) | i <- [0 .. n - 1]])]]

  describe "explicit kinds" $ do
    it "incremental needs a full" $ do
      (_, res) <- runBackup world0 Nothing $ attempt (uploadKind BackupIncremental uA a1 a1 1 "inc")
      void <$> res `shouldBe` Right (Left BackupErrFullMissing)

    it "incremental after full is accepted" $ do
      (_, res) <- runBackup world0 Nothing $ do
        _ <- uploadKind BackupFull uA a1 a1 1 "full"
        f <- uploadKind BackupIncremental uA a1 a1 1 "inc"
        fs <- filesOf uA a1 a1 1
        pure (f.kind, length fs)
      res `shouldBe` Right (BackupIncremental, 2)

    it "oversize full is rejected" $ do
      (blobs, res) <-
        runBackup world0 Nothing $
          attempt (uploadSizedKind BackupFull uA a1 1 (1024 * 1024 * 1024 + 1))
      void <$> res `shouldBe` Right (Left BackupErrTooLarge)
      blobs `shouldBe` mempty

  describe "newest full wins" $ do
    it "a new full replaces the old full and its incrementals" $ do
      (blobs, res) <- runBackup world0 Nothing $ do
        old <- uploadKind BackupFull uA a1 a1 1 "old"
        passTime 1
        oldInc <- uploadKind BackupIncremental uA a1 a1 1 "oldinc"
        passTime 1
        new <- uploadKind BackupFull uA a1 a1 1 "new"
        passTime 1
        newInc <- uploadKind BackupIncremental uA a1 a1 1 "newinc"
        fs <- filesOf uA a1 a1 1
        pure ((old, oldInc), (new, newInc), fs)
      Right ((old, oldInc), (new, newInc), fs) <- pure res
      fs `shouldBe` [new, newInc]
      Map.keys blobs
        `shouldMatchList` [locationOf uA a1 1 new, locationOf uA a1 1 newInc]
      Map.member (locationOf uA a1 1 old) blobs `shouldBe` False
      Map.member (locationOf uA a1 1 oldInc) blobs `shouldBe` False

    it "an older full finishing late deletes itself" $ do
      (blobs, res) <- runBackup world0 Nothing $ do
        passTime 1
        newer <- uploadKind BackupFull uA a1 a1 1 "newer"
        passTime (-5)
        older <- uploadKind BackupFull uA a1 a1 1 "older"
        fs <- filesOf uA a1 a1 1
        pure (newer, older, fs)
      Right (newer, older, fs) <- pure res
      fs `shouldBe` [newer]
      Map.member (locationOf uA a1 1 older) blobs `shouldBe` False

    it "only the two newest epochs are kept" $ do
      (_, res) <- runBackup world0 Nothing $ do
        _ <- upload uA a1 a1 1 "x"
        _ <- upload uA a1 a1 2 "x"
        _ <- upload uA a1 a1 3 "x"
        epochsOf uA a1 a1
      res `shouldBe` Right [2, 3]

    it "next epoch is allowed after pruning" $ do
      (_, res) <- runBackup world0 Nothing $ do
        traverse_ (\e -> upload uA a1 a1 e "x") [1, 2, 3, 4]
        epochsOf uA a1 a1
      res `shouldBe` Right [3, 4]

  describe "list user backups" $ do
    it "lists all clients with epochs, newest client first, omits clients without a full" $ do
      (_, res) <- runBackup world0 Nothing $ do
        f1 <- upload uA a1 a1 1 "x"
        passTime 1
        g1 <- upload uA a2 a2 1 "y"
        passTime 1
        g2 <- upload uA a2 a2 2 "z"
        UserBackups cs <- listUserBackups (loc uA) a1
        pure (f1, g1, g2, cs)
      Right (f1, g1, g2, cs) <- pure res
      cs
        `shouldBe` [ ClientBackups a2 [EpochBackups (BackupEpoch 1) [g1], EpochBackups (BackupEpoch 2) [g2]],
                     ClientBackups a1 [EpochBackups (BackupEpoch 1) [f1]]
                   ]

    it "is empty without backups" $ do
      (_, res) <- runBackup world0 Nothing $ listUserBackups (loc uA) a1
      res `shouldBe` Right (UserBackups [])

    it "a client that is not the caller's is not found" $ do
      (_, res) <- runBackup world0 Nothing $ listUserBackups (loc uA) b1
      void res `shouldBe` Left BackupErrNotFound

  describe "failed uploads leave nothing behind" $ do
    let both :: (BackupKind -> Spec) -> Spec
        both t = traverse_ t [BackupFull, BackupIncremental]
        -- a full file exists first, so that incrementals are allowed; epoch 2 is the target
        prepare = uploadKind BackupFull uA a1 a1 2 "base"
        target kind = case kind of
          BackupFull -> 1 :: Word32
          BackupIncremental -> 2
        baseWorld kind = if kind == BackupFull then pure () else void prepare
        expectedBlobs kind = if kind == BackupFull then 0 else 1
    both $ \kind ->
      it ("brig says client gone: " <> show kind) $ do
        (blobs, res) <- runBackup world0 {dropA1From = Just (if kind == BackupFull then 2 else 4)} Nothing $ do
          baseWorld kind
          void <$> attempt (uploadKind kind uA a1 a1 (target kind) "x")
        res `shouldBe` Right (Left BackupErrForbidden)
        Map.size blobs `shouldBe` expectedBlobs kind
    both $ \kind ->
      it ("short body: " <> show kind) $ do
        (blobs, res) <- runBackup world0 Nothing $ do
          baseWorld kind
          void
            <$> attempt (uploadBackup (loc uA) a1 a1 (BackupEpoch (target kind)) kind 10 (yield "abc"))
        res `shouldBe` Right (Left BackupErrIncompleteBody)
        Map.size blobs `shouldBe` expectedBlobs kind
    both $ \kind ->
      it ("store failure after the object was stored: " <> show kind) $ do
        (blobs, res) <- runBackupWithFailures world0 Nothing $ do
          baseWorld kind
          modify (\w -> w {failAfterPut = True})
          void <$> try @MockFailure (uploadKind kind uA a1 a1 (target kind) "x")
        fmap (either (error . show) Function.id) res `shouldBe` Right (Left StoreDown)
        Map.size blobs `shouldBe` expectedBlobs kind
