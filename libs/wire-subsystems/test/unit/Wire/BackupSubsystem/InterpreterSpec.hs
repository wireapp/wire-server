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
import Wire.BackupSubsystem.UUIDv7 (uuidV7)
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
    failAfterPut :: Bool,
    -- | 'PutBlob' first runs a deletion of the client's backups (its tombstone is newer than any
    -- file), like a deletion that overtakes an upload.
    deleteDuringPut :: Bool,
    -- | 'PutBlob' stores the object, then runs the deletion of the client's newest tombstone
    -- (again), like a deletion that lists the client's objects only after the PUT.
    sweepAfterPut :: Bool,
    -- | 'PutBlob' first stores full files in the epochs 3 and 4, like uploads of other fulls that
    -- finish meanwhile.
    newerEpochsDuringPut :: Bool,
    -- | 'PutBlob' first stores the full file 'newestLoc' in the same epoch, like an upload of a
    -- newer full that finishes meanwhile.
    newerFullDuringPut :: Bool,
    -- | 'PutBlob' stores the object, then stores the full file 'newestLoc' in the same epoch and
    -- deletes all other files of the epoch, like an upload of a newer full that finishes and
    -- prunes meanwhile.
    newerFullPrunesAfterPut :: Bool
  }

-- | The location of a full file with an id greater than any generated one.
newestLoc :: UserId -> ClientId -> BackupEpoch -> BackupLocation
newestLoc u c e =
  BackupLocation
    { user = u,
      client = c,
      epoch = e,
      file = FullFile (BackupFileId (UUID.fromWords64 maxBound maxBound))
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
     State Tombstones,
     State Blobs,
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
      failAfterPut = False,
      deleteDuringPut = False,
      sweepAfterPut = False,
      newerEpochsDuringPut = False,
      newerFullDuringPut = False,
      newerFullPrunesAfterPut = False
    }

runBackup ::
  World ->
  Maybe (TVar Word64) ->
  Sem Effs a ->
  IO (Blobs, Either BackupError a)
runBackup w counter prog =
  fmap (either (error . show) Function.id) <$> runBackupWithFailures w counter prog

-- | Like 'runBackup', but returns 'MockFailure's.
runBackupWithFailures ::
  World ->
  Maybe (TVar Word64) ->
  Sem Effs a ->
  IO (Blobs, Either MockFailure (Either BackupError a))
runBackupWithFailures w counter prog = do
  base <- generate arbitrary
  fmap (either (error . show) Function.id)
    <$> ( runM
            . runState mempty
            . evalState (mempty :: Tombstones)
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
    whenM (gets (.deleteDuringPut)) $ do
      let t = BackupFileId (UUID.fromWords64 maxBound maxBound)
      putTombstone l.user l.client t
      deleteClientBackupsBefore l.user l.client t
    whenM (gets (.newerEpochsDuringPut)) $
      for_ [3, 4] $ \e ->
        putBlob
          l {epoch = BackupEpoch e, file = FullFile (BackupFileId (UUID.fromWords64 0 (fromIntegral e)))}
          1
          (yield "n")
    whenM (gets (.newerFullDuringPut)) $
      putBlob (newestLoc l.user l.client l.epoch) 1 (yield "n")
    putBlob l n s
    whenM (gets (.newerFullPrunesAfterPut)) $ do
      let newest = newestLoc l.user l.client l.epoch
      putBlob newest 1 (yield "n")
      listBlobs l.user l.client l.epoch
        >>= traverse_ (\b -> deleteBlob BackupLocation {user = l.user, client = l.client, epoch = l.epoch, file = b.file})
          . filter ((/= newest.file) . (.file))
    whenM (gets (.sweepAfterPut)) $
      listTombstones l.user l.client
        >>= traverse_ (deleteClientBackupsBefore l.user l.client) . foldr (max . Just) Nothing
    whenM (gets (.failAfterPut)) $ throw StoreDown
  ListBlobs u c e -> listBlobs u c e
  BlobExists l -> blobExists l
  SignedDownloadUrl l -> signedDownloadUrl l
  ListBlobEpochs u c -> listBlobEpochs u c
  DeleteBlob l -> do
    whenM (gets (.failDelete)) $ throw StoreDown
    deleteBlob l
  DeleteBlobs p -> deleteBlobs p
  PutTombstone u c i -> putTombstone u c i
  ListTombstones u c -> listTombstones u c
  DeleteClientBackupsBefore u c t -> deleteClientBackupsBefore u c t

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

-- | Uploads a full file ('Nothing') or an incremental file under the given full file.
uploadTo :: Maybe BackupFileId -> UserId -> ClientId -> ClientId -> Word32 -> ByteString -> Sem Effs BackupFile
uploadTo base u z c e body =
  uploadBackup (loc u) z c (BackupEpoch e) base (fromIntegral (BS.length body)) (yield body)

uploadFull :: UserId -> ClientId -> ClientId -> Word32 -> ByteString -> Sem Effs BackupFile
uploadFull = uploadTo Nothing

-- | An incremental file under the given full file.
uploadInc :: BackupFile -> UserId -> ClientId -> ClientId -> Word32 -> ByteString -> Sem Effs BackupFile
uploadInc full = uploadTo (Just full.id)

-- | Uploads a full file if the epoch has no files yet, an incremental one under the current full
-- file otherwise.
upload :: UserId -> ClientId -> ClientId -> Word32 -> ByteString -> Sem Effs BackupFile
upload u z c e body = do
  existing <- attempt (listFiles (loc u) z c (BackupEpoch e))
  uploadTo (either (const Nothing) (Just . (.full.id)) existing) u z c e body

-- | An incremental file of the given size under the current full file.
uploadSized :: UserId -> ClientId -> Word32 -> Word64 -> Sem Effs BackupFile
uploadSized u c e n = do
  fs <- listFiles (loc u) c c (BackupEpoch e)
  uploadBackup (loc u) c c (BackupEpoch e) (Just fs.full.id) n (yield (BS.replicate (fromIntegral n) 0))

uploadSizedFull :: UserId -> ClientId -> Word32 -> Word64 -> Sem Effs BackupFile
uploadSizedFull u c e n =
  uploadBackup (loc u) c c (BackupEpoch e) Nothing n (yield (BS.replicate (fromIntegral n) 0))

-- | Moves the clock on: ids created from here on are newer than all earlier ones.
tick :: Sem Effs ()
tick = modify (addUTCTime 1)

attempt :: Sem Effs a -> Sem Effs (Either BackupError a)
attempt = try

epochsOf :: UserId -> ClientId -> ClientId -> Sem Effs [Word32]
epochsOf u z c = map (.epoch.unBackupEpoch) . (.epochs) <$> listClientBackups (loc u) z c

filesOf :: UserId -> ClientId -> ClientId -> Word32 -> Sem Effs [BackupFile]
filesOf u z c e = (\fs -> fs.full : fs.incrementals) <$> listFiles (loc u) z c (BackupEpoch e)

fullLoc :: UserId -> ClientId -> Word32 -> BackupFile -> BackupLocation
fullLoc u c e f = BackupLocation {user = u, client = c, epoch = BackupEpoch e, file = FullFile f.id}

-- | @incLoc u c e full f@: location of the incremental file @f@ under @full@
incLoc :: UserId -> ClientId -> Word32 -> BackupFile -> BackupFile -> BackupLocation
incLoc u c e full f =
  BackupLocation {user = u, client = c, epoch = BackupEpoch e, file = IncrementalFile full.id f.id}

spec :: Spec
spec = describe "BackupSubsystem" $ do
  it "upload then list then download" $ do
    (_, res) <- runBackup world0 Nothing $ do
      f <- upload uA a1 a1 1 "hello"
      es <- epochsOf uA a2 a1
      fs <- filesOf uA a2 a1 1
      url <- downloadUrl (loc uA) a2 a1 (BackupEpoch 1) f.id Nothing
      pure (f, es, fs, serializeURIRef' url)
    Right (f, es, fs, url) <- pure res
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

  it "list files: the full, then the incrementals by id" $ do
    (_, res) <- runBackup world0 Nothing $ do
      full <- upload uA a1 a1 1 "full"
      passTime 1
      inc1 <- upload uA a1 a1 1 "inc1"
      passTime 1
      inc2 <- upload uA a1 a1 1 "inc2"
      fs <- listFiles (loc uA) a1 a1 (BackupEpoch 1)
      pure ([full, inc1, inc2], fs)
    Right (uploaded, listed) <- pure res
    listed `shouldBe` BackupFiles {full = head uploaded, incrementals = drop 1 uploaded}
    let fs = listed.full : listed.incrementals
    map (.createdAt) fs `shouldBe` map toUTCTimeMillis [t0, addUTCTime 1 t0, addUTCTime 2 t0]

  it "list of empty epoch is not found" $ do
    (_, res) <- runBackup world0 Nothing $ filesOf uA a1 a1 1
    void res `shouldBe` Left BackupErrNotFound

  it "second file is incremental and capped at 10 MiB" $ do
    (_, res) <- runBackup world0 Nothing $ do
      _ <- upload uA a1 a1 1 "full"
      f <- upload uA a1 a1 1 "incr"
      tooLarge <- attempt $ uploadSized uA a1 1 (10 * 1024 * 1024 + 1)
      pure (f.size, tooLarge)
    fmap (fmap void) res `shouldBe` Right (4, Left BackupErrTooLarge)

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

  it "download of an incremental file needs its full file" $ do
    (_, res) <- runBackup world0 Nothing $ do
      f <- upload uA a1 a1 1 "x"
      attempt (downloadUrl (loc uA) a1 a1 (BackupEpoch 1) f.id (Just f.id))
    void <$> res `shouldBe` Right (Left BackupErrNotFound)

  it "download of missing file is not found" $ do
    (_, res) <- runBackup world0 Nothing $ do
      f <- upload uA a1 a1 1 "x"
      attempt (downloadUrl (loc uA) a1 a1 (BackupEpoch 2) f.id Nothing)
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
      _ <- downloadUrl (loc uA) a2 a1 (BackupEpoch 1) f.id Nothing
      pure (void blocked, es)
    res `shouldBe` Right (Left BackupErrFeatureDisabled, [1])
    Map.size blobs `shouldBe` 1

  it "client gone after PUT: rolled back, no version left" $ do
    (blobs, res) <- runBackup world0 {dropA1From = Just 2} Nothing $ uploadFull uA a1 a1 1 "x"
    void res `shouldBe` Left BackupErrForbidden
    blobs `shouldBe` mempty

  it "failing client re-check (non-BackupError) rolls back, original error surfaced" $ do
    (blobs, res) <- runBackupWithFailures world0 {failLookupFrom = Just 2} Nothing $ uploadFull uA a1 a1 1 "x"
    void <$> res `shouldBe` Left BrigDown
    blobs `shouldBe` mempty

  it "failing rollback does not mask the original error" $ do
    let w = world0 {failLookupFrom = Just 2, failDelete = True}
    (blobs, res) <- runBackupWithFailures w Nothing $ uploadFull uA a1 a1 1 "x"
    void <$> res `shouldBe` Left BrigDown
    Map.size blobs `shouldBe` 1

  it "failing rollback after client gone surfaces forbidden" $ do
    let w = world0 {dropA1From = Just 2, failDelete = True}
    (_, res) <- runBackupWithFailures w Nothing $ uploadFull uA a1 a1 1 "x"
    void <$> res `shouldBe` Right (Left BackupErrForbidden)

  it "incomplete body leaves no version" $ do
    (blobs, res) <-
      runBackup world0 Nothing $
        uploadBackup (loc uA) a1 a1 (BackupEpoch 1) Nothing 10 (yield "abc")
    void res `shouldBe` Left BackupErrIncompleteBody
    blobs `shouldBe` mempty

  it "client delete removes all epochs of own device only" $ do
    (blobs, res) <- runBackup world0 Nothing $ do
      _ <- upload uA a1 a1 1 "x"
      _ <- upload uA a1 a1 2 "y"
      g <- upload uA a2 a2 1 "z"
      tick
      deleteClientBackups (loc uA) a1 a1
      deleteClientBackups (loc uA) a1 a1
      pure (fullLoc uA a2 1 g)
    Right kept <- pure res
    Map.keys blobs `shouldBe` [kept]

  it "client delete of another device is forbidden" $ do
    (blobs, res) <- runBackup world0 Nothing $ do
      _ <- upload uA a2 a2 1 "x"
      attempt (deleteClientBackups (loc uA) a1 a2)
    res `shouldBe` Right (Left BackupErrForbidden)
    Map.size blobs `shouldBe` 1

  it "client delete works with the feature disabled, then epoch 1 is allowed again" $ do
    (blobs, res) <- runBackup world0 Nothing $ do
      _ <- upload uA a1 a1 1 "x"
      _ <- upload uA a1 a1 2 "y"
      modify (\w -> w {feature = FeatureStatusDisabled})
      tick
      deleteClientBackups (loc uA) a1 a1
      tick
      modify (\w -> w {feature = FeatureStatusEnabled})
      upload uA a1 a1 1 "z"
    void res `shouldBe` Right ()
    Map.size blobs `shouldBe` 1

  it "upload racing a client delete is rolled back, no version left" $ do
    (blobs, res) <-
      runBackup world0 {deleteDuringPut = True} Nothing $ attempt (uploadFull uA a1 a1 1 "x")
    res `shouldBe` Right (Left BackupErrForbidden)
    blobs `shouldBe` mempty

  it "incremental upload racing a client delete is rolled back, nothing left" $ do
    (blobs, res) <- runBackup world0 Nothing $ do
      f <- uploadFull uA a1 a1 1 "x"
      modify (\w -> w {deleteDuringPut = True})
      attempt (uploadInc f uA a1 a1 1 "y")
    res `shouldBe` Right (Left BackupErrForbidden)
    blobs `shouldBe` mempty

  it "full upload after a client delete is kept by the delete's late sweep" $ do
    (blobs, res) <- runBackup world0 Nothing $ do
      _ <- upload uA a1 a1 1 "x"
      deleteClientBackups (loc uA) a1 a1
      tick
      modify (\w -> w {sweepAfterPut = True})
      f <- uploadFull uA a1 a1 1 "y"
      pure (fullLoc uA a1 1 f)
    Right kept <- pure res
    Map.keys blobs `shouldBe` [kept]

  it "incremental upload after a client delete, under an older full, is rolled back" $ do
    (blobs, res) <- runBackup world0 Nothing $ do
      f <- uploadFull uA a1 a1 1 "x"
      tick
      now <- get @UTCTime
      putTombstone uA a1 (BackupFileId (uuidV7 now UUID.nil))
      tick
      modify (\w -> w {sweepAfterPut = True})
      attempt (uploadInc f uA a1 a1 1 "y")
    res `shouldBe` Right (Left BackupErrForbidden)
    blobs `shouldBe` mempty

  it "client delete keeps only the newest tombstone, uploads after it succeed" $ do
    (blobs, res) <- runBackup world0 Nothing $ do
      _ <- upload uA a1 a1 1 "x"
      deleteClientBackups (loc uA) a1 a1
      tick
      deleteClientBackups (loc uA) a1 a1
      tick
      _ <- upload uA a1 a1 1 "y"
      length <$> listTombstones uA a1
    Map.size blobs `shouldBe` 1
    res `shouldBe` Right 1

  it "an older client delete keeps the newer tombstone" $ do
    let old = BackupFileId (UUID.fromWords64 0 1)
        new = BackupFileId (UUID.fromWords64 maxBound maxBound)
    (_, res) <- runBackup world0 Nothing $ do
      putTombstone uA a1 new
      deleteClientBackupsBefore uA a1 old
      listTombstones uA a1
    res `shouldBe` Right [new]

  it "removing the client removes its tombstones" $ do
    (_, res) <- runBackup world0 Nothing $ do
      deleteClientBackups (loc uA) a1 a1
      internalDeleteClientBackups uA a1
      listTombstones uA a1
    res `shouldBe` Right []

  it "internal client delete removes only that device" $ do
    (blobs, res) <- runBackup world0 Nothing $ do
      _ <- upload uA a1 a1 1 "x"
      g <- upload uA a2 a2 1 "y"
      internalDeleteClientBackups uA a1
      pure (fullLoc uA a2 1 g)
    Right kept <- pure res
    Map.keys blobs `shouldBe` [kept]

  it "internal user delete removes all devices" $ do
    (blobs, res) <- runBackup world0 Nothing $ do
      _ <- upload uA a1 a1 1 "x"
      _ <- upload uA a2 a2 1 "y"
      g <- upload uB b1 b1 1 "z"
      internalDeleteUserBackups uA
      pure (fullLoc uB b1 1 g)
    Right kept <- pure res
    Map.keys blobs `shouldBe` [kept]

  it "upload streams chunks to blob store" $ do
    consumed <- newTVarIO 0
    let n = 64
        size = 16 * 1024
    out <-
      timeout 10_000_000 . runBackup world0 (Just consumed) $
        let len = fromIntegral (n * size)
         in uploadBackup (loc uA) a1 a1 (BackupEpoch 1) Nothing len (lockstepSource consumed n size)
    Just (blobs, res) <- pure out
    void res `shouldBe` Right ()
    Map.elems blobs
      `shouldBe` [BS.concat [BS.replicate size (fromIntegral i) | i <- [0 .. n - 1]]]

  describe "incremental files" $ do
    it "need a full file" $ do
      (blobs, res) <-
        runBackup world0 Nothing $
          attempt (uploadTo (Just (BackupFileId UUID.nil)) uA a1 a1 1 "inc")
      void <$> res `shouldBe` Right (Left BackupErrFullMissing)
      blobs `shouldBe` mempty

    it "are accepted under the full file" $ do
      (_, res) <- runBackup world0 Nothing $ do
        full <- uploadFull uA a1 a1 1 "full"
        f <- uploadInc full uA a1 a1 1 "inc"
        fs <- filesOf uA a1 a1 1
        pure (f.size, length fs)
      res `shouldBe` Right (3, 2)

    it "are rejected under an older full file" $ do
      (blobs, res) <- runBackup world0 Nothing $ do
        old <- uploadFull uA a1 a1 1 "old"
        passTime 1
        new <- uploadFull uA a1 a1 1 "new"
        rejected <- attempt (uploadInc old uA a1 a1 1 "inc")
        pure (new, void rejected)
      Right (new, rejected) <- pure res
      rejected `shouldBe` Left BackupErrFullMissing
      Map.keys blobs `shouldBe` [fullLoc uA a1 1 new]

    it "are deleted and rejected if a newer full file is stored during the upload" $ do
      (blobs, res) <- runBackup world0 Nothing $ do
        old <- uploadFull uA a1 a1 1 "old"
        modify (\w -> w {newerFullDuringPut = True})
        (old,) . void <$> attempt (uploadInc old uA a1 a1 1 "inc")
      Right (old, rejected) <- pure res
      rejected `shouldBe` Left BackupErrFullMissing
      -- the old full file is left to the newer full file's upload (not run by the mock)
      Map.keys blobs `shouldMatchList` [fullLoc uA a1 1 old, newestLoc uA a1 (BackupEpoch 1)]

    it "are rejected if a newer full file already deleted them during the upload" $ do
      (blobs, res) <- runBackup world0 Nothing $ do
        old <- uploadFull uA a1 a1 1 "old"
        modify (\w -> w {newerFullPrunesAfterPut = True})
        void <$> attempt (uploadInc old uA a1 a1 1 "inc")
      res `shouldBe` Right (Left BackupErrFullMissing)
      Map.keys blobs `shouldBe` [newestLoc uA a1 (BackupEpoch 1)]

    it "leave the full file to a newer one that is rolled back" $ do
      (blobs, res) <- runBackup world0 Nothing $ do
        old <- uploadFull uA a1 a1 1 "old"
        modify (\w -> w {newerFullDuringPut = True})
        rejected <- attempt (uploadInc old uA a1 a1 1 "inc")
        modify (\w -> w {newerFullDuringPut = False})
        deleteBlob (newestLoc uA a1 (BackupEpoch 1))
        (old,void rejected,) <$> filesOf uA a1 a1 1
      Right (old, rejected, fs) <- pure res
      rejected `shouldBe` Left BackupErrFullMissing
      fs `shouldBe` [old]
      Map.keys blobs `shouldBe` [fullLoc uA a1 1 old]

    it "of a full file that lost a race are never listed and removed by the next upload" $ do
      -- the race: the incremental file passed the check against the old full file, a newer full
      -- file was stored and pruned, then the incremental file was stored with a greater id
      (blobs, res) <- runBackup world0 Nothing $ do
        old <- uploadFull uA a1 a1 1 "old"
        passTime 1
        new <- uploadFull uA a1 a1 1 "new"
        passTime 1
        let orphan = BackupFileId (UUID.fromWords64 maxBound maxBound)
            l = BackupLocation {user = uA, client = a1, epoch = BackupEpoch 1, file = IncrementalFile old.id orphan}
        modify (Map.insert l "orphan")
        listed <- filesOf uA a1 a1 1
        passTime 1
        inc <- uploadInc new uA a1 a1 1 "inc"
        pure (new, inc, listed)
      Right (new, inc, listed) <- pure res
      listed `shouldBe` [new]
      Map.keys blobs `shouldMatchList` [fullLoc uA a1 1 new, incLoc uA a1 1 new inc]

    it "oversize full is rejected" $ do
      (blobs, res) <-
        runBackup world0 Nothing $
          attempt (uploadSizedFull uA a1 1 (1024 * 1024 * 1024 + 1))
      void <$> res `shouldBe` Right (Left BackupErrTooLarge)
      blobs `shouldBe` mempty

  describe "newest full wins" $ do
    it "a new full replaces the old full and its incrementals" $ do
      (blobs, res) <- runBackup world0 Nothing $ do
        old <- uploadFull uA a1 a1 1 "old"
        passTime 1
        oldInc <- uploadInc old uA a1 a1 1 "oldinc"
        passTime 1
        new <- uploadFull uA a1 a1 1 "new"
        passTime 1
        newInc <- uploadInc new uA a1 a1 1 "newinc"
        fs <- filesOf uA a1 a1 1
        pure (old, oldInc, new, newInc, fs)
      Right (old, oldInc, new, newInc, fs) <- pure res
      fs `shouldBe` [new, newInc]
      Map.keys blobs
        `shouldMatchList` [fullLoc uA a1 1 new, incLoc uA a1 1 new newInc]
      Map.member (fullLoc uA a1 1 old) blobs `shouldBe` False
      Map.member (incLoc uA a1 1 old oldInc) blobs `shouldBe` False

    it "an older full finishing late deletes itself and is rejected" $ do
      (blobs, res) <- runBackup world0 Nothing $ do
        passTime 1
        newer <- uploadFull uA a1 a1 1 "newer"
        passTime (-5)
        older <- attempt (uploadFull uA a1 a1 1 "older")
        fs <- filesOf uA a1 a1 1
        pure (newer, void older, fs)
      Right (newer, older, fs) <- pure res
      older `shouldBe` Left BackupErrFullMissing
      fs `shouldBe` [newer]
      Map.keys blobs `shouldBe` [fullLoc uA a1 1 newer]

    it "an older full already deleted by a newer full's prune is rejected" $ do
      (blobs, res) <- runBackup world0 Nothing $ do
        modify (\w -> w {newerFullPrunesAfterPut = True})
        void <$> attempt (uploadFull uA a1 a1 1 "older")
      res `shouldBe` Right (Left BackupErrFullMissing)
      Map.keys blobs `shouldBe` [newestLoc uA a1 (BackupEpoch 1)]

    it "only the two newest epochs are kept" $ do
      (_, res) <- runBackup world0 Nothing $ do
        _ <- upload uA a1 a1 1 "x"
        _ <- upload uA a1 a1 2 "x"
        _ <- upload uA a1 a1 3 "x"
        epochsOf uA a1 a1
      res `shouldBe` Right [2, 3]

    it "a full racing newer epochs prunes by the epochs after its upload and is rejected" $ do
      (_, res) <- runBackup world0 Nothing $ do
        _ <- upload uA a1 a1 1 "x"
        _ <- upload uA a1 a1 2 "x"
        modify (\w -> w {newerEpochsDuringPut = True})
        passTime 1
        r <- attempt (uploadFull uA a1 a1 2 "y")
        (r,) <$> epochsOf uA a1 a1
      res `shouldBe` Right (Left BackupErrEpochNotAllowed, [3, 4])

    it "next epoch is allowed after pruning" $ do
      (_, res) <- runBackup world0 Nothing $ do
        traverse_ (\e -> upload uA a1 a1 e "x") [1, 2, 3, 4]
        epochsOf uA a1 a1
      res `shouldBe` Right [3, 4]

  describe "list backups" $ do
    let summary e f = EpochSummary (BackupEpoch e) f.createdAt 1 f.size
    it "lists all clients with epoch summaries, newest client first, omits clients without a full" $ do
      (_, res) <- runBackup world0 Nothing $ do
        f1 <- upload uA a1 a1 1 "x"
        passTime 1
        g1 <- upload uA a2 a2 1 "yy"
        passTime 1
        g2 <- upload uA a2 a2 2 "zzz"
        UserBackups cs <- listUserBackups (loc uA) a1
        pure (f1, g1, g2, cs)
      Right (f1, g1, g2, cs) <- pure res
      cs
        `shouldBe` [ ClientBackups a2 [summary 1 g1, summary 2 g2],
                     ClientBackups a1 [summary 1 f1]
                   ]

    it "summarises only the current files" $ do
      (_, res) <- runBackup world0 Nothing $ do
        old <- uploadFull uA a1 a1 1 "old"
        passTime 1
        _ <- uploadInc old uA a1 a1 1 "oldinc"
        passTime 1
        new <- uploadFull uA a1 a1 1 "new!"
        passTime 1
        inc <- uploadInc new uA a1 a1 1 "ii"
        UserBackups cs <- listUserBackups (loc uA) a1
        pure (new, inc, cs)
      Right (new, inc, cs) <- pure res
      cs `shouldBe` [ClientBackups a1 [EpochSummary (BackupEpoch 1) inc.createdAt 2 (new.size + inc.size)]]

    it "is empty without backups" $ do
      (_, res) <- runBackup world0 Nothing $ listUserBackups (loc uA) a1
      res `shouldBe` Right (UserBackups [])

    it "a client that is not the caller's is not found" $ do
      (_, res) <- runBackup world0 Nothing $ listUserBackups (loc uA) b1
      void res `shouldBe` Left BackupErrNotFound

    it "list of one client: empty without backups, readable from another client" $ do
      (_, res) <- runBackup world0 Nothing $ do
        none <- listClientBackups (loc uA) a2 a1
        f <- upload uA a1 a1 1 "x"
        other <- listClientBackups (loc uA) a2 a1
        pure (none, f, other)
      Right (none, f, other) <- pure res
      none `shouldBe` ClientBackups a1 []
      other `shouldBe` ClientBackups a1 [summary 1 f]

    it "list of a foreign client is not found" $ do
      (_, res) <- runBackup world0 Nothing $ listClientBackups (loc uA) a1 b1
      void res `shouldBe` Left BackupErrNotFound

  describe "failed uploads leave nothing behind" $ do
    let both :: (Bool -> Spec) -> Spec
        both t = traverse_ t [True, False]
        -- incrementals need a full file first: epoch 2 holds it and is the target
        target full = if full then 1 else 2 :: Word32
        -- the full file the incremental upload extends
        prepare full = if full then pure Nothing else Just . (.id) <$> uploadFull uA a1 a1 2 "base"
        expectedBlobs full = if full then 0 else 1
        name full = if full then "full" else "incremental"
    both $ \full ->
      it ("brig says client gone: " <> name full) $ do
        (blobs, res) <- runBackup world0 {dropA1From = Just (if full then 2 else 4)} Nothing $ do
          base <- prepare full
          void <$> attempt (uploadTo base uA a1 a1 (target full) "x")
        res `shouldBe` Right (Left BackupErrForbidden)
        Map.size blobs `shouldBe` expectedBlobs full
    both $ \full ->
      it ("short body: " <> name full) $ do
        (blobs, res) <- runBackup world0 Nothing $ do
          base <- prepare full
          void
            <$> attempt (uploadBackup (loc uA) a1 a1 (BackupEpoch (target full)) base 10 (yield "abc"))
        res `shouldBe` Right (Left BackupErrIncompleteBody)
        Map.size blobs `shouldBe` expectedBlobs full
    both $ \full ->
      it ("store failure after the object was stored: " <> name full) $ do
        (blobs, res) <- runBackupWithFailures world0 Nothing $ do
          base <- prepare full
          modify (\w -> w {failAfterPut = True})
          void <$> try @MockFailure (uploadTo base uA a1 a1 (target full) "x")
        fmap (either (error . show) Function.id) res `shouldBe` Right (Left StoreDown)
        Map.size blobs `shouldBe` expectedBlobs full
