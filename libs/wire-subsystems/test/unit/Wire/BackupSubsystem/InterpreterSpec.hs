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
    -- | The next 'ListBlobs' returns nothing, simulating a concurrent upload.
    emptyListingOnce :: Bool,
    -- | 'LookupClients' calls from this one on (1-based) throw 'BackupErrTooManyFiles'.
    failLookupFrom :: Maybe Int
  }

type Effs =
  '[ BackupSubsystem,
     BackupBlobStore,
     BrigAPIAccess,
     GalleyAPIAccess,
     Input BackupLimits,
     Random,
     Now,
     Error BackupError,
     State World,
     State UTCTime,
     State BlobVersions,
     Error BlobUploadError,
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
      emptyListingOnce = False,
      failLookupFrom = Nothing
    }

runBackup ::
  World ->
  Maybe (TVar Word64) ->
  Sem Effs a ->
  IO (BlobVersions, Either BackupError a)
runBackup w counter prog = do
  base <- generate arbitrary
  res <-
    runM
      . runError @BlobUploadError
      . runState mempty
      . evalState t0
      . evalState w
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
      . emptyListingWhenAsked
      . interpretBackupSubsystem
      $ prog
  either (error . show) pure res

emptyListingWhenAsked :: (Member BackupBlobStore r, Member (State World) r) => Sem r a -> Sem r a
emptyListingWhenAsked = intercept $ \case
  ListBlobs u c e -> do
    hide <- gets (.emptyListingOnce)
    modify (\w -> w {emptyListingOnce = False})
    if hide then pure [] else listBlobs u c e
  PutBlob l n s -> putBlob l n s
  BlobExists l -> blobExists l
  SignedDownloadUrl l -> signedDownloadUrl l
  ListBlobEpochs u c -> listBlobEpochs u c
  DeleteBlobVersion l v -> deleteBlobVersion l v
  DeleteBlob l -> deleteBlob l
  DeleteBlobs p -> deleteBlobs p

mockBrig :: (Member (State World) r, Member (Error BackupError) r) => User -> InterpreterFor BrigAPIAccess r
mockBrig base = interpret $ \case
  GetUsers uids ->
    pure [base {userTeam = if u == uC then Nothing else Just teamT} | u <- uids]
  LookupClients uids -> do
    modify (\w -> w {lookups = w.lookups + 1})
    w <- get
    when (maybe False (w.lookups >=) w.failLookupFrom) $ throw BackupErrTooManyFiles
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

upload :: UserId -> ClientId -> ClientId -> Word32 -> ByteString -> Sem Effs BackupFile
upload u z c e body =
  uploadBackup (loc u) z c (BackupEpoch e) (fromIntegral (BS.length body)) (yield body)

uploadSized :: UserId -> ClientId -> Word32 -> Word64 -> Sem Effs BackupFile
uploadSized u c e n =
  uploadBackup (loc u) c c (BackupEpoch e) n (yield (BS.replicate (fromIntegral n) 0))

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

  it "full first even if an incremental has a smaller id" $ do
    (_, res) <- runBackup world0 Nothing $ do
      passTime 10
      full <- upload uA a1 a1 1 "full"
      passTime (-5)
      inc <- upload uA a1 a1 1 "inc"
      fs <- filesOf uA a1 a1 1
      pure (inc.id < full.id, map (.id) fs == [full.id, inc.id])
    res `shouldBe` Right (True, True)

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
      replicateM_ 3 (upload uA a1 a1 1 "x")
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
      delE <- attempt (deleteEpochBackups (loc uA) a1 a2 (BackupEpoch 1))
      delC <- attempt (deleteClientBackups (loc uA) a1 a2)
      pure (void up, delE, delC)
    res `shouldBe` Right (Left BackupErrForbidden, Left BackupErrForbidden, Left BackupErrForbidden)
    blobs `shouldBe` mempty

  it "foreign client is not found" $ do
    (_, res) <- runBackup world0 Nothing $ do
      _ <- upload uA a1 a1 1 "x"
      (,) <$> attempt (epochsOf uB b1 a1) <*> attempt (epochsOf uA a1 b1)
    res `shouldBe` Right (Left BackupErrNotFound, Left BackupErrNotFound)

  it "non-team user cannot upload" $ do
    (_, res) <- runBackup world0 Nothing $ upload uC c1 c1 1 "x"
    void res `shouldBe` Left BackupErrFeatureDisabled

  it "feature disabled blocks upload but not read or delete" $ do
    (blobs, res) <- runBackup world0 Nothing $ do
      f <- upload uA a1 a1 1 "x"
      modify (\w -> w {feature = FeatureStatusDisabled})
      blocked <- attempt $ upload uA a1 a1 1 "y"
      es <- epochsOf uA a2 a1
      _ <- downloadUrl (loc uA) a2 a1 (BackupEpoch 1) f.kind f.id
      deleteEpochBackups (loc uA) a1 a1 (BackupEpoch 1)
      pure (void blocked, es)
    res `shouldBe` Right (Left BackupErrFeatureDisabled, [1])
    blobs `shouldBe` mempty

  it "delete of empty epoch succeeds" $ do
    (_, res) <- runBackup world0 Nothing $ deleteEpochBackups (loc uA) a1 a1 (BackupEpoch 7)
    res `shouldBe` Right ()

  it "delete epoch removes all versions" $ do
    (blobs, res) <- runBackup world0 Nothing $ do
      f <- upload uA a1 a1 1 "x"
      g <- upload uA a1 a1 2 "y"
      let l = locationOf uA a1 1 f
      -- a plain delete on a versioned bucket only adds a marker
      deleteBlobVersion l Nothing
      marked <- gets (versionsOf l)
      deleteEpochBackups (loc uA) a1 a1 (BackupEpoch 1)
      pure (l, marked, locationOf uA a1 2 g)
    Right (l, marked, kept) <- pure res
    marked `shouldBe` [Nothing, Just "x"]
    versionsOf l blobs `shouldBe` []
    Map.keys blobs `shouldBe` [kept]

  it "delete client removes all its files" $ do
    (blobs, res) <- runBackup world0 Nothing $ do
      _ <- upload uA a1 a1 1 "x"
      _ <- upload uA a1 a1 2 "y"
      g <- upload uA a2 a2 1 "z"
      deleteClientBackups (loc uA) a1 a1
      pure (locationOf uA a2 1 g)
    Right kept <- pure res
    Map.keys blobs `shouldBe` [kept]

  it "client gone after PUT: rolled back, no version left" $ do
    (blobs, res) <- runBackup world0 {dropA1From = Just 2} Nothing $ upload uA a1 a1 1 "x"
    void res `shouldBe` Left BackupErrForbidden
    blobs `shouldBe` mempty

  it "failing client re-check rolls back" $ do
    (blobs, res) <- runBackup world0 {failLookupFrom = Just 2} Nothing $ upload uA a1 a1 1 "x"
    void res `shouldBe` Left BackupErrTooManyFiles
    blobs `shouldBe` mempty

  it "incomplete body leaves no version" $ do
    (blobs, res) <-
      runBackup world0 Nothing $
        uploadBackup (loc uA) a1 a1 (BackupEpoch 1) 10 (yield "abc")
    void res `shouldBe` Left BackupErrIncompleteBody
    blobs `shouldBe` mempty

  it "racing upload tolerated: second full in epoch" $ do
    (_, res) <- runBackup world0 Nothing $ do
      _ <- upload uA a1 a1 1 "x"
      modify (\w -> w {emptyListingOnce = True})
      f <- upload uA a1 a1 1 "y"
      fs <- filesOf uA a1 a1 1
      pure (f.kind, map (.kind) fs)
    res `shouldBe` Right (BackupFull, [BackupFull, BackupFull])

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
         in uploadBackup (loc uA) a1 a1 (BackupEpoch 1) len (lockstepSource consumed n size)
    Just (blobs, res) <- pure out
    void res `shouldBe` Right ()
    map (map snd) (Map.elems blobs)
      `shouldBe` [[Just (BS.concat [BS.replicate size (fromIntegral i) | i <- [0 .. n - 1]])]]
