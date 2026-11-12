{-# LANGUAGE TemplateHaskell #-}

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
import Data.Map qualified as Map
import Data.Qualified
import Data.Set qualified as Set
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
import Wire.BackupStore hiding (NewBackupFile (..))
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
    -- | Make 'GetDeviceEpochCounts' report no files, simulating a concurrent upload.
    hideCounts :: Bool
  }

type Blobs = Map BackupFileId ByteString

type Effs =
  '[ BackupSubsystem,
     BackupStore,
     BackupBlobStore,
     BrigAPIAccess,
     GalleyAPIAccess,
     Input BackupLimits,
     Random,
     Now,
     Error BackupError,
     State World,
     State BackupStoreState,
     State Blobs,
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
      hideCounts = False
    }

-- | Runs the program; returns final store and blob state and the result.
runBackup ::
  World ->
  Maybe (TVar Word64) ->
  Sem Effs a ->
  IO (BackupStoreState, Blobs, Either BackupError a)
runBackup w counter prog = do
  base <- generate arbitrary
  res <-
    runM
      . runError @BlobUploadError
      . runState mempty
      . runState mempty
      . evalState w
      . runError @BackupError
      . interpretNowConst defaultTime
      . runRandomPure
      . runInputConst limits
      . mockGalley
      . mockBrig base
      . ( \p -> case counter of
            Nothing -> inMemoryBackupBlobStoreInterpreter p
            Just t -> countingBackupBlobStoreInterpreter t p
        )
      . inMemoryBackupStoreInterpreter
      . hideCountsWhenAsked
      . interpretBackupSubsystem
      $ prog
  case res of
    Left e -> error (show e)
    Right (blobs, (store, r)) -> pure (store, blobs, r)

hideCountsWhenAsked ::
  (Member BackupStore r, Member (State World) r) => Sem r a -> Sem r a
hideCountsWhenAsked = intercept $ \case
  GetDeviceEpochCounts u c -> gets (.hideCounts) >>= bool (getDeviceEpochCounts u c) (pure mempty)
  InsertBackupFile l f -> insertBackupFile l f
  ListBackupFiles u c e -> listBackupFiles u c e
  LookupBackupFile u c e f -> lookupBackupFile u c e f
  DeleteBackupFileRow f -> deleteBackupFileRow f
  DeleteEpochRows u c e -> deleteEpochRows u c e
  DeleteClientRows u c -> deleteClientRows u c
  DeleteUserRows u -> deleteUserRows u

mockBrig :: (Member (State World) r) => User -> InterpreterFor BrigAPIAccess r
mockBrig base = interpret $ \case
  GetUsers uids ->
    pure [base {userTeam = if u == uC then Nothing else Just teamT} | u <- uids]
  LookupClients uids -> do
    modify (\w -> w {lookups = w.lookups + 1})
    w <- get
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

-- | Upload of the given size; the body is never read if rejected blobs0 streaming.
uploadSized :: UserId -> ClientId -> Word32 -> Word64 -> Sem Effs BackupFile
uploadSized u c e n =
  uploadBackup (loc u) c c (BackupEpoch e) n (yield (BS.replicate (fromIntegral n) 0))

attempt :: Sem Effs a -> Sem Effs (Either BackupError a)
attempt = try

epochsOf :: UserId -> ClientId -> ClientId -> Sem Effs [Word32]
epochsOf u z c = map (.unBackupEpoch) . (.epochs) <$> listEpochs (loc u) z c

spec :: Spec
spec = describe "BackupSubsystem" $ do
  it "upload then list then download" $ do
    (_, _, res) <- runBackup world0 Nothing $ do
      f <- upload uA a1 a1 1 "hello"
      es <- epochsOf uA a2 a1
      fs <- listFiles (loc uA) a2 a1 (BackupEpoch 1)
      url <- downloadUrl (loc uA) a2 a1 (BackupEpoch 1) f.id
      pure (f.kind, es, length fs.files, serializeURIRef' url, f.id)
    Right (kind, es, n, url, fid) <- pure res
    kind `shouldBe` BackupFull
    es `shouldBe` [1]
    n `shouldBe` 1
    url `shouldBe` "https://blob.test/" <> UUID.toASCIIBytes fid.unBackupFileId

  it "second file is incremental and capped at 10 MiB" $ do
    (_, _, res) <- runBackup world0 Nothing $ do
      _ <- upload uA a1 a1 1 "full"
      f <- upload uA a1 a1 1 "incr"
      tooLarge <- attempt $ uploadSized uA a1 1 (10 * 1024 * 1024 + 1)
      pure (f.kind, tooLarge)
    fmap (fmap void) res `shouldBe` Right (BackupIncremental, Left BackupErrTooLarge)

  it "epoch rule enforced" $ do
    (_, _, res) <-
      runBackup world0 Nothing $ upload uA a1 a1 1 "x" >> attempt (upload uA a1 a1 3 "y")
    void <$> res `shouldBe` Right (Left BackupErrEpochNotAllowed)

  it "restart at high epoch when empty" $ do
    (_, _, res) <- runBackup world0 Nothing $ upload uA a1 a1 6 "x" >> epochsOf uA a1 a1
    res `shouldBe` Right [6]

  it "upload for other device forbidden" $ do
    (st, _, res) <- runBackup world0 Nothing $ upload uA a1 a2 1 "x"
    void res `shouldBe` Left BackupErrForbidden
    st `shouldBe` mempty

  it "foreign client is not found" $ do
    (_, _, res) <- runBackup world0 Nothing $ do
      _ <- upload uA a1 a1 1 "x"
      (,) <$> attempt (epochsOf uB b1 a1) <*> attempt (epochsOf uA a1 b1)
    res `shouldBe` Right (Left BackupErrNotFound, Left BackupErrNotFound)

  it "non-team user cannot upload" $ do
    (_, _, res) <- runBackup world0 Nothing $ upload uC c1 c1 1 "x"
    void res `shouldBe` Left BackupErrFeatureDisabled

  it "feature disabled blocks upload but not read or delete" $ do
    (st, blobs, res) <- runBackup world0 Nothing $ do
      f <- upload uA a1 a1 1 "x"
      modify (\w -> w {feature = FeatureStatusDisabled})
      blocked <- attempt $ upload uA a1 a1 1 "y"
      es <- epochsOf uA a2 a1
      _ <- downloadUrl (loc uA) a2 a1 (BackupEpoch 1) f.id
      deleteEpochBackups (loc uA) a1 a1 (BackupEpoch 1)
      pure (void blocked, es)
    res `shouldBe` Right (Left BackupErrFeatureDisabled, [1])
    st `shouldBe` mempty
    blobs `shouldBe` mempty

  it "delete of empty epoch succeeds" $ do
    (_, _, res) <- runBackup world0 Nothing $ deleteEpochBackups (loc uA) a1 a1 (BackupEpoch 7)
    res `shouldBe` Right ()

  it "delete client removes rows and blobs" $ do
    (st, blobs, res) <- runBackup world0 Nothing $ do
      _ <- upload uA a1 a1 1 "x"
      _ <- upload uA a1 a1 2 "y"
      g <- upload uA a2 a2 1 "z"
      deleteClientBackups (loc uA) a1 a1
      pure g.id
    Right gid <- pure res
    Map.keys st `shouldBe` [gid]
    Map.keys blobs `shouldBe` [gid]

  it "upload by removed client is rolled back" $ do
    (st, blobs, res) <- runBackup world0 {dropA1From = Just 2} Nothing $ upload uA a1 a1 1 "x"
    void res `shouldBe` Left BackupErrForbidden
    st `shouldBe` mempty
    blobs `shouldBe` mempty

  it "rejected insert deletes blob" $ do
    -- The pre-check sees no files (as if racing with a concurrent upload); the insert re-checks.
    (st, blobs, res) <- runBackup world0 Nothing $ do
      replicateM_ 3 (upload uA a1 a1 1 "x")
      blobs0 <- get @Blobs
      modify (\w -> w {hideCounts = True})
      r <- attempt $ upload uA a1 a1 1 "y"
      pure (blobs0, void r)
    Right (blobs0, r) <- pure res
    r `shouldBe` Left BackupErrTooManyFiles
    blobs `shouldBe` blobs0
    Map.size st `shouldBe` 3

  it "internal user delete removes all devices" $ do
    (st, blobs, res) <- runBackup world0 Nothing $ do
      _ <- upload uA a1 a1 1 "x"
      _ <- upload uA a2 a2 1 "y"
      g <- upload uB b1 b1 1 "z"
      internalDeleteUserBackups uA
      pure g.id
    Right gid <- pure res
    Map.keys st `shouldBe` [gid]
    Map.keys blobs `shouldBe` [gid]

  it "upload streams chunks to blob store" $ do
    consumed <- newTVarIO 0
    let n = 64
        size = 16 * 1024
    out <-
      timeout 10_000_000 . runBackup world0 (Just consumed) $
        let len = fromIntegral (n * size)
         in uploadBackup (loc uA) a1 a1 (BackupEpoch 1) len (lockstepSource consumed n size)
    Just (_, blobs, res) <- pure out
    void res `shouldBe` Right ()
    Map.elems blobs `shouldBe` [BS.concat [BS.replicate size (fromIntegral i) | i <- [0 .. n - 1]]]
