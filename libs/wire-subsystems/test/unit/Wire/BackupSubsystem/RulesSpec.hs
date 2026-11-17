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

module Wire.BackupSubsystem.RulesSpec (spec) where

import Data.Set qualified as Set
import Data.UUID qualified as UUID
import Imports
import Test.Hspec
import Test.Hspec.QuickCheck
import Wire.API.Backup
import Wire.BackupBlobStore
import Wire.BackupBlobStore.Key (BackupFileRef (..))
import Wire.BackupSubsystem.Rules

limits :: BackupLimits
limits =
  BackupLimits
    { maxFullBytes = 1073741824,
      maxIncrementalBytes = 10485760,
      maxFilesPerEpoch = 3650
    }

-- | @check base [(epoch, files)] target size@; the target epoch has files iff it has a count: a
-- full file with id 1 and incremental files under it.
check :: Maybe BackupFileId -> [(Word32, Word32)] -> Word32 -> Word64 -> Either UploadRejection ()
check base m e =
  checkUpload
    limits
    base
    (Set.fromList (map (BackupEpoch . fst) m))
    (if files > 0 then fullB 1 1 : [incB 1 n 1 | n <- [2 .. fromIntegral files]] else [])
    (BackupEpoch e)
  where
    files = fromMaybe 0 (lookup e m)

fid :: Word64 -> BackupFileId
fid n = BackupFileId (UUID.fromWords64 n n)

fullB :: Word64 -> Word64 -> BackupBlob
fullB n size = BackupBlob {file = FullFile (fid n), size}

-- | @incB full n size@: incremental file @n@ under full file @full@
incB :: Word64 -> Word64 -> Word64 -> BackupBlob
incB f n size = BackupBlob {file = IncrementalFile (fid f) (fid n), size}

spec :: Spec
spec = describe "Rules" $ do
  describe "checkUpload" $ do
    let full = check Nothing
        inc = check (Just (fid 1))
    describe "epoch rule" $ do
      it "{} + 1 -> full" $ full [] 1 1 `shouldBe` Right ()
      it "{} + 6 -> full" $ full [] 6 1 `shouldBe` Right ()
      it "{} + 0 -> rejected" $ full [] 0 1 `shouldBe` Left EpochNotAllowed
      it "{3:1} + 3 -> incremental" $ inc [(3, 1)] 3 1 `shouldBe` Right ()
      it "{3:1} + 4 -> full" $ full [(3, 1)] 4 1 `shouldBe` Right ()
      it "{3:1} + 5 -> rejected" $ full [(3, 1)] 5 1 `shouldBe` Left EpochNotAllowed
      it "{3:1,4:1} + 3 -> rejected" $
        inc [(3, 1), (4, 1)] 3 1 `shouldBe` Left EpochNotAllowed
      it "{3:1,4:1} + 5 -> full" $ full [(3, 1), (4, 1)] 5 1 `shouldBe` Right ()
      it "{3:1,4:1} + 4 -> incremental" $ inc [(3, 1), (4, 1)] 4 1 `shouldBe` Right ()
      it "{3:1,4:1} + 6 -> rejected" $
        full [(3, 1), (4, 1)] 6 1 `shouldBe` Left EpochNotAllowed
      it "{maxBound:1} + maxBound -> incremental" $
        inc [(maxBound, 1)] maxBound 1 `shouldBe` Right ()
    describe "kind rule" $ do
      it "full into epoch with full is accepted (replaces)" $ full [(1, 1)] 1 1 `shouldBe` Right ()
      it "incremental into empty epoch is rejected" $ inc [] 1 1 `shouldBe` Left FullMissing
      it "incremental under an unknown full is rejected" $
        check (Just (fid 9)) [(1, 1)] 1 1 `shouldBe` Left FullMissing
      it "incremental into epoch with incrementals only is rejected" $
        checkUpload limits (Just (fid 1)) (Set.singleton (BackupEpoch 1)) [incB 1 2 1] (BackupEpoch 1) 1
          `shouldBe` Left FullMissing
      it "incremental under an older full is rejected" $
        checkUpload limits (Just (fid 1)) (Set.singleton (BackupEpoch 1)) [fullB 1 1, fullB 3 1] (BackupEpoch 1) 1
          `shouldBe` Left FullMissing
      it "incremental under the newest of several fulls is accepted" $
        checkUpload limits (Just (fid 3)) (Set.singleton (BackupEpoch 1)) [fullB 1 1, fullB 3 1] (BackupEpoch 1) 1
          `shouldBe` Right ()
    describe "size rule" $ do
      it "full at cap OK" $ full [] 1 1073741824 `shouldBe` Right ()
      it "full above cap rejected" $ full [] 1 1073741825 `shouldBe` Left TooLarge
      it "incremental at cap OK" $ inc [(1, 1)] 1 10485760 `shouldBe` Right ()
      it "incremental above cap rejected" $ inc [(1, 1)] 1 10485761 `shouldBe` Left TooLarge
    describe "count rule" $ do
      it "{3:3650} + 3 -> too many" $ inc [(3, 3650)] 3 1 `shouldBe` Left TooManyFiles
      it "{3:3649} + 3 -> OK" $ inc [(3, 3649)] 3 1 `shouldBe` Right ()
    prop "never accepts epoch 0" $
      \(m :: [(Word32, Word32)]) (s :: Word64) (useMax :: Bool) (k :: Bool) ->
        let m' = if useMax then (maxBound, 1) : m else m
         in isLeft (check (if k then Nothing else Just (fid 1)) m' 0 s)

  describe "currentBlobs" $ do
    it "is the newest full, then the incrementals under it by id" $
      currentBlobs
        [ incB 4 5 1,
          fullB 2 5,
          incB 2 1 1,
          fullB 4 6,
          incB 2 3 1,
          incB 4 6 1
        ]
        `shouldBe` [fullB 4 6, incB 4 5 1, incB 4 6 1]
    it "excludes incrementals of an older full even with greater ids" $
      currentBlobs [fullB 1 5, fullB 2 5, incB 1 9 1] `shouldBe` [fullB 2 5]
    it "is empty without a full" $ currentBlobs [incB 1 1 1] `shouldBe` []

  describe "currentFiles" $ do
    it "splits the full from the incrementals" $
      currentFiles [incB 1 3 1, fullB 1 5, incB 1 2 1]
        `shouldBe` Just (fullB 1 5, [incB 1 2 1, incB 1 3 1])
    it "is Nothing without a full" $ currentFiles [incB 1 2 1] `shouldBe` Nothing

  describe "epochSummary" $ do
    let e = BackupEpoch 4
    it "summarises the current files only" $
      fmap
        (\x -> (x.epoch, x.fileCount, x.size))
        ( epochSummary
            e
            [ fullB 1 5,
              incB 1 2 1,
              fullB 3 7,
              incB 3 4 2,
              incB 1 5 100
            ]
        )
        `shouldBe` Just (e, 2, 9)
    it "has the creation time of the newest current file" $
      fmap (.createdAt) (epochSummary e [fullB 3 7, incB 3 4 2])
        `shouldBe` Just (toFile (incB 3 4 2)).createdAt
    it "is Nothing without a full" $ epochSummary e [incB 1 1 1] `shouldBe` Nothing

  describe "pruneTargets" $ do
    it "is what a newer full superseded" $
      pruneTargets [fullB 1 5, incB 1 2 1, fullB 3 5]
        `shouldBe` [fullB 1 5, incB 1 2 1]
    it "includes incrementals of an older full with a greater id (orphans)" $
      pruneTargets [fullB 1 5, fullB 2 5, incB 1 9 1] `shouldBe` [fullB 1 5, incB 1 9 1]
    it "is empty if everything is current" $
      pruneTargets [fullB 1 5, incB 1 2 1] `shouldBe` []
    it "is empty without a full" $ pruneTargets [incB 1 2 1] `shouldBe` []

  describe "staleEpochs" $ do
    let es = staleEpochs . Set.fromList . map BackupEpoch
    it "keeps the two newest" $ es [1, 2, 3, 5] `shouldBe` [BackupEpoch 1, BackupEpoch 2]
    it "keeps fewer than three" $ es [1, 2] `shouldBe` []
