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
import Wire.BackupSubsystem.Rules

limits :: BackupLimits
limits =
  BackupLimits
    { maxFullBytes = 1073741824,
      maxIncrementalBytes = 10485760,
      maxFilesPerEpoch = 3650
    }

-- | @check kind [(epoch, files)] target size@; the target epoch has a full file iff it has files.
check :: BackupKind -> [(Word32, Word32)] -> Word32 -> Word64 -> Either UploadRejection ()
check kind m e =
  checkUpload
    limits
    kind
    (Set.fromList (map (BackupEpoch . fst) m))
    (files > 0)
    files
    (BackupEpoch e)
  where
    files = fromMaybe 0 (lookup e m)

fid :: Word64 -> BackupFileId
fid n = BackupFileId (UUID.fromWords64 n n)

blob :: BackupKind -> Word64 -> Word64 -> BackupBlob
blob kind n size = BackupBlob {kind, fileId = fid n, size}

spec :: Spec
spec = describe "Rules" $ do
  describe "checkUpload" $ do
    let full = check BackupFull
        inc = check BackupIncremental
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
      it "incremental into epoch without full is rejected" $
        checkUpload limits BackupIncremental (Set.singleton (BackupEpoch 1)) False 0 (BackupEpoch 1) 1
          `shouldBe` Left FullMissing
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
         in isLeft (check (if k then BackupFull else BackupIncremental) m' 0 s)

  describe "currentBlobs" $ do
    it "is the newest full, then incrementals after it by id" $
      currentBlobs
        [ blob BackupIncremental 5 1,
          blob BackupFull 2 5,
          blob BackupIncremental 1 1,
          blob BackupFull 4 6,
          blob BackupIncremental 3 1,
          blob BackupIncremental 6 1
        ]
        `shouldBe` [blob BackupFull 4 6, blob BackupIncremental 5 1, blob BackupIncremental 6 1]
    it "is empty without a full" $ currentBlobs [blob BackupIncremental 1 1] `shouldBe` []

  describe "epochSummary" $ do
    let e = BackupEpoch 4
    it "summarises the current files only" $
      fmap
        (\x -> (x.epoch, x.fileCount, x.size))
        ( epochSummary
            e
            [ blob BackupFull 1 5,
              blob BackupIncremental 2 1,
              blob BackupFull 3 7,
              blob BackupIncremental 4 2
            ]
        )
        `shouldBe` Just (e, 2, 9)
    it "has the creation time of the newest current file" $
      fmap (.createdAt) (epochSummary e [blob BackupFull 3 7, blob BackupIncremental 4 2])
        `shouldBe` Just (toFile (blob BackupIncremental 4 2)).createdAt
    it "is Nothing without a full" $ epochSummary e [blob BackupIncremental 1 1] `shouldBe` Nothing

  describe "pruneTargets" $ do
    it "is what a newer full superseded" $
      pruneTargets [blob BackupFull 1 5, blob BackupIncremental 2 1, blob BackupFull 3 5]
        `shouldBe` [blob BackupFull 1 5, blob BackupIncremental 2 1]
    it "is empty if everything is current" $
      pruneTargets [blob BackupFull 1 5, blob BackupIncremental 2 1] `shouldBe` []
    it "is empty without a full" $ pruneTargets [blob BackupIncremental 2 1] `shouldBe` []

  describe "staleEpochs" $ do
    let es = staleEpochs . Set.fromList . map BackupEpoch
    it "keeps the two newest" $ es [1, 2, 3, 5] `shouldBe` [BackupEpoch 1, BackupEpoch 2]
    it "keeps fewer than three" $ es [1, 2] `shouldBe` []
