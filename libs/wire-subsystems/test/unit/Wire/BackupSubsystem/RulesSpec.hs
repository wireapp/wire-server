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
import Imports
import Test.Hspec
import Test.Hspec.QuickCheck
import Wire.API.Backup
import Wire.BackupSubsystem.Rules

limits :: BackupLimits
limits =
  BackupLimits
    { maxFullBytes = 1073741824,
      maxIncrementalBytes = 10485760,
      maxFilesPerEpoch = 3650
    }

-- | @check [(epoch, files)] target size@
check :: [(Word32, Word32)] -> Word32 -> Word64 -> Either UploadRejection BackupKind
check m e =
  checkUpload
    limits
    (Set.fromList (map (BackupEpoch . fst) m))
    (fromMaybe 0 (lookup e m))
    (BackupEpoch e)

spec :: Spec
spec = describe "Rules.checkUpload" $ do
  describe "epoch rule" $ do
    it "{} + 1 -> full" $ check [] 1 1 `shouldBe` Right BackupFull
    it "{} + 6 -> full" $ check [] 6 1 `shouldBe` Right BackupFull
    it "{} + 0 -> rejected" $ check [] 0 1 `shouldBe` Left EpochNotAllowed
    it "{3:1} + 3 -> incremental" $ check [(3, 1)] 3 1 `shouldBe` Right BackupIncremental
    it "{3:1} + 4 -> full" $ check [(3, 1)] 4 1 `shouldBe` Right BackupFull
    it "{3:1} + 5 -> rejected" $ check [(3, 1)] 5 1 `shouldBe` Left EpochNotAllowed
    it "{3:1,4:1} + 3 -> rejected" $
      check [(3, 1), (4, 1)] 3 1 `shouldBe` Left EpochNotAllowed
    it "{3:1,4:1} + 4 -> incremental" $
      check [(3, 1), (4, 1)] 4 1 `shouldBe` Right BackupIncremental
    it "{3:1,4:1} + 5 -> rejected" $
      check [(3, 1), (4, 1)] 5 1 `shouldBe` Left EpochNotAllowed
    it "{maxBound:1} + maxBound -> incremental" $
      check [(maxBound, 1)] maxBound 1 `shouldBe` Right BackupIncremental
  describe "size rule" $ do
    it "full at cap OK" $ check [] 1 1073741824 `shouldBe` Right BackupFull
    it "full above cap rejected" $ check [] 1 1073741825 `shouldBe` Left TooLarge
    it "incremental at cap OK" $
      check [(1, 1)] 1 10485760 `shouldBe` Right BackupIncremental
    it "incremental above cap rejected" $
      check [(1, 1)] 1 10485761 `shouldBe` Left TooLarge
  describe "count rule" $ do
    it "{3:3650} + 3 -> too many" $ check [(3, 3650)] 3 1 `shouldBe` Left TooManyFiles
    it "{3:3649} + 3 -> OK" $ check [(3, 3649)] 3 1 `shouldBe` Right BackupIncremental
  prop "never accepts epoch 0" $
    \(m :: [(Word32, Word32)]) (s :: Word64) (useMax :: Bool) ->
      let m' = if useMax then (maxBound, 1) : m else m
       in isLeft (check m' 0 s)
