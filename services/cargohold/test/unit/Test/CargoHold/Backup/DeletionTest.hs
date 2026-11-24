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
module Test.CargoHold.Backup.DeletionTest (tests) where

import CargoHold.Backup.Deletion
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.UUID as UUID
import Imports
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC
import Wire.API.Backup (BackupFileId (..))

tests :: TestTree
tests =
  testGroup
    "CargoHold.Backup.Deletion"
    [ testCase "backupKey" $
        backupKey (BackupFileId UUID.nil) @?= "backups/00000000-0000-0000-0000-000000000000",
      QC.testProperty "batchesBounded" $
        QC.forAll (QC.choose (0, 2500)) propBatchesBounded,
      testCase "2001 ids make 3 batches" $
        length (deleteBatches (keysOnlyPlan (ids 2001))) @?= 3,
      testCase "goneIdsExcludesFailed" $ do
        let plan = keysOnlyPlan [a, b]
        goneIds plan (Set.singleton (ObjectTarget (backupKey a) Nothing)) @?= [b]
        goneIds plan Set.empty @?= [a, b],
      testCase "goneIds: unlisted is never gone, no targets is gone" $ do
        let plan =
              DeletionPlan
                { targets = Map.fromList [(a, []), (b, [])],
                  unlisted = Set.singleton b
                }
        goneIds plan Set.empty @?= [a]
    ]

a, b :: BackupFileId
a = BackupFileId (UUID.fromWords 0 0 0 1)
b = BackupFileId (UUID.fromWords 0 0 0 2)

-- | Ascending ids.
ids :: Int -> [BackupFileId]
ids n = [BackupFileId (UUID.fromWords 0 0 0 (fromIntegral i)) | i <- [1 .. n]]

propBatchesBounded :: Int -> QC.Property
propBatchesBounded n =
  let batches = deleteBatches (keysOnlyPlan (ids n))
   in QC.conjoin
        [ QC.counterexample "batch size" (all (\batch -> not (null batch) && length batch <= maxDeleteBatch) batches),
          QC.counterexample "concat" (concat batches QC.=== [ObjectTarget (backupKey i) Nothing | i <- ids n])
        ]
