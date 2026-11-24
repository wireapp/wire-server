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
        goneIds plan Set.empty @?= [a],
      testCase "versioningErrorListsVersions" $ do
        versioningFrom (Left ()) @?= VersioningMaybeOn
        versioningFrom (Right (Just ())) @?= VersioningMaybeOn
        versioningFrom (Right Nothing :: Either () (Maybe ())) @?= VersioningNeverEnabled,
      testCase "neverEnabledKeysOnly" $
        deleteBatches (planDeletion [a, b] VersioningNeverEnabled mempty)
          @?= [[ObjectTarget ka Nothing, ObjectTarget kb Nothing]],
      testCase "versionedDeletesAllVersionsAndMarkers" $ do
        let ts = [ObjectTarget ka (Just v) | v <- ["v1", "v2", "m1"]]
            plan = planDeletion [a] VersioningMaybeOn (Map.fromList [(a, Listed ts)])
        deleteBatches plan @?= [ts]
        goneIds plan Set.empty @?= [a],
      testCase "versionedIgnoresForeignKeys" $ do
        let own = ObjectTarget ka (Just "v1")
            foreign' = ObjectTarget (ka <> "x") (Just "v1")
            plan = planDeletion [a] VersioningMaybeOn (Map.fromList [(a, Listed [own, foreign'])])
        deleteBatches plan @?= [[own]],
      testCase "emptyListingIsGone" $ do
        let plan = planDeletion [a] VersioningMaybeOn (Map.fromList [(a, Listed [])])
        deleteBatches plan @?= []
        goneIds plan Set.empty @?= [a],
      testCase "listingFailedNotGone" $ do
        let ta = ObjectTarget ka (Just "v1")
            plan =
              planDeletion
                [a, b]
                VersioningMaybeOn
                (Map.fromList [(a, Listed [ta]), (b, ListingFailed)])
        deleteBatches plan @?= [[ta]]
        goneIds plan Set.empty @?= [a],
      testCase "partialDeleteErrors" $ do
        let va v = ObjectTarget ka (Just v)
            plan =
              planDeletion
                [a, b]
                VersioningMaybeOn
                ( Map.fromList
                    [ (a, Listed [va "v1", va "v2"]),
                      (b, Listed [ObjectTarget kb (Just "v1")])
                    ]
                )
        goneIds plan (Set.singleton (va "v2")) @?= [b],
      testCase "classifyNotImplemented" $ do
        classifyListError 501 "" @?= ListNotImplemented
        classifyListError 400 "NotImplemented" @?= ListNotImplemented
        classifyListError 403 "AccessDenied" @?= ListOtherError,
      QC.testProperty "versionedBatchesBounded" $
        QC.forAll (QC.choose (0, 2500)) propVersionedBatchesBounded
    ]

ka, kb :: Text
ka = backupKey a
kb = backupKey b

-- | Versions spread over files (n in total).
propVersionedBatchesBounded :: Int -> QC.Property
propVersionedBatchesBounded n =
  let perFile =
        [ (i, [ObjectTarget (backupKey i) (Just (fromString (show j))) | j <- [lo .. hi]])
        | (i, (lo, hi)) <- zip (ids 3) [(1, n `div` 2), (n `div` 2 + 1, n `div` 2 + 7), (n `div` 2 + 8, n)]
        ]
      plan = planDeletion (map fst perFile) VersioningMaybeOn (Map.fromList [(i, Listed ts) | (i, ts) <- perFile])
      batches = deleteBatches plan
   in QC.conjoin
        [ QC.counterexample "batch size" (all (\batch -> not (null batch) && length batch <= maxDeleteBatch) batches),
          QC.counterexample "concat" (sort (concat batches) QC.=== sort (concatMap snd perFile))
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
