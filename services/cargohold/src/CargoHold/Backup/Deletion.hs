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

-- | Pure planning of S3 deletions of backup files (no amazonka types).
module CargoHold.Backup.Deletion
  ( ObjectTarget (..),
    DeletionPlan (..),
    backupKey,
    maxDeleteBatch,
    keysOnlyPlan,
    Versioning (..),
    versioningFrom,
    Listing (..),
    ListError (..),
    classifyListError,
    planDeletion,
    deleteBatches,
    goneIds,
  )
where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.UUID as UUID
import Imports
import Wire.API.Backup (BackupFileId (..))

-- | An S3 object (version) to delete.
data ObjectTarget = ObjectTarget
  { key :: Text,
    versionId :: Maybe Text
  }
  deriving (Eq, Ord, Show)

data DeletionPlan = DeletionPlan
  { -- | The objects of each file.
    targets :: Map BackupFileId [ObjectTarget],
    -- | Files whose objects could not be determined: never reported as gone.
    unlisted :: Set BackupFileId
  }

-- | S3 key of a backup file.
backupKey :: BackupFileId -> Text
backupKey (BackupFileId u) = "backups/" <> UUID.toText u

-- | S3 accepts at most 1000 keys per @DeleteObjects@ call.
maxDeleteBatch :: Int
maxDeleteBatch = 1000

-- | One versionless target per file.
keysOnlyPlan :: [BackupFileId] -> DeletionPlan
keysOnlyPlan ids =
  DeletionPlan
    { targets = Map.fromList [(i, [ObjectTarget (backupKey i) Nothing]) | i <- ids],
      unlisted = Set.empty
    }

-- | All targets in chunks of at most 'maxDeleteBatch'; no chunk is empty.
deleteBatches :: DeletionPlan -> [[ObjectTarget]]
deleteBatches plan = chunks (concat (Map.elems plan.targets))
  where
    chunks [] = []
    chunks ts = let (batch, rest) = splitAt maxDeleteBatch ts in batch : chunks rest

-- | The files that are gone, given the targets that failed to delete: all targets of a file
-- must have succeeded (a file without targets is gone). Unlisted files are never gone.
goneIds :: DeletionPlan -> Set ObjectTarget -> [BackupFileId]
goneIds plan failed =
  [ fid
  | (fid, ts) <- Map.toList plan.targets,
    fid `Set.notMember` plan.unlisted,
    all (`Set.notMember` failed) ts
  ]

-- | Versioning state of the bucket.
data Versioning = VersioningNeverEnabled | VersioningMaybeOn
  deriving (Eq, Show)

-- | No status means versioning was never enabled. An error means unknown: take the safe path.
versioningFrom :: Either e (Maybe s) -> Versioning
versioningFrom (Right Nothing) = VersioningNeverEnabled
versioningFrom _ = VersioningMaybeOn

-- | Result of listing the object versions of one file.
data Listing = Listed [ObjectTarget] | ListingFailed

data ListError = ListNotImplemented | ListOtherError
  deriving (Eq, Show)

-- | Stores without @ListObjectVersions@ answer 501 or the error code @NotImplemented@.
classifyListError :: Int -> Text -> ListError
classifyListError 501 _ = ListNotImplemented
classifyListError _ "NotImplemented" = ListNotImplemented
classifyListError _ _ = ListOtherError

-- | Without versioning only the keys are deleted. Otherwise all listed versions and delete
-- markers with exactly the key of the file are deleted; files without (successful) listing
-- are unlisted.
planDeletion :: [BackupFileId] -> Versioning -> Map BackupFileId Listing -> DeletionPlan
planDeletion ids VersioningNeverEnabled _ = keysOnlyPlan ids
planDeletion ids VersioningMaybeOn listings =
  DeletionPlan
    { targets = Map.fromList [(i, exact i ts) | (i, Listed ts) <- listed],
      unlisted = Set.fromList [i | (i, ListingFailed) <- listed]
    }
  where
    listed = [(i, fromMaybe ListingFailed (Map.lookup i listings)) | i <- ids]
    exact i = filter ((== backupKey i) . (.key))
