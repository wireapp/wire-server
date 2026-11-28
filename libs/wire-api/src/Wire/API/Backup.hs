-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.API.Backup
  ( BackupEpoch (..),
    BackupFileId (..),
    BackupFile (..),
    BackupFiles (..),
    EpochSummary (..),
    ClientBackups (..),
    UserBackups (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Id (ClientId, uuidSchema)
import Data.Json.Util (UTCTimeMillis)
import Data.OpenApi qualified as S
import Data.Schema
import Data.UUID (UUID)
import Imports
import Test.QuickCheck (Arbitrary (..), choose)
import Web.HttpApiData (FromHttpApiData (..), ToHttpApiData (..))
import Wire.Arbitrary (GenericUniform (..))

-- | Backup epoch number. Distinct from MLS 'Wire.API.MLS.Epoch.Epoch'; valid values are >= 1.
newtype BackupEpoch = BackupEpoch {unBackupEpoch :: Word32}
  deriving stock (Eq, Ord, Show)
  deriving newtype (ToSchema, S.ToParamSchema)
  deriving (FromJSON, ToJSON, S.ToSchema) via (Schema BackupEpoch)

instance FromHttpApiData BackupEpoch where
  parseUrlPiece t = do
    n <- parseUrlPiece t
    when (n == 0) $ Left "backup epoch must be >= 1"
    pure (BackupEpoch n)

instance ToHttpApiData BackupEpoch where
  toUrlPiece = toUrlPiece . unBackupEpoch

instance Arbitrary BackupEpoch where
  arbitrary = BackupEpoch <$> choose (1, maxBound)

newtype BackupFileId = BackupFileId {unBackupFileId :: UUID}
  deriving stock (Eq, Ord, Show)
  deriving newtype (Arbitrary, FromHttpApiData, ToHttpApiData, S.ToParamSchema)
  deriving (FromJSON, ToJSON, S.ToSchema) via (Schema BackupFileId)

instance ToSchema BackupFileId where
  schema = BackupFileId <$> unBackupFileId .= uuidSchema

data BackupFile = BackupFile
  { id :: BackupFileId,
    size :: Word64,
    createdAt :: UTCTimeMillis
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform BackupFile)
  deriving (FromJSON, ToJSON, S.ToSchema) via (Schema BackupFile)

instance ToSchema BackupFile where
  schema =
    object $
      BackupFile
        <$> (.id) .= field "id" schema
        <*> (.size) .= field "size" schema
        <*> (.createdAt) .= field "createdAt" schema

-- | The current files of an epoch: the newest full file and the incremental files stored under
-- it (by id). The id of the full file is the @fullId@ in the paths of all of these files.
data BackupFiles = BackupFiles
  { full :: BackupFile,
    incrementals :: [BackupFile]
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform BackupFiles)
  deriving (FromJSON, ToJSON, S.ToSchema) via (Schema BackupFiles)

instance ToSchema BackupFiles where
  schema =
    object $
      BackupFiles
        <$> (.full) .= field "full" schema
        <*> (.incrementals) .= field "incrementals" (array schema)

-- | Summary of the current files of one epoch: the newest full file and the incremental files
-- after it.
data EpochSummary = EpochSummary
  { epoch :: BackupEpoch,
    -- | Creation time of the newest current file
    createdAt :: UTCTimeMillis,
    -- | Number of current files, including the full file
    fileCount :: Word32,
    -- | Total size of the current files in bytes
    size :: Word64
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform EpochSummary)
  deriving (FromJSON, ToJSON, S.ToSchema) via (Schema EpochSummary)

instance ToSchema EpochSummary where
  schema =
    object $
      EpochSummary
        <$> (.epoch) .= field "epoch" schema
        <*> (.createdAt) .= field "createdAt" schema
        <*> (.fileCount) .= field "fileCount" schema
        <*> (.size) .= field "size" schema

data ClientBackups = ClientBackups
  { client :: ClientId,
    epochs :: [EpochSummary]
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform ClientBackups)
  deriving (FromJSON, ToJSON, S.ToSchema) via (Schema ClientBackups)

instance ToSchema ClientBackups where
  schema =
    object $
      ClientBackups
        <$> (.client) .= field "client" schema
        <*> (.epochs) .= field "epochs" (array schema)

newtype UserBackups = UserBackups {clients :: [ClientBackups]}
  deriving stock (Eq, Show)
  deriving newtype (Arbitrary)
  deriving (FromJSON, ToJSON, S.ToSchema) via (Schema UserBackups)

instance ToSchema UserBackups where
  schema =
    object $
      UserBackups <$> (.clients) .= field "clients" (array schema)
