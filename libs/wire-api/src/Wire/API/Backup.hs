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
{-# LANGUAGE OverloadedRecordDot #-}

module Wire.API.Backup
  ( BackupEpoch (..),
    BackupFileId (..),
    BackupFile (..),
    BackupFiles (..),
    EpochSummary (..),
    ClientBackups (..),
    BackupList (..),
    BackupSource (..),
    BackupBody,
    epochAccepted,
    staleEpochs,
  )
where

import Conduit
import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString.Conversion (ToByteString (..))
import Data.Id (ClientId, uuidSchema)
import Data.Json.Util (UTCTimeMillis)
import Data.OpenApi qualified as S
import Data.OpenApi.Internal.Schema qualified as OpenApi
import Data.Schema
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Imports
import Servant
import Servant.Conduit ()
import Servant.OpenApi.Internal.Orphans ()
import Test.QuickCheck (choose)
import Wire.Arbitrary (Arbitrary (..), GenericUniform (..))

-- | Epoch number of a client's backup, in @[1, maxBound]@.
newtype BackupEpoch = BackupEpoch Word32
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (Bounded)
  deriving (ToJSON, FromJSON, S.ToSchema) via Schema BackupEpoch

instance ToSchema BackupEpoch where
  schema = (\(BackupEpoch w) -> w) .= withParser schema parseEpoch
    where
      parseEpoch 0 = fail "backup epoch must be at least 1"
      parseEpoch w = pure (BackupEpoch w)

instance Arbitrary BackupEpoch where
  arbitrary = BackupEpoch <$> choose (1, maxBound)

instance S.ToParamSchema BackupEpoch where
  toParamSchema _ = S.toParamSchema (Proxy @Word32)

instance FromHttpApiData BackupEpoch where
  parseUrlPiece t =
    parseUrlPiece t >>= \case
      0 -> Left "backup epoch must be at least 1"
      w -> Right (BackupEpoch w)

instance ToHttpApiData BackupEpoch where
  toUrlPiece (BackupEpoch w) = toUrlPiece w

-- | Id of a backup file; also the S3 object id.
newtype BackupFileId = BackupFileId UUID
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (Arbitrary)
  deriving (ToJSON, FromJSON, S.ToSchema) via Schema BackupFileId

instance ToSchema BackupFileId where
  schema = (\(BackupFileId u) -> u) .= (BackupFileId <$> uuidSchema)

instance S.ToParamSchema BackupFileId where
  toParamSchema _ = S.toParamSchema (Proxy @UUID)

instance FromHttpApiData BackupFileId where
  parseUrlPiece = fmap BackupFileId . parseUrlPiece

instance ToHttpApiData BackupFileId where
  toUrlPiece (BackupFileId u) = toUrlPiece u

instance ToByteString BackupFileId where
  builder (BackupFileId u) = builder (UUID.toText u)

data BackupFile = BackupFile
  { id :: BackupFileId,
    size :: Word64,
    createdAt :: UTCTimeMillis
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform BackupFile)
  deriving (ToJSON, FromJSON, S.ToSchema) via Schema BackupFile

instance ToSchema BackupFile where
  schema =
    object $
      BackupFile
        <$> (.id) .= field "id" schema
        <*> (.size) .= field "size" schema
        <*> (.createdAt) .= field "createdAt" schema

data BackupFiles = BackupFiles
  { full :: BackupFile,
    incrementals :: [BackupFile]
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform BackupFiles)
  deriving (ToJSON, FromJSON, S.ToSchema) via Schema BackupFiles

instance ToSchema BackupFiles where
  schema =
    object $
      BackupFiles
        <$> (.full) .= field "full" schema
        <*> (.incrementals) .= field "incrementals" (array schema)

-- | Summary of the current files of an epoch.
data EpochSummary = EpochSummary
  { epoch :: BackupEpoch,
    -- | Creation time of the newest current file.
    createdAt :: UTCTimeMillis,
    -- | Number of current files, including the full.
    fileCount :: Word32,
    -- | Total size of the current files in bytes.
    size :: Word64
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform EpochSummary)
  deriving (ToJSON, FromJSON, S.ToSchema) via Schema EpochSummary

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
    -- | Ascending by epoch.
    epochs :: [EpochSummary]
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform ClientBackups)
  deriving (ToJSON, FromJSON, S.ToSchema) via Schema ClientBackups

instance ToSchema ClientBackups where
  schema =
    object $
      ClientBackups
        <$> (.client) .= field "client" schema
        <*> (.epochs) .= field "epochs" (array schema)

newtype BackupList = BackupList
  { -- | Ascending by client id; clients without epochs are absent.
    clients :: [ClientBackups]
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform BackupList)
  deriving (ToJSON, FromJSON, S.ToSchema) via Schema BackupList

instance ToSchema BackupList where
  schema =
    object $
      BackupList
        <$> (.clients) .= field "clients" (array schema)

-- | Raw request body, streamed (never buffered).
newtype BackupSource = BackupSource
  { getBackupSource :: ConduitT () ByteString (ResourceT IO) ()
  }
  deriving newtype (FromSourceIO ByteString, ToSourceIO ByteString)

instance S.ToSchema BackupSource where
  declareNamedSchema _ = pure $ OpenApi.named "BackupSource" mempty

type BackupBody =
  StreamBody'
    '[Description "Raw backup bytes"]
    NoFraming
    OctetStream
    BackupSource

-- | Is an upload for the given epoch allowed, given the client's current maximum epoch?
-- Without epochs only @1@ is accepted, otherwise the maximum itself (replace) and its
-- successor (new epoch). Never overflows at 'maxBound'.
epochAccepted :: Maybe BackupEpoch -> BackupEpoch -> Bool
epochAccepted Nothing e = e == BackupEpoch 1
epochAccepted (Just (BackupEpoch m)) (BackupEpoch e) = e == m || (m < maxBound && e == m + 1)

-- | Epochs @<= staleEpochs e@ are removed when @e@ is created. Callers must only use it
-- when @e >= 3@.
staleEpochs :: BackupEpoch -> BackupEpoch
staleEpochs (BackupEpoch e) = BackupEpoch (e - 2)
