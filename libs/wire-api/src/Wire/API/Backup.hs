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
    BackupKind (..),
    BackupFile (..),
    BackupEpochs (..),
    BackupFiles (..),
  )
where

import Control.Lens ((?~))
import Data.Aeson (FromJSON, ToJSON)
import Data.Id (uuidSchema)
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

data BackupKind = BackupFull | BackupIncremental
  deriving stock (Eq, Ord, Show, Generic, Bounded, Enum)
  deriving (Arbitrary) via (GenericUniform BackupKind)
  deriving (FromJSON, ToJSON, S.ToSchema) via (Schema BackupKind)

instance ToSchema BackupKind where
  schema =
    enum @Text $
      mconcat
        [ element "full" BackupFull,
          element "incremental" BackupIncremental
        ]

instance ToHttpApiData BackupKind where
  toUrlPiece BackupFull = "full"
  toUrlPiece BackupIncremental = "incremental"

instance FromHttpApiData BackupKind where
  parseUrlPiece "full" = Right BackupFull
  parseUrlPiece "incremental" = Right BackupIncremental
  parseUrlPiece _ = Left "backup kind must be full or incremental"

instance S.ToParamSchema BackupKind where
  toParamSchema _ =
    mempty
      & S.type_ ?~ S.OpenApiString
      & S.enum_ ?~ ["full", "incremental"]

data BackupFile = BackupFile
  { id :: BackupFileId,
    kind :: BackupKind,
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
        <*> (.kind) .= field "kind" schema
        <*> (.size) .= field "size" schema
        <*> (.createdAt) .= field "createdAt" schema

newtype BackupEpochs = BackupEpochs {epochs :: [BackupEpoch]}
  deriving stock (Eq, Show)
  deriving newtype (Arbitrary)
  deriving (FromJSON, ToJSON, S.ToSchema) via (Schema BackupEpochs)

instance ToSchema BackupEpochs where
  schema =
    object $
      BackupEpochs <$> (.epochs) .= field "epochs" (array schema)

newtype BackupFiles = BackupFiles {files :: [BackupFile]}
  deriving stock (Eq, Show)
  deriving newtype (Arbitrary)
  deriving (FromJSON, ToJSON, S.ToSchema) via (Schema BackupFiles)

instance ToSchema BackupFiles where
  schema =
    object $
      BackupFiles <$> (.files) .= field "files" (array schema)
