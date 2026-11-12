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

-- | Used when no backup HMAC key is configured: no backup can exist, so reads and uploads
-- are rejected and deletes succeed as no-ops (brig's client/user deletion keeps working).
module Wire.BackupSubsystem.Disabled (interpretBackupSubsystemDisabled) where

import Imports
import Polysemy
import Polysemy.Error
import Wire.BackupSubsystem

interpretBackupSubsystemDisabled :: (Member (Error BackupError) r) => InterpreterFor BackupSubsystem r
interpretBackupSubsystemDisabled = interpret $ \case
  ListEpochs {} -> throw BackupErrFeatureDisabled
  UploadBackup {} -> throw BackupErrFeatureDisabled
  ListFiles {} -> throw BackupErrFeatureDisabled
  DownloadUrl {} -> throw BackupErrFeatureDisabled
  DeleteEpochBackups {} -> pure ()
  DeleteClientBackups {} -> pure ()
  InternalDeleteClientBackups {} -> pure ()
  InternalDeleteUserBackups {} -> pure ()
