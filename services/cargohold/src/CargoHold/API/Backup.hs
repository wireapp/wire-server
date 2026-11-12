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

module CargoHold.API.Backup (backupServer) where

import Imports
import Polysemy
import Servant.API
import Servant.Server (ServerT)
import Wire.API.Asset (AssetLocation (..))
import Wire.API.Routes.AssetBody (AssetSource (..))
import Wire.API.Routes.Named
import Wire.API.Routes.Public.Cargohold.Backup (BackupAPI)
import Wire.BackupSubsystem

-- | 'backup-download' never answers 'Nothing': a missing file is thrown as 'BackupErrNotFound'
-- like in @assets-download@. The list routes never throw 'BackupErrForbidden' or
-- 'BackupErrFeatureDisabled' (the subsystem only checks read access there).
backupServer :: (Member BackupSubsystem r) => ServerT BackupAPI (Sem r)
backupServer =
  Named @"backup-list-epochs" listEpochs
    :<|> Named @"backup-upload" (\u z c e len body -> uploadBackup u z c e len body.getAssetSource)
    :<|> Named @"backup-list-files" listFiles
    :<|> Named @"backup-download" (\u z c e fid -> Just . AssetLocation <$> downloadUrl u z c e fid)
    :<|> Named @"backup-delete-epoch" deleteEpochBackups
    :<|> Named @"backup-delete-client" deleteClientBackups
