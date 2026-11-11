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

module Wire.API.Routes.Public.Cargohold.Backup where

import Data.Id
import Imports
import Servant
import URI.ByteString (Absolute)
import Wire.API.Asset (AssetLocation)
import Wire.API.Backup
import Wire.API.Error
import Wire.API.Error.Cargohold
import Wire.API.Routes.AssetBody (AssetSource)
import Wire.API.Routes.MultiVerb
import Wire.API.Routes.Named
import Wire.API.Routes.Public
import Wire.API.Routes.Version

type BackupLocationHeader =
  '[DescHeader "Location" "Backup file location" (AssetLocation Absolute)]

type BackupRedirect =
  WithHeaders
    BackupLocationHeader
    (AssetLocation Absolute)
    (RespondEmpty 302 "Backup file found")

-- | Raw file bytes, streamed to the blob store.
type BackupBody =
  StreamBody'
    '[Description "The raw backup file bytes."]
    NoFraming
    OctetStream
    AssetSource

type DownloadLinkDescription =
  "Presigned URLs are bearer links valid for `downloadLinkTTL` seconds and expiry is \
  \checked at request start, so the list returns no URLs and each file is fetched via \
  \its own redirect."

type BackupAPI =
  Named
    "backup-list-epochs"
    ( Summary "List the epochs that have backup files for a client"
        :> From 'V19
        :> CanThrow 'BackupNotFound
        :> ZLocalUser
        :> ZClient
        :> "backups"
        :> "clients"
        :> Capture "cid" ClientId
        :> "epochs"
        :> Get '[JSON] BackupEpochs
    )
    :<|> Named
           "backup-upload"
           ( Summary "Upload a backup file"
               :> From 'V19
               :> CanThrow 'BackupNotFound
               :> CanThrow 'BackupForbidden
               :> CanThrow 'BackupFeatureDisabled
               :> CanThrow 'BackupEpochNotAllowed
               :> CanThrow 'BackupTooManyFiles
               :> CanThrow 'BackupTooLarge
               :> CanThrow 'InvalidLength
               :> CanThrow 'IncompleteBody
               :> ZLocalUser
               :> ZClient
               :> "backups"
               :> "clients"
               :> Capture "cid" ClientId
               :> "epochs"
               :> Capture "epoch" BackupEpoch
               :> Header' '[Required, Strict] "Content-Length" Word64
               :> BackupBody
               :> Post '[JSON] BackupFile
           )
    :<|> Named
           "backup-list-files"
           ( Summary "List the backup files of an epoch"
               :> From 'V19
               :> Description DownloadLinkDescription
               :> CanThrow 'BackupNotFound
               :> ZLocalUser
               :> ZClient
               :> "backups"
               :> "clients"
               :> Capture "cid" ClientId
               :> "epochs"
               :> Capture "epoch" BackupEpoch
               :> Get '[JSON] BackupFiles
           )
    :<|> Named
           "backup-download"
           ( Summary "Download a backup file"
               :> From 'V19
               :> Description DownloadLinkDescription
               :> CanThrow 'BackupNotFound
               :> ZLocalUser
               :> ZClient
               :> "backups"
               :> "clients"
               :> Capture "cid" ClientId
               :> "epochs"
               :> Capture "epoch" BackupEpoch
               :> "files"
               :> Capture "id" BackupFileId
               :> MultiVerb
                    'GET
                    '[JSON]
                    '[ErrorResponse 'BackupNotFound, BackupRedirect]
                    (Maybe (AssetLocation Absolute))
           )
    :<|> Named
           "backup-delete-epoch"
           ( Summary "Delete all backup files of an epoch"
               :> From 'V19
               :> CanThrow 'BackupNotFound
               :> CanThrow 'BackupForbidden
               :> ZLocalUser
               :> ZClient
               :> "backups"
               :> "clients"
               :> Capture "cid" ClientId
               :> "epochs"
               :> Capture "epoch" BackupEpoch
               :> MultiVerb 'DELETE '[JSON] '[RespondEmpty 204 "Epoch deleted"] ()
           )
    :<|> Named
           "backup-delete-client"
           ( Summary "Delete all backup files of a client"
               :> From 'V19
               :> CanThrow 'BackupNotFound
               :> CanThrow 'BackupForbidden
               :> ZLocalUser
               :> ZClient
               :> "backups"
               :> "clients"
               :> Capture "cid" ClientId
               :> MultiVerb 'DELETE '[JSON] '[RespondEmpty 204 "Client backups deleted"] ()
           )
