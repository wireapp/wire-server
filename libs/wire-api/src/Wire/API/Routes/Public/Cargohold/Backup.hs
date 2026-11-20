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

type FullUploadDescription =
  "The new full backup replaces the epoch's previous full backup and the incremental files \
  \of it; the newest full backup (by id) wins. If a newer full backup is stored during the \
  \upload, this one is deleted and the upload fails with `backup-full-missing`. A failed or \
  \aborted upload leaves no file behind. Only the two newest epochs are kept: after the full backup is stored, all files of \
  \the older epochs are deleted. If two newer epochs are stored during the upload, this epoch is deleted \
  \and the upload fails with `backup-epoch-not-allowed`."

type IncrementalUploadDescription =
  "Extends the full backup `fullId`, which must be the newest full backup of the epoch \
  \(`backup-full-missing` otherwise, also if a newer full backup is stored during the upload: \
  \upload a new full backup instead). A failed or aborted \
  \upload leaves no file behind."

type DeleteClientBackupsDescription =
  "Deletes all backups of the client, which must be the calling client. The team feature is not \
  \checked, so a client can clean up while the feature is disabled. Succeeds if there is nothing to \
  \delete. Uploads still running \
  \when this is called are rejected (403) and leave no file behind. Uploads started after the \
  \response create backups again, so a client should stop scheduling uploads before calling this."

type ListFilesDescription =
  "Lists the newest full backup of the epoch and the incremental files stored under it. Presigned \
  \URLs are bearer links valid for `downloadLinkTTL` seconds and expiry is checked at request \
  \start, so the list returns no URLs and each file is fetched via its own redirect."

type ListBackupsDescription =
  "Lists the restorable backups of all clients of the user: per client its epochs, per epoch \
  \a summary of the newest full backup and the incremental files stored under it (newest creation \
  \time, number of files, total size). Epochs without a full backup and clients without any \
  \are omitted. Clients are ordered by their newest backup, newest first. The files of an \
  \epoch are listed by `backup-list-files`."

type ListClientBackupsDescription =
  "Lists the restorable epochs of one client as in `backup-list`. An empty `epochs` list means \
  \the client has no restorable backup."

type BackupAPI =
  Named
    "backup-list"
    ( Summary "List the backups of all clients of the user"
        :> From 'V19
        :> Description ListBackupsDescription
        :> CanThrow 'BackupNotFound
        :> CanThrow 'BackupFeatureDisabled
        :> ZLocalUser
        :> ZClient
        :> "backups"
        :> Get '[JSON] UserBackups
    )
    :<|> Named
           "backup-list-client"
           ( Summary "List the backups of a client"
               :> From 'V19
               :> Description ListClientBackupsDescription
               :> CanThrow 'BackupNotFound
               :> CanThrow 'BackupFeatureDisabled
               :> ZLocalUser
               :> ZClient
               :> "backups"
               :> "clients"
               :> Capture "cid" ClientId
               :> Get '[JSON] ClientBackups
           )
    :<|> Named
           "backup-upload-full"
           ( Summary "Upload the full backup file of an epoch"
               :> From 'V19
               :> Description FullUploadDescription
               :> CanThrow 'BackupNotFound
               :> CanThrow 'BackupForbidden
               :> CanThrow 'BackupFeatureDisabled
               :> CanThrow 'BackupEpochNotAllowed
               :> CanThrow 'BackupFullMissing
               :> CanThrow 'BackupTooLarge
               :> CanThrow 'IncompleteBody
               :> CanThrow 'InvalidLength
               :> ZLocalUser
               :> ZClient
               :> "backups"
               :> "clients"
               :> Capture "cid" ClientId
               :> "epochs"
               :> Capture "epoch" BackupEpoch
               :> "full"
               :> Header' '[Required, Strict] "Content-Length" Word64
               :> BackupBody
               :> Post '[JSON] BackupFile
           )
    :<|> Named
           "backup-upload-incremental"
           ( Summary "Upload an incremental backup file of an epoch"
               :> From 'V19
               :> Description IncrementalUploadDescription
               :> CanThrow 'BackupNotFound
               :> CanThrow 'BackupForbidden
               :> CanThrow 'BackupFeatureDisabled
               :> CanThrow 'BackupEpochNotAllowed
               :> CanThrow 'BackupFullMissing
               :> CanThrow 'BackupTooManyFiles
               :> CanThrow 'BackupTooLarge
               :> CanThrow 'IncompleteBody
               :> CanThrow 'InvalidLength
               :> ZLocalUser
               :> ZClient
               :> "backups"
               :> "clients"
               :> Capture "cid" ClientId
               :> "epochs"
               :> Capture "epoch" BackupEpoch
               :> "full"
               :> Capture "fullId" BackupFileId
               :> "incremental"
               :> Header' '[Required, Strict] "Content-Length" Word64
               :> BackupBody
               :> Post '[JSON] BackupFile
           )
    :<|> Named
           "backup-list-files"
           ( Summary "List the backup files of an epoch"
               :> From 'V19
               :> Description ListFilesDescription
               :> CanThrow 'BackupNotFound
               :> CanThrow 'BackupFeatureDisabled
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
           "backup-delete-client"
           ( Summary "Delete all backups of a client"
               :> From 'V19
               :> Description DeleteClientBackupsDescription
               :> CanThrow 'BackupNotFound
               :> CanThrow 'BackupForbidden
               :> CanThrow 'BackupFeatureDisabled
               :> ZLocalUser
               :> ZClient
               :> "backups"
               :> "clients"
               :> Capture "cid" ClientId
               :> MultiVerb 'DELETE '[JSON] '[RespondEmpty 204 "Backups deleted"] ()
           )
    :<|> Named
           "backup-download-full"
           ( Summary "Download the full backup file of an epoch"
               :> From 'V19
               :> Description DownloadLinkDescription
               :> CanThrow 'BackupNotFound
               :> CanThrow 'BackupFeatureDisabled
               :> ZLocalUser
               :> ZClient
               :> "backups"
               :> "clients"
               :> Capture "cid" ClientId
               :> "epochs"
               :> Capture "epoch" BackupEpoch
               :> "full"
               :> Capture "fullId" BackupFileId
               :> MultiVerb
                    'GET
                    '[JSON]
                    '[ErrorResponse 'BackupNotFound, BackupRedirect]
                    (Maybe (AssetLocation Absolute))
           )
    :<|> Named
           "backup-download-incremental"
           ( Summary "Download an incremental backup file of an epoch"
               :> From 'V19
               :> Description DownloadLinkDescription
               :> CanThrow 'BackupNotFound
               :> CanThrow 'BackupFeatureDisabled
               :> ZLocalUser
               :> ZClient
               :> "backups"
               :> "clients"
               :> Capture "cid" ClientId
               :> "epochs"
               :> Capture "epoch" BackupEpoch
               :> "full"
               :> Capture "fullId" BackupFileId
               :> "incremental"
               :> Capture "id" BackupFileId
               :> MultiVerb
                    'GET
                    '[JSON]
                    '[ErrorResponse 'BackupNotFound, BackupRedirect]
                    (Maybe (AssetLocation Absolute))
           )
