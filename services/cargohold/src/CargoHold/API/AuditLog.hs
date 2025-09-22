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

module CargoHold.API.AuditLog
  ( logUpload,
    logDownload,
    logSignedURLCreation,
    logDownloadRemoteAsset,
  )
where

import CargoHold.S3 (AssetAuditLogMetadata (..), S3AssetMeta (..))
import qualified CargoHold.Types.V3 as V3
import Codec.MIME.Type (showType)
import Data.ByteString.Conversion.To (toByteString)
import Data.Id (UserId, botUserId)
import Data.Qualified (Local, Qualified, Remote, qDomain, qUnqualified, tDomain, tUnqualified)
import Imports
import qualified System.Logger.Class as Log
import System.Logger.Message (msg, val, (.=), (~~))
import Wire.API.Asset (unAssetMIMEType)

logUpload :: (Log.MonadLogger m) => Local V3.Principal -> Maybe AssetAuditLogMetadata -> m ()
logUpload lwon mMeta =
  Log.info $
    auditTrue
      ~~ "event" .= ("file-upload" :: Text)
      ~~ uploaderFields
      ~~ auditMetaFields mMeta
      ~~ msg (val "Asset audit log: upload")
  where
    uploaderFields :: Log.Msg -> Log.Msg
    uploaderFields =
      principal ~~ principalDomain
      where
        principal =
          case tUnqualified lwon of
            V3.UserPrincipal u ->
              "uploader.type" .= ("user" :: Text)
                ~~ "uploader.id" .= toByteString u
            V3.BotPrincipal b ->
              "uploader.type" .= ("bot" :: Text)
                ~~ "uploader.id" .= toByteString (botUserId b)
            V3.ProviderPrincipal p ->
              "uploader.type" .= ("provider" :: Text)
                ~~ "uploader.id" .= toByteString p
        principalDomain = "uploader.domain" .= toByteString (tDomain lwon)

logDownload :: (Log.MonadLogger m) => Maybe (Qualified V3.Principal) -> S3AssetMeta -> m ()
logDownload mqDownloader s3 =
  -- TODO: log path
  Log.info $
    auditTrue
      ~~ "event" .= ("file-download" :: Text)
      ~~ downloaderFields mqDownloader
      ~~ auditMetaFields (v3AssetAuditLogMetadata s3)
      ~~ msg (val "Asset audit log: download")

logSignedURLCreation :: (Log.MonadLogger m) => Maybe (Qualified V3.Principal) -> Maybe S3AssetMeta -> m ()
logSignedURLCreation mqCreator mMeta =
  -- TODO: log path
  Log.info $
    auditTrue
      ~~ "event" .= ("download-url-creation" :: Text)
      ~~ downloaderFields mqCreator
      ~~ auditMetaFields (mMeta >>= v3AssetAuditLogMetadata)
      ~~ msg (val "Asset audit log: signed URL creation")

logDownloadRemoteAsset :: (Log.MonadLogger m) => Local UserId -> Remote () -> m ()
logDownloadRemoteAsset luid remote = do
  Log.info $
    auditTrue
      ~~ "event" .= ("file-download" :: Text)
      ~~ "downloader.type" .= ("user" :: Text)
      ~~ "downloader.id" .= toByteString (tUnqualified luid)
      ~~ "downloader.domain" .= toByteString (tDomain luid)
      ~~ "remote.domain" .= toByteString (tDomain remote)
      ~~ msg (val "Asset audit log: remote download")

------------------------------------------------------------------------------
-- Internal helpers

auditMetaFields :: Maybe AssetAuditLogMetadata -> Log.Msg -> Log.Msg
auditMetaFields mMeta =
  case mMeta of
    Just meta ->
      "conversation.id" .= toByteString (qUnqualified meta.convId)
        ~~ "conversation.domain" .= toByteString (qDomain meta.convId)
        ~~ "file.name" .= meta.filename
        ~~ "file.type" .= showType (unAssetMIMEType meta.filetype)
    Nothing ->
      "conversation.id" .= ("N/A" :: Text)
        ~~ "conversation.domain" .= ("N/A" :: Text)
        ~~ "file.name" .= ("N/A" :: Text)
        ~~ "file.type" .= ("N/A" :: Text)

downloaderFields :: Maybe (Qualified V3.Principal) -> Log.Msg -> Log.Msg
downloaderFields mqDownloader =
  case mqDownloader of
    Just qDownloader ->
      case qUnqualified qDownloader of
        V3.UserPrincipal u ->
          "downloader.type" .= ("user" :: Text)
            ~~ "downloader.id" .= toByteString u
            ~~ "downloader.domain" .= toByteString (qDomain qDownloader)
        V3.BotPrincipal b ->
          "downloader.type" .= ("bot" :: Text)
            ~~ "downloader.id" .= toByteString (botUserId b)
            ~~ "downloader.domain" .= toByteString (qDomain qDownloader)
        V3.ProviderPrincipal p ->
          "downloader.type" .= ("provider" :: Text)
            ~~ "downloader.id" .= toByteString p
            ~~ "downloader.domain" .= toByteString (qDomain qDownloader)
    Nothing ->
      "downloader.type" .= ("N/A internal access" :: Text)
        ~~ "downloader.id" .= ("N/A internal access" :: Text)
        ~~ "downloader.domain" .= ("N/A internal access" :: Text)

auditTrue :: Log.Msg -> Log.Msg
auditTrue = "audit" .= True
