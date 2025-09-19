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
  )
where

import CargoHold.S3 (AssetAuditLogMetadata (..))
import qualified CargoHold.Types.V3 as V3
import Codec.MIME.Type (showType)
import Data.ByteString.Conversion.To (toByteString)
import Data.Domain (Domain)
import Data.Id (botUserId)
import Data.Qualified (qDomain, qUnqualified)
import Imports
import qualified System.Logger.Class as Log
import System.Logger.Message (msg, val, (.=), (~~))
import Wire.API.Asset (unAssetMIMEType)

-- | Emit an audit log entry for an asset upload event.
--
-- When metadata is present, enrich the log with conversation and file details.
logUpload :: (Log.MonadLogger m) => Domain -> V3.Principal -> Maybe AssetAuditLogMetadata -> m ()
logUpload domain own mMeta =
  Log.info $
    base
      ~~ principal
      ~~ principalDomain
      ~~ audit
      ~~ msg (val "Asset audit log: upload")
  where
    base =
      "audit" .= True
        ~~ "event" .= ("file-upload" :: Text)
    principal =
      case own of
        V3.UserPrincipal u ->
          "uploader.type" .= ("user" :: Text)
            ~~ "uploader.id" .= toByteString u
        V3.BotPrincipal b ->
          "uploader.type" .= ("bot" :: Text)
            ~~ "uploader.id" .= toByteString (botUserId b)
        V3.ProviderPrincipal p ->
          "uploader.type" .= ("provider" :: Text)
            ~~ "uploader.id" .= toByteString p
    principalDomain = "uploader.domain" .= toByteString domain
    audit =
      case mMeta of
        Nothing -> id
        Just meta ->
          "conversation.id" .= toByteString (qUnqualified meta.convId)
            ~~ "conversation.domain" .= toByteString (qDomain meta.convId)
            ~~ "file.name" .= meta.filename
            ~~ "file.type" .= showType (unAssetMIMEType meta.filetype)
