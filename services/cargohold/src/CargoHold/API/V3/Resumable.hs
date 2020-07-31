-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
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

module CargoHold.API.V3.Resumable
  ( create,
    status,
    upload,
  )
where

import qualified CargoHold.API.Error as Error
import CargoHold.API.V3 (randToken)
import CargoHold.App
import CargoHold.Options
import qualified CargoHold.S3 as S3
import CargoHold.Types.V3 as V3
import CargoHold.Types.V3.Resumable as V3
import Control.Error (throwE)
import Control.Lens (set, view)
import Data.ByteString.Conversion
import Data.Coerce
import Data.Conduit
import Data.Id
import Data.Time.Clock
import Data.UUID.V4 (nextRandom)
import Imports
import System.Logger.Class (field, msg, val, (~~))
import qualified System.Logger.Class as Log

create :: V3.Principal -> V3.ResumableSettings -> V3.TotalSize -> Handler V3.ResumableAsset
create own sets size = do
  let cl = fromIntegral size
  when (cl <= 0) $
    throwE Error.invalidLength
  maxTotalBytes <- view (settings . setMaxTotalBytes)
  when (cl > maxTotalBytes) $
    throwE Error.assetTooLarge
  aid <- liftIO $ Id <$> nextRandom
  tok <-
    if view setResumablePublic sets
      then return Nothing
      else Just <$> randToken
  let ret = view setResumableRetention sets
  let typ = view setResumableType sets
  let key = V3.AssetKeyV3 aid ret
  astExpire <- case V3.assetRetentionSeconds ret of
    Just n -> Just . addUTCTime n <$> liftIO getCurrentTime
    Nothing -> return Nothing
  Log.debug $
    field "asset" (toByteString aid)
      ~~ field "asset.size" (toByteString size)
      ~~ msg (val "Initialising resumable upload")
  r <- S3.createResumable key own typ size tok
  let chunkSize = S3.resumableChunkSize r
  let uplExpire = S3.resumableExpires r
  let ast =
        V3.mkAsset key
          & set V3.assetExpires astExpire
          & set V3.assetToken tok
  return $! mkResumableAsset ast uplExpire chunkSize

status :: V3.Principal -> AssetKey -> Handler (Maybe (V3.Offset, V3.TotalSize))
status own key = do
  Log.debug $
    field "asset" (toByteString key)
      ~~ msg (val "Getting status of resumable upload")
  r <- getResumable key
  return $
    if own /= S3.resumableOwner r
      then Nothing
      else
        let total = S3.resumableTotalSize r
            offset = S3.resumableOffset r
         in Just (offset, total)

upload :: V3.Principal -> AssetKey -> Offset -> Word -> ConduitM () ByteString IO () -> Handler (Offset, UTCTime)
upload own key off len src = do
  r <- getResumable key
  let offset = S3.resumableOffset r
  validate r offset
  if off == Offset (totalSize r)
    then complete r
    else resume r offset
  where
    complete r = do
      fin <- S3.getMetadataV3 key
      unless (isJust fin) $
        S3.completeResumable r
      return (off, S3.resumableExpires r)
    resume r offset = do
      Log.debug $
        field "asset" (toByteString key)
          ~~ field "asset.offset" (toByteString offset)
          ~~ msg (val "Resuming upload")
      (r', offset') <- consume r offset len (sealConduitT src)
      when (offset' == Offset (totalSize r')) $
        -- TODO: Completion might take a while, such that we may need to
        -- keep the client connection alive by sending whitespace after the
        -- response status line and headers but before the final response body,
        -- just like S3 does when completing multipart uploads.
        S3.completeResumable r'
      return (offset', S3.resumableExpires r')
    consume r offset 0 _ = return (r, offset)
    consume r offset remaining rsrc = do
      let totalBytes = V3.totalSizeBytes (S3.resumableTotalSize r)
      let numBytes = min (chunkSize r) remaining
      if numBytes < chunkSize r && coerce offset + remaining < totalBytes
        then -- Remaining input that is not a full chunk size and does
        -- not constitute the last chunk is ignored, i.e. all chunks
        -- except the last must have the same size (the chunk size).
          return (r, offset)
        else do
          (r', rsrc') <- S3.uploadChunk r offset rsrc
          let offset' = offset + Offset numBytes
          let remaining' = remaining - numBytes
          consume r' offset' remaining' rsrc'
    validate r o
      | invalidOwner r = throwE Error.assetNotFound
      | invalidOffset o = throwE (Error.invalidOffset o off)
      | tooSmall r o = throwE Error.uploadTooSmall
      | tooLarge r = throwE Error.uploadTooLarge
      | otherwise = return ()
    invalidOwner r = own /= S3.resumableOwner r
    invalidOffset o = o /= off
    tooSmall r o = len < chunkSize r && missingBytes r o > chunkSize r
    tooLarge r = proposedBytes > S3.resumableTotalSize r
    chunkSize = chunkSizeBytes . S3.resumableChunkSize
    totalSize = totalSizeBytes . S3.resumableTotalSize
    missingBytes r o = totalSize r - V3.offsetBytes o
    proposedBytes = V3.TotalSize (V3.offsetBytes off + len)

getResumable :: AssetKey -> Handler S3.S3Resumable
getResumable key = do
  rs <- S3.getResumable key
  maybe (throwE Error.assetNotFound) return rs
