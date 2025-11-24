{-# LANGUAGE RecordWildCards #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.CleanupOldMeetings
  ( cleanupOldMeetings,
    CleanupConfig (..),
  )
where

import Data.Time.Clock
import Hasql.Pool qualified as Hasql
import Hasql.Session qualified as Hasql
import Hasql.Statement qualified as Hasql
import Hasql.TH qualified as TH
import Imports
import System.Logger qualified as Log
import Wire.BackgroundWorker.Env

data CleanupConfig = CleanupConfig
  { retentionDays :: Int,
    batchSize :: Int
  }
  deriving (Show, Eq)

cleanupOldMeetings :: CleanupConfig -> AppT IO ()
cleanupOldMeetings config = do
  env <- ask
  now <- liftIO getCurrentTime
  let cutoffTime = addUTCTime (negate $ fromIntegral config.retentionDays * 24 * 3600) now

  Log.info env.logger $
    Log.msg (Log.val "Starting cleanup of old meetings")
      . Log.field "cutoff_time" (show cutoffTime)
      . Log.field "retention_days" config.retentionDays
      . Log.field "batch_size" config.batchSize

  -- Loop until no more meetings are deleted
  totalDeleted <- cleanupLoop env cutoffTime config.batchSize 0

  Log.info env.logger $
    Log.msg (Log.val "Completed cleanup of old meetings")
      . Log.field "total_deleted" totalDeleted

cleanupLoop :: Env -> UTCTime -> Int -> Int64 -> AppT IO Int64
cleanupLoop env cutoffTime batchSize totalSoFar = do
  result <- liftIO $ Hasql.use env.hasqlPool (deleteOldMeetingsSession cutoffTime batchSize)
  case result of
    Left err -> do
      Log.err env.logger $
        Log.msg (Log.val "Failed to cleanup old meetings batch")
          . Log.field "error" (show err)
          . Log.field "total_deleted_so_far" totalSoFar
      pure totalSoFar
    Right deletedCount -> do
      let newTotal = totalSoFar + deletedCount
      Log.info env.logger $
        Log.msg (Log.val "Cleaned up meetings batch")
          . Log.field "batch_deleted" deletedCount
          . Log.field "total_deleted" newTotal
      -- Continue if we deleted a full batch (meaning there might be more)
      if deletedCount >= fromIntegral batchSize
        then cleanupLoop env cutoffTime batchSize newTotal
        else pure newTotal

deleteOldMeetingsSession :: UTCTime -> Int -> Hasql.Session Int64
deleteOldMeetingsSession cutoffTime batchSize =
  Hasql.statement (cutoffTime, batchSize) deleteOldMeetingsStatement

deleteOldMeetingsStatement :: Hasql.Statement (UTCTime, Int) Int64
deleteOldMeetingsStatement =
  [TH.singletonStatement|
    DELETE FROM meetings
    WHERE id IN (
      SELECT id
      FROM meetings
      WHERE end_date < $1 :: timestamptz
      ORDER BY end_date ASC
      LIMIT $2 :: int4
    )
  |]
