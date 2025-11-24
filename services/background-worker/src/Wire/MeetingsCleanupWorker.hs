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

module Wire.MeetingsCleanupWorker
  ( startWorker,
    cleanupOldMeetings,
    CleanupConfig (..),
  )
where

import Data.Bifunctor (first)
import Data.Time.Clock
import Hasql.Pool (UsageError)
import Imports
import Polysemy
import Polysemy.Error (runError)
import Polysemy.Input (runInputConst)
import System.Logger qualified as Log
import UnliftIO (async)
import Wire.BackgroundWorker.Env (AppT, Env (..))
import Wire.BackgroundWorker.Options (MeetingsCleanupConfig (..))
import Wire.BackgroundWorker.Util (CleanupAction)
import Wire.ConversationStore.Postgres (interpretConversationStoreToPostgres)
import Wire.MeetingsStore.Postgres (interpretMeetingsStoreToPostgres)
import Wire.MeetingsSubsystem qualified as Meetings
import Wire.MeetingsSubsystem.Interpreter (interpretMeetingsSubsystem)

data CleanupConfig = CleanupConfig
  { retentionHours :: Int,
    batchSize :: Int
  }
  deriving (Show, Eq)

-- | Start the meetings cleanup worker thread
--
-- This worker runs periodically to clean up old meetings based on the configuration.
-- It sleeps for the configured frequency and then runs the cleanup operation.
startWorker ::
  MeetingsCleanupConfig ->
  AppT IO CleanupAction
startWorker config = do
  env <- ask
  -- Start the worker loop in a separate thread
  void . async $ workerLoop env config
  -- Return a no-op cleanup action (worker will be killed when the process exits)
  pure $ pure ()

-- | Worker loop that runs periodically
workerLoop :: Env -> MeetingsCleanupConfig -> AppT IO ()
workerLoop env config = forever $ do
  -- Sleep for the configured frequency (convert seconds to microseconds)
  liftIO $ threadDelay (config.cleanFrequencySeconds * 1_000_000)

  Log.info env.logger $
    Log.msg (Log.val "Starting scheduled meetings cleanup")
      . Log.field "clean_older_than_hours" config.cleanOlderThanHours
      . Log.field "batch_size" config.batchSize
      . Log.field "frequency_seconds" config.cleanFrequencySeconds

  -- Run the cleanup
  cleanupOldMeetings (configFromOptions config)

-- | Convert MeetingsCleanupConfig to CleanupConfig
configFromOptions :: MeetingsCleanupConfig -> CleanupConfig
configFromOptions cfg =
  CleanupConfig
    { retentionHours = cfg.cleanOlderThanHours,
      batchSize = cfg.batchSize
    }

-- | Main cleanup function that orchestrates the cleanup process
cleanupOldMeetings :: CleanupConfig -> AppT IO ()
cleanupOldMeetings config = do
  env <- ask
  now <- liftIO getCurrentTime
  let cutoffTime = addUTCTime (negate $ fromIntegral config.retentionHours * 3600) now

  Log.info env.logger $
    Log.msg (Log.val "Starting cleanup of old meetings")
      . Log.field "cutoff_time" (show cutoffTime)
      . Log.field "retention_hours" config.retentionHours
      . Log.field "batch_size" config.batchSize

  -- Loop until no more meetings are deleted
  totalDeleted <- cleanupLoop env cutoffTime config.batchSize 0

  Log.info env.logger $
    Log.msg (Log.val "Completed cleanup of old meetings")
      . Log.field "total_deleted" totalDeleted

cleanupLoop :: Env -> UTCTime -> Int -> Int64 -> AppT IO Int64
cleanupLoop env cutoffTime batchSize totalSoFar = do
  -- Run the subsystem to handle cleanup logic
  result <- liftIO $ runMeetingsCleanup env cutoffTime batchSize

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

-- Run the meetings cleanup using the subsystem
runMeetingsCleanup :: Env -> UTCTime -> Int -> IO (Either String Int64)
runMeetingsCleanup env cutoffTime batchSize =
  fmap (first show)
    . runM
    . runError @UsageError
    . runInputConst env.hasqlPool
    . interpretMeetingsStoreToPostgres
    . runInputConst env.hasqlPool
    . interpretConversationStoreToPostgres
    . interpretMeetingsSubsystem
    $ Meetings.cleanupOldMeetings cutoffTime batchSize
