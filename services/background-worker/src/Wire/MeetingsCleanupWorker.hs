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

module Wire.MeetingsCleanupWorker
  ( startWorker,
    CleanupConfig (..),
  )
where

import Control.Monad.Catch
import Data.Id (RequestId (RequestId))
import Data.Text qualified as T
import Data.Time.Clock
import Imports
import Polysemy.Error (runError)
import Prometheus (incCounter)
import System.Cron (Job (..), forkJob)
import System.Logger qualified as Log
import Wire.BackgroundWorker.Env (AppT, Env (..), MeetingsCleanupMetrics (..), runAppT)
import Wire.BackgroundWorker.Options (MeetingsCleanupConfig (..))
import Wire.BackgroundWorker.Util (CleanupAction)
import Wire.Effects
import Wire.ExternalAccess.External
import Wire.MeetingsStore.Postgres (interpretMeetingsStoreToPostgres)
import Wire.MeetingsSubsystem
import Wire.MeetingsSubsystem.Interpreter

data CleanupConfig = CleanupConfig
  { retentionHours :: Double,
    batchSize :: Int
  }
  deriving (Show, Eq)

-- | Start the meetings cleanup worker thread
--
-- This worker runs periodically to clean up old meetings based on the configuration.
startWorker ::
  MeetingsCleanupConfig ->
  AppT IO CleanupAction
startWorker config = do
  env <- ask
  Log.info env.logger $
    Log.msg (Log.val "Starting meetings cleanup worker")
      . Log.field "schedule" (show config.schedule)
      . Log.field "clean_older_than_hours" config.cleanOlderThanHours

  void . liftIO $ do
    forkJob $
      Job config.schedule $
        runAppT env $ do
          Log.info env.logger $ Log.msg (Log.val "Starting scheduled meetings cleanup")
          runCleanupOldMeetings (configFromOptions config)
          liftIO $ incCounter env.meetingsCleanupMetrics.runsCounter

  pure $ pure ()

-- | Convert MeetingsCleanupConfig to CleanupConfig
configFromOptions :: MeetingsCleanupConfig -> CleanupConfig
configFromOptions cfg =
  CleanupConfig
    { retentionHours = cfg.cleanOlderThanHours,
      batchSize = cfg.batchSize
    }

-- | Main cleanup function that orchestrates the cleanup process
runCleanupOldMeetings :: CleanupConfig -> AppT IO ()
runCleanupOldMeetings config = do
  env <- ask
  now <- liftIO getCurrentTime
  let validityPeriod = realToFrac config.retentionHours * 3600
      cutoffTime = addUTCTime (negate validityPeriod) now

  Log.info env.logger $
    Log.msg (Log.val "Starting cleanup of old meetings")
      . Log.field "cutoff_time" (show cutoffTime)
      . Log.field "retention_hours" config.retentionHours

  -- Loop until no more meetings are deleted
  totalDeleted <- cleanupLoop env cutoffTime validityPeriod config.batchSize 0

  Log.info env.logger $
    Log.msg (Log.val "Completed cleanup of old meetings")
      . Log.field "total_deleted" totalDeleted

cleanupLoop :: Env -> UTCTime -> NominalDiffTime -> Int -> Int64 -> AppT IO Int64
cleanupLoop env cutoffTime validityPeriod batchSize totalSoFar = do
  when (batchSize <= 0) $ do
    Log.err env.logger $
      Log.msg (Log.val "Invalid batch size: must be greater than 0")
        . Log.field "batch_size" batchSize
    liftIO $ throwM $ WorkerException "Invalid batch size: must be greater than 0"
  -- Run the subsystem to handle cleanup logic
  result <- liftIO $ runMeetingsCleanup env cutoffTime validityPeriod batchSize

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
        then cleanupLoop env cutoffTime validityPeriod batchSize newTotal
        else pure newTotal

-- Run the meetings cleanup using the subsystem
runMeetingsCleanup :: Env -> UTCTime -> NominalDiffTime -> Int -> IO (Either Text Int64)
runMeetingsCleanup env cutoffTime validityPeriod batchSize = do
  let disableTlsV1 = True
  extEnv <- initExtEnv disableTlsV1
  let mergeErrors = either (Left . T.pack . show) Right
  fmap (either Left mergeErrors)
    . runBackgroundWorkerEffects env extEnv (RequestId "meetings-cleanup") Nothing
    . interpretMeetingsStoreToPostgres
    . runError @MeetingError
    . interpretMeetingsSubsystem validityPeriod
    $ Wire.MeetingsSubsystem.cleanupOldMeetings cutoffTime batchSize

data WorkerException = WorkerException Text
  deriving stock (Show)

instance Exception WorkerException
