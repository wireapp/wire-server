{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2026 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option)
-- any later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

module Wire.BackgroundWorker.ScheduledJobs (startWorker) where

import Arbiter.Core qualified as ArbiterCore
import Arbiter.Hasql.HasqlDb qualified as ArbiterHasql
import Arbiter.Migrations qualified as ArbiterMigrations
import Arbiter.Worker qualified as ArbiterWorker
import Data.Aeson (FromJSON, ToJSON, Value (Null), parseJSON, toJSON)
import Data.Proxy (Proxy (..))
import Imports
import System.Cron (Job (..), forkJob)
import System.Logger qualified as Log
import UnliftIO.Async qualified as Async
import Wire.BackgroundWorker.Env (AppT, Env (..), runAppT)
import Wire.BackgroundWorker.Options (MeetingsCleanupConfig (..))
import Wire.BackgroundWorker.Util (CleanupAction)
import Wire.MeetingsCleanupWorker
  ( CleanupConfig (..),
    runCleanupOldMeetings,
  )

type ScheduledJobsRegistry =
  '[ '("meetings_cleanup_jobs", MeetingsCleanupJob)
   ]

-- | Empty payload because the schedule itself carries all execution context.
data MeetingsCleanupJob = MeetingsCleanupJob
  deriving stock (Eq, Generic, Show)

instance ToJSON MeetingsCleanupJob where
  toJSON MeetingsCleanupJob = Null

instance FromJSON MeetingsCleanupJob where
  parseJSON Null = pure MeetingsCleanupJob
  parseJSON _ = fail "MeetingsCleanupJob expects null"

startWorker :: MeetingsCleanupConfig -> AppT IO CleanupAction
startWorker config = do
  env <- ask
  Log.info env.logger $
    Log.msg (Log.val "Starting scheduled meetings cleanup jobs")
      . Log.field "schedule" (show config.schedule)
      . Log.field "clean_older_than_hours" config.cleanOlderThanHours

  let cleanupConfig =
        CleanupConfig
          { retentionHours = config.cleanOlderThanHours,
            batchSize = config.batchSize
          }
      schemaName = ArbiterCore.defaultSchemaName
  -- Arbiter keeps its own schema and migrations separate from the existing
  -- meetings tables, so the worker can claim jobs independently.
  arbiterEnv <-
    liftIO $
      ArbiterHasql.createHasqlEnv (Proxy @ScheduledJobsRegistry) env.arbiterConnStr schemaName

  -- Insert a single queued job for each tick. The dedup key prevents one
  -- scheduler from enqueuing duplicate runs for the same logical job.
  let jobWrite :: ArbiterCore.JobWrite MeetingsCleanupJob
      jobWrite =
        (ArbiterCore.defaultGroupedJob "meetings-cleanup" MeetingsCleanupJob)
          { ArbiterCore.dedupKey = Just (ArbiterCore.IgnoreDuplicate "meetings-cleanup")
          }

      enqueueCleanupJob :: ArbiterHasql.HasqlDb ScheduledJobsRegistry IO ()
      enqueueCleanupJob = void $ ArbiterCore.insertJob jobWrite

      workerHandler _ _ = do
        -- Arbiter claims the job; the handler just runs the existing cleanup
        -- logic inside the background-worker application environment.
        liftIO $
          runAppT env $ do
            Log.info env.logger $ Log.msg (Log.val "Running scheduled meetings cleanup job")
            runCleanupOldMeetings cleanupConfig

  -- Create the Arbiter queue tables for this registry on startup.
  void $
    liftIO $
      ArbiterMigrations.runMigrationsForRegistry
        (Proxy @ScheduledJobsRegistry)
        env.arbiterConnStr
        schemaName
        ArbiterMigrations.defaultMigrationConfig

  workerConfig <-
    liftIO $
      -- One worker thread is enough for now because each background-worker
      -- instance enqueues the same logical cleanup job, and Arbiter's dedup
      -- key keeps duplicate runs from stacking up.
      -- One worker thread is enough for this proof of concept.
      ( ArbiterWorker.defaultWorkerConfig
          env.arbiterConnStr
          1
          workerHandler ::
          IO
            ( ArbiterWorker.WorkerConfig
                (ArbiterHasql.HasqlDb ScheduledJobsRegistry IO)
                MeetingsCleanupJob
                ()
            )
      )

  schedulerThread <- liftIO . forkJob . Job config.schedule $ do
    -- The scheduler only inserts the next job row; Arbiter does the claiming
    -- and execution.
    Log.info env.logger $
      Log.msg (Log.val "Enqueuing scheduled meetings cleanup job")
        . Log.field "queue_name" ("meetings_cleanup_jobs" :: String)
    void $ ArbiterHasql.runHasqlDb arbiterEnv enqueueCleanupJob

  workerAsync <-
    liftIO . Async.async $
      -- Run the Arbiter worker loop in the same process.
      ArbiterHasql.runHasqlDb arbiterEnv $
        ArbiterWorker.runWorkerPool workerConfig

  pure $ do
    liftIO $ do
      killThread schedulerThread
      ArbiterWorker.shutdownWorker workerConfig
    Async.cancel workerAsync
