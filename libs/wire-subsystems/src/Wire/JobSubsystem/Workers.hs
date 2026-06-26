{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

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
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
-- FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
-- for more details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

module Wire.JobSubsystem.Workers
  ( RecurringJobRunnerConfig (..),
    OneOffJobRunnerConfig (..),
    runRecurringJobRunner,
    runOneOffJobRunner,
  )
where

import Arbiter.Core qualified as ArbiterCore
import Arbiter.Core.QueueRegistry (RegistryTables, TableForPayload)
import Arbiter.Hasql.HasqlDb qualified as ArbiterHasql
import Arbiter.Migrations qualified as ArbiterMigrations
import Arbiter.Worker qualified as ArbiterWorker
import Arbiter.Worker.Config qualified as ArbiterWorkerConfig
import Arbiter.Worker.Cron qualified as ArbiterWorkerCron
import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString qualified as ByteString
import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import Data.Text qualified as T
import GHC.TypeLits (KnownSymbol)
import Imports
import System.Cron (CronSchedule, serializeCronSchedule)
import System.Logger qualified as Log
import UnliftIO.Async qualified as Async
import Wire.API.Jobs (MeetingsCleanupJob (..))

data RecurringJobRunnerConfig registry = RecurringJobRunnerConfig
  { recurringJobRunnerLogger :: Log.Logger,
    recurringJobRunnerSchedule :: CronSchedule,
    recurringJobRunnerArbiterConnStr :: ByteString.ByteString,
    recurringJobRunnerSchemaName :: Text,
    recurringJobRunnerWorkerThreads :: Int,
    recurringJobRunnerJobName :: Text,
    recurringJobRunnerQueueName :: Text
  }

data OneOffJobRunnerConfig registry (payload :: Type) = OneOffJobRunnerConfig
  { oneOffJobRunnerLogger :: Log.Logger,
    oneOffJobRunnerArbiterConnStr :: ByteString.ByteString,
    oneOffJobRunnerSchemaName :: Text,
    oneOffJobRunnerWorkerThreads :: Int,
    oneOffJobRunnerJobName :: Text,
    oneOffJobRunnerQueueName :: Text
  }

-- This runner is specialized to the meetings cleanup payload for now.
-- If we add another cron job later, we can either reuse this helper with a
-- shared payload type or factor out the common Arbiter setup first.
runRecurringJobRunner ::
  forall registry.
  ( RegistryTables registry,
    KnownSymbol (TableForPayload MeetingsCleanupJob registry),
    FromJSON MeetingsCleanupJob,
    ToJSON MeetingsCleanupJob
  ) =>
  Proxy registry ->
  RecurringJobRunnerConfig registry ->
  (MeetingsCleanupJob -> IO ()) ->
  IO (IO ())
runRecurringJobRunner registry RecurringJobRunnerConfig {..} runJob = do
  Log.info recurringJobRunnerLogger $
    Log.msg (Log.val "Starting scheduled jobs worker")
      . Log.field "job_name" recurringJobRunnerJobName
      . Log.field "queue_name" recurringJobRunnerQueueName
      . Log.field "schedule" (show recurringJobRunnerSchedule)

  arbiterEnv <-
    ArbiterHasql.createHasqlEnv
      registry
      recurringJobRunnerArbiterConnStr
      recurringJobRunnerSchemaName

  let workerHandler _conn job =
        liftIO $ do
          Log.info recurringJobRunnerLogger $
            Log.msg (Log.val "Running scheduled job")
              . Log.field "job_name" recurringJobRunnerJobName
              . Log.field "queue_name" recurringJobRunnerQueueName
          runJob (ArbiterCore.payload job)

      cronJob =
        case
          ArbiterWorkerCron.cronJob
            recurringJobRunnerJobName
            (serializeCronSchedule recurringJobRunnerSchedule)
            ArbiterWorkerCron.SkipOverlap
            (\_ scheduledFor ->
               (ArbiterCore.defaultGroupedJob recurringJobRunnerQueueName MeetingsCleanupJob)
                 { ArbiterCore.notVisibleUntil = Just scheduledFor
                 }
            )
        of
          Left err -> error $ "Invalid cron schedule for " <> T.unpack recurringJobRunnerJobName <> ": " <> err
          Right job -> job

  void $
    ArbiterMigrations.runMigrationsForRegistry
      registry
      recurringJobRunnerArbiterConnStr
      recurringJobRunnerSchemaName
      ArbiterMigrations.defaultMigrationConfig

  workerConfig <-
    ( ArbiterWorker.defaultWorkerConfig
        recurringJobRunnerArbiterConnStr
        recurringJobRunnerWorkerThreads
        workerHandler ::
        IO
          ( ArbiterWorker.WorkerConfig
              (ArbiterHasql.HasqlDb registry IO)
              MeetingsCleanupJob
              ()
          )
    )
  let workerConfig' =
        workerConfig
          { ArbiterWorkerConfig.cronJobs = [cronJob]
          }

  workerAsync <-
    Async.async $
      ArbiterHasql.runHasqlDb arbiterEnv $
        ArbiterWorker.runWorkerPool workerConfig'

  pure $ do
    ArbiterWorker.shutdownWorker workerConfig'
    Async.cancel workerAsync

runOneOffJobRunner ::
  forall registry (payload :: Type).
  ( RegistryTables registry,
    KnownSymbol (TableForPayload payload registry),
    FromJSON payload,
    ToJSON payload
  ) =>
  Proxy registry ->
  OneOffJobRunnerConfig registry payload ->
  (payload -> IO ()) ->
  IO (IO ())
runOneOffJobRunner registry OneOffJobRunnerConfig {..} runJob = do
  Log.info oneOffJobRunnerLogger $
    Log.msg (Log.val "Starting one-off jobs worker")
      . Log.field "job_name" oneOffJobRunnerJobName
      . Log.field "queue_name" oneOffJobRunnerQueueName

  arbiterEnv <-
    ArbiterHasql.createHasqlEnv
      registry
      oneOffJobRunnerArbiterConnStr
      oneOffJobRunnerSchemaName

  let workerHandler _conn job =
        liftIO $ do
          Log.info oneOffJobRunnerLogger $
            Log.msg (Log.val "Running one-off job")
              . Log.field "job_name" oneOffJobRunnerJobName
              . Log.field "queue_name" oneOffJobRunnerQueueName
          runJob (ArbiterCore.payload job)

  void $
    ArbiterMigrations.runMigrationsForRegistry
      registry
      oneOffJobRunnerArbiterConnStr
      oneOffJobRunnerSchemaName
      ArbiterMigrations.defaultMigrationConfig

  workerConfig <-
    ( ArbiterWorker.defaultWorkerConfig
        oneOffJobRunnerArbiterConnStr
        oneOffJobRunnerWorkerThreads
        workerHandler ::
        IO
          ( ArbiterWorker.WorkerConfig
              (ArbiterHasql.HasqlDb registry IO)
              payload
              ()
          )
    )

  workerAsync <-
    Async.async $
      ArbiterHasql.runHasqlDb arbiterEnv $
        ArbiterWorker.runWorkerPool workerConfig

  pure $ do
    ArbiterWorker.shutdownWorker workerConfig
    Async.cancel workerAsync
