{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
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
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
-- FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
-- for more details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

module Wire.JobSubsystem.Recurring
  ( RecurringJobRunnerConfig (..),
    OneOffJobRunnerConfig (..),
    runRecurringJobRunner,
    runOneOffJobRunner,
  )
where

import Arbiter.Core.QueueRegistry (RegistryTables, TableForPayload)
import Arbiter.Hasql.HasqlDb qualified as ArbiterHasql
import Arbiter.Migrations qualified as ArbiterMigrations
import Arbiter.Worker qualified as ArbiterWorker
import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString qualified as ByteString
import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import Data.Time.Clock (UTCTime, getCurrentTime)
import GHC.TypeLits (KnownSymbol)
import Imports
import System.Cron (CronSchedule, nextMatch)
import System.Logger qualified as Log
import UnliftIO.Async qualified as Async

data RecurringJobRunnerConfig registry (payload :: Type) = RecurringJobRunnerConfig
  { recurringJobRunnerLogger :: Log.Logger,
    recurringJobRunnerSchedule :: CronSchedule,
    recurringJobRunnerArbiterConnStr :: ByteString.ByteString,
    recurringJobRunnerSchemaName :: Text,
    recurringJobRunnerWorkerThreads :: Int,
    recurringJobRunnerEnqueueAt :: UTCTime -> ArbiterHasql.HasqlDb registry IO (),
    recurringJobRunnerRunJob :: IO (),
    recurringJobRunnerJobName :: Text,
    recurringJobRunnerQueueName :: Text
  }

data OneOffJobRunnerConfig registry (payload :: Type) = OneOffJobRunnerConfig
  { oneOffJobRunnerLogger :: Log.Logger,
    oneOffJobRunnerArbiterConnStr :: ByteString.ByteString,
    oneOffJobRunnerSchemaName :: Text,
    oneOffJobRunnerWorkerThreads :: Int,
    oneOffJobRunnerRunJob :: IO (),
    oneOffJobRunnerJobName :: Text,
    oneOffJobRunnerQueueName :: Text
  }

runRecurringJobRunner ::
  forall registry (payload :: Type).
  ( RegistryTables registry,
    KnownSymbol (TableForPayload payload registry),
    FromJSON payload,
    ToJSON payload
  ) =>
  Proxy registry ->
  RecurringJobRunnerConfig registry payload ->
  IO (IO ())
runRecurringJobRunner registry RecurringJobRunnerConfig {..} = do
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

  let enqueueNextRun :: UTCTime -> IO ()
      enqueueNextRun scheduledFor =
        void $
          ArbiterHasql.runHasqlDb arbiterEnv $
            recurringJobRunnerEnqueueAt scheduledFor

      enqueueIfScheduled :: UTCTime -> IO Bool
      enqueueIfScheduled now = case nextMatch recurringJobRunnerSchedule now of
        Nothing -> do
          Log.warn recurringJobRunnerLogger $
            Log.msg (Log.val "Scheduled job will not be enqueued")
              . Log.field "job_name" recurringJobRunnerJobName
              . Log.field "queue_name" recurringJobRunnerQueueName
          pure False
        Just nextRun -> do
          Log.info recurringJobRunnerLogger $
            Log.msg (Log.val "Enqueuing scheduled job")
              . Log.field "job_name" recurringJobRunnerJobName
              . Log.field "queue_name" recurringJobRunnerQueueName
              . Log.field "scheduled_for" (show nextRun)
          enqueueNextRun nextRun
          pure True

      workerHandler _ _ =
        liftIO $ do
          Log.info recurringJobRunnerLogger $
            Log.msg (Log.val "Running scheduled job")
              . Log.field "job_name" recurringJobRunnerJobName
              . Log.field "queue_name" recurringJobRunnerQueueName
          recurringJobRunnerRunJob
          now <- getCurrentTime
          void $ enqueueIfScheduled now

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
              payload
              ()
          )
    )

  now <- getCurrentTime
  void $ enqueueIfScheduled now

  workerAsync <-
    Async.async $
      ArbiterHasql.runHasqlDb arbiterEnv $
        ArbiterWorker.runWorkerPool workerConfig

  pure $ do
    ArbiterWorker.shutdownWorker workerConfig
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
  IO (IO ())
runOneOffJobRunner registry OneOffJobRunnerConfig {..} = do
  Log.info oneOffJobRunnerLogger $
    Log.msg (Log.val "Starting one-off jobs worker")
      . Log.field "job_name" oneOffJobRunnerJobName
      . Log.field "queue_name" oneOffJobRunnerQueueName

  arbiterEnv <-
    ArbiterHasql.createHasqlEnv
      registry
      oneOffJobRunnerArbiterConnStr
      oneOffJobRunnerSchemaName

  let workerHandler _ _ =
        liftIO $ do
          Log.info oneOffJobRunnerLogger $
            Log.msg (Log.val "Running one-off job")
              . Log.field "job_name" oneOffJobRunnerJobName
              . Log.field "queue_name" oneOffJobRunnerQueueName
          oneOffJobRunnerRunJob

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
