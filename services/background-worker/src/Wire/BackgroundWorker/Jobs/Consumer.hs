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

module Wire.BackgroundWorker.Jobs.Consumer (startWorker, BackgroundJobsMetrics (..)) where

import Control.Concurrent.Timeout qualified as Timeout
import Control.Retry
import Data.Aeson qualified as Aeson
import Data.Misc hiding (duration)
import Data.Range (Range (fromRange))
import Data.Timeout
import Imports
import Network.AMQP qualified as Q
import Network.AMQP.Extended
import Network.AMQP.Lifted qualified as QL
import Prometheus
import System.Logger.Class qualified as Log
import System.Time.Extra (duration)
import UnliftIO
import Wire.API.BackgroundJobs
import Wire.BackgroundWorker.Env
import Wire.BackgroundWorker.Jobs.Registry
import Wire.BackgroundWorker.Options
import Wire.BackgroundWorker.Util (CleanupAction)

data BackgroundJobsMetrics = BackgroundJobsMetrics
  { workersBusy :: Gauge,
    concurrencyConfigured :: Gauge,
    jobsReceived :: Vector Text Counter,
    jobsStarted :: Vector Text Counter,
    jobsSucceeded :: Vector Text Counter,
    jobsFailed :: Vector Text Counter,
    jobsInvalid :: Vector Text Counter,
    jobsRedelivered :: Vector Text Counter,
    jobDuration :: Vector Text Histogram
  }

mkMetrics :: IO BackgroundJobsMetrics
mkMetrics = do
  workersBusy <- register (gauge $ Info {metricName = "wire_background_jobs_workers_busy", metricHelp = "In-flight background jobs"})
  concurrencyConfigured <- register (gauge $ Info {metricName = "wire_background_jobs_concurrency_configured", metricHelp = "Configured concurrency for this process"})
  jobsReceived <- register (vector "job_type" $ counter $ Info "wire_background_jobs_received_total" "Jobs received")
  jobsStarted <- register (vector "job_type" $ counter $ Info "wire_background_jobs_started_total" "Jobs started")
  jobsSucceeded <- register (vector "job_type" $ counter $ Info "wire_background_jobs_succeeded_total" "Jobs succeeded")
  jobsFailed <- register (vector "job_type" $ counter $ Info "wire_background_jobs_failed_total" "Jobs failed")
  jobsInvalid <- register (vector "job_type" $ counter $ Info "wire_background_jobs_invalid_total" "Invalid jobs received")
  jobsRedelivered <- register (vector "job_type" $ counter $ Info "wire_background_jobs_redelivered_total" "Jobs marked redelivered by broker")
  jobDuration <- register (vector "job_type" $ histogram (Info "wire_background_jobs_duration_seconds" "Job duration seconds") defaultBuckets)
  pure BackgroundJobsMetrics {..}

startWorker :: AmqpEndpoint -> AppT IO CleanupAction
startWorker rabbitmqOpts = do
  env <- ask
  let cfg = env.backgroundJobsConfig
  metrics <- liftIO mkMetrics
  markAsNotWorking BackgroundJobConsumer
  void . async . liftIO $
    openConnectionWithRetries env.logger rabbitmqOpts (Just "background-job-consumer") $
      RabbitMqHooks
        { onNewChannel = \chan -> do
            -- declare queue and set prefetch to concurrency
            ensureBackgroundJobsQueue chan
            Q.qos chan 0 (fromIntegral $ fromRange cfg.concurrency) False
            -- set gauges
            setGauge metrics.concurrencyConfigured (fromIntegral $ fromRange cfg.concurrency)
            -- start consuming with manual ack and keep the channel alive
            void $ QL.consumeMsgs chan backgroundJobsQueueName Q.Ack (void . runAppT env . handleDelivery metrics cfg)
            runAppT env $ markAsWorking BackgroundJobConsumer
            forever $ threadDelay maxBound,
          onChannelException = \e -> do
            runAppT env $ markAsNotWorking BackgroundJobConsumer
            let connClosed =
                  case (fromException e :: Maybe Q.AMQPException) of
                    Just (Q.ConnectionClosedException _ _) -> True
                    _ -> False
            unless (Q.isNormalChannelClose e || connClosed) $
              runAppT env $
                Log.err $
                  Log.msg (Log.val "Unexpected RabbitMQ channel exception in background job consumer")
                    . Log.field "exception" (displayException e),
          onConnectionClose =
            runAppT env $ do
              markAsNotWorking BackgroundJobConsumer
              Log.info $ Log.msg (Log.val "RabbitMQ connection closed for background job consumer")
        }
  pure $ runAppT env $ cleanup
  where
    cleanup :: AppT IO ()
    cleanup = do
      -- nothing to close explicitly; the AMQP helper closes channel/connection on shutdown
      Log.info $ Log.msg (Log.val "Background job consumer cleanup")
      markAsNotWorking BackgroundJobConsumer

handleDelivery :: BackgroundJobsMetrics -> BackgroundJobsConfig -> (Q.Message, Q.Envelope) -> AppT IO ()
handleDelivery metrics cfg (msg, env) = do
  case Aeson.eitherDecode @Job (Q.msgBody msg) of
    Left err -> do
      withLabel metrics.jobsInvalid "invalid" incCounter
      Log.err $ Log.msg (Log.val "Invalid background job JSON") . Log.field "error" err
      Timeout.threadDelay (200 # MilliSecond) -- avoid tight redelivery loop
      liftIO $ Q.rejectEnv env True
    Right job -> do
      let lbl = jobPayloadLabel job.payload
      when (Q.envRedelivered env) $ withLabel metrics.jobsRedelivered lbl incCounter
      withLabel metrics.jobsReceived lbl incCounter
      UnliftIO.bracket_ (incGauge metrics.workersBusy) (decGauge metrics.workersBusy) $ do
        outcome <- runAttempts lbl job
        case outcome of
          Right () -> do
            withLabel metrics.jobsSucceeded lbl incCounter
            liftIO $ Q.ackEnv env
          Left e -> do
            withLabel metrics.jobsFailed lbl incCounter
            Log.err $ Log.msg (Log.val "Background job failed after retries") . Log.field "error" e
            liftIO $ Q.rejectEnv env False
  where
    runAttempts :: Text -> Job -> AppT IO (Either Text ())
    runAttempts lbl job = do
      let retries = max 0 (fromRange cfg.maxAttempts - 1)
          policy = limitRetries retries <> fullJitterBackoff 100000 -- 100ms base
      retrying policy shouldRetry $ \_rs -> do
        withLabel metrics.jobsStarted lbl incCounter
        (dur, r) <-
          duration $
            fromMaybe (Left "job timeout")
              <$> timeout (durationToMicros cfg.jobTimeout) (dispatchJob job)
        withLabel metrics.jobDuration lbl (`observe` dur)
        pure r
      where
        shouldRetry _ (Right _) = pure False
        shouldRetry _ (Left _) = pure True
