{-# LANGUAGE BlockArguments #-}

module Wire.BackgroundWorker where

import Data.Metrics.Servant qualified as Metrics
import Data.Text qualified as T
import Imports
import Network.AMQP.Extended (demoteOpts)
import Network.Wai.Utilities.Server
import Servant
import Servant.Server.Generic
import UnliftIO (Concurrently (..), runConcurrently)
import Util.Options
import Wire.BackendNotificationPusher qualified as BackendNotificationPusher
import Wire.BackgroundWorker.Env
import Wire.BackgroundWorker.Health qualified as Health
import Wire.BackgroundWorker.Jobs.Consumer qualified as Jobs
import Wire.BackgroundWorker.Options
import Wire.DeadUserNotificationWatcher qualified as DeadUserNotificationWatcher
import Wire.MigrateConversations qualified as MigrateConversations

run :: Opts -> IO ()
run opts = do
  env <- mkEnv opts
  let amqpEP = either id demoteOpts opts.rabbitmq.unRabbitMqOpts
  cleanupBackendNotifPusher <-
    runAppT env $
      withNamedLogger "backend-notification-pusher" $
        BackendNotificationPusher.startWorker amqpEP
  cleanupDeadUserNotifWatcher <-
    runAppT env $
      withNamedLogger "dead-user-notification-watcher" $
        DeadUserNotificationWatcher.startWorker amqpEP
  cleanupConvMigration <-
    if opts.migrateConversations
      then
        runAppT env $
          withNamedLogger "migrate-conversations" $
            MigrateConversations.startWorker
      else pure $ pure ()
  cleanupJobs <-
    runAppT env $
      withNamedLogger "background-job-consumer" $
        Jobs.startWorker amqpEP
  let cleanup =
        void . runConcurrently $
          (,,,)
            <$> Concurrently cleanupDeadUserNotifWatcher
            <*> Concurrently cleanupBackendNotifPusher
            <*> Concurrently cleanupConvMigration
            <*> Concurrently cleanupJobs

  let server = defaultServer (T.unpack opts.backgroundWorker.host) opts.backgroundWorker.port env.logger
  let settings = newSettings server
  -- Additional cleanup when shutting down via signals.
  runSettingsWithCleanup cleanup settings (servantApp env) Nothing

servantApp :: Env -> Application
servantApp env =
  Metrics.servantPrometheusMiddleware (Proxy @(ToServant Health.HealthAPI AsApi)) $
    genericServe $
      Health.api env
