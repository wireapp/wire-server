{-# LANGUAGE BlockArguments #-}

module Wire.BackgroundWorker where

import Data.Metrics.Servant qualified as Metrics
import Data.Text qualified as T
import Imports
import Network.NATS.Extended (demoteNatsOpts)
import Network.Wai.Utilities.Server
import Servant
import Servant.Server.Generic
import UnliftIO (concurrently_)
import Util.Options
import Wire.BackendNotificationPusher qualified as BackendNotificationPusher
import Wire.BackgroundWorker.Env
import Wire.BackgroundWorker.Health qualified as Health
import Wire.BackgroundWorker.Options
import Wire.DeadUserNotificationWatcher qualified as DeadUserNotificationWatcher

run :: Opts -> IO ()
run opts = do
  env <- mkEnv opts
  let natsEP = either id demoteNatsOpts opts.nats.unNatsOpts
  cleanupBackendNotifPusher <-
    runAppT env $
      withNamedLogger "backend-notifcation-pusher" $
        BackendNotificationPusher.startWorker natsEP
  cleanupDeadUserNotifWatcher <-
    runAppT env $
      withNamedLogger "dead-user-notification-watcher" $
        DeadUserNotificationWatcher.startWorker natsEP
  let -- cleanup will run in a new thread when the signal is caught, so we need to use IORefs and
      -- specific exception types to message threads to clean up
      cleanup = do
        concurrently_ cleanupDeadUserNotifWatcher cleanupBackendNotifPusher
  let server = defaultServer (T.unpack $ opts.backgroundWorker.host) opts.backgroundWorker.port env.logger
  let settings = newSettings server
  -- Additional cleanup when shutting down via signals.
  runSettingsWithCleanup cleanup settings (servantApp env) Nothing

servantApp :: Env -> Application
servantApp env =
  Metrics.servantPrometheusMiddleware (Proxy @(ToServant Health.HealthAPI AsApi)) $
    genericServe $
      Health.api env
