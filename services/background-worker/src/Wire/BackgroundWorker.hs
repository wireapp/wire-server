module Wire.BackgroundWorker where

import qualified Data.Metrics.Servant as Metrics
import Imports
import Network.Wai.Utilities.Server
import Servant
import Servant.Server.Generic
import UnliftIO.Async
import Util.Options
import qualified Wire.BackendNotificationPusher as BackendNotificationPusher
import Wire.BackgroundWorker.Env
import qualified Wire.BackgroundWorker.Health as Health
import Wire.BackgroundWorker.Options
import qualified System.Logger as Log

-- FUTUREWORK: Start an http service with status and metrics endpoints
run :: Opts -> IO ()
run opts = do
  env <- mkEnv opts
  -- We can fire and forget this thread because it keeps respawning itself using the
  -- 'onConnectionClosedHandler'. We do need the Async handle to explicitly cancel the
  -- thread when we shutdown via signals.
  notificationThread <- async $ runAppT env $ BackendNotificationPusher.startWorker opts.rabbitmq
  let -- cleanup will run in a new thread when the signal is caught
      cleanup = do
        -- Clean up the threads running queue listeners
        let l = logger env
        -- Cancel the thread and wait for it to close.
        Log.info l $ Log.msg (Log.val "Cancelling the notification pusher thread thread")
        cancel notificationThread
  let server = defaultServer (cs $ opts.backgroundWorker._epHost) opts.backgroundWorker._epPort env.logger env.metrics
  settings <- newSettings server
  -- Additional cleanup when shutting down via signals.
  runSettingsWithShutdown' cleanup settings (servantApp env) Nothing

servantApp :: Env -> Application
servantApp env =
  Metrics.servantPrometheusMiddleware (Proxy @(ToServant Health.HealthAPI AsApi)) $
    genericServe $
      Health.api env
