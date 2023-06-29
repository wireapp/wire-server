module Wire.BackgroundWorker where

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

-- FUTUREWORK: Start an http service with status and metrics endpoints
run :: Opts -> IO ()
run opts = do
  env <- mkEnv opts
  -- We can fire and forget this thread because it keeps respawning itself using the
  -- 'onConnectionClosedHandler'
  void $ async $ runAppT env $ BackendNotificationPusher.startWorker opts.rabbitmq
  let server = defaultServer (cs $ opts.backgroundWorker._epHost) opts.backgroundWorker._epPort env.logger env.metrics
  settings <- newSettings server
  runSettingsWithShutdown settings (servantApp env) Nothing

servantApp :: Env -> Application
servantApp = genericServe . Health.api
