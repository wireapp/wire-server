module Wire.BackgroundWorker where

import Control.Monad.Catch
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
import Wire.Defederation as Defederation

-- FUTUREWORK: Start an http service with status and metrics endpoints
run :: Opts -> IO ()
run opts = do
  (env, syncThread) <- mkEnv opts
  handle (\e@AsyncCancelled -> cancel syncThread >> throwM e) $ do
    -- We can fire and forget these thread because they keep respawning themselves using the
    -- 'onConnectionClosedHandler'
    void $ async $ runAppT env $ BackendNotificationPusher.startWorker opts.rabbitmq
    void $ async $ runAppT env $ Defederation.startWorker opts.rabbitmq
    let server = defaultServer (cs $ opts.backgroundWorker._epHost) opts.backgroundWorker._epPort env.logger env.metrics
    settings <- newSettings server
    runSettingsWithShutdown settings (servantApp env) Nothing

servantApp :: Env -> Application
servantApp env =
  Metrics.servantPrometheusMiddleware (Proxy @(ToServant Health.HealthAPI AsApi)) $
    genericServe $
      Health.api env
