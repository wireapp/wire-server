{-# LANGUAGE BlockArguments #-}

module Wire.BackgroundWorker where

import Control.Concurrent.Async
import Control.Exception
import Data.ByteString.Conversion
import Data.Metrics.Servant qualified as Metrics
import Data.Text qualified as T
import Imports
import Network.AMQP.Extended (demoteOpts)
import Network.Wai.Utilities.Server
import Servant
import Servant.Server.Generic
import System.Logger qualified as Log
import System.Random
import Util.Options
import Wire.BackendDeadUserNotificationWatcher qualified as DeadUserNotificationWatcher
import Wire.BackendNotificationPusher qualified as BackendNotificationPusher
import Wire.BackgroundWorker.Env
import Wire.BackgroundWorker.Health qualified as Health
import Wire.BackgroundWorker.Options

-- | Restart crashed worker threads: if thread terminates normally, return result.  If thread
-- is cancelled, return `Nothing`.  If anything else happens to the it, log an error and
-- restart.
supervisor ::
  forall a.
  -- | for `runAppT` and logging
  Env ->
  -- | worker name
  String ->
  -- | action
  AppT IO a ->
  -- | the ascyn thread (result is `Nothing` if thread is cancelled)
  IO (Async (Maybe a))
supervisor env workerName workerAction = async loop
  where
    loop :: IO (Maybe a)
    loop = do
      result <- try (runAppT env workerAction)
      case result of
        Right a -> pure (Just a)
        Left e ->
          if fromException e == Just AsyncCancelled
            then do
              Log.info (logger env) $
                (Log.field "worker name: " workerName)
                  . Log.msg (Log.val $ "worker cancelled, shutting down.")
              pure Nothing
            else do
              Log.err (logger env) $
                (Log.field "worker name: " workerName)
                  . Log.msg (Log.val $ "worker crashed: " <> toByteString' (show e) <> ", restarting in <3s")
              threadDelay =<< randomRIO (300_000, 3_000_000)
              loop

run :: Opts -> IO ()
run opts = do
  env <- mkEnv opts
  let amqpEP = either id demoteOpts opts.rabbitmq.unRabbitMqOpts

  cleanupCallback :: MVar (IO ()) <- newMVar (pure ())
  let addToCleanupCallback action = do
        modifyMVar_ cleanupCallback (\actions -> pure (actions >> action))
      runCleanupCallback = do
        join (readMVar cleanupCallback)

  runAppT env (BackendNotificationPusher.startWorker amqpEP) >>= \pusherState ->
    addToCleanupCallback (runAppT env (BackendNotificationPusher.cancelWorker pusherState))

  supervisor env "DeadUserNotificationWatcher" (DeadUserNotificationWatcher.startWorker amqpEP) >>= \worker ->
    addToCleanupCallback (cancel worker)

  let server = defaultServer (T.unpack $ opts.backgroundWorker.host) opts.backgroundWorker.port env.logger
  settings <- newSettings server
  runSettingsWithCleanup runCleanupCallback settings (servantApp env) Nothing

servantApp :: Env -> Application
servantApp env =
  Metrics.servantPrometheusMiddleware (Proxy @(ToServant Health.HealthAPI AsApi)) $
    genericServe $
      Health.api env
