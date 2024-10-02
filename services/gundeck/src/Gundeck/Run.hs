-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
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
{-# LANGUAGE NumericUnderscores #-}

module Gundeck.Run where

import AWS.Util (readAuthExpiration)
import Amazonka qualified as AWS
import Cassandra (runClient, shutdown)
import Cassandra.Schema (versionCheck)
import Control.Error (ExceptT (ExceptT))
import Control.Exception (finally)
import Control.Lens ((.~), (^.))
import Control.Monad.Extra
import Data.Metrics.AWS (gaugeTokenRemaing)
import Data.Metrics.Servant qualified as Metrics
import Data.Proxy (Proxy (Proxy))
import Data.Text (unpack)
import Database.Redis qualified as Redis
import Gundeck.API.Internal as Internal (InternalAPI, servantSitemap)
import Gundeck.API.Public as Public (servantSitemap)
import Gundeck.Aws qualified as Aws
import Gundeck.Env
import Gundeck.Env qualified as Env
import Gundeck.Monad
import Gundeck.Options hiding (host, port)
import Gundeck.React
import Gundeck.Schema.Run (lastSchemaVersion)
import Gundeck.ThreadBudget
import Imports
import Network.AMQP
import Network.Wai as Wai
import Network.Wai.Middleware.Gunzip qualified as GZip
import Network.Wai.Middleware.Gzip qualified as GZip
import Network.Wai.Utilities.Request
import Network.Wai.Utilities.Server hiding (serverPort)
import OpenTelemetry.Instrumentation.Wai (newOpenTelemetryWaiMiddleware)
import OpenTelemetry.Trace (defaultSpanArguments, inSpan, kind)
import OpenTelemetry.Trace qualified as Otel
import Servant (Handler (Handler), (:<|>) (..))
import Servant qualified
import System.Logger qualified as Log
import System.Timeout (timeout)
import UnliftIO.Async qualified as Async
import Util.Options
import Wire.API.Notification
import Wire.API.Routes.Public.Gundeck (GundeckAPI)
import Wire.API.Routes.Version
import Wire.API.Routes.Version.Wai
import Wire.OpenTelemetry

run :: Opts -> IO ()
run opts = withTracer \tracer -> do
  (rThreads, env) <- createEnv opts
  let logger = env ^. applog

  setUpRabbitMqExchangesAndQueues logger (env ^. rabbitMqChannel)

  runClient (env ^. cstate) $
    versionCheck lastSchemaVersion
  s <- newSettings $ defaultServer (unpack . host $ opts ^. gundeck) (port $ opts ^. gundeck) logger
  let throttleMillis = fromMaybe defSqsThrottleMillis $ opts ^. (settings . sqsThrottleMillis)

  lst <- Async.async $ Aws.execute (env ^. awsEnv) (Aws.listen throttleMillis (runDirect env . onEvent))
  wtbs <- forM (env ^. threadBudgetState) $ \tbs -> Async.async $ runDirect env $ watchThreadBudgetState tbs 10
  wCollectAuth <- Async.async (collectAuthMetrics (Aws._awsEnv (Env._awsEnv env)))

  app <- middleware env <*> pure (mkApp env)
  inSpan tracer "gundeck" defaultSpanArguments {kind = Otel.Server} (runSettingsWithShutdown s app Nothing) `finally` do
    Log.info logger $ Log.msg (Log.val "Shutting down ...")
    shutdown (env ^. cstate)
    Async.cancel lst
    Async.cancel wCollectAuth
    forM_ wtbs Async.cancel
    forM_ rThreads Async.cancel
    Redis.disconnect =<< takeMVar (env ^. rstate)
    whenJust (env ^. rstateAdditionalWrite) $ (=<<) Redis.disconnect . takeMVar
    Log.close (env ^. applog)
  where
    setUpRabbitMqExchangesAndQueues :: Log.Logger -> MVar Channel -> IO ()
    setUpRabbitMqExchangesAndQueues logger chanMVar = do
      mChan <- timeout 1_000_000 $ readMVar chanMVar
      case mChan of
        Nothing -> do
          -- TODO(leif): we should probably fail here
          Log.err logger $ Log.msg (Log.val "RabbitMQ could not connect")
        Just chan -> do
          Log.info logger $ Log.msg (Log.val "setting up RabbitMQ exchanges and queues")
          createUserNotificationsExchange chan
          createDeadUserNotificationsExchange chan

    createUserNotificationsExchange :: Channel -> IO ()
    createUserNotificationsExchange chan = do
      declareExchange chan newExchange {exchangeName = userNotificationExchangeName, exchangeType = "topic"}

    createDeadUserNotificationsExchange :: Channel -> IO ()
    createDeadUserNotificationsExchange chan = do
      declareExchange chan newExchange {exchangeName = userNotificationDlxName, exchangeType = "direct"}

      let routingKey = userNotificationDlqName
      void $ declareQueue chan newQueue {queueName = userNotificationDlqName}
      bindQueue chan userNotificationDlqName userNotificationDlxName routingKey

    middleware :: Env -> IO Middleware
    middleware env = do
      otelMiddleWare <- newOpenTelemetryWaiMiddleware
      pure $
        versionMiddleware (foldMap expandVersionExp (opts ^. settings . disabledAPIVersions))
          . otelMiddleWare
          . requestIdMiddleware (env ^. applog) defaultRequestIdHeaderName
          . Metrics.servantPrometheusMiddleware (Proxy @(GundeckAPI :<|> InternalAPI))
          . GZip.gunzip
          . GZip.gzip GZip.def
          . catchErrors (env ^. applog) defaultRequestIdHeaderName

mkApp :: Env -> Wai.Application
mkApp env0 req cont = do
  let rid = getRequestId defaultRequestIdHeaderName req
      env = reqId .~ rid $ env0
  Servant.serve (Proxy @(GundeckAPI :<|> InternalAPI)) (servantSitemap' env) req cont

servantSitemap' :: Env -> Servant.Server (GundeckAPI :<|> InternalAPI)
servantSitemap' env = Servant.hoistServer (Proxy @(GundeckAPI :<|> InternalAPI)) toServantHandler (Public.servantSitemap :<|> Internal.servantSitemap)
  where
    toServantHandler :: Gundeck a -> Handler a
    toServantHandler m = Handler . ExceptT $ Right <$> runDirect env m

collectAuthMetrics :: (MonadIO m) => AWS.Env -> m ()
collectAuthMetrics env = do
  liftIO $
    forever $ do
      mbRemaining <- readAuthExpiration env
      gaugeTokenRemaing mbRemaining
      threadDelay 1_000_000
