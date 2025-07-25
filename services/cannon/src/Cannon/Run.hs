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

module Cannon.Run
  ( run,
    CombinedAPI,
  )
where

import Bilge (ManagerSettings (..), defaultManagerSettings, newManager)
import Cannon.API.Internal
import Cannon.API.Public
import Cannon.App (maxPingInterval)
import Cannon.Dict qualified as D
import Cannon.Options
import Cannon.RabbitMq
import Cannon.Types hiding (Env)
import Cannon.WS hiding (drainOpts, env)
import Cassandra.Util (defInitCassandra)
import Control.Concurrent
import Control.Concurrent.Async qualified as Async
import Control.Exception qualified as E
import Control.Exception.Safe (catchAny)
import Control.Lens ((^.))
import Control.Monad.Catch (MonadCatch)
import Control.Monad.Codensity
import Data.Metrics.Servant
import Data.Proxy
import Data.Text (pack, strip)
import Data.Text.Encoding (encodeUtf8)
import Data.Typeable
import Imports hiding (head, threadDelay)
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp hiding (run)
import Network.Wai.Middleware.Gzip qualified as Gzip
import Network.Wai.Utilities.Server
import OpenTelemetry.Instrumentation.Wai
import OpenTelemetry.Trace hiding (Server)
import OpenTelemetry.Trace qualified as Otel
import Prometheus qualified as Prom
import Servant
import System.IO.Strict qualified as Strict
import System.Logger.Class qualified as LC
import System.Logger.Extended qualified as L
import System.Posix.Signals
import System.Posix.Signals qualified as Signals
import System.Random.MWC (createSystemRandom)
import Wire.API.Routes.Internal.Cannon qualified as Internal
import Wire.API.Routes.Public.Cannon
import Wire.API.Routes.Version
import Wire.API.Routes.Version.Wai
import Wire.OpenTelemetry (withTracer)

type CombinedAPI = CannonAPI :<|> Internal.API

run :: Opts -> IO ()
run o = lowerCodensity $ do
  lift $ validateOpts o
  tracer <- Codensity withTracer
  when (o ^. drainOpts . millisecondsBetweenBatches == 0) $
    error "drainOpts.millisecondsBetweenBatches must not be set to 0."
  when (o ^. drainOpts . gracePeriodSeconds == 0) $
    error "drainOpts.gracePeriodSeconds must not be set to 0."
  ext <- lift loadExternal
  g <-
    Codensity $
      E.bracket
        (L.mkLogger (o ^. logLevel) (o ^. logNetStrings) (o ^. logFormat))
        L.close
  cassandra <- lift $ defInitCassandra (o ^. cassandraOpts) g

  e <- do
    d1 <- D.empty numDictSlices
    d2 <- D.empty numDictSlices
    man <- lift $ newManager defaultManagerSettings {managerConnCount = 128}
    rnd <- lift createSystemRandom
    clk <- lift mkClock
    mkEnv ext o cassandra g d1 d2 man rnd clk (o ^. Cannon.Options.rabbitmq)

  void $ Codensity $ Async.withAsync $ runCannon e refreshMetrics
  let s = newSettings $ Server (o ^. cannon . host) (o ^. cannon . port) (applog e) (Just idleTimeout)

  otelMiddleWare <- lift newOpenTelemetryWaiMiddleware
  let middleware :: Wai.Middleware
      middleware =
        versionMiddleware (foldMap expandVersionExp (o ^. disabledAPIVersions))
          . requestIdMiddleware g defaultRequestIdHeaderName
          . servantPrometheusMiddleware (Proxy @CombinedAPI)
          . otelMiddleWare
          . Gzip.gzip Gzip.def
          . catchErrors g defaultRequestIdHeaderName
      app :: Application
      app = middleware (serve (Proxy @CombinedAPI) server)
      server :: Servant.Server CombinedAPI
      server =
        hoistServer (Proxy @CannonAPI) (runCannonToServant e) publicAPIServer
          :<|> hoistServer (Proxy @Internal.API) (runCannonToServant e) internalServer
  tid <- lift myThreadId

  Codensity $ \k ->
    inSpan tracer "cannon" defaultSpanArguments {kind = Otel.Server} (k ())
  lift $
    E.handle uncaughtExceptionHandler $ do
      let handler = signalHandler (env e) (o ^. drainOpts) tid
      void $ installHandler sigTERM handler Nothing
      void $ installHandler sigINT handler Nothing
      -- FUTUREWORK(@akshaymankar, @fisx): we may want to call `runSettingsWithShutdown` here,
      -- but it's a sensitive change, and it looks like this is closing all the websockets at
      -- the same time and then calling the drain script. I suspect this might be due to some
      -- cleanup in wai.  this needs to be tested very carefully when touched.
      runSettings s app
  where
    idleTimeout = fromIntegral $ maxPingInterval + 3
    -- Each cannon instance advertises its own location (ip or dns name) to gundeck.
    -- Either externalHost or externalHostFile must be set (externalHost takes precedence if both are defined)
    loadExternal :: IO ByteString
    loadExternal = do
      let extFile = fromMaybe (error "One of externalHost or externalHostFile must be defined") (o ^. cannon . externalHostFile)
      maybe (readExternal extFile) (pure . encodeUtf8) (o ^. cannon . externalHost)
    readExternal :: FilePath -> IO ByteString
    readExternal f = encodeUtf8 . strip . pack <$> Strict.readFile f

signalHandler :: Env -> DrainOpts -> ThreadId -> Signals.Handler
signalHandler e opts mainThread = CatchOnce $ do
  runWS e drain
  drainRabbitMqPool e.pool opts
  throwTo mainThread SignalledToExit

-- | This is called when the main thread receives the exception generated by
-- SIGTERM or SIGINT. When that happens, we can simply exit without doing
-- anything. If we leave this exception uncaught, the default runtime behaviour
-- is to print it to stdout, which contaminates the test output.
uncaughtExceptionHandler :: SignalledToExit -> IO ()
uncaughtExceptionHandler _ = pure ()

data SignalledToExit = SignalledToExit
  deriving (Typeable, Show)

instance Exception SignalledToExit

refreshMetrics :: Cannon ()
refreshMetrics = do
  c <- clients
  safeForever $ do
    s <- D.size c
    Prom.setGauge websocketClientsGauge (fromIntegral s)
    -- gaugeSet (fromIntegral s) (path "") m
    liftIO $ threadDelay 1000000
  where
    safeForever :: (MonadIO m, LC.MonadLogger m, MonadCatch m) => m () -> m ()
    safeForever action =
      forever $
        action `catchAny` \exc -> do
          LC.err $ "error" LC..= show exc LC.~~ LC.msg (LC.val "refreshMetrics failed")
          liftIO $ threadDelay 60000000 -- pause to keep worst-case noise in logs manageable

{-# NOINLINE websocketClientsGauge #-}
websocketClientsGauge :: Prom.Gauge
websocketClientsGauge =
  Prom.unsafeRegister $
    Prom.gauge
      Prom.Info
        { Prom.metricName = "net_websocket_clients",
          Prom.metricHelp = "Number of connected websocket clients"
        }
