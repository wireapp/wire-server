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
import qualified Cannon.Dict as D
import Cannon.Options
import Cannon.Types (Cannon, applog, clients, env, mkEnv, monitor, runCannon', runCannonToServant)
import Cannon.WS hiding (env)
import qualified Control.Concurrent.Async as Async
import Control.Exception.Safe (catchAny)
import Control.Lens ((^.))
import Control.Monad.Catch (MonadCatch, finally)
import Data.Metrics.Middleware (gaugeSet, path)
import qualified Data.Metrics.Middleware as Middleware
import Data.Metrics.Servant
import Data.Proxy
import Data.Text (pack, strip, unpack)
import Data.Text.Encoding (encodeUtf8)
import Imports hiding (head)
import qualified Network.Wai as Wai
import Network.Wai.Handler.Warp hiding (run)
import qualified Network.Wai.Middleware.Gzip as Gzip
import Network.Wai.Utilities.Server
import Servant
import Servant.Client
import qualified System.IO.Strict as Strict
import qualified System.Logger.Class as LC
import qualified System.Logger.Extended as L
import System.Posix.Signals
import qualified System.Posix.Signals as Signals
import System.Random.MWC (createSystemRandom)
import UnliftIO.Concurrent (myThreadId, throwTo)
import Wire.API.Routes.FederationDomainConfig
import qualified Wire.API.Routes.Internal.Brig as IAPI
import qualified Wire.API.Routes.Internal.Cannon as Internal
import Wire.API.Routes.Named (namedClient)
import Wire.API.Routes.Public.Cannon
import Wire.API.Routes.Version.Wai

type CombinedAPI = PublicAPI :<|> Internal.API

run :: Opts -> IO ()
run o = do
  when (o ^. drainOpts . millisecondsBetweenBatches == 0) $
    error "drainOpts.millisecondsBetweenBatches must not be set to 0."
  when (o ^. drainOpts . gracePeriodSeconds == 0) $
    error "drainOpts.gracePeriodSeconds must not be set to 0."
  ext <- loadExternal
  m <- Middleware.metrics
  g <- L.mkLogger (o ^. logLevel) (o ^. logNetStrings) (o ^. logFormat)
  e <-
    mkEnv m ext o g
      <$> D.empty 128
      <*> newManager defaultManagerSettings {managerConnCount = 128}
      <*> createSystemRandom
      <*> mkClock
  refreshMetricsThread <- Async.async $ runCannon' e refreshMetrics
  s <- newSettings $ Server (o ^. cannon . host) (o ^. cannon . port) (applog e) m (Just idleTimeout)

  -- Get the federaion domain list from Brig and start the updater loop
  manager <- newManager defaultManagerSettings
  let Brig bh bp = o ^. brig
      baseUrl = BaseUrl Http (unpack bh) (fromIntegral bp) ""
      clientEnv = ClientEnv manager baseUrl Nothing defaultMakeClientRequest
      -- Loop the request until we get an answer. This is helpful during integration
      -- tests where services are being brought up in parallel.
      getInitialFedDomains = do
        runClientM getFedRemotes clientEnv >>= \case
          Right strat -> pure strat
          Left err -> do
            print $ "Could not retrieve the latest list of federation domains from Brig: " <> show err
            threadDelay $ o ^. domainUpdateInterval
            getInitialFedDomains
  fedStrat <- getInitialFedDomains
  tEnv <- newTVarIO fedStrat
  let callback :: FederationDomainConfigs -> IO ()
      callback = atomically . writeTVar tEnv
  updateDomainsThread <- Async.async $ updateDomains clientEnv callback

  let middleware :: Wai.Middleware
      middleware =
        versionMiddleware (fold (o ^. disabledAPIVersions))
          . servantPrometheusMiddleware (Proxy @CombinedAPI)
          . Gzip.gzip Gzip.def
          . catchErrors g [Right m]
      app :: Application
      app = middleware (serve (Proxy @CombinedAPI) server)
      server :: Servant.Server CombinedAPI
      server =
        hoistServer (Proxy @PublicAPI) (runCannonToServant e) publicAPIServer
          :<|> hoistServer (Proxy @Internal.API) (runCannonToServant e) internalServer
  tid <- myThreadId
  void $ installHandler sigTERM (signalHandler (env e) tid) Nothing
  void $ installHandler sigINT (signalHandler (env e) tid) Nothing
  runSettings s app `finally` do
    -- FUTUREWORK(@akshaymankar, @fisx): we may want to call `runSettingsWithShutdown` here,
    -- but it's a sensitive change, and it looks like this is closing all the websockets at
    -- the same time and then calling the drain script. I suspect this might be due to some
    -- cleanup in wai.  this needs to be tested very carefully when touched.
    Async.cancel refreshMetricsThread
    Async.cancel updateDomainsThread
    L.close (applog e)
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

    getFedRemotes = namedClient @IAPI.API @"get-federation-remotes"

    updateDomains :: ClientEnv -> (FederationDomainConfigs -> IO ()) -> IO ()
    updateDomains clientEnv update = forever $ do
      threadDelay $ o ^. domainUpdateInterval
      strat <- runClientM getFedRemotes clientEnv
      either
        print
        update
        strat

signalHandler :: Env -> ThreadId -> Signals.Handler
signalHandler e mainThread = CatchOnce $ do
  runWS e drain
  throwTo mainThread SignalledToExit

data SignalledToExit = SignalledToExit
  deriving (Show)

instance Exception SignalledToExit

refreshMetrics :: Cannon ()
refreshMetrics = do
  m <- monitor
  c <- clients
  safeForever $ do
    s <- D.size c
    gaugeSet (fromIntegral s) (path "net.websocket.clients") m
    threadDelay 1000000
  where
    safeForever :: (MonadIO m, LC.MonadLogger m, MonadCatch m) => m () -> m ()
    safeForever action =
      forever $
        action `catchAny` \exc -> do
          LC.err $ "error" LC..= show exc LC.~~ LC.msg (LC.val "refreshMetrics failed")
          threadDelay 60000000 -- pause to keep worst-case noise in logs manageable
