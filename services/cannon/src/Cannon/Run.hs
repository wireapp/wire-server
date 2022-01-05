-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
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
  )
where

import Bilge (ManagerSettings (..), defaultManagerSettings, newManager)
import Cannon.API (sitemap)
import Cannon.API.Public
import Cannon.App (maxPingInterval)
import qualified Cannon.Dict as D
import Cannon.Options
import Cannon.Types (Cannon, Env, applog, clients, mkEnv, monitor, runCannon, runCannon')
import Cannon.WS hiding (env)
import qualified Control.Concurrent.Async as Async
import Control.Exception.Safe (catchAny)
import Control.Lens ((^.))
import Control.Monad.Catch (MonadCatch, finally)
import Data.Metrics.Middleware (gaugeSet, path)
import qualified Data.Metrics.Middleware as Middleware
import Data.Metrics.Servant
import Data.Proxy
import Data.Text (pack, strip)
import Data.Text.Encoding (encodeUtf8)
import Imports hiding (head)
import qualified Network.Wai as Wai
import Network.Wai.Handler.Warp hiding (run)
import qualified Network.Wai.Middleware.Gzip as Gzip
import Network.Wai.Utilities.Server
import Servant
import qualified System.IO.Strict as Strict
import qualified System.Logger.Class as LC
import qualified System.Logger.Extended as L
import System.Random.MWC (createSystemRandom)
import Wire.API.Routes.Public.Cannon

run :: Opts -> IO ()
run o = do
  ext <- loadExternal
  m <- Middleware.metrics
  g <- L.mkLogger (o ^. logLevel) (o ^. logNetStrings) (o ^. logFormat)
  e <-
    mkEnv <$> pure m
      <*> pure ext
      <*> pure o
      <*> pure g
      <*> D.empty 128
      <*> newManager defaultManagerSettings {managerConnCount = 128}
      <*> createSystemRandom
      <*> mkClock
  refreshMetricsThread <- Async.async $ runCannon' e refreshMetrics
  s <- newSettings $ Server (o ^. cannon . host) (o ^. cannon . port) (applog e) m (Just idleTimeout)
  let rtree = compile sitemap
      app r k = runCannon e (Network.Wai.Utilities.Server.route rtree r k) r
      middleware :: Wai.Middleware
      middleware =
        servantPlusWAIPrometheusMiddleware sitemap (Proxy @ServantAPI)
          . Gzip.gzip Gzip.def
          . catchErrors g [Right m]
      start :: Application
      start = middleware app
      -- TODO(sven): This heap of function definitions can be cleaned up.
      serverPublicApi :: Servant.Server ServantAPI
      serverPublicApi = hoistServer (Proxy @ServantAPI) (nt e) publicAPIServer
      serverRaw :: Servant.Server Raw
      serverRaw = Tagged start
      server :: Servant.Server API
      server = serverPublicApi :<|> serverRaw
  runSettings s (serve (Proxy @API) server) `finally` do
    Async.cancel refreshMetricsThread
    L.close (applog e)
  where
    idleTimeout = fromIntegral $ maxPingInterval + 3
    -- Each cannon instance advertises its own location (ip or dns name) to gundeck.
    -- Either externalHost or externalHostFile must be set (externalHost takes precedence if both are defined)
    loadExternal :: IO ByteString
    loadExternal = do
      let extFile = fromMaybe (error "One of externalHost or externalHostFile must be defined") (o ^. cannon . externalHostFile)
      fromMaybe (readExternal extFile) (return . encodeUtf8 <$> o ^. cannon . externalHost)
    readExternal :: FilePath -> IO ByteString
    readExternal f = encodeUtf8 . strip . pack <$> Strict.readFile f
    nt :: Cannon.Types.Env -> Cannon x -> Handler x
    nt env c = liftIO $ runCannon' env c

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
