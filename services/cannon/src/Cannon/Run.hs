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
import Cannon.App (maxPingInterval)
import qualified Cannon.Dict as D
import Cannon.Options
import Cannon.Types (Cannon, applog, clients, mkEnv, monitor, runCannon, runCannon')
import Cannon.WS hiding (env)
import qualified Control.Concurrent.Async as Async
import Control.Exception.Safe (catchAny)
import Control.Lens ((^.))
import Control.Monad.Catch (MonadCatch, finally)
import Data.Metrics.Middleware (gaugeSet, path)
import qualified Data.Metrics.Middleware as Middleware
import Data.Metrics.Middleware.Prometheus (waiPrometheusMiddleware)
import Data.Text (pack, strip)
import Data.Text.Encoding (encodeUtf8)
import Imports hiding (head)
import qualified Network.Wai as Wai
import Network.Wai.Handler.Warp hiding (run)
import qualified Network.Wai.Middleware.Gzip as Gzip
import Network.Wai.Utilities.Server
import qualified System.IO.Strict as Strict
import qualified System.Logger.Class as LC
import qualified System.Logger.Extended as L
import System.Random.MWC (createSystemRandom)

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
      app r k = runCannon e (route rtree r k) r
      middleware :: Wai.Middleware
      middleware =
        waiPrometheusMiddleware sitemap
          . catchErrors g [Right m]
          . Gzip.gzip Gzip.def
      start = middleware app
  runSettings s start `finally` do
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
