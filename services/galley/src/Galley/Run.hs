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

module Galley.Run
  ( run,
    mkApp,
  )
where

import Cassandra (runClient, shutdown)
import Cassandra.Schema (versionCheck)
import qualified Control.Concurrent.Async as Async
import Control.Exception (finally)
import Control.Lens (view, (^.))
import qualified Data.Metrics.Middleware as M
import Data.Metrics.Middleware.Prometheus (waiPrometheusMiddleware)
import Data.Misc (portNumber)
import Data.Text (unpack)
import Galley.API (sitemap)
import qualified Galley.API.Internal as Internal
import Galley.App
import qualified Galley.App as App
import qualified Galley.Data as Data
import Galley.Options (Opts, optGalley)
import qualified Galley.Queue as Q
import Imports
import Network.Wai (Application)
import qualified Network.Wai.Middleware.Gunzip as GZip
import qualified Network.Wai.Middleware.Gzip as GZip
import Network.Wai.Utilities.Server
import Servant (Proxy (Proxy))
import Servant.API ((:<|>) ((:<|>)))
import qualified Servant.API as Servant
import qualified Servant.Server as Servant
import qualified System.Logger.Class as Log
import Util.Options

run :: Opts -> IO ()
run o = do
  (app, e) <- mkApp o
  let l = e ^. App.applog
  s <-
    newSettings $
      defaultServer
        (unpack $ o ^. optGalley . epHost)
        (portNumber $ fromIntegral $ o ^. optGalley . epPort)
        l
        (e ^. monitor)
  deleteQueueThread <- Async.async $ evalGalley e Internal.deleteLoop
  refreshMetricsThread <- Async.async $ evalGalley e refreshMetrics
  runSettingsWithShutdown s app 5 `finally` do
    Async.cancel deleteQueueThread
    Async.cancel refreshMetricsThread
    shutdown (e ^. cstate)
    Log.flush l
    Log.close l

mkApp :: Opts -> IO (Application, Env)
mkApp o = do
  m <- M.metrics
  e <- App.createEnv m o
  let l = e ^. App.applog
  runClient (e ^. cstate) $
    versionCheck Data.schemaVersion
  return (middlewares l m $ servantApp e, e)
  where
    rtree = compile sitemap
    app e r k = runGalley e r (route rtree r k)
    -- the servant API wraps the one defined using wai-routing
    servantApp e r =
      Servant.serve
        -- we don't host any Servant endpoints yet, but will add some for the
        -- federation API, replacing the empty API.
        (Proxy @(Servant.EmptyAPI :<|> Servant.Raw))
        (Servant.emptyServer :<|> Servant.Tagged (app e))
        r
    middlewares l m =
      waiPrometheusMiddleware sitemap
        . catchErrors l [Right m]
        . GZip.gunzip
        . GZip.gzip GZip.def

refreshMetrics :: Galley ()
refreshMetrics = do
  m <- view monitor
  q <- view deleteQueue
  Internal.safeForever "refreshMetrics" $ do
    n <- Q.len q
    M.gaugeSet (fromIntegral n) (M.path "galley.deletequeue.len") m
    threadDelay 1000000
