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

module Galley.Run (run) where

import Cassandra (runClient, shutdown)
import Cassandra.Schema (versionCheck)
import qualified Control.Concurrent.Async as Async
import Control.Exception (finally)
import Control.Lens ((^.))
import qualified Data.Metrics.Middleware as M
import Data.Metrics.Middleware.Prometheus (waiPrometheusMiddleware)
import Data.Misc (portNumber)
import Data.Text (unpack)
import Galley.API (sitemap)
import qualified Galley.API.Internal as Internal
import qualified Galley.App as App
import Galley.App
import qualified Galley.Data as Data
import Galley.Options (Opts, optGalley)
import Imports
import Network.Wai (Middleware)
import qualified Network.Wai.Middleware.Gunzip as GZip
import qualified Network.Wai.Middleware.Gzip as GZip
import Network.Wai.Utilities.Server
import qualified System.Logger.Class as Log
import Util.Options

run :: Opts -> IO ()
run o = do
  m <- M.metrics
  e <- App.createEnv m o
  let l = e ^. App.applog
  s <-
    newSettings $
      defaultServer
        (unpack $ o ^. optGalley . epHost)
        (portNumber $ fromIntegral $ o ^. optGalley . epPort)
        l
        m
  runClient (e ^. cstate) $
    versionCheck Data.schemaVersion
  deleteQueueThread <- Async.async $ evalGalley e Internal.deleteLoop
  refreshMetricsThread <- Async.async $ evalGalley e Internal.refreshMetrics
  let rtree = compile sitemap
      app r k = runGalley e r (route rtree r k)
      middlewares :: Middleware
      middlewares =
        waiPrometheusMiddleware sitemap
          . catchErrors l [Right m]
          . GZip.gunzip
          . GZip.gzip GZip.def
  runSettingsWithShutdown s (middlewares app) 5 `finally` do
    Async.cancel deleteQueueThread
    Async.cancel refreshMetricsThread
    shutdown (e ^. cstate)
    Log.flush l
    Log.close l
