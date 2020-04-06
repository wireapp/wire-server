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

module Gundeck.Run where

import Cassandra (runClient, shutdown)
import Cassandra.Schema (versionCheck)
import Control.Exception (finally)
import Control.Lens hiding (enum)
import Data.Metrics.Middleware (metrics)
import Data.Metrics.Middleware.Prometheus (waiPrometheusMiddleware)
import Data.Text (unpack)
import Gundeck.API (sitemap)
import qualified Gundeck.Aws as Aws
import Gundeck.Env
import Gundeck.Monad
import Gundeck.Options
import Gundeck.React
import Gundeck.ThreadBudget
import Imports hiding (head)
import Network.Wai as Wai
import qualified Network.Wai.Middleware.Gunzip as GZip
import qualified Network.Wai.Middleware.Gzip as GZip
import Network.Wai.Utilities.Server hiding (serverPort)
import qualified System.Logger as Log
import qualified UnliftIO.Async as Async
import Util.Options

run :: Opts -> IO ()
run o = do
  m <- metrics
  e <- createEnv m o
  runClient (e ^. cstate) $
    versionCheck schemaVersion
  let l = e ^. applog
  s <- newSettings $ defaultServer (unpack $ o ^. optGundeck . epHost) (o ^. optGundeck . epPort) l m
  lst <- Async.async $ Aws.execute (e ^. awsEnv) (Aws.listen (runDirect e . onEvent))
  wtbs <- forM (e ^. threadBudgetState) $ \tbs -> Async.async $ runDirect e $ watchThreadBudgetState m tbs 10
  runSettingsWithShutdown s (middleware e $ app e) 5 `finally` do
    Log.info l $ Log.msg (Log.val "Shutting down ...")
    shutdown (e ^. cstate)
    Async.cancel lst
    forM_ wtbs Async.cancel
    Log.close (e ^. applog)
  where
    middleware :: Env -> Wai.Middleware
    middleware e =
      waiPrometheusMiddleware sitemap
        . catchErrors (e ^. applog) [Right $ e ^. monitor]
        . GZip.gunzip
        . GZip.gzip GZip.def
    app :: Env -> Wai.Application
    app e r k = runGundeck e r (route routes r k)
    routes = compile sitemap
