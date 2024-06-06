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

module Proxy.Run
  ( run,
  )
where

import Control.Lens hiding ((.=))
import Control.Monad.Catch
import Data.Metrics.Middleware.Prometheus (waiPrometheusMiddleware)
import Imports hiding (head)
import Network.Wai.Middleware.Gunzip qualified as GZip
import Network.Wai.Utilities.Server hiding (serverPort)
import Proxy.API (sitemap)
import Proxy.Env
import Proxy.Options
import Proxy.Proxy
import Wire.API.Routes.Version
import Wire.API.Routes.Version.Wai

run :: Opts -> IO ()
run o = do
  e <- createEnv o
  s <- newSettings $ defaultServer (o ^. host) (o ^. port) (e ^. applog)
  let rtree = compile (sitemap e)
  let app r k = runProxy e r (route rtree r k)
  let middleware =
        versionMiddleware (foldMap expandVersionExp (o ^. disabledAPIVersions))
          . requestIdMiddleware (e ^. applog) defaultRequestIdHeaderName
          . waiPrometheusMiddleware (sitemap e)
          . GZip.gunzip
          . catchErrors (e ^. applog) defaultRequestIdHeaderName
  runSettingsWithShutdown s (middleware app) Nothing `finally` destroyEnv e
