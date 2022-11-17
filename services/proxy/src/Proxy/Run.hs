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

import Control.Error (ExceptT (ExceptT))
import Control.Lens hiding ((.=))
import Control.Monad.Catch hiding (Handler)
import Data.Metrics.Middleware hiding (path)
import Data.Metrics.Middleware.Prometheus (waiPrometheusMiddleware)
import qualified Data.Proxy as Data
import Imports hiding (head)
import Network.Wai.Utilities.Server hiding (serverPort)
import Proxy.API.Internal as I (InternalAPI, servantSitemap)
import Proxy.API.Public as P (servantSitemap, sitemap)
import Proxy.Env
import Proxy.Options
import Proxy.Proxy
import Servant hiding (Proxy, route)
import Wire.API.Routes.Public.Proxy
import Wire.API.Routes.Version.Wai

run :: Opts -> IO ()
run o = do
  m <- metrics
  e <- createEnv m o
  s <- newSettings $ defaultServer (o ^. host) (o ^. port) (e ^. applog) m
  let rtree = compile (P.sitemap e)
  let waiRouteApp r k = runProxy e r (route rtree r k)
  let toServantHandler :: Proxy a -> Handler a
      toServantHandler p = Handler . ExceptT $ Right <$> runDirect e p
  let toServantSitemap =
        Servant.hoistServer
          (Data.Proxy @(ProxyAPI :<|> I.InternalAPI))
          toServantHandler
          (P.servantSitemap :<|> I.servantSitemap)
  let app =
        Servant.serve
          (Data.Proxy @((ProxyAPI :<|> InternalAPI) :<|> Servant.Raw))
          (toServantSitemap :<|> Servant.Tagged waiRouteApp)
  let middleware =
        versionMiddleware
          . waiPrometheusMiddleware (P.sitemap e)
          . catchErrors (e ^. applog) [Right m]
  runSettingsWithShutdown s (middleware app) Nothing `finally` destroyEnv e
