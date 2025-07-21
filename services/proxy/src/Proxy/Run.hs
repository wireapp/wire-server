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

import Bilge.Request (requestIdName)
import Cassandra.Options (host, port)
import Control.Error
import Control.Lens hiding ((.=))
import Control.Monad.Catch
import Data.Id (RequestId (RequestId), defRequestId)
import Data.Metrics.Middleware.Prometheus (waiPrometheusMiddlewarePaths)
import Data.Metrics.Servant
import Data.Text qualified as T
import Imports hiding (head)
import Network.Wai (Middleware, Request, requestHeaders)
import Network.Wai.Middleware.Gunzip qualified as GZip
import Network.Wai.Utilities.Server hiding (serverPort)
import Proxy.API.Internal as I
import Proxy.API.Public as P
import Proxy.Env
import Proxy.Options
import Proxy.Proxy
import Servant qualified
import Wire.API.Routes.Public.Proxy
import Wire.API.Routes.Version
import Wire.API.Routes.Version.Wai

type CombinedAPI = InternalAPI Servant.:<|> ProxyAPI

combinedSitemap :: Env -> Servant.ServerT CombinedAPI Proxy
combinedSitemap env = I.servantSitemap Servant.:<|> P.servantSitemap env

run :: Opts -> IO ()
run o = do
  e <- createEnv o
  let s = newSettings $ defaultServer (o ^. proxy . to (T.unpack . host)) (o ^. proxy . to port) (e ^. applog)

  let metricsMW :: Middleware
      metricsMW = waiPrometheusMiddlewarePaths (routesToPaths @ProxyAPI <> routesToPaths @InternalAPI)

      middleware :: Middleware
      middleware =
        versionMiddleware (foldMap expandVersionExp (o ^. disabledAPIVersions))
          . requestIdMiddleware (e ^. applog) defaultRequestIdHeaderName
          . metricsMW
          . GZip.gunzip
          . catchErrors (e ^. applog) defaultRequestIdHeaderName

  runSettingsWithShutdown s (middleware (mkApp e)) Nothing `finally` destroyEnv e

mkApp :: Env -> Servant.Application
mkApp env req = Servant.serve (Servant.Proxy @CombinedAPI) toServantSitemap req
  where
    toServantSitemap :: Servant.Server CombinedAPI
    toServantSitemap = Servant.hoistServer (Servant.Proxy @CombinedAPI) toServantHandler (combinedSitemap env)

    toServantHandler :: Proxy a -> Servant.Handler a
    toServantHandler p = Servant.Handler . ExceptT $ Right <$> runProxy (injectReqId req env) p

    injectReqId :: Request -> Env -> Env
    injectReqId r = reqId .~ lookupReqId r
      where
        lookupReqId = RequestId . fromMaybe defRequestId . lookup requestIdName . requestHeaders
