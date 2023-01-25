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
import Control.Error (ExceptT (ExceptT))
import Control.Lens hiding ((.=))
import Control.Monad.Catch hiding (Handler)
import Data.Default (def)
import Data.Id (RequestId (RequestId))
import Data.Metrics.Middleware hiding (path)
import Data.Metrics.Servant (servantPrometheusMiddleware)
import qualified Data.Proxy as DP (Proxy (Proxy))
import qualified Data.Proxy as Data
import Imports hiding (head)
import Network.Wai (Request, requestHeaders)
import Network.Wai.Utilities.Server (catchErrors, defaultServer, newSettings, runSettingsWithShutdown)
import Proxy.API.Internal as I (InternalAPI, servantSitemap)
import Proxy.API.Public as P (servantSitemap)
import Proxy.Env
import Proxy.Options
import Proxy.Proxy
import Servant hiding (Proxy, route)
import System.Logger (Logger)
import Wire.API.Routes.Public.Proxy
import Wire.API.Routes.Version.Wai

type CombinedAPI = ProxyAPI :<|> I.InternalAPI

combinedSitemap :: ServerT CombinedAPI Proxy
combinedSitemap = P.servantSitemap :<|> I.servantSitemap

run :: Opts -> IO ()
run o = do
  let app :: Env -> Application
      app e0 req = Servant.serve (Data.Proxy @CombinedAPI) toServantSitemap req
        where
          toServantSitemap :: Server CombinedAPI
          toServantSitemap = Servant.hoistServer (Data.Proxy @CombinedAPI) toServantHandler combinedSitemap

          toServantHandler :: Proxy a -> Handler a
          toServantHandler p = Handler . ExceptT $ Right <$> runProxy (injectReqId req e0) p

      injectReqId :: Request -> Env -> Env
      injectReqId req = reqId .~ lookupReqId req
        where
          lookupReqId = maybe def RequestId . lookup requestIdName . requestHeaders

      middleware :: Metrics -> Logger -> Application -> Application
      middleware m lgr =
        versionMiddleware (fold (o ^. disabledAPIVersions))
          . servantPrometheusMiddleware (DP.Proxy @CombinedAPI)
          -- `catchErrors` plucks the request id from the request again by hand, so it's ok
          -- the logger we pass here doesn't do that implicitly.  not very elegant or robust,
          -- but works!
          . catchErrors lgr [Right m]

  m <- metrics
  e0 <- createEnv m o
  s <- newSettings $ defaultServer (o ^. host) (o ^. port) (e0 ^. applog) m

  runSettingsWithShutdown s (middleware m (e0 ^. applog) (app e0)) Nothing `finally` destroyEnv e0
