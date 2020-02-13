module Proxy.Run (run) where

import Control.Lens hiding ((.=))
import Control.Monad.Catch
import Data.Metrics.Middleware hiding (path)
import Data.Metrics.Middleware.Prometheus (waiPrometheusMiddleware)
import Imports hiding (head)
import Network.Wai.Handler.Warp (runSettings)
import Network.Wai.Utilities.Server hiding (serverPort)
import Proxy.API (sitemap)
import Proxy.Env
import Proxy.Options
import Proxy.Proxy

run :: Opts -> IO ()
run o = do
  m <- metrics
  e <- createEnv m o
  s <- newSettings $ defaultServer (o ^. host) (o ^. port) (e ^. applog) m
  let rtree = compile (sitemap e)
  let app r k = runProxy e r (route rtree r k)
  let middleware =
        waiPrometheusMiddleware (sitemap e)
          . catchErrors (e ^. applog) [Right m]
  runSettings s (middleware app) `finally` destroyEnv e
