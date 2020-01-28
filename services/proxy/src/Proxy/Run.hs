module Proxy.Run (run) where

import Imports hiding (head)
import Control.Monad.Catch
import Control.Lens hiding ((.=))
import Data.Metrics.Middleware hiding (path)
import Data.Metrics.Middleware.Prometheus (waiPrometheusMiddleware)
import Network.Wai.Utilities.Server hiding (serverPort)
import Network.Wai.Handler.Warp (runSettings)
import Proxy.Env
import Proxy.Proxy
import Proxy.Options
import Proxy.API (sitemap)

run :: Opts -> IO ()
run o = do
    m <- metrics
    e <- createEnv m o
    s <- newSettings $ defaultServer (o^.host) (o^.port) (e^.applog) m
    let rtree    = compile (sitemap e)
    let app r k  = runProxy e r (route rtree r k)
    let middleware = waiPrometheusMiddleware (sitemap e)
                   . catchErrors (e^.applog) [Right m]
    runSettings s (middleware app) `finally` destroyEnv e
