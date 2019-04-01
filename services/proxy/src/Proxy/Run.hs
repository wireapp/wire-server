module Proxy.Run (run) where

import Imports hiding (head)
import Control.Monad.Catch
import Control.Lens hiding ((.=))
import Data.Metrics.Middleware hiding (path)
import Data.Metrics.Middleware.Prometheus (waiPrometheusMiddleware)
import Data.Metrics.WaiRoute (treeToPaths)
import Network.Wai.Utilities.Server hiding (serverPort)
import Network.Wai.Handler.Warp (runSettings)
import Proxy.Env
import Proxy.Proxy
import Proxy.Options
import Proxy.API (sitemap)

import qualified Prometheus as Prm

run :: Opts -> IO ()
run o = do
    m <- metrics
    mx <- Prm.register (Prm.counter $ Prm.Info "net_errors" "count status >= 500 responses")
    e <- createEnv m o
    s <- newSettings $ defaultServer (o^.host) (o^.port) (e^.applog) m
    let rtree    = compile (sitemap e)
    let measured = measureRequests m (treeToPaths rtree)
    let app r k  = runProxy e r (route rtree r k)
    let middleware = waiPrometheusMiddleware (sitemap e)
                   . measured
                   . catchErrors (e^.applog) [Left mx, Right m]
    runSettings s (middleware app) `finally` destroyEnv e
