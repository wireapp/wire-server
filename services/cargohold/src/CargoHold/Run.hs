module CargoHold.Run (run) where

import           Imports
import           Control.Lens                       ((^.))
import           Control.Monad.Catch                (finally)
import           Data.Metrics.WaiRoute              (treeToPaths)
import           Data.Metrics.Middleware.Prometheus (waiPrometheusMiddleware)
import           Data.Text                          (unpack)
import           Util.Options
import qualified Network.Wai                        as Wai
import qualified Network.Wai.Middleware.Gzip        as GZip
import qualified Network.Wai.Utilities.Server       as Server
import           Network.Wai.Utilities.Server
import qualified Prometheus                         as Prm

import           CargoHold.Options
import           CargoHold.App
import           CargoHold.API     (sitemap)

run :: Opts -> IO ()
run o = do
    e <- newEnv o
    mx <- Prm.register (Prm.counter $ Prm.Info "net_errors" "count status >= 500 responses")
    s <- Server.newSettings (server e)
    runSettingsWithShutdown s (middleware e mx $ serve e) 5
        `finally` closeEnv e
  where
    rtree      = compile sitemap
    server   e = defaultServer (unpack $ o^.optCargohold.epHost) (o^.optCargohold.epPort) (e^.appLogger) (e^.metrics)
    middleware :: Env -> Prm.Counter -> Wai.Middleware
    middleware e mx = waiPrometheusMiddleware sitemap
                 . measureRequests (e^.metrics) (treeToPaths rtree)
                 . catchErrors (e^.appLogger) [Left mx, Right $ e^.metrics]
                 . GZip.gzip GZip.def

    serve e r k = runHandler e r (Server.route rtree r k) k
