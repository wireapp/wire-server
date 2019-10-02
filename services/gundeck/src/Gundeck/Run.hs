module Gundeck.Run where

import Imports hiding (head)
import Cassandra (runClient, shutdown)
import Cassandra.Schema (versionCheck)
import Control.Exception (finally)
import Control.Lens hiding (enum)
import Data.Metrics.Middleware
import Data.Metrics.Middleware.Prometheus (waiPrometheusMiddleware)
import Data.Metrics.WaiRoute (treeToPaths)
import Data.Text (unpack)
import Gundeck.API (sitemap)
import Gundeck.Env
import Gundeck.Monad
import Gundeck.Options
import Gundeck.React
import Gundeck.ThreadBudget
import Network.Wai as Wai
import Network.Wai.Utilities.Server hiding (serverPort)
import Util.Options

import qualified UnliftIO.Async as Async
import qualified Gundeck.Aws as Aws
import qualified Network.Wai.Middleware.Gzip as GZip
import qualified Network.Wai.Middleware.Gunzip as GZip
import qualified System.Logger as Log

run :: Opts -> IO ()
run o = do
    m <- metrics
    e <- createEnv m o
    runClient (e^.cstate) $
        versionCheck schemaVersion
    let l = e^.applog
    s <- newSettings $ defaultServer (unpack $ o^.optGundeck.epHost) (o^.optGundeck.epPort) l m
    lst <- Async.async $ Aws.execute (e^.awsEnv) (Aws.listen (runDirect e . onEvent))
    wtbs <- forM (e ^. threadBudgetState) $ \tbs -> Async.async $ runDirect e $ watchThreadBudgetState m tbs 63
    runSettingsWithShutdown s (middleware e $ app e) 5 `finally` do
        Log.info l $ Log.msg (Log.val "Shutting down ...")
        shutdown (e^.cstate)
        Async.cancel lst
        forM_ wtbs Async.cancel
        Log.close (e^.applog)
  where
    middleware :: Env -> Wai.Middleware
    middleware e = waiPrometheusMiddleware sitemap
                 . measureRequests (e^.monitor) (treeToPaths routes)
                 . catchErrors (e^.applog) [Right $ e^.monitor]
                 . GZip.gunzip . GZip.gzip GZip.def
    app :: Env -> Wai.Application
    app e r k = runGundeck e r (route routes r k)
    routes = compile sitemap
