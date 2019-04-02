module Brig.Run (run) where

import           Imports                      hiding (head)
import           Brig.App
import           Brig.API                     (sitemap)
import           Brig.AWS                     (sesQueue)
import           Brig.API.Handler
import           Brig.Options                 hiding (internalEvents, sesQueue)
import           Control.Monad.Catch          (finally)
import           Control.Lens                 ((^.))
import           Data.Metrics.WaiRoute        (treeToPaths)
import           Data.Text                    (unpack)
import           Network.Wai.Utilities.Server
import           Util.Options

import qualified Brig.AWS                           as AWS
import qualified Brig.AWS.SesNotification           as SesNotification
import qualified Brig.InternalEvent.Process         as Internal
import qualified Brig.Queue                         as Queue
import qualified Control.Concurrent.Async           as Async
import qualified Data.Metrics.Middleware.Prometheus as Metrics
import qualified Network.Wai                        as Wai
import qualified Network.Wai.Middleware.Gunzip      as GZip
import qualified Network.Wai.Middleware.Gzip        as GZip
import qualified Network.Wai.Utilities.Server       as Server
import qualified Prometheus as Prm


run :: Opts -> IO ()
run o = do
    e <- newEnv o
    mx <- Prm.register (Prm.counter $ Prm.Info "net_errors" "count status >= 500 responses")
    s <- Server.newSettings (server e)
    emailListener <- for (e^.awsEnv.sesQueue) $ \q ->
        Async.async $
        AWS.execute (e^.awsEnv) $
        AWS.listen q (runAppT e . SesNotification.onEvent)
    internalEventListener <- Async.async $
        runAppT e $ Queue.listen (e^.internalEvents) Internal.onEvent
    runSettingsWithShutdown s (middleware e mx $ serve e) 5 `finally` do
        mapM_ Async.cancel emailListener
        Async.cancel internalEventListener
        closeEnv e
  where
    rtree      = compile (sitemap o)
    endpoint   = brig o
    server   e = defaultServer (unpack $ endpoint^.epHost) (endpoint^.epPort) (e^.applog) (e^.metrics)
    middleware :: Env -> Prm.Counter -> Wai.Middleware
    middleware e mx = Metrics.waiPrometheusMiddleware (sitemap o)
                 . measureRequests (e^.metrics) (treeToPaths rtree)
                 . catchErrors (e^.applog) [Left mx, Right $ e^.metrics]
                 . GZip.gunzip . GZip.gzip GZip.def
    serve e r k = runHandler e r (Server.route rtree r k) k
