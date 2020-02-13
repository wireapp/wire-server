module Brig.Run (run, mkApp) where

import Brig.API (sitemap)
import Brig.API.Handler
import Brig.AWS (sesQueue)
import qualified Brig.AWS as AWS
import qualified Brig.AWS.SesNotification as SesNotification
import Brig.App
import qualified Brig.InternalEvent.Process as Internal
import Brig.Options hiding (internalEvents, sesQueue)
import qualified Brig.Queue as Queue
import qualified Control.Concurrent.Async as Async
import Control.Lens ((^.))
import Control.Monad.Catch (finally)
import qualified Data.Metrics.Middleware.Prometheus as Metrics
import Data.Text (unpack)
import Imports hiding (head)
import qualified Network.Wai as Wai
import qualified Network.Wai.Middleware.Gunzip as GZip
import qualified Network.Wai.Middleware.Gzip as GZip
import Network.Wai.Utilities.Server
import qualified Network.Wai.Utilities.Server as Server
import Util.Options

run :: Opts -> IO ()
run o = do
  (app, e) <- mkApp o
  s <- Server.newSettings (server e)
  internalEventListener <-
    Async.async
      $ runAppT e
      $ Queue.listen (e ^. internalEvents) Internal.onEvent
  emailListener <- for (e ^. awsEnv . sesQueue) $ \q ->
    Async.async
      $ AWS.execute (e ^. awsEnv)
      $ AWS.listen q (runAppT e . SesNotification.onEvent)
  runSettingsWithShutdown s app 5 `finally` do
    mapM_ Async.cancel emailListener
    Async.cancel internalEventListener
    closeEnv e
  where
    endpoint = brig o
    server e = defaultServer (unpack $ endpoint ^. epHost) (endpoint ^. epPort) (e ^. applog) (e ^. metrics)

mkApp :: Opts -> IO (Wai.Application, Env)
mkApp o = do
  e <- newEnv o
  return (middleware e $ serve e, e)
  where
    rtree = compile (sitemap o)
    middleware :: Env -> Wai.Middleware
    middleware e =
      Metrics.waiPrometheusMiddleware (sitemap o)
        . catchErrors (e ^. applog) [Right $ e ^. metrics]
        . GZip.gunzip
        . GZip.gzip GZip.def
    serve e r k = runHandler e r (Server.route rtree r k) k
