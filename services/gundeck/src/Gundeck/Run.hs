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
{-# LANGUAGE NumericUnderscores #-}

module Gundeck.Run where

import AWS.Util (readAuthExpiration)
import Amazonka qualified as AWS
import Cassandra (runClient, shutdown)
import Cassandra.Schema (versionCheck)
import Control.Error (ExceptT (ExceptT))
import Control.Exception (finally)
import Control.Lens ((.~), (^.))
import Control.Monad.Extra
import Data.Id (RequestId (..))
import Data.Metrics (Metrics)
import Data.Metrics.AWS (gaugeTokenRemaing)
import Data.Metrics.Middleware (metrics)
import Data.Metrics.Middleware.Prometheus (waiPrometheusMiddleware)
import Data.Proxy (Proxy (Proxy))
import Data.Text (unpack)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID
import Database.Redis qualified as Redis
import Gundeck.API (sitemap)
import Gundeck.API.Public (servantSitemap)
import Gundeck.Aws qualified as Aws
import Gundeck.Env
import Gundeck.Env qualified as Env
import Gundeck.Monad
import Gundeck.Options hiding (host, port)
import Gundeck.React
import Gundeck.ThreadBudget
import Imports hiding (head)
import Network.Wai as Wai
import Network.Wai.Middleware.Gunzip qualified as GZip
import Network.Wai.Middleware.Gzip qualified as GZip
import Network.Wai.Utilities (lookupRequestId)
import Network.Wai.Utilities.Server hiding (serverPort)
import Servant (Handler (Handler), (:<|>) (..))
import Servant qualified
import System.Logger ((.=), (~~))
import System.Logger qualified as Log
import UnliftIO.Async qualified as Async
import Util.Options
import Wire.API.Routes.Public.Gundeck (GundeckAPI)
import Wire.API.Routes.Version
import Wire.API.Routes.Version.Wai

run :: Opts -> IO ()
run o = do
  m <- metrics
  (rThreads, e) <- createEnv m o
  runClient (e ^. cstate) $
    versionCheck schemaVersion
  let l = e ^. applog
  s <- newSettings $ defaultServer (unpack $ o ^. gundeck . host) (o ^. gundeck . port) l m
  let throttleMillis = fromMaybe defSqsThrottleMillis $ o ^. (settings . sqsThrottleMillis)

  lst <- Async.async $ Aws.execute (e ^. awsEnv) (Aws.listen throttleMillis (runDirect e . onEvent))
  wtbs <- forM (e ^. threadBudgetState) $ \tbs -> Async.async $ runDirect e $ watchThreadBudgetState m tbs 10
  wCollectAuth <- Async.async (collectAuthMetrics m (Aws._awsEnv (Env._awsEnv e)))

  let app = middleware e (\requestId -> mkApp (e & reqId .~ requestId))
  runSettingsWithShutdown s app Nothing `finally` do
    Log.info l $ Log.msg (Log.val "Shutting down ...")
    shutdown (e ^. cstate)
    Async.cancel lst
    Async.cancel wCollectAuth
    forM_ wtbs Async.cancel
    forM_ rThreads Async.cancel
    Redis.disconnect =<< takeMVar (e ^. rstate)
    whenJust (e ^. rstateAdditionalWrite) $ (=<<) Redis.disconnect . takeMVar
    Log.close (e ^. applog)
  where
    middleware :: Env -> (RequestId -> Wai.Application) -> Wai.Application
    middleware e =
      versionMiddleware (foldMap expandVersionExp (o ^. settings . disabledAPIVersions))
        . waiPrometheusMiddleware sitemap
        . GZip.gunzip
        . GZip.gzip GZip.def
        . catchErrors (e ^. applog) [Right $ e ^. monitor]
        . lookupRequestIdMiddleware (e ^. applog)

    lookupRequestIdMiddleware :: Log.Logger -> (RequestId -> Wai.Application) -> Wai.Application
    lookupRequestIdMiddleware logger mkapp req cont = do
      case lookupRequestId req of
        Just rid -> do
          mkapp (RequestId rid) req cont
        Nothing -> do
          localRid <- RequestId . cs . UUID.toText <$> UUID.nextRandom
          Log.info logger $
            "request-id" .= localRid
              ~~ "method" .= Wai.requestMethod req
              ~~ "path" .= Wai.rawPathInfo req
              ~~ Log.msg (Log.val "generated a new request id for local request")
          mkapp localRid req cont

type CombinedAPI = GundeckAPI :<|> Servant.Raw

mkApp :: Env -> Wai.Application
mkApp env =
  Servant.serve
    (Proxy @CombinedAPI)
    (servantSitemap' env :<|> Servant.Tagged (runGundeckWithRoutes env))
  where
    runGundeckWithRoutes :: Env -> Wai.Application
    runGundeckWithRoutes e r k = runGundeck e r (route (compile sitemap) r k)

servantSitemap' :: Env -> Servant.Server GundeckAPI
servantSitemap' env = Servant.hoistServer (Proxy @GundeckAPI) toServantHandler servantSitemap
  where
    toServantHandler :: Gundeck a -> Handler a
    toServantHandler m = Handler . ExceptT $ Right <$> runDirect env m

collectAuthMetrics :: MonadIO m => Metrics -> AWS.Env -> m ()
collectAuthMetrics m env = do
  liftIO $
    forever $ do
      mbRemaining <- readAuthExpiration env
      gaugeTokenRemaing m mbRemaining
      threadDelay 1_000_000
