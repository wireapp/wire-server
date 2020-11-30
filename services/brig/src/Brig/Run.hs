{-# LANGUAGE NumericUnderscores #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
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

module Brig.Run
  ( run,
    mkApp,
  )
where

import Brig.API (sitemap)
import Brig.API.Handler
import Brig.API.Public (ServantAPI, servantSitemap)
import qualified Brig.API.User as API
import Brig.AWS (sesQueue)
import qualified Brig.AWS as AWS
import qualified Brig.AWS.SesNotification as SesNotification
import Brig.App
import qualified Brig.Calling as Calling
import Brig.Data.PendingActivation (PendingActivationExpiration (..), removeTrackedExpirations, searchTrackedExpirations)
import Brig.Data.User (lookupStatus)
import qualified Brig.InternalEvent.Process as Internal
import Brig.Options hiding (internalEvents, sesQueue)
import qualified Brig.Queue as Queue
import qualified Brig.Team.DB as Data
import Brig.Types.Intra (AccountStatus (..))
import qualified Control.Concurrent.Async as Async
import Control.Exception.Safe (catchAny)
import Control.Lens (view, (.~), (^.))
import Control.Monad.Catch (MonadCatch, finally)
import Data.Coerce (coerce)
import Data.Default (Default (def))
import Data.Id (RequestId (..))
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Metrics.Middleware.Prometheus as Metrics
import Data.Proxy (Proxy (Proxy))
import Data.String.Conversions (cs)
import Data.Text (unpack)
import Data.Time.Calendar (Day (..), addDays)
import Data.Time.Clock (UTCTime (..))
import Imports hiding (head)
import qualified Network.Wai as Wai
import qualified Network.Wai.Middleware.Gunzip as GZip
import qualified Network.Wai.Middleware.Gzip as GZip
import Network.Wai.Utilities (lookupRequestId)
import Network.Wai.Utilities.Server
import qualified Network.Wai.Utilities.Server as Server
import Servant ((:<|>) (..))
import qualified Servant
import System.Logger (msg, val, (.=), (~~))
import System.Logger.Class (MonadLogger, err)
import Util.Options

-- FUTUREWORK: If any of these async threads die, we will have no clue about it
-- and brig could start misbehaving. We should ensure that brig dies whenever a
-- thread terminates for any reason.
-- https://github.com/zinfra/backend-issues/issues/1647
run :: Opts -> IO ()
run o = do
  (app, e) <- mkApp o
  s <- Server.newSettings (server e)
  internalEventListener <-
    Async.async $
      runAppT e $
        Queue.listen (e ^. internalEvents) Internal.onEvent
  let throttleMillis = fromMaybe defSqsThrottleMillis $ setSqsThrottleMillis (optSettings o)
  emailListener <- for (e ^. awsEnv . sesQueue) $ \q ->
    Async.async $
      AWS.execute (e ^. awsEnv) $
        AWS.listen throttleMillis q (runAppT e . SesNotification.onEvent)
  sftDiscovery <- forM (e ^. sftEnv) $ Async.async . Calling.startSFTServiceDiscovery (e ^. applog)
  expiryCleanup <- Async.async (runAppT e cleanUpExpired)

  runSettingsWithShutdown s app 5 `finally` do
    mapM_ Async.cancel emailListener
    Async.cancel internalEventListener
    mapM_ Async.cancel sftDiscovery
    Async.cancel expiryCleanup
    closeEnv e
  where
    endpoint = brig o
    server e = defaultServer (unpack $ endpoint ^. epHost) (endpoint ^. epPort) (e ^. applog) (e ^. metrics)

mkApp :: Opts -> IO (Wai.Application, Env)
mkApp o = do
  e <- newEnv o
  return (middleware e $ \reqId -> servantApp (e & requestId .~ reqId), e)
  where
    rtree = compile (sitemap o)
    middleware :: Env -> (RequestId -> Wai.Application) -> Wai.Application
    middleware e =
      Metrics.waiPrometheusMiddleware (sitemap o)
        . catchErrors (e ^. applog) [Right $ e ^. metrics]
        . GZip.gunzip
        . GZip.gzip GZip.def
        . lookupRequestIdMiddleware
    app e r k = runHandler e r (Server.route rtree r k) k
    -- the servant API wraps the one defined using wai-routing
    servantApp :: Env -> Wai.Application
    servantApp e =
      Servant.serve
        (Proxy @(ServantAPI :<|> Servant.Raw))
        (Servant.hoistServer (Proxy @ServantAPI) (toServantHandler e) servantSitemap :<|> Servant.Tagged (app e))

lookupRequestIdMiddleware :: (RequestId -> Wai.Application) -> Wai.Application
lookupRequestIdMiddleware mkapp req cont = do
  let reqid = maybe def RequestId $ lookupRequestId req
  mkapp reqid req cont

cleanUpExpired :: AppIO ()
cleanUpExpired = do
  let nDays = 7
  safeForever "cleanUpExpired" $ do
    today <- utctDay <$> (liftIO =<< view currentTime)
    for_ [0 .. (nDays -1)] $ \i ->
      cleanUpDay (addDays (- i) today)
    liftIO $ threadDelay 1_000_000
  where
    cleanUpDay :: Day -> AppIO ()
    cleanUpDay day =
      forTrackedExpirations day $ \exps -> do
        for_ exps $ \(PendingActivationExpiration uid _ tid) -> do
          isPendingInvitation <- (Just PendingInvitation ==) <$> lookupStatus uid
          invExpired <- isNothing <$> Data.lookupInvitation tid (coerce uid)
          when (isPendingInvitation && invExpired) $
            API.deleteUserNoVerify uid
          removeTrackedExpirations day (maximum exps)

    forTrackedExpirations :: Day -> (NonEmpty PendingActivationExpiration -> AppIO ()) -> AppIO ()
    forTrackedExpirations day f = do
      exps <- searchTrackedExpirations day Nothing
      go exps
      where
        go (e : es) = do
          let exps = e :| es
          f exps
          go =<< searchTrackedExpirations day (Just (maximum exps))
        go [] = pure ()

    safeForever :: (MonadIO m, MonadLogger m, MonadCatch m) => String -> m () -> m ()
    safeForever funName action =
      forever $
        action `catchAny` \exc -> do
          err $ "error" .= show exc ~~ msg (val $ cs funName <> " failed")
          threadDelay 60_000_000 -- pause to keep worst-case noise in logs manageable
