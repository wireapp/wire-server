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
import Brig.Data.PendingActivation (PendingActivationExpiration (..), removeTrackedExpiration, searchTrackedExpirations)
import Brig.Data.User (lookupStatus)
import qualified Brig.InternalEvent.Process as Internal
import Brig.Options hiding (internalEvents, sesQueue)
import qualified Brig.Queue as Queue
import qualified Brig.Team.DB as Data
import Brig.Types.Intra (AccountStatus (..))
import Cassandra.Exec (Page (Page), liftClient)
import qualified Control.Concurrent.Async as Async
import Control.Exception.Safe (catchAny)
import Control.Lens (view, (.~), (^.))
import Control.Monad.Catch (MonadCatch, finally)
-- import Control.Monad.Random (Random (randomRIO))

import Control.Monad.Random (Random (randomRIO))
import Data.Coerce (coerce)
import Data.Default (Default (def))
import Data.Id (RequestId (..), UserId)
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
  expiryCleanup <- Async.async (runAppT e cleanExpiredPendingInvitations)

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

cleanExpiredPendingInvitations :: AppIO ()
cleanExpiredPendingInvitations = do
  safeForever "cleanExpiredPendingInvitations" $ do
    today <- utctDay <$> (liftIO =<< view currentTime)
    for_ [0 .. 9] $ \i ->
      cleanUpDay (addDays (- i) today)
    let d :: Int = 24 * 60 * 60
    randomSecs <- liftIO (round <$> randomRIO @Double (0.5 * fromIntegral d, fromIntegral d))
    threadDelay (randomSecs * 1_000_000)
  where
    cleanUpDay :: Day -> AppIO ()
    cleanUpDay day = do
      expiredEntries <- forTrackedExpirations day $ \exps ->
        catMaybes
          <$> ( for exps $ \(PendingActivationExpiration uid expiresAt tid) -> do
                  isExpired <- (expiresAt <=) <$> (liftIO =<< view currentTime)
                  if isExpired
                    then do
                      isPendingInvitation <- (Just PendingInvitation ==) <$> lookupStatus uid
                      invExpired <- isNothing <$> Data.lookupInvitation tid (coerce uid)
                      when (isPendingInvitation && invExpired) $ do
                        API.deleteUserNoVerify uid
                      pure (Just uid)
                    else pure Nothing
              )
      unless (null expiredEntries) $
        removeTrackedExpiration day expiredEntries

    forTrackedExpirations :: Day -> ([PendingActivationExpiration] -> AppIO [UserId]) -> AppIO [UserId]
    forTrackedExpirations day f = do
      page <- searchTrackedExpirations day
      go [] page
      where
        go :: [UserId] -> Page PendingActivationExpiration -> AppIO [UserId]
        go users (Page hasMore result nextPage) = do
          users' <- (<> users) <$> f result
          if hasMore
            then go users' =<< liftClient nextPage
            else pure users'

    safeForever :: (MonadIO m, MonadLogger m, MonadCatch m) => String -> m () -> m ()
    safeForever funName action =
      forever $
        action `catchAny` \exc -> do
          err $ "error" .= show exc ~~ msg (val $ cs funName <> " failed")
          -- pause to keep worst-case noise in logs manageable
          threadDelay 60_000_000
