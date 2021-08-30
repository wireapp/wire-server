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
import Brig.API.Federation (federationSitemap)
import Brig.API.Handler
import qualified Brig.API.Internal as IAPI
import Brig.API.Public (ServantAPI, SwaggerDocsAPI, servantSitemap, swaggerDocsAPI)
import qualified Brig.API.User as API
import Brig.AWS (sesQueue)
import qualified Brig.AWS as AWS
import qualified Brig.AWS.SesNotification as SesNotification
import Brig.App
import qualified Brig.Calling as Calling
import Brig.Data.UserPendingActivation (UserPendingActivation (..), usersPendingActivationList, usersPendingActivationRemoveMultiple)
import qualified Brig.InternalEvent.Process as Internal
import Brig.Options hiding (internalEvents, sesQueue)
import qualified Brig.Queue as Queue
import Brig.Types.Intra (AccountStatus (PendingInvitation))
import Cassandra (Page (Page), liftClient)
import qualified Control.Concurrent.Async as Async
import Control.Exception.Safe (catchAny)
import Control.Lens (view, (.~), (^.))
import Control.Monad.Catch (MonadCatch, finally)
import Control.Monad.Random (Random (randomRIO))
import qualified Data.Aeson as Aeson
import Data.Default (Default (def))
import Data.Id (RequestId (..))
import qualified Data.Metrics.Servant as Metrics
import Data.Proxy (Proxy (Proxy))
import Data.String.Conversions (cs)
import Data.Text (unpack)
import Imports hiding (head)
import qualified Network.HTTP.Media as HTTPMedia
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import qualified Network.Wai.Middleware.Gunzip as GZip
import qualified Network.Wai.Middleware.Gzip as GZip
import Network.Wai.Routing (Tree)
import Network.Wai.Routing.Route (App)
import Network.Wai.Utilities (lookupRequestId)
import Network.Wai.Utilities.Server
import qualified Network.Wai.Utilities.Server as Server
import Servant (Context ((:.)), (:<|>) (..))
import qualified Servant
import Servant.API.Generic (ToServantApi, genericApi)
import System.Logger (msg, val, (.=), (~~))
import System.Logger.Class (MonadLogger, err)
import Util.Options
import qualified Wire.API.Federation.API.Brig as FederationBrig

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
  pendingActivationCleanupAsync <- Async.async (runAppT e pendingActivationCleanup)

  runSettingsWithShutdown s app 5 `finally` do
    mapM_ Async.cancel emailListener
    Async.cancel internalEventListener
    mapM_ Async.cancel sftDiscovery
    Async.cancel pendingActivationCleanupAsync
    closeEnv e
  where
    endpoint = brig o
    server e = defaultServer (unpack $ endpoint ^. epHost) (endpoint ^. epPort) (e ^. applog) (e ^. metrics)

mkApp :: Opts -> IO (Wai.Application, Env)
mkApp o = do
  e <- newEnv o
  return (middleware e $ \reqId -> servantApp (e & requestId .~ reqId), e)
  where
    rtree :: Tree (App Handler)
    rtree = compile sitemap

    middleware :: Env -> (RequestId -> Wai.Application) -> Wai.Application
    middleware e =
      Metrics.servantPlusWAIPrometheusMiddleware sitemap (Proxy @ServantCombinedAPI)
        . GZip.gunzip
        . GZip.gzip GZip.def
        . catchErrors (e ^. applog) [Right $ e ^. metrics]
        . lookupRequestIdMiddleware
    app e r k = runHandler e r (Server.route rtree r k) k

    -- the servant API wraps the one defined using wai-routing
    servantApp :: Env -> Wai.Application
    servantApp e =
      Servant.serveWithContext
        (Proxy @ServantCombinedAPI)
        (customFormatters :. Servant.EmptyContext)
        ( swaggerDocsAPI
            :<|> Servant.hoistServer (Proxy @ServantAPI) (toServantHandler e) servantSitemap
            :<|> Servant.hoistServer (Proxy @IAPI.ServantAPI) (toServantHandler e) IAPI.servantSitemap
            :<|> Servant.hoistServer (genericApi (Proxy @FederationBrig.Api)) (toServantHandler e) federationSitemap
            :<|> Servant.Tagged (app e)
        )

type ServantCombinedAPI =
  ( SwaggerDocsAPI
      :<|> ServantAPI
      :<|> IAPI.ServantAPI
      :<|> ToServantApi FederationBrig.Api
      :<|> Servant.Raw
  )

lookupRequestIdMiddleware :: (RequestId -> Wai.Application) -> Wai.Application
lookupRequestIdMiddleware mkapp req cont = do
  let reqid = maybe def RequestId $ lookupRequestId req
  mkapp reqid req cont

customFormatters :: Servant.ErrorFormatters
customFormatters =
  Servant.defaultErrorFormatters
    { Servant.bodyParserErrorFormatter = bodyParserErrorFormatter
    }

bodyParserErrorFormatter :: Servant.ErrorFormatter
bodyParserErrorFormatter _ _ errMsg =
  Servant.ServerError
    { Servant.errHTTPCode = HTTP.statusCode HTTP.status400,
      Servant.errReasonPhrase = cs $ HTTP.statusMessage HTTP.status400,
      Servant.errBody =
        Aeson.encode $
          Aeson.object
            [ "code" Aeson..= Aeson.Number 400,
              "message" Aeson..= errMsg,
              "label" Aeson..= ("bad-request" :: Text)
            ],
      Servant.errHeaders = [(HTTP.hContentType, HTTPMedia.renderHeader (Servant.contentType (Proxy @Servant.JSON)))]
    }

pendingActivationCleanup :: AppIO ()
pendingActivationCleanup = do
  safeForever "pendingActivationCleanup" $ do
    now <- liftIO =<< view currentTime
    forExpirationsPaged $ \exps -> do
      uids <-
        ( for exps $ \(UserPendingActivation uid expiresAt) -> do
            isPendingInvitation <- (Just PendingInvitation ==) <$> API.lookupStatus uid
            pure $
              ( expiresAt < now,
                isPendingInvitation,
                uid
              )
          )

      API.deleteUsersNoVerify $
        catMaybes
          ( uids <&> \(isExpired, isPendingInvitation, uid) ->
              if isExpired && isPendingInvitation then Just uid else Nothing
          )

      usersPendingActivationRemoveMultiple $
        catMaybes
          ( uids <&> \(isExpired, _isPendingInvitation, uid) ->
              if isExpired then Just uid else Nothing
          )

    threadDelayRandom
  where
    safeForever :: (MonadIO m, MonadLogger m, MonadCatch m) => String -> m () -> m ()
    safeForever funName action =
      forever $
        action `catchAny` \exc -> do
          err $ "error" .= show exc ~~ msg (val $ cs funName <> " failed")
          -- pause to keep worst-case noise in logs manageable
          threadDelay 60_000_000

    forExpirationsPaged :: ([UserPendingActivation] -> AppIO ()) -> AppIO ()
    forExpirationsPaged f = do
      go =<< usersPendingActivationList
      where
        go :: (Page UserPendingActivation) -> AppIO ()
        go (Page hasMore result nextPage) = do
          f result
          when hasMore $
            go =<< liftClient nextPage

    threadDelayRandom :: AppIO ()
    threadDelayRandom = do
      cleanupTimeout <- fromMaybe (hours 24) . setExpiredUserCleanupTimeout <$> view settings
      let d = realToFrac cleanupTimeout
      randomSecs :: Int <- liftIO (round <$> randomRIO @Double (0.5 * d, d))
      threadDelay (randomSecs * 1_000_000)

    hours :: Double -> Timeout
    hours n = realToFrac (n * 60 * 60)
