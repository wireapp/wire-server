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

module Brig.Run (run, mkApp) where

import AWS.Util (readAuthExpiration)
import Brig.API.Federation
import Brig.API.Handler
import Brig.API.Internal qualified as IAPI
import Brig.API.Public
import Brig.API.User qualified as API
import Brig.AWS qualified as AWS
import Brig.AWS.SesNotification qualified as SesNotification
import Brig.App
import Brig.Calling qualified as Calling
import Brig.CanonicalInterpreter
import Brig.Effects.UserPendingActivationStore (UserPendingActivation (UserPendingActivation), UserPendingActivationStore)
import Brig.Effects.UserPendingActivationStore qualified as UsersPendingActivationStore
import Brig.InternalEvent.Process qualified as Internal
import Brig.Queue qualified as Queue
import Brig.Version
import Control.Concurrent.Async qualified as Async
import Control.Exception.Safe (catchAny)
import Control.Lens ((.~))
import Control.Monad.Catch (MonadCatch, finally)
import Control.Monad.Random (randomRIO)
import Data.Aeson qualified as Aeson
import Data.ByteString.UTF8 qualified as UTF8
import Data.Metrics.AWS (gaugeTokenRemaing)
import Data.Metrics.Servant qualified as Metrics
import Data.Proxy (Proxy (Proxy))
import Data.Text (unpack)
import Imports hiding (head)
import Network.HTTP.Media qualified as HTTPMedia
import Network.HTTP.Types qualified as HTTP
import Network.Wai qualified as Wai
import Network.Wai.Middleware.Gunzip qualified as GZip
import Network.Wai.Middleware.Gzip qualified as GZip
import Network.Wai.Utilities.Request
import Network.Wai.Utilities.Server
import Network.Wai.Utilities.Server qualified as Server
import OpenTelemetry.Instrumentation.Wai qualified as Otel
import OpenTelemetry.Trace as Otel
import Polysemy (Member)
import Servant (Context ((:.)), (:<|>) (..))
import Servant qualified
import System.Logger (msg, val, (.=), (~~))
import System.Logger.Class (MonadLogger, err)
import Util.Options
import Util.Timeout
import Wire.API.Routes.API
import Wire.API.Routes.Internal.Brig qualified as IAPI
import Wire.API.Routes.Public.Brig
import Wire.API.Routes.Version
import Wire.API.Routes.Version.Wai
import Wire.API.User (AccountStatus (PendingInvitation))
import Wire.DeleteQueue
import Wire.OpenTelemetry (withTracer)
import Wire.Sem.Paging qualified as P
import Wire.ServerOptions.Brig hiding (internalEvents, sesQueue)
import Wire.UserStore

-- FUTUREWORK: If any of these async threads die, we will have no clue about it
-- and brig could start misbehaving. We should ensure that brig dies whenever a
-- thread terminates for any reason.
-- https://github.com/zinfra/backend-issues/issues/1647
run :: Opts -> IO ()
run opts = withTracer \tracer -> do
  (app, e) <- mkApp opts
  s <- Server.newSettings (server e)
  internalEventListener <-
    Async.async $
      runBrigToIO e $
        wrapHttpClient $
          Queue.listen e.internalEvents $
            liftIO . runBrigToIO e . liftSem . Internal.onEvent
  let throttleMillis = fromMaybe defSqsThrottleMillis opts.settings.sqsThrottleMillis
  emailListener <- for e.awsEnv._sesQueue $ \q ->
    Async.async $
      AWS.execute e.awsEnv $
        AWS.listen throttleMillis q (runBrigToIO e . SesNotification.onEvent)
  sftDiscovery <- forM e.sftEnv $ Async.async . Calling.startSFTServiceDiscovery e.appLogger
  turnDiscovery <- Calling.startTurnDiscovery e.appLogger e.fsWatcher e.turnEnv
  authMetrics <- Async.async (runBrigToIO e collectAuthMetrics)
  pendingActivationCleanupAsync <- Async.async (runBrigToIO e pendingActivationCleanup)

  inSpan tracer "brig" defaultSpanArguments {kind = Otel.Server} (runSettingsWithShutdown s app Nothing) `finally` do
    Async.cancelMany $
      [internalEventListener, pendingActivationCleanupAsync, authMetrics]
        <> catMaybes [emailListener, sftDiscovery]
        <> turnDiscovery
    closeEnv e
  where
    brig = opts.brig
    server e = defaultServer (unpack $ brig.host) brig.port e.appLogger

mkApp :: Opts -> IO (Wai.Application, Env)
mkApp opts = do
  e <- newEnv opts
  otelMiddleware <- Otel.newOpenTelemetryWaiMiddleware
  pure (otelMiddleware . middleware e $ servantApp e, e)
  where
    middleware :: Env -> Wai.Middleware
    middleware e =
      -- these rewrite the request, so they must be at the top (i.e. applied last)
      versionMiddleware e.disabledVersions
        . internalHandleCompatibilityMiddleware
        -- this also rewrites the request
        . requestIdMiddleware e.appLogger defaultRequestIdHeaderName
        . Metrics.servantPrometheusMiddleware (Proxy @ServantCombinedAPI)
        . GZip.gunzip
        . GZip.gzip GZip.def
        . catchErrors e.appLogger defaultRequestIdHeaderName

    servantApp :: Env -> Wai.Application
    servantApp e req cont = do
      let rid = getRequestId defaultRequestIdHeaderName req
      let env = requestIdLens .~ rid $ e
      let localDomain = env.settings.federationDomain
      Servant.serveWithContext
        (Proxy @ServantCombinedAPI)
        (customFormatters :. localDomain :. Servant.EmptyContext)
        ( docsAndOptionsAPI
            :<|> hoistServerWithDomain @BrigAPI (toServantHandler env) servantSitemap
            :<|> hoistServerWithDomain @IAPI.API (toServantHandler env) IAPI.servantSitemap
            :<|> hoistServerWithDomain @FederationAPI (toServantHandler env) federationSitemap
            :<|> hoistServerWithDomain @VersionAPI (toServantHandler env) versionAPI
        )
        req
        cont

-- FUTUREWORK: this rewrites /i/users/handles to /i/handles, for backward
-- compatibility with the old endpoint path during deployment. Once the new
-- endpoint has been deployed, this middleware can be removed.
internalHandleCompatibilityMiddleware :: Wai.Middleware
internalHandleCompatibilityMiddleware app req k =
  app
    ( case Wai.pathInfo req of
        ("i" : "users" : "handles" : rest) ->
          req
            { Wai.pathInfo = ("i" : "handles" : rest)
            }
        _ -> req
    )
    k

type ServantCombinedAPI =
  ( DocsAndOptionsAPI
      :<|> BrigAPI
      :<|> IAPI.API
      :<|> FederationAPI
      :<|> VersionAPI
  )

customFormatters :: Servant.ErrorFormatters
customFormatters =
  Servant.defaultErrorFormatters
    { Servant.bodyParserErrorFormatter = bodyParserErrorFormatter
    }

bodyParserErrorFormatter :: Servant.ErrorFormatter
bodyParserErrorFormatter _ _ errMsg =
  Servant.ServerError
    { Servant.errHTTPCode = HTTP.statusCode HTTP.status400,
      Servant.errReasonPhrase = UTF8.toString $ HTTP.statusMessage HTTP.status400,
      Servant.errBody =
        Aeson.encode $
          Aeson.object
            [ "code" Aeson..= Aeson.Number 400,
              "message" Aeson..= errMsg,
              "label" Aeson..= ("bad-request" :: Text)
            ],
      Servant.errHeaders = [(HTTP.hContentType, HTTPMedia.renderHeader (Servant.contentType (Proxy @Servant.JSON)))]
    }

-- | Go through expired pending activations/invitations and delete them.  This could probably
-- be done with cassandra TTLs, but it involves several tables and may require adjusting their
-- write operations.
pendingActivationCleanup ::
  forall r p.
  ( P.Paging p,
    Member (UserPendingActivationStore p) r,
    Member DeleteQueue r,
    Member UserStore r
  ) =>
  AppT r ()
pendingActivationCleanup = do
  safeForever "pendingActivationCleanup" $ do
    now <- liftIO =<< asks (.currentTime)
    forExpirationsPaged $ \exps -> do
      uids <-
        for exps $ \(UserPendingActivation uid expiresAt) -> do
          isPendingInvitation <- (Just PendingInvitation ==) <$> liftSem (lookupStatus uid)
          pure
            ( expiresAt < now,
              isPendingInvitation,
              uid
            )

      API.deleteUsersNoVerify $
        mapMaybe
          ( \(isExpired, isPendingInvitation, uid) ->
              if isExpired && isPendingInvitation then Just uid else Nothing
          )
          uids

      liftSem . UsersPendingActivationStore.removeMultiple $
        mapMaybe
          ( \(isExpired, _isPendingInvitation, uid) ->
              if isExpired then Just uid else Nothing
          )
          uids

    threadDelayRandom
  where
    safeForever :: (MonadIO m, MonadLogger m, MonadCatch m) => String -> m () -> m ()
    safeForever funName action =
      forever $
        action `catchAny` \exc -> do
          err $ "error" .= show exc ~~ msg (val $ UTF8.fromString funName <> " failed")
          -- pause to keep worst-case noise in logs manageable
          threadDelay 60_000_000

    forExpirationsPaged :: ([UserPendingActivation] -> (AppT r) ()) -> (AppT r) ()
    forExpirationsPaged f = do
      go =<< liftSem (UsersPendingActivationStore.list Nothing)
      where
        go :: P.Page p UserPendingActivation -> (AppT r) ()
        go p = do
          f (P.pageItems p)
          when (P.pageHasMore p) $ do
            go =<< liftSem (UsersPendingActivationStore.list $ Just $ P.pageState p)

    threadDelayRandom :: (AppT r) ()
    threadDelayRandom = do
      cleanupTimeout <- fromMaybe (hours 24) <$> asks (.settings.expiredUserCleanupTimeout)
      let d = realToFrac cleanupTimeout
      randomSecs :: Int <- liftIO (round <$> randomRIO @Double (0.5 * d, d))
      threadDelay (randomSecs * 1_000_000)

    hours :: Double -> Timeout
    hours n = realToFrac (n * 60 * 60)

collectAuthMetrics :: forall r. AppT r ()
collectAuthMetrics = do
  env <- asks (.awsEnv._amazonkaEnv)
  liftIO $
    forever $ do
      mbRemaining <- readAuthExpiration env
      gaugeTokenRemaing mbRemaining
      threadDelay 1_000_000
