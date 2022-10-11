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

module Galley.Run
  ( run,
    mkApp,
  )
where

import AWS.Util (readAuthExpiration)
import qualified Amazonka as AWS
import Bilge.Request (requestIdName)
import Cassandra (runClient, shutdown)
import Cassandra.Schema (versionCheck)
import qualified Control.Concurrent.Async as Async
import Control.Exception (finally)
import Control.Lens (view, (.~), (^.))
import Control.Monad.Codensity
import qualified Data.Aeson as Aeson
import Data.Default
import Data.Id
import Data.Metrics (Metrics)
import Data.Metrics.AWS (gaugeTokenRemaing)
import qualified Data.Metrics.Middleware as M
import Data.Metrics.Servant (servantPlusWAIPrometheusMiddleware)
import Data.Misc (portNumber)
import Data.String.Conversions (cs)
import Data.Text (unpack)
import qualified Galley.API as API
import Galley.API.Federation (FederationAPI, federationSitemap)
import Galley.API.Internal
import Galley.App
import qualified Galley.App as App
import Galley.Aws (awsEnv)
import Galley.Cassandra
import Galley.Monad
import Galley.Options
import qualified Galley.Queue as Q
import Imports
import qualified Network.HTTP.Media.RenderHeader as HTTPMedia
import qualified Network.HTTP.Types as HTTP
import Network.Wai
import qualified Network.Wai.Middleware.Gunzip as GZip
import qualified Network.Wai.Middleware.Gzip as GZip
import Network.Wai.Utilities.Server
import Servant
import Servant.Server.Internal.Delayed (emptyDelayed)
import Servant.Server.Internal.ErrorFormatter (mkContextWithErrorFormatter)
import Servant.Server.Internal.RouteResult
import Servant.Server.Internal.Router
import Servant.Server.Internal.RoutingApplication (RoutingApplication)
import qualified System.Logger as Log
import Util.Options
import Wire.API.Routes.API
import qualified Wire.API.Routes.Public.Galley as GalleyAPI
import Wire.API.Routes.Version.Wai

run :: Opts -> IO ()
run opts = lowerCodensity $ do
  (app, env) <- mkApp opts
  settings <-
    lift $
      newSettings $
        defaultServer
          (unpack $ opts ^. optGalley . epHost)
          (portNumber $ fromIntegral $ opts ^. optGalley . epPort)
          (env ^. App.applog)
          (env ^. monitor)

  forM_ (env ^. aEnv) $ \aws ->
    void $ Codensity $ Async.withAsync $ collectAuthMetrics (env ^. monitor) (aws ^. awsEnv)
  void $ Codensity $ Async.withAsync $ runApp env deleteLoop
  void $ Codensity $ Async.withAsync $ runApp env refreshMetrics
  lift $ finally (runSettingsWithShutdown settings app Nothing) (shutdown (env ^. cstate))

mkApp :: Opts -> Codensity IO (Application, Env)
mkApp opts =
  do
    metrics <- lift $ M.metrics
    env <- lift $ App.createEnv metrics opts
    lift $ runClient (env ^. cstate) $ versionCheck schemaVersion

    let logger = env ^. App.applog

    let middlewares =
          versionMiddleware
            . servantPlusWAIPrometheusMiddleware API.sitemap (Proxy @CombinedAPI)
            . GZip.gunzip
            . GZip.gzip GZip.def
            . catchErrors logger [Right metrics]
    Codensity $ \k -> finally (k ()) $ do
      Log.info logger $ Log.msg @Text "Galley application finished."
      Log.flush logger
      Log.close logger
    pure (middlewares $ servantApp env, env)
  where
    rtree = compile API.sitemap
    runGalley e r k = evalGalleyToIO e (Network.Wai.Utilities.Server.route rtree r k)
    -- the servant API wraps the one defined using wai-routing
    servantApp e0 r =
      let e = reqId .~ lookupReqId r $ e0
          context =
            ( view (options . optSettings . setFederationDomain) e
                :. customFormatters
                :. Servant.EmptyContext
            )
       in ( routesToApp
              context
              [ mkRouterApp (Proxy @GalleyAPI.ServantAPI) context id (hoistAPIHandler (toServantHandler e) API.servantSitemap),
                mkRouterApp (Proxy @InternalAPI) context id (hoistAPIHandler (toServantHandler e) internalAPI),
                mkRouterApp (Proxy @FederationAPI) context id (hoistServerWithDomain @FederationAPI (toServantHandler e) federationSitemap),
                mkRouterApp (Proxy @Servant.Raw) context id (Servant.Tagged (runGalley e))
              ]
          )
            r

    lookupReqId :: Request -> RequestId
    lookupReqId = maybe def RequestId . lookup requestIdName . requestHeaders

mkRouterApp ::
  forall api context m.
  (HasServer api context, ServerContext context) =>
  Proxy api ->
  Context context ->
  (forall x. m x -> Handler x) ->
  ServerT api m ->
  RoutingApplication
mkRouterApp p context toHandler server =
  runRouter format404 $ Servant.route p context (emptyDelayed router)
  where
    router = Route $ hoistServerWithContext p (Proxy :: Proxy context) toHandler server
    format404 = notFoundErrorFormatter . getContextEntry . mkContextWithErrorFormatter $ context

routesToApp ::
  ServerContext context =>
  Context context ->
  [RoutingApplication] ->
  Application
routesToApp context rs = toApplication (runChoice format404 (map const rs) ())
  where
    format404 = notFoundErrorFormatter . getContextEntry . mkContextWithErrorFormatter $ context

customFormatters :: Servant.ErrorFormatters
customFormatters =
  defaultErrorFormatters
    { bodyParserErrorFormatter = bodyParserErrorFormatter'
    }

bodyParserErrorFormatter' :: Servant.ErrorFormatter
bodyParserErrorFormatter' _ _ errMsg =
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

type CombinedAPI =
  GalleyAPI.ServantAPI
    :<|> InternalAPI
    :<|> FederationAPI
    :<|> Servant.Raw

refreshMetrics :: App ()
refreshMetrics = do
  m <- view monitor
  q <- view deleteQueue
  safeForever "refreshMetrics" $ do
    n <- Q.len q
    M.gaugeSet (fromIntegral n) (M.path "galley.deletequeue.len") m
    threadDelay 1000000

collectAuthMetrics :: MonadIO m => Metrics -> AWS.Env -> m ()
collectAuthMetrics m env = do
  liftIO $
    forever $ do
      mbRemaining <- readAuthExpiration env
      gaugeTokenRemaing m mbRemaining
      threadDelay 1_000_000
