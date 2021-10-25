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

module Galley.Run
  ( run,
    mkApp,
  )
where

import Cassandra (runClient, shutdown)
import Cassandra.Schema (versionCheck)
import qualified Control.Concurrent.Async as Async
import Control.Exception (finally)
import Control.Lens (view, (^.))
import qualified Data.Aeson as Aeson
import qualified Data.Metrics.Middleware as M
import Data.Metrics.Servant (servantPlusWAIPrometheusMiddleware)
import Data.Misc (portNumber)
import Data.String.Conversions (cs)
import Data.Text (unpack)
import qualified Galley.API as API
import Galley.API.Federation (federationSitemap)
import qualified Galley.API.Internal as Internal
import Galley.App
import qualified Galley.App as App
import qualified Galley.Data as Data
import Galley.Options (Opts, optGalley)
import qualified Galley.Queue as Q
import Imports
import qualified Network.HTTP.Media.RenderHeader as HTTPMedia
import qualified Network.HTTP.Types as HTTP
import Network.Wai (Application)
import qualified Network.Wai.Middleware.Gunzip as GZip
import qualified Network.Wai.Middleware.Gzip as GZip
import Network.Wai.Utilities.Server
import Servant (Context ((:.)), Proxy (Proxy))
import Servant.API ((:<|>) ((:<|>)))
import qualified Servant.API as Servant
import Servant.API.Generic (ToServantApi, genericApi)
import qualified Servant.Server as Servant
import qualified System.Logger as Log
import Util.Options
import qualified Wire.API.Federation.API.Galley as FederationGalley
import qualified Wire.API.Routes.Public.Galley as GalleyAPI

run :: Opts -> IO ()
run o = do
  (app, e, appFinalizer) <- mkApp o
  let l = e ^. App.applog
  s <-
    newSettings $
      defaultServer
        (unpack $ o ^. optGalley . epHost)
        (portNumber $ fromIntegral $ o ^. optGalley . epPort)
        l
        (e ^. monitor)
  deleteQueueThread <- Async.async $ evalGalley e Internal.deleteLoop
  refreshMetricsThread <- Async.async $ evalGalley e refreshMetrics
  runSettingsWithShutdown s app 5 `finally` do
    Async.cancel deleteQueueThread
    Async.cancel refreshMetricsThread
    shutdown (e ^. cstate)
    appFinalizer

mkApp :: Opts -> IO (Application, Env, IO ())
mkApp o = do
  m <- M.metrics
  e <- App.createEnv m o
  let l = e ^. App.applog
  runClient (e ^. cstate) $
    versionCheck Data.schemaVersion
  let finalizer = do
        Log.info l $ Log.msg @Text "Galley application finished."
        Log.flush l
        Log.close l
      middlewares =
        servantPlusWAIPrometheusMiddleware API.sitemap (Proxy @CombinedAPI)
          . GZip.gunzip
          . GZip.gzip GZip.def
          . catchErrors l [Right m]
  return (middlewares $ servantApp e, e, finalizer)
  where
    rtree = compile API.sitemap
    app e r k = runGalley e r (route rtree r k)
    -- the servant API wraps the one defined using wai-routing
    servantApp e r =
      Servant.serveWithContext
        (Proxy @CombinedAPI)
        (customFormatters :. Servant.EmptyContext)
        ( Servant.hoistServer (Proxy @GalleyAPI.ServantAPI) (toServantHandler e) API.servantSitemap
            :<|> Servant.hoistServer (Proxy @Internal.ServantAPI) (toServantHandler e) Internal.servantSitemap
            :<|> Servant.hoistServer (genericApi (Proxy @FederationGalley.Api)) (toServantHandler e) federationSitemap
            :<|> Servant.Tagged (app e)
        )
        r

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

type CombinedAPI = GalleyAPI.ServantAPI :<|> Internal.ServantAPI :<|> ToServantApi FederationGalley.Api :<|> Servant.Raw

refreshMetrics :: Galley r ()
refreshMetrics = liftGalley0 $ do
  m <- view monitor
  q <- view deleteQueue
  Internal.safeForever "refreshMetrics" $ do
    n <- Q.len q
    M.gaugeSet (fromIntegral n) (M.path "galley.deletequeue.len") m
    threadDelay 1000000
