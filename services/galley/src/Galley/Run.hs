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
import Data.Domain
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
import Galley.Cassandra
import Galley.Monad
import Galley.Options
import qualified Galley.Queue as Q
import Imports
import qualified Network.HTTP.Media.RenderHeader as HTTPMedia
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai.Middleware.Gunzip as GZip
import qualified Network.Wai.Middleware.Gzip as GZip
import Network.Wai.Utilities.Server
import Servant hiding (route)
import Servant.API.Generic (ToServantApi)
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
  deleteQueueThread <- Async.async $ runApp e Internal.deleteLoop
  refreshMetricsThread <- Async.async $ runApp e refreshMetrics
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
    versionCheck schemaVersion
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
        ( view (options . optSettings . setFederationDomain) e
            :. customFormatters
            :. Servant.EmptyContext
        )
        ( hoistServer' @GalleyAPI.ServantAPI (toServantHandler e) API.servantSitemap
            :<|> hoistServer' @Internal.ServantAPI (toServantHandler e) Internal.servantSitemap
            :<|> hoistServer' @(ToServantApi FederationGalley.Api) (toServantHandler e) federationSitemap
            :<|> Servant.Tagged (app e)
        )
        r

-- Servant needs a context type argument here that contains *at least* the
-- context types required by all the HasServer instances. In reality, this should
-- not be necessary, because the contexts are only used by the @route@ functions,
-- but unfortunately the 'hoistServerWithContext' function is also part of the
-- 'HasServer' typeclass, even though it cannot possibly make use of its @context@
-- type argument.
hoistServer' ::
  forall api m n.
  HasServer api '[Domain] =>
  (forall x. m x -> n x) ->
  ServerT api m ->
  ServerT api n
hoistServer' = hoistServerWithContext (Proxy @api) (Proxy @'[Domain])

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

type CombinedAPI = GalleyAPI.ServantAPI :<|> Internal.ServantAPI :<|> ToServantApi FederationGalley.Api :<|> Servant.Raw

refreshMetrics :: App ()
refreshMetrics = do
  m <- view monitor
  q <- view deleteQueue
  Internal.safeForever "refreshMetrics" $ do
    n <- Q.len q
    M.gaugeSet (fromIntegral n) (M.path "galley.deletequeue.len") m
    threadDelay 1000000
