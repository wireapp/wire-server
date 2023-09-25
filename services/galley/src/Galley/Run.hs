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

module Galley.Run
  ( run,
    mkApp,
    mkLogger,
  )
where

import AWS.Util (readAuthExpiration)
import Amazonka qualified as AWS
import Bilge.Request (requestIdName)
import Cassandra (runClient, shutdown)
import Cassandra.Schema (versionCheck)
import Control.Concurrent.Async qualified as Async
import Control.Exception (finally)
import Control.Lens (view, (.~), (^.))
import Control.Monad.Codensity
import Data.Aeson qualified as Aeson
import Data.Default
import Data.Id
import Data.Metrics (Metrics)
import Data.Metrics.AWS (gaugeTokenRemaing)
import Data.Metrics.Middleware qualified as M
import Data.Metrics.Servant (servantPlusWAIPrometheusMiddleware)
import Data.Misc (portNumber)
import Data.Singletons
import Data.Text (unpack)
import Galley.API qualified as API
import Galley.API.Federation
import Galley.API.Internal
import Galley.App
import Galley.App qualified as App
import Galley.Aws (awsEnv)
import Galley.Cassandra
import Galley.Monad
import Galley.Options
import Galley.Queue qualified as Q
import Imports
import Network.HTTP.Media.RenderHeader qualified as HTTPMedia
import Network.HTTP.Types qualified as HTTP
import Network.Wai
import Network.Wai.Middleware.Gunzip qualified as GZip
import Network.Wai.Middleware.Gzip qualified as GZip
import Network.Wai.Utilities.Server
import Servant hiding (route)
import System.Logger qualified as Log
import System.Logger.Extended (mkLogger)
import Util.Options
import Wire.API.Routes.API
import Wire.API.Routes.Public.Galley
import Wire.API.Routes.Version.Wai

run :: Opts -> IO ()
run opts = lowerCodensity $ do
  (app, env) <- mkApp opts
  settings' <-
    lift $
      newSettings $
        defaultServer
          (unpack $ opts ^. galley . host)
          (portNumber $ fromIntegral $ opts ^. galley . port)
          (env ^. App.applog)
          (env ^. monitor)

  forM_ (env ^. aEnv) $ \aws ->
    void $ Codensity $ Async.withAsync $ collectAuthMetrics (env ^. monitor) (aws ^. awsEnv)

  void $ Codensity $ Async.withAsync $ runApp env deleteLoop
  void $ Codensity $ Async.withAsync $ runApp env refreshMetrics
  lift $ finally (runSettingsWithShutdown settings' app Nothing) (closeApp env)

mkApp :: Opts -> Codensity IO (Application, Env)
mkApp opts =
  do
    logger <- lift $ mkLogger (opts ^. logLevel) (opts ^. logNetStrings) (opts ^. logFormat)
    metrics <- lift $ M.metrics
    env <- lift $ App.createEnv metrics opts logger
    lift $ runClient (env ^. cstate) $ versionCheck schemaVersion
    let middlewares =
          versionMiddleware (opts ^. settings . disabledAPIVersions . traverse)
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
    runGalley e r k = evalGalleyToIO e (route rtree r k)
    -- the servant API wraps the one defined using wai-routing
    servantApp e0 r =
      let e = reqId .~ lookupReqId r $ e0
       in Servant.serveWithContext
            (Proxy @CombinedAPI)
            ( view (options . settings . federationDomain) e
                :. customFormatters
                :. Servant.EmptyContext
            )
            ( hoistAPIHandler (toServantHandler e) API.servantSitemap
                :<|> hoistAPIHandler (toServantHandler e) internalAPI
                :<|> hoistServerWithDomain @FederationAPI (toServantHandler e) federationSitemap
                :<|> Servant.Tagged (runGalley e)
            )
            r

    lookupReqId :: Request -> RequestId
    lookupReqId = maybe def RequestId . lookup requestIdName . requestHeaders

closeApp :: Env -> IO ()
closeApp env = do
  shutdown (env ^. cstate)

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
  GalleyAPI
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

collectAuthMetrics :: (MonadIO m) => Metrics -> AWS.Env -> m ()
collectAuthMetrics m env = do
  liftIO $
    forever $ do
      mbRemaining <- readAuthExpiration env
      gaugeTokenRemaing m mbRemaining
      threadDelay 1_000_000
