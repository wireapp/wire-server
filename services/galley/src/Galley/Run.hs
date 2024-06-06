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
import Cassandra (runClient, shutdown)
import Cassandra.Schema (versionCheck)
import Control.Concurrent.Async qualified as Async
import Control.Exception (finally)
import Control.Lens (view, (.~), (^.))
import Control.Monad.Codensity
import Data.Aeson qualified as Aeson
import Data.ByteString.UTF8 qualified as UTF8
import Data.Metrics (Metrics)
import Data.Metrics.AWS (gaugeTokenRemaing)
import Data.Metrics.Middleware qualified as M
import Data.Metrics.Servant
import Data.Misc (portNumber)
import Data.Singletons
import Data.Text (unpack)
import Galley.API.Federation
import Galley.API.Internal
import Galley.API.Public.Servant
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
import Network.Wai.Utilities.Error
import Network.Wai.Utilities.Request
import Network.Wai.Utilities.Server
import Servant hiding (route)
import System.Logger qualified as Log
import System.Logger.Extended (mkLogger)
import Util.Options
import Wire.API.Routes.API
import Wire.API.Routes.Public.Galley
import Wire.API.Routes.Version
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
          versionMiddleware (foldMap expandVersionExp (opts ^. settings . disabledAPIVersions))
            . requestIdMiddleware logger defaultRequestIdHeaderName
            . servantPrometheusMiddleware (Proxy @CombinedAPI)
            . GZip.gunzip
            . GZip.gzip GZip.def
            . catchErrors logger defaultRequestIdHeaderName
    Codensity $ \k -> finally (k ()) $ do
      Log.info logger $ Log.msg @Text "Galley application finished."
      Log.flush logger
      Log.close logger
    pure (middlewares $ servantApp env, env)
  where
    -- Used as a last case in the servant tree. Previously, there used to be a
    -- wai-routing application in that position. That was causing any `Fail`
    -- route results in any servant endpoint to be recovered and ultimately
    -- report a 404 since no other matching path would normally be found in
    -- the wai-routing application. Now there is no wai-routing application
    -- anymore, so without this fallback, any `Fail` result would commit to the
    -- failed endpoint and return the error for that specific path, which would
    -- break compatibility with older API versions.
    --
    -- Note that, since we have many overlapping paths (e.g.
    -- `/conversations/:uuid` and `/conversations/list`), even without the
    -- fallback, errors would not entirely be consistent. For example, a `Fail`
    -- result when attempting to call `/conversations/list`, say if the content
    -- type is incorrect, would cause `conversations/:uuid` to be matched and
    -- report a 400 `invalid UUID` error.
    fallback :: Application
    fallback _ k =
      k $
        responseLBS HTTP.status404 [("Content-Type", "application/json")] $
          Aeson.encode $
            mkError HTTP.status404 "no-endpoint" "The requested endpoint does not exist"

    servantApp :: Env -> Application
    servantApp e0 r cont = do
      let rid = getRequestId defaultRequestIdHeaderName r
          e = reqId .~ rid $ e0
      Servant.serveWithContext
        (Proxy @CombinedAPI)
        ( view (options . settings . federationDomain) e
            :. customFormatters
            :. Servant.EmptyContext
        )
        ( hoistAPIHandler (toServantHandler e) servantSitemap
            :<|> hoistAPIHandler (toServantHandler e) internalAPI
            :<|> hoistServerWithDomain @FederationAPI (toServantHandler e) federationSitemap
            :<|> Tagged fallback
        )
        r
        cont

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

type CombinedAPI =
  GalleyAPI
    :<|> InternalAPI
    :<|> FederationAPI
    :<|> Raw

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
