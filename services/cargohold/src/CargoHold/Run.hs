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

module CargoHold.Run
  ( run,
    mkApp,
  )
where

import AWS.Util (readAuthExpiration)
import qualified Amazonka as AWS
import CargoHold.API.Federation
import CargoHold.API.Public
import CargoHold.AWS (amazonkaEnv)
import CargoHold.App hiding (settings)
import CargoHold.Options hiding (aws)
import Control.Exception (bracket)
import Control.Lens ((.~), (^.))
import Control.Monad.Codensity
import Data.Metrics.AWS (gaugeTokenRemaing)
import Data.Metrics.Servant
import Data.Proxy
import Data.Text (unpack)
import Imports
import qualified Network.Wai as Wai
import qualified Network.Wai.Middleware.Gzip as GZip
import Network.Wai.Utilities.Request
import Network.Wai.Utilities.Server
import qualified Network.Wai.Utilities.Server as Server
import qualified Servant
import Servant.API
import Servant.Server hiding (Handler, runHandler)
import qualified UnliftIO.Async as Async
import Util.Options
import Wire.API.Routes.API
import Wire.API.Routes.Internal.Cargohold
import Wire.API.Routes.Public.Cargohold
import Wire.API.Routes.Version
import Wire.API.Routes.Version.Wai

type CombinedAPI = FederationAPI :<|> CargoholdAPI :<|> InternalAPI

run :: Opts -> IO ()
run o = lowerCodensity $ do
  (app, e) <- mkApp o
  void $ Codensity $ Async.withAsync (collectAuthMetrics (e ^. aws . amazonkaEnv))
  liftIO $ do
    s <-
      Server.newSettings $
        defaultServer
          (unpack $ o ^. cargohold . host)
          (o ^. cargohold . port)
          (e ^. appLogger)
    runSettingsWithShutdown s app Nothing

mkApp :: Opts -> Codensity IO (Application, Env)
mkApp o = Codensity $ \k ->
  bracket (newEnv o) closeEnv $ \e ->
    k (middleware e (servantApp e), e)
  where
    middleware :: Env -> Wai.Middleware
    middleware e =
      versionMiddleware (foldMap expandVersionExp (o ^. settings . disabledAPIVersions))
        . requestIdMiddleware (e ^. appLogger) defaultRequestIdHeaderName
        . servantPrometheusMiddleware (Proxy @CombinedAPI)
        . GZip.gzip GZip.def
        . catchErrors (e ^. appLogger) defaultRequestIdHeaderName
    servantApp :: Env -> Application
    servantApp e0 r cont = do
      let rid = getRequestId defaultRequestIdHeaderName r
          e = requestId .~ rid $ e0
      Servant.serveWithContext
        (Proxy @CombinedAPI)
        ((o ^. settings . federationDomain) :. Servant.EmptyContext)
        ( hoistServerWithDomain @FederationAPI (toServantHandler e) federationSitemap
            :<|> hoistServerWithDomain @CargoholdAPI (toServantHandler e) servantSitemap
            :<|> hoistServerWithDomain @InternalAPI (toServantHandler e) internalSitemap
        )
        r
        cont

toServantHandler :: Env -> Handler a -> Servant.Handler a
toServantHandler env = liftIO . runHandler env

collectAuthMetrics :: (MonadIO m) => AWS.Env -> m ()
collectAuthMetrics env = do
  liftIO $
    forever $ do
      mbRemaining <- readAuthExpiration env
      gaugeTokenRemaing mbRemaining
      threadDelay 1_000_000
