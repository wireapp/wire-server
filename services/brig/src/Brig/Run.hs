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
import Brig.API.Error (Error (RichError, StdError), invalidEmail, phoneError, zauthError)
import Brig.API.Handler
import qualified Brig.API.Handler as Brig
import Brig.API.Public (ServantAPI, servantSitemap)
import Brig.AWS (sesQueue)
import qualified Brig.AWS as AWS
import qualified Brig.AWS.SesNotification as SesNotification
import Brig.App
import qualified Brig.Calling as Calling
import qualified Brig.InternalEvent.Process as Internal
import Brig.Options hiding (internalEvents, sesQueue)
import Brig.Phone (PhoneException)
import qualified Brig.Queue as Queue
import qualified Control.Concurrent.Async as Async
import Control.Error (runExceptT)
import Control.Lens ((^.))
import Control.Monad.Catch (catches, finally, throwM)
import qualified Control.Monad.Catch as Catch
import qualified Data.Aeson as Aeson
import qualified Data.Metrics.Middleware.Prometheus as Metrics
import Data.Proxy (Proxy (Proxy))
import Data.Text (unpack)
import qualified Data.Text.Lazy as LText
import qualified Data.Text.Lazy.Encoding as LText
import qualified Data.ZAuth.Validation as ZV
import Imports hiding (head)
import Network.HTTP.Types (Status (statusCode))
import qualified Network.Wai as Wai
import qualified Network.Wai.Middleware.Gunzip as GZip
import qualified Network.Wai.Middleware.Gzip as GZip
import qualified Network.Wai.Utilities.Error as Wai
import Network.Wai.Utilities.Server
import qualified Network.Wai.Utilities.Server as Server
import Servant ((:<|>) (..))
import qualified Servant
import Servant.Server (ServerError (ServerError))
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
  runSettingsWithShutdown s app 5 `finally` do
    mapM_ Async.cancel emailListener
    Async.cancel internalEventListener
    mapM_ Async.cancel sftDiscovery
    closeEnv e
  where
    endpoint = brig o
    server e = defaultServer (unpack $ endpoint ^. epHost) (endpoint ^. epPort) (e ^. applog) (e ^. metrics)

mkApp :: Opts -> IO (Wai.Application, Env)
mkApp o = do
  e <- newEnv o
  return (middleware e $ servantApp e, e)
  where
    rtree = compile (sitemap o)
    middleware :: Env -> Wai.Middleware
    middleware e =
      Metrics.waiPrometheusMiddleware (sitemap o)
        . catchErrors (e ^. applog) [Right $ e ^. metrics]
        . GZip.gunzip
        . GZip.gzip GZip.def
    app e r k = runHandler e r (Server.route rtree r k) k
    -- the servant API wraps the one defined using wai-routing
    servantApp :: Env -> Wai.Application
    servantApp e =
      Servant.serve
        -- (Proxy @ServantAPI)
        -- (Servant.hoistServer (Proxy @ServantAPI) handlerToHandler servantSitemap)
        (Proxy @(ServantAPI :<|> Servant.Raw))
        (Servant.hoistServer (Proxy @ServantAPI) (handlerToHandler e) servantSitemap :<|> Servant.Tagged (app e))

-- TODO: Implement this
handlerToHandler :: Env -> Brig.Handler a -> Servant.Handler a
handlerToHandler env action = do
  a <- liftIO $ runAppT env (runExceptT action) `catches` errors
  case a of
    Left (StdError (Wai.Error code label msg)) -> throwM $ ServerError (statusCode code) (LText.unpack label) (LText.encodeUtf8 msg) []
    Left (RichError (Wai.Error code label _) json headers) -> throwM $ ServerError (statusCode code) (LText.unpack label) (Aeson.encode json) headers
    Right x -> pure x
  where
    errors =
      [ Catch.Handler $ \(ex :: PhoneException) ->
          pure (Left (phoneError ex)),
        Catch.Handler $ \(ex :: ZV.Failure) ->
          pure (Left (zauthError ex)),
        Catch.Handler $ \(ex :: AWS.Error) ->
          case ex of
            AWS.SESInvalidDomain -> pure (Left (StdError invalidEmail))
            _ -> throwM ex
      ]
