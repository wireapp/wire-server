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

module CargoHold.Run
  ( run,
  )
where

import CargoHold.API (servantSitemap, sitemap)
import CargoHold.API.Federation
import CargoHold.App
import CargoHold.Options
import Control.Lens (set, (^.))
import Control.Monad.Catch (finally)
import Data.Default
import Data.Domain
import Data.Id
import Data.Metrics.Middleware.Prometheus (waiPrometheusMiddleware)
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
import Util.Options
import qualified Wire.API.Routes.Public.Cargohold as Public

type CombinedAPI =
  FederationAPI
    :<|> Public.ServantAPI
    :<|> Servant.Raw

run :: Opts -> IO ()
run o = do
  e <- newEnv o
  s <- Server.newSettings (server e)
  runSettingsWithShutdown s (middleware e $ servantApp e) 5
    `finally` closeEnv e
  where
    rtree = compile sitemap
    server e = defaultServer (unpack $ o ^. optCargohold . epHost) (o ^. optCargohold . epPort) (e ^. appLogger) (e ^. metrics)
    middleware :: Env -> Wai.Middleware
    middleware e =
      waiPrometheusMiddleware sitemap
        . GZip.gzip GZip.def
        . catchErrors (e ^. appLogger) [Right $ e ^. metrics]
    app e r k = runHandler e (Server.route rtree r k)
    servantApp e0 r =
      let e = set requestId (maybe def RequestId (lookupRequestId r)) e0
       in Servant.serveWithContext
            (Proxy @CombinedAPI)
            -- TODO: use actual federation domain here
            (Domain "example.com" :. Servant.EmptyContext)
            ( hoistServer' @FederationAPI (toServantHandler e) federationSitemap
                :<|> hoistServer' @Public.ServantAPI (toServantHandler e) servantSitemap
                :<|> Servant.Tagged (app e)
            )
            r

toServantHandler :: Env -> Handler a -> Servant.Handler a
toServantHandler env = liftIO . runHandler env

-- | See 'Galley.Run' for an explanation of this function.
hoistServer' ::
  forall api m n.
  HasServer api '[Domain] =>
  (forall x. m x -> n x) ->
  ServerT api m ->
  ServerT api n
hoistServer' = hoistServerWithContext (Proxy @api) (Proxy @'[Domain])
