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

import CargoHold.API (sitemap)
import CargoHold.App
import CargoHold.Options
import Control.Lens ((^.))
import Control.Monad.Catch (finally)
import Data.Metrics.Middleware.Prometheus (waiPrometheusMiddleware)
import Data.Text (unpack)
import Imports
import qualified Network.Wai as Wai
import qualified Network.Wai.Middleware.Gzip as GZip
import Network.Wai.Utilities.Server
import qualified Network.Wai.Utilities.Server as Server
import Util.Options

run :: Opts -> IO ()
run o = do
  e <- newEnv o
  s <- Server.newSettings (server e)
  runSettingsWithShutdown s (middleware e $ serve e) 5
    `finally` closeEnv e
  where
    rtree = compile sitemap
    server e = defaultServer (unpack $ o ^. optCargohold . epHost) (o ^. optCargohold . epPort) (e ^. appLogger) (e ^. metrics)
    middleware :: Env -> Wai.Middleware
    middleware e =
      waiPrometheusMiddleware sitemap
        . catchErrors (e ^. appLogger) [Right $ e ^. metrics]
        . GZip.gzip GZip.def
    serve e r k = runHandler e r (Server.route rtree r k) k
