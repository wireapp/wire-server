-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
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

module Servant.API.Extended.Endpath where

import Data.Metrics.Servant
import Imports
import Servant
import Servant.Server.Internal.Delayed
import Servant.Server.Internal.ErrorFormatter
import Servant.Server.Internal.Router

-- | Doesn't allow any trailing path components when used with 'Raw'
data Endpath

instance (HasServer api context, HasContextEntry (context .++ DefaultErrorFormatters) ErrorFormatters) => HasServer (Endpath :> api) context where
  type ServerT (Endpath :> api) m = ServerT api m

  route :: Proxy (Endpath :> api) -> Context context -> Delayed env (Server (Endpath :> api)) -> Router env
  route _ ctx delayed =
    let fmt404 = notFoundErrorFormatter $ getContextEntry $ mkContextWithErrorFormatter ctx
     in StaticRouter mempty $ [runRouterEnv fmt404 (route (Proxy @api) ctx delayed)]

  hoistServerWithContext :: Proxy (Endpath :> api) -> Proxy context -> (forall x. m x -> n x) -> ServerT (Endpath :> api) m -> ServerT (Endpath :> api) n
  hoistServerWithContext _ proxyCtx f s = hoistServerWithContext (Proxy @api) proxyCtx f s

-- Endpath :> route
instance (RoutesToPaths route) => RoutesToPaths (Endpath :> route) where
  getRoutes = getRoutes @route
