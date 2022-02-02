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

module Wire.API.Routes.Named where

import Data.Metrics.Servant
import Data.Proxy
import Imports
import Servant.Client
import Servant.Client.Core
import Servant.Server
import Servant.Swagger

newtype Named named x = Named {unnamed :: x}
  deriving (Functor)

instance HasSwagger api => HasSwagger (Named name api) where
  toSwagger _ = toSwagger (Proxy @api)

instance HasServer api ctx => HasServer (Named name api) ctx where
  type ServerT (Named name api) m = Named name (ServerT api m)

  route _ ctx action = route (Proxy @api) ctx (fmap unnamed action)
  hoistServerWithContext _ ctx f =
    fmap (hoistServerWithContext (Proxy @api) ctx f)

instance (RunClient m, HasClient m api) => HasClient m (Named name api) where
  type Client m (Named name api) = Client m api
  clientWithRoute m _ = clientWithRoute m (Proxy @api)
  hoistClientMonad m _ = hoistClientMonad m (Proxy @api)

instance RoutesToPaths api => RoutesToPaths (Named name api) where
  getRoutes = getRoutes @api
