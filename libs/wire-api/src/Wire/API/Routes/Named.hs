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
import GHC.TypeLits
import Imports
import Servant
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

instance RoutesToPaths api => RoutesToPaths (Named name api) where
  getRoutes = getRoutes @api

type family FindName n (api :: *) :: (n, *) where
  FindName n (Named name api) = '(name, api)
  FindName n (x :> api) = AddPrefix x (FindName n api)
  FindName n api = '(TypeError ('Text "Named combinator not found"), api)

type family AddPrefix x napi where
  AddPrefix x '(name, api) = '(name, x :> api)

type family LiftNamed' napi where
  LiftNamed' '(name, api) = Named name api

type family Flatten api where
  Flatten (x :> api) = Flatten1 x (Flatten api)
  Flatten api = api

type family Flatten1 x api where
  Flatten1 x (api1 :<|> api2) = Flatten1 x api1 :<|> Flatten1 x api2
  Flatten1 x api = x :> api

type family LiftFlatNamed n api where
  LiftFlatNamed n (api1 :<|> api2) = LiftFlatNamed n api1 :<|> LiftFlatNamed n api2
  LiftFlatNamed n api = LiftNamed' (FindName n api)

type LiftNamedOfKind n api = LiftFlatNamed n (Flatten api)

type LiftNamed api = LiftNamedOfKind Symbol api
