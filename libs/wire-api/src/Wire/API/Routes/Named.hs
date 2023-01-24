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

import Data.Kind
import Data.Metrics.Servant
import Data.Proxy
import GHC.TypeLits
import Imports
import Servant
import Servant.Client
import Servant.Client.Core (clientIn)
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

instance HasLink endpoint => HasLink (Named name endpoint) where
  type MkLink (Named name endpoint) a = MkLink endpoint a
  toLink toA _ = toLink toA (Proxy @endpoint)

instance RoutesToPaths api => RoutesToPaths (Named name api) where
  getRoutes = getRoutes @api

instance HasClient m api => HasClient m (Named n api) where
  type Client m (Named n api) = Client m api
  clientWithRoute pm _ req = clientWithRoute pm (Proxy @api) req
  hoistClientMonad pm _ f = hoistClientMonad pm (Proxy @api) f

type family FindName n (api :: Type) :: (n, Type) where
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

------------------------------------
-- Lookup

type family MappendMaybe (x :: Maybe k) (y :: Maybe k) :: Maybe k where
  MappendMaybe 'Nothing y = y
  MappendMaybe ('Just x) y = 'Just x

type family FMap (f :: a -> b) (m :: Maybe a) :: Maybe b where
  FMap _ 'Nothing = 'Nothing
  FMap f ('Just a) = 'Just (f a)

type family LookupEndpoint api name :: Maybe Type where
  LookupEndpoint (Named name endpoint) name = 'Just endpoint
  LookupEndpoint (api1 :<|> api2) name =
    MappendMaybe
      (LookupEndpoint api1 name)
      (LookupEndpoint api2 name)
  LookupEndpoint (prefix :> api) name = FMap ((:>) prefix) (LookupEndpoint api name)
  LookupEndpoint api name = 'Nothing

-------------------------------------
-- Named Client

type HasEndpoint api endpoint name = ('Just endpoint ~ LookupEndpoint api name)

-- | Return a client for a named endpoint.
namedClient ::
  forall api (name :: Symbol) m endpoint.
  (HasEndpoint api endpoint name, HasClient m endpoint) =>
  Client m endpoint
namedClient = clientIn (Proxy @endpoint) (Proxy @m)
