{-# OPTIONS_GHC -Wno-redundant-constraints #-}

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

import Control.Lens ((%~))
import Data.Kind
import Data.Metrics.Servant
import Data.OpenApi.Lens hiding (HasServer)
import Data.OpenApi.Operation
import Data.Proxy
import Data.Text qualified as T
import GHC.Generics
import GHC.TypeLits
import Imports
import Servant
import Servant.Client
import Servant.Client.Core (clientIn)
import Servant.OpenApi

-- | See http://docs.wire.com/developer/developer/servant.html#named-and-internal-route-ids-in-swagger
newtype Named name x = Named {unnamed :: x}
  deriving (Functor)

-- | For 'HasSwagger' instance of 'Named'.  'KnownSymbol' isn't enough because we're using
-- types other than string literals in some places.
class RenderableSymbol a where
  renderSymbol :: Text

instance (KnownSymbol a) => RenderableSymbol a where
  renderSymbol = T.pack . show $ symbolVal (Proxy @a)

instance (RenderableSymbol a, RenderableSymbol b) => RenderableSymbol '(a, b) where
  renderSymbol = "(" <> (renderSymbol @a) <> ", " <> (renderSymbol @b) <> ")"

newtype RenderableTypeName a = RenderableTypeName a

instance (GRenderableSymbol (Rep a)) => RenderableSymbol (RenderableTypeName a) where
  renderSymbol = grenderSymbol @(Rep a)

class GRenderableSymbol f where
  grenderSymbol :: Text

instance (KnownSymbol tyName) => GRenderableSymbol (D1 (MetaData tyName modName pkg b) k) where
  grenderSymbol = T.pack $ symbolVal (Proxy @tyName)

instance (HasOpenApi api, RenderableSymbol name) => HasOpenApi (Named name api) where
  toOpenApi _ =
    toOpenApi (Proxy @api)
      & allOperations . description %~ (Just (dscr <> "\n\n") <>)
    where
      dscr :: Text
      dscr =
        " [<a href=\"https://docs.wire.com/developer/developer/servant.html#named-and-internal-route-ids\">internal route ID:</a> "
          <> renderSymbol @name
          <> "]"

instance (HasServer api ctx) => HasServer (Named name api) ctx where
  type ServerT (Named name api) m = Named name (ServerT api m)

  route _ ctx action = route (Proxy @api) ctx (fmap unnamed action)
  hoistServerWithContext _ ctx f =
    fmap (hoistServerWithContext (Proxy @api) ctx f)

instance (HasLink endpoint) => HasLink (Named name endpoint) where
  type MkLink (Named name endpoint) a = MkLink endpoint a
  toLink toA _ = toLink toA (Proxy @endpoint)

instance (RoutesToPaths api) => RoutesToPaths (Named name api) where
  getRoutes = getRoutes @api

instance (HasClient m api) => HasClient m (Named n api) where
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

-- | Utility to push a Servant combinator inside Named APIs.
--
-- For example:
-- @@
-- From 'V5 ::> (Named "foo" (Get '[JSON] Foo) :<|> Named "bar" (Post '[JSON] Bar))
-- ==
-- Named "foo" (From 'V5 :> Get '[JSON] Foo) :<|> Named "bar" (From 'V5 :> Post '[JSON] Bar)
-- @@
type family x ::> api

infixr 4 ::>

type instance
  x ::> (Named name api) =
    Named name (x :> api)

type instance
  x ::> (api1 :<|> api2) =
    (x ::> api1) :<|> (x ::> api2)
