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

module Wire.API.Deprecated
  ( Deprecated,
  )
where

import Control.Lens
import Data.Kind (Type)
import Data.Metrics.Servant
import Data.OpenApi hiding (HasServer)
import Imports
import Servant
import Servant.Client
import Servant.OpenApi

-- Annotate that the route is deprecated
data Deprecated deriving (Typeable)

-- All of these instances are very similar to the instances
-- for Summary. These don't impact the API directly, but are
-- for marking the deprecated flag in the openapi output.
instance (HasLink sub) => HasLink (Deprecated :> sub :: Type) where
  type MkLink (Deprecated :> sub) a = MkLink sub a
  toLink =
    let simpleToLink toA _ = toLink toA (Proxy :: Proxy sub)
     in simpleToLink

instance (HasOpenApi api) => HasOpenApi (Deprecated :> api :: Type) where
  toOpenApi _ =
    toOpenApi (Proxy @api)
      & allOperations . deprecated ?~ True

instance (HasServer api ctx) => HasServer (Deprecated :> api) ctx where
  type ServerT (Deprecated :> api) m = ServerT api m
  route _ = route $ Proxy @api
  hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy @api) pc nt s

instance (HasClient m api) => HasClient m (Deprecated :> api) where
  type Client m (Deprecated :> api) = Client m api
  clientWithRoute pm _ = clientWithRoute pm (Proxy :: Proxy api)
  hoistClientMonad pm _ f cl = hoistClientMonad pm (Proxy :: Proxy api) f cl

instance (RoutesToPaths rest) => RoutesToPaths (Deprecated :> rest) where
  getRoutes = getRoutes @rest
