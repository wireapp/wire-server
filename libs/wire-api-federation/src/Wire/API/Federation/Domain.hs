-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2021 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.API.Federation.Domain where

import Data.Domain (Domain)
import Data.Metrics.Servant
import Data.Proxy (Proxy (..))
import GHC.TypeLits (Symbol, symbolVal)
import Imports
import Servant.API (Header', Required, Strict, (:>))
import Servant.Client
import Servant.Server
import Servant.Server.Internal (MkContextWithErrorFormatter)
import Servant.Swagger

type OriginDomainHeaderName = "Wire-Origin-Domain" :: Symbol

data OriginDomainHeader

instance HasSwagger api => HasSwagger (OriginDomainHeader :> api) where
  -- TODO(md)
  toSwagger = undefined

instance RoutesToPaths api => RoutesToPaths (OriginDomainHeader :> api) where
  getRoutes = getRoutes @api

instance HasClient m api => HasClient m (OriginDomainHeader :> api) where
  type Client m (OriginDomainHeader :> api) = Client m api
  clientWithRoute pm _ req = clientWithRoute pm (Proxy @api) req
  hoistClientMonad pm _ = hoistClientMonad pm (Proxy @api)

type OriginDomainHeaderHasServer = Header' [Strict, Required] OriginDomainHeaderName Domain

instance
  ( HasServer api context,
    HasContextEntry (MkContextWithErrorFormatter context) ErrorFormatters
  ) =>
  HasServer (OriginDomainHeader :> api) context
  where
  type ServerT (OriginDomainHeader :> api) m = Domain -> ServerT api m
  route _pa = route (Proxy @(OriginDomainHeaderHasServer :> api))
  hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy :: Proxy api) pc nt . s

originDomainHeaderName :: IsString a => a
originDomainHeaderName = fromString $ symbolVal (Proxy @OriginDomainHeaderName)
