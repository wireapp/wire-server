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

module Wire.API.Federation.Domain (DomainHeaderName, DomainHeader, domainHeaderName) where

import Data.Domain (Domain)
import Data.Proxy (Proxy (..))
import GHC.TypeLits (Symbol, symbolVal)
import Imports
import Servant.API (Header', Required, Strict, (:>))
import Servant.Client.Core (HasClient (..))
-- import Servant.Server (DefaultErrorFormatters, ErrorFormatters, HasContextEntry, HasServer (..), (.++))
import Servant.Server

type DomainHeaderName = "Wire-Origin-Domain" :: Symbol

type DomainHeaderType = Header' [Strict, Required] DomainHeaderName Domain

data DomainHeader

domainHeaderName :: IsString a => a
domainHeaderName = fromString $ symbolVal (Proxy @DomainHeaderName)

instance HasClient m api => HasClient m (DomainHeader :> api) where
  type Client m (DomainHeader :> api) = Client m api
  clientWithRoute _ _ = clientWithRoute (Proxy @m) (Proxy @api)
  hoistClientMonad _ _ = hoistClientMonad (Proxy @m) (Proxy @api)

instance
  ( HasContextEntry (context .++ DefaultErrorFormatters) ErrorFormatters,
    HasServer api context
  ) =>
  HasServer (DomainHeader :> api) context
  where
  type ServerT (DomainHeader :> api) m = ServerT (DomainHeaderType :> api) m
  route _ = route (Proxy @(DomainHeaderType :> api))
  hoistServerWithContext _ = hoistServerWithContext (Proxy @(DomainHeaderType :> api))
