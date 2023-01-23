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

module Wire.API.Federation.API
  ( FedApi,
    HasFedEndpoint,
    HasUnsafeFedEndpoint,
    fedClient,
    fedClientIn,
    unsafeFedClientIn,
    module Wire.API.MakesFederatedCall,

    -- * Re-exports
    Component (..),
  )
where

import Data.Kind
import Data.Proxy
import GHC.TypeLits
import Imports
import Servant.Client
import Servant.Client.Core
import Wire.API.Federation.API.Brig
import Wire.API.Federation.API.Cargohold
import Wire.API.Federation.API.Galley
import Wire.API.Federation.Client
import Wire.API.MakesFederatedCall
import Wire.API.Routes.Named

-- Note: this type family being injective means that in most cases there is no need
-- to add component annotations when invoking the federator client
type family FedApi (comp :: Component) = (api :: Type) | api -> comp

type instance FedApi 'Galley = GalleyApi

type instance FedApi 'Brig = BrigApi

type instance FedApi 'Cargohold = CargoholdApi

type HasFedEndpoint comp api name = (HasUnsafeFedEndpoint comp api name, CallsFed comp name)

-- | Like 'HasFedEndpoint', but doesn't propagate a 'CallsFed' constraint.
-- Useful for tests, but unsafe in the sense that incorrect usage will allow
-- you to forget about some federated calls.
type HasUnsafeFedEndpoint comp api name = 'Just api ~ LookupEndpoint (FedApi comp) name

-- | Return a client for a named endpoint.
fedClient ::
  forall (comp :: Component) (name :: Symbol) m api.
  (CallsFed comp name, HasFedEndpoint comp api name, HasClient m api, m ~ FederatorClient comp) =>
  Client m api
fedClient = clientIn (Proxy @api) (Proxy @m)

fedClientIn ::
  forall (comp :: Component) (name :: Symbol) m api.
  (HasFedEndpoint comp api name, HasClient m api) =>
  Client m api
fedClientIn = clientIn (Proxy @api) (Proxy @m)

-- | Like 'fedClientIn', but doesn't propagate a 'CallsFed' constraint. Inteded
-- to be used in test situations only.
unsafeFedClientIn ::
  forall (comp :: Component) (name :: Symbol) m api.
  (HasUnsafeFedEndpoint comp api name, HasClient m api) =>
  Client m api
unsafeFedClientIn = clientIn (Proxy @api) (Proxy @m)
