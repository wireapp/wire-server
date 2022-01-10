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

module Wire.API.Federation.API
  ( FedApi,
    HasFedEndpoint,
    fedClient,

    -- * Re-exports
    Component (..),
  )
where

import Data.Proxy
import GHC.TypeLits
import Imports
import Servant.Client
import Servant.Client.Core
import Wire.API.Federation.API.Brig
import Wire.API.Federation.API.Cargohold
import Wire.API.Federation.API.Galley
import Wire.API.Federation.Component
import Wire.API.Federation.Endpoint

-- Note: this type family being injective means that in most cases there is no need
-- to add component annotations when invoking the federator client
type family FedApi (comp :: Component) = (api :: *) | api -> comp

type instance FedApi 'Galley = GalleyApi

type instance FedApi 'Brig = BrigApi

type instance FedApi 'Cargohold = CargoholdApi

type HasFedEndpoint comp api name = ('Just api ~ LookupEndpoint (FedApi comp) name)

-- | Return a client for a named endpoint.
fedClient ::
  forall (comp :: Component) (name :: Symbol) m api.
  (HasFedEndpoint comp api name, HasClient m api) =>
  Client m api
fedClient = clientIn (Proxy @api) (Proxy @m)
