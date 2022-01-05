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
    clientRoutes,

    -- * Re-exports
    Component (..),
  )
where

import Servant.API
import Servant.Client.Generic
import Wire.API.Federation.API.Brig
import Wire.API.Federation.API.Cargohold
import Wire.API.Federation.API.Galley
import Wire.API.Federation.API.Version
import Wire.API.Federation.Client
import Wire.API.Federation.Component

class HasFederationAPI (comp :: Component) where
  -- Note: this type family being injective means that in most cases there is no need
  -- to add component annotations when invoking the federator client
  type FedApiV comp = (api :: Version -> * -> *) | api -> comp
  clientRoutes :: FedApiV comp v (AsClientT (FederatorClient comp))

instance HasFederationAPI 'Galley where
  type FedApiV 'Galley = GalleyApi
  clientRoutes = genericClient

instance HasFederationAPI 'Brig where
  type FedApiV 'Brig = BrigApi
  clientRoutes = genericClient

instance HasFederationAPI 'Cargohold where
  type FedApiV 'Cargohold = CargoholdApi
  clientRoutes = genericClient

type family CombinedAPI (versions :: [Version]) (comp :: Component) (mode :: *) :: *

type instance CombinedAPI '[v] comp mode = FedApiV comp v mode

type instance
  CombinedAPI (v1 ': v2 ': vs) comp mode =
    FedApiV comp v1 mode :<|> CombinedAPI (v2 ': vs) comp mode

type FedApi comp mode = CombinedAPI SupportedVersions comp mode
