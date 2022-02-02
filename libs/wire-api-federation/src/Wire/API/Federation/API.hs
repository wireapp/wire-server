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
    VersionedFedApi,
    VersionedFedApi',
    HasFedEndpoint,
    fedClient,
    fedClientIn,
    mkVersionedServer,

    -- * Re-exports
    Component (..),
    Version (..),
    VL,
  )
where

import Data.Proxy
import GHC.TypeLits
import Imports
import Servant
import Servant.Client
import Servant.Client.Core
import Wire.API.Federation.API.Brig
import Wire.API.Federation.API.Cargohold
import Wire.API.Federation.API.Galley
import Wire.API.Federation.Client
import Wire.API.Federation.Component
import Wire.API.Federation.Endpoint
import Wire.API.Federation.Version
import Wire.API.Federation.Version.Info

type family FedApi (comp :: Component) (v :: Version) :: * where
  FedApi 'Galley v = GalleyApi v
  FedApi 'Brig v = BrigApi v
  FedApi 'Cargohold v = CargoholdApi v

type HasFedEndpoint comp v api name = ('Just api ~ LookupEndpoint (FedApi comp v) name)

-- | Return a client for a named endpoint.
fedClient ::
  forall (comp :: Component) (v :: Version) (name :: Symbol) api.
  ( HasFedEndpoint comp v api name,
    HasClient FederatorClient api,
    KnownSymbol (ComponentPrefix comp)
  ) =>
  Client FederatorClient api
fedClient = fedClientIn @comp @v @name @FederatorClient

fedClientIn ::
  forall (comp :: Component) (v :: Version) (name :: Symbol) m api.
  ( HasFedEndpoint comp v api name,
    HasClient m api,
    KnownSymbol (ComponentPrefix comp)
  ) =>
  Client m api
fedClientIn = clientIn (Proxy @(ComponentPrefix comp :> api)) (Proxy @m)

type family CombinedApi (comp :: Component) (vs :: [Version]) where
  CombinedApi comp '[v0] = FedApi comp v0
  CombinedApi comp (v0 ': vs) = FedApi comp v0 :<|> CombinedApi comp vs

apiVersionEndpoint :: Applicative m => ServerT ApiVersionEndpoint m
apiVersionEndpoint = pure supportedVersionInfo

-- | All versions of the federation API.
type VersionedFedApi (comp :: Component) = CombinedApi comp SupportedVersions

-- | All versions of the federation API, plus an endpoint returning all versions.
type VersionedFedApi' (comp :: Component) =
  ApiVersionEndpoint :<|> VersionedFedApi comp

mkVersionedServer ::
  forall (comp :: Component) m.
  Applicative m =>
  ServerT (VersionedFedApi comp) m ->
  ServerT (VersionedFedApi' comp) m
mkVersionedServer h = apiVersionEndpoint :<|> h
