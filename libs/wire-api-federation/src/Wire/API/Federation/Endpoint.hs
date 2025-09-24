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

module Wire.API.Federation.Endpoint
  ( ApplyMods,
    module Wire.API.Federation.Endpoint,
  )
where

import Control.Lens ((?~))
import Data.Kind
import Data.Metrics.Servant
import Data.Misc (IpAddr)
import Data.OpenApi qualified as S
import Data.Proxy (Proxy (..))
import GHC.TypeLits
import Imports
import Servant.API
import Servant.Client
import Servant.OpenApi (HasOpenApi (toOpenApi))
import Servant.Server
import Servant.Server.Internal (MkContextWithErrorFormatter)
import Wire.API.ApplyMods
import Wire.API.Federation.API.Common
import Wire.API.Federation.Domain
import Wire.API.Federation.HasNotificationEndpoint
import Wire.API.Routes.ClientAlgebra
import Wire.API.Routes.Named
import Wire.API.Routes.SpecialiseToVersion (SpecialiseToVersion)

data Versioned v name

instance {-# OVERLAPPING #-} (RenderableSymbol a) => RenderableSymbol (Versioned v a) where
  renderSymbol = renderSymbol @a
  renderOperationId = renderOperationId @a

type family FedPath (name :: k) :: Symbol

type instance FedPath (name :: Symbol) = name

type instance FedPath (Versioned v name) = name

type UnnamedFedEndpointWithMods (mods :: [Type]) path input output =
  ( ApplyMods
      mods
      (path :> OriginDomainHeader :> ReqBody '[JSON] input :> Post '[JSON] output)
  )

type FedEndpointWithMods (mods :: [Type]) name input output =
  Named
    name
    ( UnnamedFedEndpointWithMods mods (FedPath name) input output
    )

type FedEndpoint name input output = FedEndpointWithMods '[] name input output

type NotificationFedEndpointWithMods (mods :: [Type]) name path input =
  Named name (UnnamedFedEndpointWithMods mods path input EmptyResponse)

type NotificationFedEndpoint tag =
  MkNotificationFedEndpoint
    (NotificationMods tag)
    (NotificationPath tag)
    (NotificationVersionTag tag)
    (Payload tag)

type StreamingFedEndpoint name input output =
  Named
    name
    ( name
        :> OriginDomainHeader
        :> OriginIpHeader
        :> ReqBody '[JSON] input
        :> StreamPost NoFraming OctetStream output
    )

type family
  MkNotificationFedEndpoint
    (m :: [Type])
    (s :: Symbol)
    (v :: Maybe k)
    (p :: Type)

type instance
  MkNotificationFedEndpoint m s 'Nothing p =
    NotificationFedEndpointWithMods m s s p

type instance
  MkNotificationFedEndpoint m s ('Just v) p =
    NotificationFedEndpointWithMods m (Versioned v s) s p

type OriginIpHeaderName = "Wire-Origin-IP" :: Symbol

data OriginIpHeader

instance (RoutesToPaths api) => RoutesToPaths (OriginIpHeader :> api) where
  getRoutes = getRoutes @api

type instance SpecialiseToVersion v (OriginIpHeader :> api) = OriginIpHeader :> SpecialiseToVersion v api

instance (HasClient m api) => HasClient m (OriginIpHeader :> api) where
  type Client m (OriginIpHeader :> api) = Client m api
  clientWithRoute pm _ req = clientWithRoute pm (Proxy @api) req
  hoistClientMonad pm _ = hoistClientMonad pm (Proxy @api)

instance (HasClientAlgebra m api) => HasClientAlgebra m (OriginIpHeader :> api) where
  joinClient = joinClient @m @api
  bindClient = bindClient @m @api

type OriginIpHeaderHasServer = Header' '[Strict] OriginIpHeaderName IpAddr

instance
  ( HasServer api context,
    HasContextEntry (MkContextWithErrorFormatter context) ErrorFormatters
  ) =>
  HasServer (OriginIpHeader :> api) context
  where
  type ServerT (OriginIpHeader :> api) m = Maybe IpAddr -> ServerT api m
  route _pa = route (Proxy @(OriginIpHeaderHasServer :> api))
  hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy :: Proxy api) pc nt . s

originIpHeaderName :: (IsString a) => a
originIpHeaderName = fromString $ symbolVal (Proxy @OriginIpHeaderName)

instance (HasOpenApi api) => HasOpenApi (OriginIpHeader :> api) where
  toOpenApi _ = desc $ toOpenApi (Proxy @api)
    where
      desc =
        S.allOperations
          . S.description
          ?~ ("Federated endpoints may include optional origin IP header: `" <> originIpHeaderName <> "`")
