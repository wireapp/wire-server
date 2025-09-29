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
import Data.ByteString.Conversion (fromByteString)
import Data.Kind
import Data.Metrics.Servant
import Data.Misc (IpAddr)
import Data.OpenApi qualified as S
import Data.Sequence qualified as Seq
import GHC.TypeLits
import Imports
import Network.HTTP.Types qualified as HTTP
import Servant
import Servant.Client
import Servant.Client.Core
import Servant.OpenApi (HasOpenApi (toOpenApi))
import Servant.Server.Internal (MkContextWithErrorFormatter)
import Wire.API.ApplyMods
import Wire.API.Federation.API.Common
import Wire.API.Federation.Domain
import Wire.API.Federation.HasNotificationEndpoint
import Wire.API.Routes.Named

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
        :> StreamPostWithRemoteIp NoFraming OctetStream output
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

-- | The remote backend's origin IP is best-effort forensic metadata only.
--   Do not use for auth, policy, or attribution; cert identity is authoritative.
--   IP reflects only the socket peer at our edge (often LB/NAT/egress) and may be inaccurate.
data OriginIpHeader

instance (RoutesToPaths api) => RoutesToPaths (OriginIpHeader :> api) where
  getRoutes = getRoutes @api

instance (HasClient m api) => HasClient m (OriginIpHeader :> api) where
  type Client m (OriginIpHeader :> api) = Client m api
  clientWithRoute pm _ req = clientWithRoute pm (Proxy @api) req
  hoistClientMonad pm _ = hoistClientMonad pm (Proxy @api)

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

-- Header carrying the best-effort remote peer IP observed by our internal
-- federator when calling a remote backend. Forensic metadata only; do not use
-- for auth/policy or attribution.
type RemoteIpHeaderName = "Wire-Remote-IP" :: Symbol

remoteIpHeaderName :: (IsString a) => a
remoteIpHeaderName = fromString $ symbolVal (Proxy @RemoteIpHeaderName)

instance (HasOpenApi api) => HasOpenApi (OriginIpHeader :> api) where
  toOpenApi _ = desc $ toOpenApi (Proxy @api)
    where
      desc =
        S.allOperations
          . S.description
          ?~ ("Federated endpoints may include optional origin IP header: `" <> originIpHeaderName <> "`")

-- | A streaming POST combinator that behaves like Servant's 'StreamPost',
--   but whose client additionally returns an optional remote IP parsed from
--   the 'Wire-Remote-IP' response header.
data StreamPostWithRemoteIp framing (ct :: Type) a

-- Server-side simply delegates to the standard 'StreamPost' implementation.
instance
  ( HasServer (StreamPost framing ct a) context
  ) =>
  HasServer (StreamPostWithRemoteIp framing ct a) context
  where
  type ServerT (StreamPostWithRemoteIp framing ct a) m = ServerT (StreamPost framing ct a) m
  route _ = route (Proxy @(StreamPost framing ct a))
  hoistServerWithContext _ = hoistServerWithContext (Proxy @(StreamPost framing ct a))

-- OpenAPI, metrics and path routing can delegate to the underlying StreamPost
instance (RoutesToPaths (StreamPost framing ct a)) => RoutesToPaths (StreamPostWithRemoteIp framing ct a) where
  getRoutes = getRoutes @(StreamPost framing ct a)

instance (HasOpenApi (StreamPost framing ct a)) => HasOpenApi (StreamPostWithRemoteIp framing ct a) where
  toOpenApi _ = toOpenApi (Proxy @(StreamPost framing ct a))

-- Client-side: make the streaming request and return the body together with an
-- optional 'IpAddr' parsed from the 'Wire-Remote-IP' header.
instance
  ( RunStreamingClient m,
    Accept ct,
    FromSourceIO ByteString a
  ) =>
  HasClient m (StreamPostWithRemoteIp framing ct a)
  where
  type Client m (StreamPostWithRemoteIp framing ct a) = m (Maybe IpAddr, a)

  clientWithRoute _ _ req =
    withStreamingRequest
      req
        { requestMethod = HTTP.methodPost,
          requestAccept = Seq.singleton (contentType (Proxy @ct))
        }
      $ \resp -> do
        let hdrs = toList (responseHeaders resp)
            mIp = lookup remoteIpHeaderName hdrs >>= fromByteString
        body <- fromSourceIO (responseBody resp)
        pure (mIp, body)

  hoistClientMonad _ _ f c = f c
