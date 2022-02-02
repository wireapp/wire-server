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

module Wire.API.Federation.Endpoint where

import Data.Singletons hiding (type (@@))
import Imports
import Servant.API
import Servant.Client
import Servant.Client.Core
import Wire.API.Federation.Domain
import Wire.API.Federation.Version
import Wire.API.Federation.Version.Info
import Wire.API.Routes.Named

type FedEndpoint name input output =
  Named
    name
    (name :> OriginDomainHeader :> ReqBody '[JSON] input :> Post '[JSON] output)

type StreamingFedEndpoint name input output =
  Named
    name
    ( name :> OriginDomainHeader :> ReqBody '[JSON] input
        :> StreamPost NoFraming OctetStream output
    )

type family MappendMaybe (x :: Maybe k) (y :: Maybe k) :: Maybe k where
  MappendMaybe 'Nothing y = y
  MappendMaybe ('Just x) y = 'Just x

type family LookupEndpoint api name :: Maybe * where
  LookupEndpoint (Named name endpoint) name = 'Just endpoint
  LookupEndpoint (api1 :<|> api2) name =
    MappendMaybe
      (LookupEndpoint api1 name)
      (LookupEndpoint api2 name)
  LookupEndpoint api name = 'Nothing

type family FromJust (x :: Maybe k) where
  FromJust ('Just x) = x

type ApiVersionEndpoint = "api-versions" :> Post '[JSON] VersionInfo

type family (@@) (e :: *) (v :: Version) :: *

data (@!) :: * -> name -> *

type instance (api @! name) @@ v = FromJust (LookupEndpoint (api @@ v) name)

class VersionedApi (vapi :: *) where
  hoistV :: Sing v -> (forall x. m x -> n x) -> Client m (vapi @@ v) -> Client n (vapi @@ v)
  clientV :: RunClient m => Proxy m -> Sing v -> Client m (vapi @@ v)
  flattenV :: Monad m => Sing v -> m (Client m (vapi @@ v)) -> Client m (vapi @@ v)

-- flattenV :: m (Client m (vapi @@ v)) -> Client m (vapi @@ v)

hoistV0 ::
  forall vapi v m n.
  HasClient ClientM (vapi @@ v) =>
  Sing v ->
  (forall x. m x -> n x) ->
  Client m (vapi @@ v) ->
  Client n (vapi @@ v)
hoistV0 _ = hoistClient (Proxy @(vapi @@ v))
