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

module Wire.API.Routes.QualifiedCapture
  ( QualifiedCapture,
    QualifiedCapture',
    WithDomain,
  )
where

import Data.Domain
import Data.Kind
import Data.Metrics.Servant
import Data.OpenApi hiding (HasServer, value)
import Data.Qualified
import GHC.TypeLits
import Imports
import Servant
import Servant.API.Description
import Servant.API.Modifiers
import Servant.Client.Core.HasClient
import Servant.OpenApi
import Servant.Server.Internal.ErrorFormatter
import Wire.API.Routes.Version

-- | Capture a value qualified by a domain, with modifiers.
data QualifiedCapture' (mods :: [Type]) (capture :: Symbol) (a :: Type)

-- | Capture a value qualified by a domain.
--
-- This works exactly like capturing a domain first then the value, but it
-- provides a 'Qualified' value to the handler, instead of both a domain and a
-- value.
type QualifiedCapture capture a = QualifiedCapture' '[] capture a

type WithDomain mods capture a api =
  Capture (AppendSymbol capture "_domain") Domain
    :> Capture' mods capture a
    :> api

type instance
  SpecialiseToVersion v (QualifiedCapture' mods capture a :> api) =
    QualifiedCapture' mods capture a :> SpecialiseToVersion v api

instance
  ( ToParamSchema a,
    HasOpenApi api,
    KnownSymbol capture,
    KnownSymbol (AppendSymbol capture "_domain"),
    KnownSymbol (FoldDescription mods)
  ) =>
  HasOpenApi (QualifiedCapture' mods capture a :> api)
  where
  toOpenApi _ = toOpenApi (Proxy @(WithDomain mods capture a api))

instance
  ( KnownSymbol capture,
    Typeable a,
    FromHttpApiData a,
    HasServer api context,
    SBoolI (FoldLenient mods),
    KnownSymbol (AppendSymbol capture "_domain"),
    HasContextEntry (MkContextWithErrorFormatter context) ErrorFormatters
  ) =>
  HasServer (QualifiedCapture' mods capture a :> api) context
  where
  type
    ServerT (QualifiedCapture' mods capture a :> api) m =
      Qualified (If (FoldLenient mods) (Either String a) a) ->
      ServerT api m

  hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy :: Proxy api) pc nt . s

  route _ pc m = route (Proxy @(WithDomain mods capture a api)) pc (fmap qualify m)
    where
      qualify handler domain value = handler (Qualified value domain)

instance
  ( ToHttpApiData a,
    HasClient m api
  ) =>
  HasClient m (QualifiedCapture' mods capture a :> api)
  where
  type
    Client m (QualifiedCapture' mods capture a :> api) =
      Qualified a -> Client m api

  clientWithRoute pm _ req (Qualified value domain) =
    clientWithRoute pm (Proxy @(WithDomain mods capture a api)) req domain value
  hoistClientMonad pm _ f cl = hoistClientMonad pm (Proxy @api) f . cl

instance (RoutesToPaths api, KnownSymbol (AppendSymbol capture "_domain"), KnownSymbol capture) => RoutesToPaths (QualifiedCapture' mods capture a :> api) where
  getRoutes =
    getRoutes
      @( Capture' mods (AppendSymbol capture "_domain") Domain
           :> Capture' mods capture a
           :> api
       )
