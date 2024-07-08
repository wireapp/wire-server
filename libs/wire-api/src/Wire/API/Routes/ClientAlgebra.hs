-- Disabling for `(Monad m, AllMime cs, HasClient m (MultiVerb method cs as r)) =>`
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

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

module Wire.API.Routes.ClientAlgebra where

import GHC.TypeLits
import Imports
import Servant
import Servant.API.ContentTypes
import Servant.Client.Core
import Wire.API.Routes.MultiVerb

-- | The type of a Servant client is always an algebra over the underlying
-- monad. Such an algebra structure can easily be constructed by induction over
-- the structure of the API. The base case is an API consisting of only a
-- response, in which case @Client m api = m R@, where @R@ is the response
-- type, and @m R@ is always an algebra over @m@.
--
-- Minimal definition: 'joinClient' | 'bindClient'.
class (HasClient m api) => HasClientAlgebra m api where
  joinClient :: m (Client m api) -> Client m api
  joinClient x = bindClient @m @api x id

  bindClient :: m a -> (a -> Client m api) -> Client m api
  bindClient x f = joinClient @m @api (fmap f x)

instance (HasClient m (Verb method s cs a)) => HasClientAlgebra m (Verb method s cs a) where
  joinClient = join
  bindClient = (>>=)

instance
  (Monad m, AllMime cs, HasClient m (MultiVerb method cs as r)) =>
  HasClientAlgebra m (MultiVerb method cs as r)
  where
  joinClient = join
  bindClient = (>>=)

instance
  ( HasClientAlgebra m api,
    HasClient m (ReqBody' mods (ct ': cts) a :> api)
  ) =>
  HasClientAlgebra m (ReqBody' mods (ct ': cts) a :> api)
  where
  joinClient x a = joinClient @m @api $ x <*> pure a

instance
  ( HasClientAlgebra m api,
    KnownSymbol sym
  ) =>
  HasClientAlgebra m ((sym :: Symbol) :> api)
  where
  joinClient = joinClient @m @api
  bindClient = bindClient @m @api
