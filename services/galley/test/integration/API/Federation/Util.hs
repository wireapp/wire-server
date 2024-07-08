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

module API.Federation.Util
  ( mkHandler,

    -- * the remote backend type
    BackendReachability (..),
    Backend (..),
    rbReachable,
    participating,
  )
where

import Data.Kind
import Data.Qualified
import Data.SOP
import Data.String.Conversions
import GHC.TypeLits
import Imports
import Servant
import Wire.API.Federation.Domain
import Wire.API.MakesFederatedCall
import Wire.API.Routes.Named
import Wire.API.VersionInfo

class HasTrivialHandler api where
  trivialHandler :: String -> Server api

instance HasTrivialHandler (Verb m c cs a) where
  trivialHandler name = throwError err501 {errBody = cs ("mock not implemented: " <> name)}

instance (HasTrivialHandler api) => HasTrivialHandler ((path :: Symbol) :> api) where
  trivialHandler = trivialHandler @api

instance (HasTrivialHandler api) => HasTrivialHandler (OriginDomainHeader :> api) where
  trivialHandler name _ = trivialHandler @api name

instance (HasTrivialHandler api) => HasTrivialHandler (MakesFederatedCall comp name :> api) where
  trivialHandler name _ = trivialHandler @api name

instance (HasTrivialHandler api) => HasTrivialHandler (ReqBody cs a :> api) where
  trivialHandler name _ = trivialHandler @api name

instance (HasTrivialHandler api) => HasTrivialHandler (Until v :> api) where
  trivialHandler = trivialHandler @api

instance (HasTrivialHandler api) => HasTrivialHandler (From v :> api) where
  trivialHandler = trivialHandler @api

trivialNamedHandler ::
  forall (name :: Symbol) api.
  (KnownSymbol name, HasTrivialHandler api) =>
  Server (Named name api)
trivialNamedHandler = Named (trivialHandler @api (symbolVal (Proxy @name)))

-- | Generate a servant handler from an incomplete list of handlers of named
-- endpoints.
class PartialAPI (api :: Type) (hs :: Type) where
  mkHandler :: hs -> Server api

instance
  (KnownSymbol name, HasTrivialHandler endpoint) =>
  PartialAPI (Named (name :: Symbol) endpoint) EmptyAPI
  where
  mkHandler _ = trivialNamedHandler @name @endpoint

instance
  {-# OVERLAPPING #-}
  (KnownSymbol name, HasTrivialHandler endpoint, PartialAPI api EmptyAPI) =>
  PartialAPI (Named (name :: Symbol) endpoint :<|> api) EmptyAPI
  where
  mkHandler h = trivialNamedHandler @name @endpoint :<|> mkHandler @api h

instance
  {-# OVERLAPPING #-}
  (h ~ Server endpoint, PartialAPI api hs) =>
  PartialAPI (Named (name :: Symbol) endpoint :<|> api) (Named name h :<|> hs)
  where
  mkHandler (h :<|> hs) = h :<|> mkHandler @api hs

instance
  (KnownSymbol name, HasTrivialHandler endpoint, PartialAPI api hs) =>
  PartialAPI (Named (name :: Symbol) endpoint :<|> api) hs
  where
  mkHandler hs = trivialNamedHandler @name @endpoint :<|> mkHandler @api hs

instance
  (h ~ Server endpoint) =>
  PartialAPI (Named (name :: Symbol) endpoint) (Named name h)
  where
  mkHandler = id

instance
  {-# OVERLAPPING #-}
  (h ~ Server endpoint, PartialAPI api EmptyAPI) =>
  PartialAPI (Named (name :: Symbol) endpoint :<|> api) (Named name h)
  where
  mkHandler h = h :<|> mkHandler @api EmptyAPI

--------------------------------------------------------------------------------
-- The remote backend type

data BackendReachability = BackendReachable | BackendUnreachable
  deriving (Eq, Ord)

data Backend = Backend
  { bReachable :: BackendReachability,
    bUsers :: Nat
  }
  deriving (Eq, Ord)

rbReachable :: Remote Backend -> BackendReachability
rbReachable = bReachable . tUnqualified

participating :: Remote Backend -> [a] -> [a]
participating rb users =
  if rbReachable rb == BackendReachable
    then users
    else []
