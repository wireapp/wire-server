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

module Galley.Effects.FederatorAccess
  ( -- * Federator access effect
    FederatorAccess (..),
    runFederated,
    runFederatedEither,
    runFederatedConcurrently,
    runFederatedConcurrentlyEither,
    runFederatedConcurrently_,
  )
where

import Data.Qualified
import Galley.Intra.Federator.Types
import Imports
import Polysemy
import Wire.API.Federation.Client
import Wire.API.Federation.GRPC.Types

data FederatorAccess m a where
  RunFederated ::
    forall (c :: Component) a m x.
    Remote x ->
    FederatedRPC c a ->
    FederatorAccess m a
  RunFederatedEither ::
    forall (c :: Component) a m x.
    Remote x ->
    FederatedRPC c a ->
    FederatorAccess m (Either FederationError a)
  RunFederatedConcurrently ::
    forall (c :: Component) f a m x.
    (Foldable f, Functor f) =>
    f (Remote x) ->
    (Remote [x] -> FederatedRPC c a) ->
    FederatorAccess m [Remote a]
  RunFederatedConcurrentlyEither ::
    forall (c :: Component) f a m x.
    (Foldable f, Functor f) =>
    f (Remote x) ->
    (Remote [x] -> FederatedRPC c a) ->
    FederatorAccess m [Either (Remote [x], FederationError) (Remote a)]

makeSem ''FederatorAccess

runFederatedConcurrently_ ::
  (Foldable f, Functor f, Member FederatorAccess r) =>
  f (Remote a) ->
  (Remote [a] -> FederatedRPC c ()) ->
  Sem r ()
runFederatedConcurrently_ xs = void . runFederatedConcurrently xs
