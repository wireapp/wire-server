{-# LANGUAGE TemplateHaskell #-}

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

module Galley.Effects.FederatorAccess
  ( -- * Federator access effect
    FederatorAccess (..),
    runFederated,
    runFederatedEither,
    runFederatedConcurrently,
    runFederatedConcurrentlyEither,
    runFederatedConcurrentlyBucketsEither,
    isFederationConfigured,
  )
where

import Data.Qualified
import Imports
import Polysemy
import Wire.API.Federation.Client
import Wire.API.Federation.Component
import Wire.API.Federation.Error

data FederatorAccess m a where
  RunFederated ::
    (KnownComponent c) =>
    Remote x ->
    FederatorClient c a ->
    FederatorAccess m a
  RunFederatedEither ::
    (KnownComponent c) =>
    Remote x ->
    FederatorClient c a ->
    FederatorAccess m (Either FederationError a)
  RunFederatedConcurrently ::
    (KnownComponent c, Foldable f, Functor f) =>
    f (Remote x) ->
    (Remote [x] -> FederatorClient c a) ->
    FederatorAccess m [Remote a]
  RunFederatedConcurrentlyEither ::
    forall (c :: Component) f a m x.
    (KnownComponent c, Foldable f, Functor f) =>
    f (Remote x) ->
    (Remote [x] -> FederatorClient c a) ->
    FederatorAccess m [Either (Remote [x], FederationError) (Remote a)]
  -- | An action similar to 'RunFederatedConcurrentlyEither', but whose input is
  -- already in buckets. The buckets are paired with arbitrary data that affect
  -- the payload of the request for each remote backend.
  RunFederatedConcurrentlyBucketsEither ::
    forall (c :: Component) f a m x.
    (KnownComponent c, Foldable f) =>
    f (Remote x) ->
    (Remote x -> FederatorClient c a) ->
    FederatorAccess m [Either (Remote x, FederationError) (Remote a)]
  IsFederationConfigured :: FederatorAccess m Bool

makeSem ''FederatorAccess
