{-# LANGUAGE TemplateHaskell #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.FederationAPIAccess where

import Data.Kind
import Data.Qualified
import Imports
import Polysemy
import Polysemy.Error
import Wire.API.Federation.Component
import Wire.API.Federation.Error

data FederationAPIAccess (fedM :: Component -> Type -> Type) m a where
  RunFederatedEither ::
    (KnownComponent c) =>
    Remote x ->
    fedM c a ->
    FederationAPIAccess fedM m (Either FederationError a)
  RunFederatedConcurrentlyEither ::
    (KnownComponent c, Foldable f, Functor f) =>
    f (Remote x) ->
    (Remote [x] -> fedM c a) ->
    FederationAPIAccess fedM m [Either (Remote [x], FederationError) (Remote a)]
  RunFederatedConcurrentlyBucketsEither ::
    (KnownComponent c, Foldable f) =>
    f (Remote x) ->
    (Remote x -> fedM c a) ->
    FederationAPIAccess fedM m [Either (Remote x, FederationError) (Remote a)]
  IsFederationConfigured :: FederationAPIAccess fedM m Bool

makeSem ''FederationAPIAccess

runFederated ::
  forall c fedM x a r.
  ( Member (FederationAPIAccess fedM) r,
    Member (Error FederationError) r,
    KnownComponent c
  ) =>
  Remote x ->
  fedM c a ->
  Sem r a
runFederated rx c = runFederatedEither rx c >>= fromEither

runFederatedConcurrently ::
  forall c fedM f x a r.
  ( Member (FederationAPIAccess fedM) r,
    Member (Error FederationError) r,
    KnownComponent c,
    Foldable f,
    Functor f
  ) =>
  f (Remote x) ->
  (Remote [x] -> fedM c a) ->
  Sem r [Remote a]
runFederatedConcurrently rx c = do
  results <- runFederatedConcurrentlyEither rx c
  fromEither $ mapLeft snd $ sequence results
