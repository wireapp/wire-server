{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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

module Galley.Intra.Federator (interpretFederatorAccess) where

import Control.Lens
import Control.Monad.Except
import Data.Bifunctor
import Data.Qualified
import Galley.Effects.FederatorAccess (FederatorAccess (..))
import Galley.Env
import Galley.Monad
import Galley.Options
import Imports
import Polysemy
import Polysemy.Input
import UnliftIO
import Wire.API.Federation.Client
import Wire.API.Federation.Component
import Wire.API.Federation.Error

interpretFederatorAccess ::
  Members '[Embed IO, Input Env] r =>
  Sem (FederatorAccess ': r) a ->
  Sem r a
interpretFederatorAccess = interpret $ \case
  RunFederated dom rpc -> embedApp $ runFederated dom rpc
  RunFederatedEither dom rpc -> embedApp $ runFederatedEither dom rpc
  RunFederatedConcurrently rs f -> embedApp $ runFederatedConcurrently rs f
  RunFederatedConcurrentlyEither rs f ->
    embedApp $
      runFederatedConcurrentlyEither rs f
  IsFederationConfigured -> embedApp $ isJust <$> view federator

runFederatedEither ::
  KnownComponent c =>
  Remote x ->
  FederatorClient c a ->
  App (Either FederationError a)
runFederatedEither (tDomain -> remoteDomain) rpc = do
  ownDomain <- view (options . optSettings . setFederationDomain)
  mfedEndpoint <- view federator
  case mfedEndpoint of
    Nothing -> pure (Left FederationNotConfigured)
    Just fedEndpoint -> do
      let ce =
            FederatorClientEnv
              { ceOriginDomain = ownDomain,
                ceTargetDomain = remoteDomain,
                ceFederator = fedEndpoint
              }
      liftIO . fmap (first FederationCallFailure) $ runFederatorClient ce rpc

runFederated ::
  KnownComponent c =>
  Remote x ->
  FederatorClient c a ->
  App a
runFederated dom rpc =
  runFederatedEither dom rpc
    >>= either (throwIO . federationErrorToWai) pure

runFederatedConcurrently ::
  ( Foldable f,
    Functor f,
    KnownComponent c
  ) =>
  f (Remote a) ->
  (Remote [a] -> FederatorClient c b) ->
  App [Remote b]
runFederatedConcurrently xs rpc =
  pooledForConcurrentlyN 8 (bucketRemote xs) $ \r ->
    qualifyAs r <$> runFederated r (rpc r)

runFederatedConcurrentlyEither ::
  (Foldable f, Functor f, KnownComponent c) =>
  f (Remote a) ->
  (Remote [a] -> FederatorClient c b) ->
  App [Either (Remote [a], FederationError) (Remote b)]
runFederatedConcurrentlyEither xs rpc =
  pooledForConcurrentlyN 8 (bucketRemote xs) $ \r ->
    bimap (r,) (qualifyAs r) <$> runFederatedEither r (rpc r)
