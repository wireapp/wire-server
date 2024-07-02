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
import Data.Bifunctor
import Data.Qualified
import Galley.Cassandra.Util
import Galley.Effects.FederatorAccess (FederatorAccess (..))
import Galley.Env
import Galley.Env qualified as E
import Galley.Monad
import Galley.Options
import Imports
import Polysemy
import Polysemy.Input
import Polysemy.TinyLog
import UnliftIO
import Wire.API.Federation.Client
import Wire.API.Federation.Error

interpretFederatorAccess ::
  ( Member (Embed IO) r,
    Member (Input Env) r,
    Member TinyLog r
  ) =>
  Sem (FederatorAccess ': r) a ->
  Sem r a
interpretFederatorAccess = interpret $ \case
  RunFederated dom rpc -> do
    logEffect "FederatorAccess.RunFederated"
    embedApp $ runFederated dom rpc
  RunFederatedEither dom rpc -> do
    logEffect "FederatorAccess.RunFederatedEither"
    embedApp $ runFederatedEither dom rpc
  RunFederatedConcurrently rs f -> do
    logEffect "FederatorAccess.RunFederatedConcurrently"
    embedApp $ runFederatedConcurrently rs f
  RunFederatedConcurrentlyEither rs f -> do
    logEffect "FederatorAccess.RunFederatedConcurrentlyEither"
    embedApp $ runFederatedConcurrentlyEither rs f
  RunFederatedConcurrentlyBucketsEither rs f -> do
    logEffect "FederatorAccess.RunFederatedConcurrentlyBucketsEither"
    embedApp $ runFederatedConcurrentlyBucketsEither rs f
  IsFederationConfigured -> do
    logEffect "FederatorAccess.IsFederationConfigured"
    embedApp $ isJust <$> view E.federator

runFederatedEither ::
  Remote x ->
  FederatorClient c a ->
  App (Either FederationError a)
runFederatedEither (tDomain -> remoteDomain) rpc = do
  ownDomain <- view (options . settings . federationDomain)
  mfedEndpoint <- view E.federator
  mgr <- view http2Manager
  rid <- view reqId
  case mfedEndpoint of
    Nothing -> pure (Left FederationNotConfigured)
    Just fedEndpoint -> do
      let ce =
            FederatorClientEnv
              { ceOriginDomain = ownDomain,
                ceTargetDomain = remoteDomain,
                ceFederator = fedEndpoint,
                ceHttp2Manager = mgr,
                ceOriginRequestId = rid
              }
      liftIO . fmap (first FederationCallFailure) $ runFederatorClient ce rpc

runFederated ::
  Remote x ->
  FederatorClient c a ->
  App a
runFederated dom rpc =
  runFederatedEither dom rpc
    >>= either (throwIO . federationErrorToWai) pure

runFederatedConcurrently ::
  ( Foldable f,
    Functor f
  ) =>
  f (Remote a) ->
  (Remote [a] -> FederatorClient c b) ->
  App [Remote b]
runFederatedConcurrently xs rpc =
  pooledForConcurrentlyN 8 (bucketRemote xs) $ \r ->
    qualifyAs r <$> runFederated r (rpc r)

runFederatedConcurrentlyEither ::
  (Foldable f, Functor f) =>
  f (Remote a) ->
  (Remote [a] -> FederatorClient c b) ->
  App [Either (Remote [a], FederationError) (Remote b)]
runFederatedConcurrentlyEither xs rpc =
  pooledForConcurrentlyN 8 (bucketRemote xs) $ \r ->
    bimap (r,) (qualifyAs r) <$> runFederatedEither r (rpc r)

runFederatedConcurrentlyBucketsEither ::
  (Foldable f) =>
  f (Remote x) ->
  (Remote x -> FederatorClient c b) ->
  App [Either (Remote x, FederationError) (Remote b)]
runFederatedConcurrentlyBucketsEither xs rpc =
  pooledForConcurrentlyN 8 (toList xs) $ \r ->
    bimap (r,) (qualifyAs r) <$> runFederatedEither r (rpc r)
