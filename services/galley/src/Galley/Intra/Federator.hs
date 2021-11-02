{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
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

import Control.Monad.Except
import Data.Qualified
import Galley.Effects.FederatorAccess (FederatorAccess (..))
import Galley.Env
import Galley.Intra.Federator.Types
import Imports
import Polysemy
import qualified Polysemy.Reader as P
import UnliftIO
import Wire.API.Federation.Client
import Wire.API.Federation.Error

embedFederationM ::
  Members '[Embed IO, P.Reader Env] r =>
  FederationM a ->
  Sem r a
embedFederationM action = do
  env <- P.ask
  embed $ runFederationM env action

interpretFederatorAccess ::
  Members '[Embed IO, P.Reader Env] r =>
  Sem (FederatorAccess ': r) a ->
  Sem r a
interpretFederatorAccess = interpret $ \case
  RunFederated dom rpc -> embedFederationM $ runFederated dom rpc
  RunFederatedEither dom rpc -> embedFederationM $ runFederatedEither dom rpc
  RunFederatedConcurrently rs f -> embedFederationM $ runFederatedConcurrently rs f

runFederatedEither ::
  Remote x ->
  FederatedRPC c a ->
  FederationM (Either FederationError a)
runFederatedEither (tDomain -> remoteDomain) rpc = do
  env <- ask
  liftIO $ runFederationM env (runExceptT (executeFederated remoteDomain rpc))

runFederated ::
  Remote x ->
  FederatedRPC c a ->
  FederationM a
runFederated dom rpc =
  runFederatedEither dom rpc
    >>= either (throwIO . federationErrorToWai) pure

runFederatedConcurrently ::
  (Foldable f, Functor f) =>
  f (Remote a) ->
  (Remote [a] -> FederatedRPC c b) ->
  FederationM [Remote b]
runFederatedConcurrently xs rpc =
  pooledForConcurrentlyN 8 (bucketRemote xs) $ \r ->
    qualifyAs r <$> runFederated r (rpc r)
