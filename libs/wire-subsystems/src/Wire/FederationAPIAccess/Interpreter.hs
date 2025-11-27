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

module Wire.FederationAPIAccess.Interpreter where

import Data.Bifunctor (first)
import Data.Domain
import Data.Id
import Data.Qualified
import HTTP2.Client.Manager
import Imports
import Polysemy
import Util.Options
import Wire.API.Federation.Client
import Wire.API.Federation.Error
import Wire.FederationAPIAccess (FederationAPIAccess (..))
import Wire.Sem.Concurrency

data FederationAPIAccessConfig = FederationAPIAccessConfig
  { ownDomain :: Domain,
    federatorEndpoint :: Maybe Endpoint,
    http2Manager :: Http2Manager,
    requestId :: RequestId
  }

type FederatedActionRunner fedM r = forall c x. Domain -> fedM c x -> Sem r (Either FederationError x)

noFederationAPIAccess ::
  forall r fedM.
  (Member (Concurrency 'Unsafe) r) =>
  InterpreterFor (FederationAPIAccess fedM) r
noFederationAPIAccess =
  interpretFederationAPIAccessGeneral
    (\_ _ -> pure $ Left FederationNotConfigured)
    (pure False)

interpretFederationAPIAccess ::
  forall r.
  (Member (Embed IO) r, Member (Concurrency 'Unsafe) r) =>
  FederationAPIAccessConfig ->
  InterpreterFor (FederationAPIAccess FederatorClient) r
interpretFederationAPIAccess config action = do
  let isFederationConfigured = isJust config.federatorEndpoint
      runner :: FederatedActionRunner FederatorClient r
      runner remoteDomain rpc =
        case config.federatorEndpoint of
          Nothing -> pure (Left FederationNotConfigured)
          Just fedEndpoint -> do
            let ce =
                  FederatorClientEnv
                    { ceOriginDomain = config.ownDomain,
                      ceTargetDomain = remoteDomain,
                      ceFederator = fedEndpoint,
                      ceHttp2Manager = config.http2Manager,
                      ceOriginRequestId = config.requestId
                    }
            embed . fmap (first FederationCallFailure) $ runFederatorClient ce rpc
  interpretFederationAPIAccessGeneral runner (pure isFederationConfigured) action

interpretFederationAPIAccessGeneral ::
  forall fedM r.
  (Member (Concurrency 'Unsafe) r) =>
  FederatedActionRunner fedM r ->
  (Sem r Bool) ->
  InterpreterFor (FederationAPIAccess fedM) r
interpretFederationAPIAccessGeneral runFedM isFederationConfigured =
  interpret $
    \case
      RunFederatedEither remote rpc -> runFederatedEither runFedM remote rpc
      RunFederatedConcurrentlyEither remotes rpc -> runFederatedConcurrently runFedM remotes rpc
      RunFederatedConcurrentlyBucketsEither remotes rpc -> runFederatedBucketed runFedM remotes rpc
      IsFederationConfigured -> isFederationConfigured

runFederatedEither ::
  FederatedActionRunner fedM r ->
  Remote a ->
  fedM c b ->
  Sem r (Either FederationError b)
runFederatedEither runFedM (tDomain -> remoteDomain) rpc =
  runFedM remoteDomain rpc

runFederatedConcurrently ::
  ( Foldable f,
    Member (Concurrency 'Unsafe) r
  ) =>
  FederatedActionRunner fedM r ->
  f (Remote a) ->
  (Remote a -> fedM c b) ->
  Sem r [Either (Remote a, FederationError) (Remote b)]
runFederatedConcurrently runFedM xs rpc =
  unsafePooledForConcurrentlyN 8 (toList xs) $ \r ->
    bimap (r,) (qualifyAs r) <$> runFederatedEither runFedM r (rpc r)

runFederatedBucketed ::
  ( Foldable f,
    Member (Concurrency 'Unsafe) r
  ) =>
  FederatedActionRunner fedM r ->
  f (Remote a) ->
  (Remote a -> fedM c b) ->
  Sem r [Either (Remote a, FederationError) (Remote b)]
runFederatedBucketed runFedM xs rpc =
  unsafePooledForConcurrentlyN 8 (toList xs) $ \r ->
    bimap (r,) (qualifyAs r) <$> runFederatedEither runFedM r (rpc r)
