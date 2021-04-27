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
module Galley.Federation.Client where

import Control.Error (throwE)
import Control.Error.Util ((!?))
import Control.Lens (view, (^.))
import Control.Monad.Except (ExceptT, runExceptT)
import Data.Domain
import qualified Data.Text as T
import Galley.API.Util (viewFederationDomain)
import Galley.App (Galley, federator)
import Imports
import Network.GRPC.Client.Helpers (GrpcClient, grpcClientConfigSimple)
import Util.Options (epHost, epPort)
import Wire.API.Federation.Client (FederationClientError, FederationError (..), FederatorClient, runFederatorClientWith)
import Wire.API.Federation.GRPC.Client

type FederationAppIO = ExceptT FederationError Galley

-- FUTUREWORK: It would be nice to share the client across all calls to
-- federator and not call this function on every invocation of federated
-- requests, but there are some issues in http2-client which might need some
-- fixing first. More context here:
-- https://github.com/lucasdicioccio/http2-client/issues/37
-- https://github.com/lucasdicioccio/http2-client/issues/49
mkFederatorClient :: FederationAppIO GrpcClient
mkFederatorClient = do
  federatorEndpoint <- view federator !? FederationNotConfigured
  let cfg = grpcClientConfigSimple (T.unpack (federatorEndpoint ^. epHost)) (fromIntegral (federatorEndpoint ^. epPort)) False
  createGrpcClient cfg
    >>= either (throwE . FederationUnavailable . reason) pure

executeFederated :: Domain -> FederatorClient component (ExceptT FederationClientError FederationAppIO) a -> FederationAppIO a
executeFederated targetDomain action = do
  federatorClient <- mkFederatorClient
  originDomain <- viewFederationDomain
  runExceptT (runFederatorClientWith federatorClient targetDomain originDomain action)
    >>= either (throwE . FederationCallFailure) pure
