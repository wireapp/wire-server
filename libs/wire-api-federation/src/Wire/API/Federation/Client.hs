{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists #-}

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

module Wire.API.Federation.Client where

import Control.Monad.Except (ExceptT, MonadError (..), withExceptT)
import Data.ByteString.Builder (toLazyByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Domain (Domain, domainText)
import qualified Data.Text as T
import Imports
import Mu.GRpc.Client.TyApps (GRpcMessageProtocol (MsgProtoBuf), GRpcReply (..), GrpcClient, gRpcCall, grpcClientConfigSimple)
import qualified Network.HTTP.Types as HTTP
import Servant.Client (ResponseF (..))
import qualified Servant.Client as Servant
import Servant.Client.Core (RequestBody (..), RequestF (..), RunClient (..))
import Util.Options (Endpoint (..))
import Wire.API.Federation.GRPC.Client (createGrpcClient, reason)
import qualified Wire.API.Federation.GRPC.Types as Proto

data FederatorClientEnv = FederatorClientEnv
  { grpcClient :: GrpcClient,
    targetDomain :: Domain,
    originDomain :: Domain
  }

newtype FederatorClient (component :: Proto.Component) m a = FederatorClient {runFederatorClient :: ReaderT FederatorClientEnv m a}
  deriving newtype (Functor, Applicative, Monad, MonadReader FederatorClientEnv, MonadIO)

runFederatorClientWith :: GrpcClient -> Domain -> Domain -> FederatorClient component m a -> m a
runFederatorClientWith client targetDomain originDomain = flip runReaderT (FederatorClientEnv client targetDomain originDomain) . runFederatorClient

class KnownComponent (c :: Proto.Component) where
  componentVal :: Proto.Component

instance KnownComponent 'Proto.Brig where
  componentVal = Proto.Brig

-- | expectedStatuses is ignored as we don't get any status from the federator,
-- all responses have '200 OK' as their status.
instance (Monad m, MonadError FederationClientError m, MonadIO m, KnownComponent component) => RunClient (FederatorClient component m) where
  runRequestAcceptStatus _expectedStatuses req = do
    env <- ask
    body <- readBody . maybe (RequestBodyBS "") fst $ requestBody req
    let call =
          Proto.ValidatedFederatedRequest
            (targetDomain env)
            ( Proto.Request
                (componentVal @component)
                (LBS.toStrict . toLazyByteString $ requestPath req)
                body
                (domainText (originDomain env))
            )
    grpcResponse <- callRemote (grpcClient env) call
    case grpcResponse of
      GRpcTooMuchConcurrency _tmc -> rpcErr "too much concurrency"
      GRpcErrorCode code -> rpcErr $ "grpc error code: " <> T.pack (show code)
      GRpcErrorString msg -> rpcErr $ "grpc error: " <> T.pack msg
      GRpcClientError msg -> rpcErr $ "grpc client error: " <> T.pack (show msg)
      GRpcOk (Proto.OutwardResponseError err) -> throwError (FederationClientOutwardError err)
      GRpcOk (Proto.OutwardResponseBody res) -> do
        pure $
          Response
            { responseStatusCode = HTTP.ok200,
              -- This is required so servant can parse the body
              responseHeaders = [(HTTP.hContentType, "application/json")],
              -- Here HTTP 1.1 is hardcoded with the hope that it wouldn't
              -- really be used anywhere.
              responseHttpVersion = HTTP.http11,
              responseBody = LBS.fromStrict res
            }
    where
      rpcErr = throwError . FederationClientRPCError
      readBody = \case
        RequestBodyLBS lbs -> pure $ LBS.toStrict lbs
        RequestBodyBS bs -> pure bs
        RequestBodySource _ -> throwError FederationClientStreamingUnsupported
  throwClientError = throwError . FederationClientServantError

instance (Monad m, MonadError FederationClientError m) => MonadError FederationClientError (FederatorClient c m) where
  throwError = FederatorClient . throwError
  catchError (FederatorClient action) f = FederatorClient $ catchError action (runFederatorClient . f)

data FederationError
  = FederationUnavailable Text
  | FederationNotImplemented
  | FederationNotConfigured
  | FederationCallFailure FederationClientError

data FederationClientError
  = FederationClientInvalidMethod HTTP.Method
  | FederationClientStreamingUnsupported
  | FederationClientRPCError Text
  | FederationClientOutwardError Proto.OutwardError
  | FederationClientServantError Servant.ClientError
  deriving (Show, Eq)

callRemote :: MonadIO m => GrpcClient -> Proto.ValidatedFederatedRequest -> m (GRpcReply Proto.OutwardResponse)
callRemote fedClient call = liftIO $ gRpcCall @'MsgProtoBuf @Proto.Outward @"Outward" @"call" fedClient (Proto.validatedFederatedRequestToFederatedRequest call)

class HasFederatorConfig m where
  federatorEndpoint :: m (Maybe Endpoint)
  federationDomain :: m Domain

-- FUTUREWORK: It would be nice to share the client across all calls to
-- federator and not call this function on every invocation of federated
-- requests, but there are some issues in http2-client which might need some
-- fixing first. More context here:
-- https://github.com/lucasdicioccio/http2-client/issues/37
-- https://github.com/lucasdicioccio/http2-client/issues/49
mkFederatorClient ::
  (MonadIO m, HasFederatorConfig m) =>
  ExceptT FederationError m GrpcClient
mkFederatorClient = do
  mbFedEndpoint <- lift federatorEndpoint
  Endpoint host port <- maybe (throwError FederationNotConfigured) pure mbFedEndpoint
  let cfg = grpcClientConfigSimple (T.unpack host) (fromIntegral port) False
  createGrpcClient cfg
    >>= either (throwError . FederationUnavailable . reason) pure

executeFederated ::
  (MonadIO m, HasFederatorConfig m) =>
  Domain ->
  FederatorClient component (ExceptT FederationClientError m) a ->
  ExceptT FederationError m a
executeFederated targetDomain action = do
  federatorClient <- mkFederatorClient
  originDomain <- lift federationDomain
  withExceptT FederationCallFailure (runFederatorClientWith federatorClient targetDomain originDomain action)
