{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

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

module Brig.Federation.Client where

import Brig.API.Error (notFound, throwStd)
import Brig.API.Handler (Handler)
import Brig.App (federator)
import Brig.Types.User
import Control.Lens (view, (^.))
import qualified Data.Aeson as Aeson
import Data.Handle
import Data.Qualified
import Data.String.Conversions
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Imports
import Mu.GRpc.Client.TyApps
import qualified System.Logger.Class as Log
import Util.Options (epHost, epPort)
import Wire.API.Federation.API.Brig
import qualified Wire.API.Federation.GRPC.Types as Proto

-- FUTUREWORK(federation): As of now, any failure in making a remote call results in 404.
-- This is not correct, we should figure out how we communicate failure
-- scenarios to the clients.
-- See https://wearezeta.atlassian.net/browse/SQCORE-491 for the issue on error handling improvements.
getUserHandleInfo :: Qualified Handle -> Handler (Maybe UserHandleInfo)
getUserHandleInfo (Qualified handle domain) = do
  Log.info $ Log.msg $ T.pack "Brig-federation: handle lookup call on remote backend"
  federatorClient <- mkFederatorClient
  let call = Proto.ValidatedFederatedRequest domain (mkGetUserInfoByHandle handle)
  res <- expectOk =<< callRemote federatorClient call
  case Proto.responseStatus res of
    404 -> pure Nothing
    200 -> case Aeson.eitherDecodeStrict (Proto.responseBody res) of
      Left err -> throwStd $ notFound $ "Failed to parse response: " <> LT.pack err
      Right x -> pure $ Just x
    code -> throwStd $ notFound $ "Invalid response from remote: " <> LT.pack (show code)

-- FUTUREWORK: It would be nice to share the client across all calls to
-- federator and not call this function on every invocation of federated
-- requests, but there are some issues in http2-client which might need some
-- fixing first. More context here:
-- https://github.com/lucasdicioccio/http2-client/issues/37
-- https://github.com/lucasdicioccio/http2-client/issues/49
mkFederatorClient :: Handler GrpcClient
mkFederatorClient = do
  maybeFederatorEndpoint <- view federator
  federatorEndpoint <- case maybeFederatorEndpoint of
    Nothing -> throwStd $ notFound "no federator configured"
    Just ep -> pure ep

  eitherClient <- setupGrpcClient' (grpcClientConfigSimple (T.unpack (federatorEndpoint ^. epHost)) (fromIntegral (federatorEndpoint ^. epPort)) False)
  case eitherClient of
    Left _ -> throwStd $ notFound "federator unreachable"
    Right c -> pure c

callRemote :: MonadIO m => GrpcClient -> Proto.ValidatedFederatedRequest -> m (GRpcReply Proto.Response)
callRemote fedClient call = liftIO $ gRpcCall @'MsgProtoBuf @Proto.Outward @"Outward" @"call" fedClient (Proto.validatedFederatedRequestToFederatedRequest call)

-- FUTUREWORK(federation) All of this code is only exercised in the test which
-- goes between two Backends. This is not ideal, we should figure out a way to
-- test client side of federated code without needing another backend. We could
-- do this either by mocking the second backend in integration tests or making
-- all of this independent of the Handler monad and write unit tests.
expectOk :: GRpcReply Proto.Response -> Handler Proto.HTTPResponse
expectOk = \case
  GRpcTooMuchConcurrency _tmc -> throwStd $ notFound "Too much concurrency"
  GRpcErrorCode errCode -> throwStd $ notFound $ LT.pack $ "GRPCError: " <> show errCode
  GRpcErrorString errStr -> throwStd $ notFound $ LT.pack $ "GRPCError: " <> show errStr
  GRpcClientError clErr -> throwStd $ notFound $ LT.pack $ "GRPC ClietnError: " <> show clErr
  GRpcOk (Proto.ResponseErr err) -> throwStd $ notFound $ LT.pack $ "Remote component errored: " <> show err
  GRpcOk (Proto.ResponseHTTPResponse res) -> pure res
