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

getUserHandleInfo :: Qualified Handle -> Handler (Maybe UserHandleInfo)
getUserHandleInfo (Qualified handle domain) = do
  Log.info $ Log.msg $ T.pack "Brig-federation: handle lookup call on remote backend"
  fedClient <- federatorClient
  let call = Proto.ValidatedRemoteCall domain (mkGetUserInfoByHandle handle)
  res <- expectOk =<< callRemote fedClient call
  case Proto.responseStatus res of
    404 -> pure Nothing
    200 -> case Aeson.eitherDecodeStrict (Proto.responseBody res) of
      Left err -> throwStd $ notFound $ "Failed to parse response: " <> LT.pack err
      Right x -> pure $ Just x
    code -> throwStd $ notFound $ "Invalid response from remote: " <> LT.pack (show code)

-- FUTUREWORK: Maybe it makes sense to share the client and not this function on every invocation of federated requests.
federatorClient :: Handler GrpcClient
federatorClient = do
  maybeFederatorEndpoint <- view federator
  federatorEndpoint <- case maybeFederatorEndpoint of
    Nothing -> throwStd $ notFound "no federator configured" -- TODO: Do we need a special HTTP status code for this?
    Just ep -> pure ep
  -- TODO: Move this somewhere shared, maybe Env?
  eitherClient <- setupGrpcClient' (grpcClientConfigSimple (T.unpack (federatorEndpoint ^. epHost)) (fromIntegral (federatorEndpoint ^. epPort)) False)
  case eitherClient of
    Left _ -> throwStd $ notFound "failed to connect to the federator" -- TODO: This is wrong
    Right c -> pure c

callRemote :: MonadIO m => GrpcClient -> Proto.ValidatedRemoteCall -> m (GRpcReply Proto.Response)
callRemote fedClient call = liftIO $ gRpcCall @'MsgProtoBuf @Proto.RouteToRemote @"RouteToRemote" @"call" fedClient (Proto.validatedRemoteCallToRemoteCall call)

-- TODO:  Handle errors better
expectOk :: GRpcReply Proto.Response -> Handler Proto.HTTPResponse
expectOk = \case
  GRpcTooMuchConcurrency _tmc -> throwStd $ notFound "Too much concurrency"
  GRpcErrorCode errCode -> throwStd $ notFound $ LT.pack $ "GRPCError: " <> show errCode
  GRpcErrorString errStr -> throwStd $ notFound $ LT.pack $ "GRPCError: " <> show errStr
  GRpcClientError clErr -> throwStd $ notFound $ LT.pack $ "GRPC ClietnError: " <> show clErr
  GRpcOk (Proto.ResponseErr err) -> throwStd $ notFound $ LT.pack $ "Remote component errored: " <> show err
  GRpcOk (Proto.ResponseHTTPResponse res) -> pure res
