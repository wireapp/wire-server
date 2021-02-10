{-# LANGUAGE RecordWildCards #-}

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

module Federator.Remote where

import Data.Domain (Domain, domainText)
import Data.String.Conversions (cs)
import Federator.Discovery (DiscoverFederator, LookupError, discoverFederator)
import Imports
import Mu.GRpc.Client.Optics (GRpcReply)
import Mu.GRpc.Client.Record (GRpcMessageProtocol (MsgProtoBuf), GrpcClient, grpcClientConfigSimple, setupGrpcClient')
import Mu.GRpc.Client.TyApps (gRpcCall)
import Network.HTTP2.Client (ClientError)
import Polysemy
import Polysemy.TinyLog (TinyLog)
import qualified Polysemy.TinyLog as Log
import qualified System.Logger.Message as Log
import Wire.API.Federation.GRPC.Types
import Wire.Network.DNS.SRV (SrvTarget (SrvTarget))

data RemoteError
  = RemoteErrorDiscoveryFailure LookupError Domain
  | RemoteErrorClientFailure ClientError SrvTarget
  deriving (Show, Eq)

data Remote m a where
  DiscoverAndCall :: ValidatedRemoteCall -> Remote m (Either RemoteError (GRpcReply Response))

makeSem ''Remote

interpretRemote :: (Members [Embed IO, DiscoverFederator, TinyLog] r) => Sem (Remote ': r) a -> Sem r a
interpretRemote = interpret $ \case
  DiscoverAndCall ValidatedRemoteCall {..} -> do
    eitherTarget <- discoverFederator vDomain
    case eitherTarget of
      Left err -> do
        Log.debug $
          Log.msg ("Failed to find remote federator" :: ByteString)
            . Log.field "domain" (domainText vDomain)
            . Log.field "error" (show err)
        pure $ Left (RemoteErrorDiscoveryFailure err vDomain)
      Right target -> do
        -- FUTUREWORK: Make this use TLS, maybe make it configurable
        -- FUTUREWORK: Cache this client and use it for many requests
        eitherClient <- mkGrpcClient target
        case eitherClient of
          Right client ->
            Right <$> callRemoteFederator client vLocalCall
          Left err -> pure $ Left err

callRemoteFederator :: MonadIO m => GrpcClient -> LocalCall -> m (GRpcReply Response)
callRemoteFederator client localCall =
  liftIO $ gRpcCall @'MsgProtoBuf @RouteToInternal @"RouteToInternal" @"call" client localCall

mkGrpcClient :: Members '[Embed IO, TinyLog] r => SrvTarget -> Sem r (Either RemoteError GrpcClient)
mkGrpcClient target@(SrvTarget host port) = do
  let cfg = grpcClientConfigSimple (cs host) (fromInteger $ toInteger port) False
  eitherClient <- setupGrpcClient' cfg
  case eitherClient of
    Left err -> do
      Log.debug $
        Log.msg ("Failed to connect to remote federator" :: ByteString)
          . Log.field "host" host
          . Log.field "port" port
          . Log.field "error" (show err)
      pure $ Left (RemoteErrorClientFailure err target)
    Right client -> pure $ Right client
