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

import Data.Default (def)
import Data.Domain (Domain, domainText)
import Data.String.Conversions (cs)
import Data.X509.CertificateStore
import Federator.Discovery (DiscoverFederator, LookupError, discoverFederator)
import Imports
import Mu.GRpc.Client.Optics (GRpcReply)
import Mu.GRpc.Client.Record (GRpcMessageProtocol (MsgProtoBuf))
import Mu.GRpc.Client.TyApps (gRpcCall)
import Network.GRPC.Client.Helpers
import Network.TLS
import qualified Network.TLS as TLS
import qualified Network.TLS.Extra.Cipher as TLS
import Paths_federator
import Polysemy
import Polysemy.TinyLog (TinyLog)
import qualified Polysemy.TinyLog as Log
import qualified System.Logger.Message as Log
import System.X509
import Wire.API.Federation.GRPC.Client
import Wire.API.Federation.GRPC.Types
import Wire.Network.DNS.SRV (SrvTarget (SrvTarget))

data RemoteError
  = RemoteErrorDiscoveryFailure LookupError Domain
  | RemoteErrorClientFailure GrpcClientErr SrvTarget
  deriving (Show, Eq)

data Remote m a where
  DiscoverAndCall :: ValidatedFederatedRequest -> Remote m (Either RemoteError (GRpcReply InwardResponse))

makeSem ''Remote

interpretRemote :: (Members [Embed IO, DiscoverFederator, TinyLog] r) => Sem (Remote ': r) a -> Sem r a
interpretRemote = interpret $ \case
  DiscoverAndCall ValidatedFederatedRequest {..} -> do
    eitherTarget <- discoverFederator vDomain
    case eitherTarget of
      Left err -> do
        Log.debug $
          Log.msg ("Failed to find remote federator" :: ByteString)
            . Log.field "domain" (domainText vDomain)
            . Log.field "error" (show err)
        pure $ Left (RemoteErrorDiscoveryFailure err vDomain)
      Right target -> do
        eitherClient <- mkGrpcClient target
        case eitherClient of
          Right client ->
            Right <$> callInward client vRequest
          Left err -> pure $ Left err

callInward :: MonadIO m => GrpcClient -> Request -> m (GRpcReply InwardResponse)
callInward client request =
  liftIO $ gRpcCall @'MsgProtoBuf @Inward @"Inward" @"call" client request

-- FUTUREWORK(federation): Make this use TLS with real certificate validation
-- FUTUREWORK(federation): Allow a configurable trust store to be used in TLS certificate validation
--   See also https://github.com/lucasdicioccio/http2-client/issues/76
-- FUTUREWORK(federation): Cache this client and use it for many requests
mkGrpcClient :: Members '[Embed IO, TinyLog] r => SrvTarget -> Sem r (Either RemoteError GrpcClient)
mkGrpcClient target@(SrvTarget host port) = do
  -- FUTUREWORK(federation): grpcClientConfigSimple using TLS is INSECURE and IGNORES any certificates and there's no way
  -- to change that (at least not when using the default functions from mu or http2-grpc-client)
  -- See https://github.com/haskell-grpc-native/http2-grpc-haskell/issues/47
  -- While early testing, this is "convenient" but needs to be fixed!
  --
  -- FUTUREWORK: load client certificate and client key from disk
  -- and use it when making a request
  let cfg = grpcClientConfigSimple (cs host) (fromInteger $ toInteger port) True

  -- FUTUREWORK: get review on blessed ciphers
  let blessed_ciphers =
        [ TLS.cipher_ECDHE_RSA_AES256GCM_SHA384,
          TLS.cipher_ECDHE_RSA_AES256CBC_SHA384,
          TLS.cipher_ECDHE_RSA_AES128GCM_SHA256,
          TLS.cipher_ECDHE_RSA_AES128CBC_SHA256
        ]

  -- TODO: read this path from config file
  let caPath = "../../deploy/services-demo/conf/nginz/integration-ca.pem" :: FilePath
  -- FUTUREWORK: review if a fallback to system trust store is a good idea
  customTrustStore <-
    embed $
      readCertificateStore caPath
        >>= maybe getSystemCertificateStore pure

  let betterTLSConfig =
        (defaultParamsClient (cs host) (cs $ show port))
          { TLS.clientSupported = def {TLS.supportedCiphers = blessed_ciphers},
            TLS.clientHooks = def, -- FUTUREWORK: use onCertificateRequest to provide client certificates
            TLS.clientShared = def {TLS.sharedCAStore = customTrustStore}
          }
  let cfg' = cfg {_grpcClientConfigTLS = Just betterTLSConfig}
  eitherClient <- createGrpcClient cfg'
  case eitherClient of
    Left err -> do
      Log.debug $
        Log.msg ("Failed to connect to remote federator" :: ByteString)
          . Log.field "host" host
          . Log.field "port" port
          . Log.field "error" (show err)
      pure $ Left (RemoteErrorClientFailure err target)
    Right client -> pure $ Right client
