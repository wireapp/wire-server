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

module Federator.Remote
  ( Remote,
    RemoteError (..),
    discoverAndCall,
    interpretRemote,
    mkGrpcClient,
    blessedCiphers,
  )
where

import Control.Lens ((^.))
import Data.Default (def)
import Data.Domain (Domain, domainText)
import Data.String.Conversions (cs)
import qualified Data.X509 as X509
import qualified Data.X509.Validation as X509
import Federator.Discovery
import Federator.Env (TLSSettings, caStore, creds)
import Federator.Options
import Federator.Validation
import Imports
import Mu.GRpc.Client.Optics (GRpcReply)
import Mu.GRpc.Client.Record (GRpcMessageProtocol (MsgProtoBuf))
import Mu.GRpc.Client.TyApps (gRpcCall)
import Network.GRPC.Client.Helpers
import Network.TLS as TLS
import qualified Network.TLS.Extra.Cipher as TLS
import Polysemy
import qualified Polysemy.Error as Polysemy
import qualified Polysemy.Reader as Polysemy
import Polysemy.TinyLog (TinyLog)
import qualified Polysemy.TinyLog as Log
import qualified System.Logger.Message as Log
import Wire.API.Federation.GRPC.Client
import Wire.API.Federation.GRPC.Types
import Wire.Network.DNS.SRV (SrvTarget (SrvTarget))

data RemoteError
  = RemoteErrorDiscoveryFailure Domain LookupError
  | RemoteErrorClientFailure SrvTarget GrpcClientErr
  | RemoteErrorTLSException SrvTarget TLSException
  deriving (Show, Eq)

data Remote m a where
  DiscoverAndCall :: ValidatedFederatedRequest -> Remote m (Either RemoteError (GRpcReply InwardResponse))

makeSem ''Remote

interpretRemote ::
  Members
    '[ Embed IO,
       DiscoverFederator,
       TinyLog,
       Polysemy.Reader RunSettings,
       Polysemy.Reader TLSSettings
     ]
    r =>
  Sem (Remote ': r) a ->
  Sem r a
interpretRemote = interpret $ \case
  DiscoverAndCall ValidatedFederatedRequest {..} -> Polysemy.runError . logRemoteErrors $ do
    target <-
      Polysemy.mapError (RemoteErrorDiscoveryFailure vDomain) $
        discoverFederatorWithError vDomain
    client <- mkGrpcClient target
    callInward client vRequest

callInward :: MonadIO m => GrpcClient -> Request -> m (GRpcReply InwardResponse)
callInward client request =
  liftIO $ gRpcCall @'MsgProtoBuf @Inward @"Inward" @"call" client request

-- FUTUREWORK: get review on blessed ciphers
blessedCiphers :: [Cipher]
blessedCiphers =
  [ TLS.cipher_TLS13_AES128CCM8_SHA256,
    TLS.cipher_TLS13_AES128CCM_SHA256,
    TLS.cipher_TLS13_AES128GCM_SHA256,
    TLS.cipher_TLS13_AES256GCM_SHA384,
    TLS.cipher_TLS13_CHACHA20POLY1305_SHA256,
    -- For TLS 1.2 (copied from default nginx ingress config):
    TLS.cipher_ECDHE_ECDSA_AES256GCM_SHA384,
    TLS.cipher_ECDHE_RSA_AES256GCM_SHA384,
    TLS.cipher_ECDHE_RSA_AES128GCM_SHA256,
    TLS.cipher_ECDHE_ECDSA_AES128GCM_SHA256,
    TLS.cipher_ECDHE_ECDSA_CHACHA20POLY1305_SHA256,
    TLS.cipher_ECDHE_RSA_CHACHA20POLY1305_SHA256
  ]

-- FUTUREWORK(federation): Consider using HsOpenSSL instead of tls for better
-- security and to avoid having to depend on cryptonite and override validation
-- hooks. This might involve forking http2-client: https://github.com/lucasdicioccio/http2-client/issues/76
-- FUTUREWORK(federation): Use openssl
--   See also https://github.com/lucasdicioccio/http2-client/issues/76
-- FUTUREWORK(federation): Cache this client and use it for many requests
mkGrpcClient ::
  Members
    '[ Embed IO,
       Polysemy.Error RemoteError,
       Polysemy.Reader TLSSettings
     ]
    r =>
  SrvTarget ->
  Sem r GrpcClient
mkGrpcClient target@(SrvTarget host port) = do
  -- grpcClientConfigSimple using TLS is INSECURE and IGNORES any certificates
  -- See https://github.com/haskell-grpc-native/http2-grpc-haskell/issues/47
  --
  -- FUTUREWORK: load client certificate and client key from disk
  -- and use it when making a request
  let cfg = grpcClientConfigSimple (cs host) (fromInteger $ toInteger port) True

  settings <- Polysemy.ask

  let tlsConfig =
        (defaultParamsClient (cs host) (cs $ show port))
          { TLS.clientSupported =
              def
                { TLS.supportedCiphers = blessedCiphers,
                  -- FUTUREWORK: Figure out if we can drop TLS 1.2
                  TLS.supportedVersions = [TLS.TLS12, TLS.TLS13]
                },
            TLS.clientHooks =
              def
                { TLS.onServerCertificate =
                    X509.validate
                      X509.HashSHA256
                      (X509.defaultHooks {X509.hookValidateName = validateDomainName})
                      X509.defaultChecks,
                  TLS.onCertificateRequest = \_ -> pure (Just (settings ^. creds))
                },
            TLS.clientShared = def {TLS.sharedCAStore = settings ^. caStore}
          }
  let cfg' = cfg {_grpcClientConfigTLS = Just tlsConfig}
  Polysemy.mapError (RemoteErrorClientFailure target)
    . Polysemy.fromEither
    =<< Polysemy.fromExceptionVia (RemoteErrorTLSException target) (createGrpcClient cfg')

logRemoteErrors ::
  Members '[Polysemy.Error RemoteError, TinyLog] r =>
  Sem r x ->
  Sem r x
logRemoteErrors action = Polysemy.catch action $ \err -> do
  Log.debug $
    Log.msg ("Failed to connect to remote federator" :: ByteString)
      . addFields err
  Polysemy.throw err
  where
    addFields (RemoteErrorDiscoveryFailure domain err) =
      Log.field "domain" (domainText domain)
        . Log.field "error" (show err)
    addFields (RemoteErrorClientFailure (SrvTarget host port) err) =
      Log.field "host" host
        . Log.field "port" port
        . Log.field "error" (show err)
    addFields (RemoteErrorTLSException (SrvTarget host port) err) =
      Log.field "host" host
        . Log.field "port" port
        . Log.field "error" (show err)
