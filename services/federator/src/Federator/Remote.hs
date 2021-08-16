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
  )
where

import Control.Lens ((^.))
import Data.Default (def)
import Data.Domain (Domain, domainText)
import Data.String.Conversions (cs)
import qualified Data.X509 as X509
import qualified Data.X509.Validation as X509
import Federator.Discovery (DiscoverFederator, LookupError, discoverFederator)
import Federator.Env (TLSSettings, caStore, creds)
import Federator.Options
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
  (Members [Embed IO, DiscoverFederator, TinyLog, Polysemy.Reader RunSettings, Polysemy.Reader TLSSettings] r) =>
  Sem (Remote ': r) a ->
  Sem r a
interpretRemote = interpret $ \case
  DiscoverAndCall ValidatedFederatedRequest {..} -> do
    eitherTarget <- discoverFederator vDomain
    case eitherTarget of
      Left err -> do
        Log.debug $
          Log.msg ("Failed to find remote federator" :: ByteString)
            . Log.field "domain" (domainText vDomain)
            . Log.field "error" (show err)
        pure $ Left (RemoteErrorDiscoveryFailure vDomain err)
      Right target -> do
        eitherClient <- mkGrpcClient target
        case eitherClient of
          Right client ->
            Right <$> callInward client vRequest
          Left err -> pure $ Left err

callInward :: MonadIO m => GrpcClient -> Request -> m (GRpcReply InwardResponse)
callInward client request =
  liftIO $ gRpcCall @'MsgProtoBuf @Inward @"Inward" @"call" client request

-- FUTUREWORK(federation): Consider using HsOpenSSL instead of tls for better
-- security and to avoid having to depend on cryptonite and override validation
-- hooks. This might involve forking http2-client: https://github.com/lucasdicioccio/http2-client/issues/76
-- FUTUREWORK(federation): Use openssl
--   See also https://github.com/lucasdicioccio/http2-client/issues/76
-- FUTUREWORK(federation): Cache this client and use it for many requests
mkGrpcClient ::
  Members '[Embed IO, TinyLog, Polysemy.Reader TLSSettings] r =>
  SrvTarget ->
  Sem r (Either RemoteError GrpcClient)
mkGrpcClient target@(SrvTarget host port) = logAndReturn target $ do
  -- grpcClientConfigSimple using TLS is INSECURE and IGNORES any certificates
  -- See https://github.com/haskell-grpc-native/http2-grpc-haskell/issues/47
  --
  -- FUTUREWORK: load client certificate and client key from disk
  -- and use it when making a request
  let cfg = grpcClientConfigSimple (cs host) (fromInteger $ toInteger port) True

  -- FUTUREWORK: get review on blessed ciphers
  let blessed_ciphers =
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

  settings <- Polysemy.ask

  -- validate the hostname without a trailing dot as the certificate is not
  -- expected to have the trailing dot.
  let stripDot hostname
        | "." `isSuffixOf` hostname = take (length hostname - 1) hostname
        | otherwise = hostname
  let validateName hostname cert =
        TLS.hookValidateName X509.defaultHooks (stripDot hostname) cert

  let tlsConfig =
        (defaultParamsClient (cs host) (cs $ show port))
          { TLS.clientSupported =
              def
                { TLS.supportedCiphers = blessed_ciphers,
                  -- FUTUREWORK: Figure out if we can drop TLS 1.2
                  TLS.supportedVersions = [TLS.TLS12, TLS.TLS13]
                },
            TLS.clientHooks =
              def
                { TLS.onServerCertificate =
                    X509.validate
                      X509.HashSHA256
                      (X509.defaultHooks {TLS.hookValidateName = validateName})
                      X509.defaultChecks,
                  TLS.onCertificateRequest = \_ -> pure (Just (settings ^. creds))
                },
            TLS.clientShared = def {TLS.sharedCAStore = settings ^. caStore}
          }
  let cfg' = cfg {_grpcClientConfigTLS = Just tlsConfig}
  Polysemy.mapError (RemoteErrorClientFailure target)
    . Polysemy.fromEither
    =<< Polysemy.fromExceptionVia (RemoteErrorTLSException target) (createGrpcClient cfg')

logAndReturn :: Members '[TinyLog] r => SrvTarget -> Sem (Polysemy.Error RemoteError ': r) a -> Sem r (Either RemoteError a)
logAndReturn (SrvTarget host port) action = do
  eitherClient <- Polysemy.runError action
  case eitherClient of
    Left err -> do
      Log.debug $
        Log.msg ("Failed to connect to remote federator" :: ByteString)
          . Log.field "host" host
          . Log.field "port" port
          . Log.field "error" (show err)
      pure ()
    _ -> pure ()
  pure eitherClient
