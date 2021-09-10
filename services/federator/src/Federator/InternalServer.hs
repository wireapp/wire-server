{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures -Wno-unused-imports #-}

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

module Federator.InternalServer where

import Control.Lens (view)
import Data.Domain (domainText)
import Data.Either.Validation (Validation (..))
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.X509.CertificateStore
import Federator.App (Federator, runAppT)
import Federator.Discovery (DiscoverFederator, LookupError (LookupErrorDNSError, LookupErrorSrvNotAvailable), runFederatorDiscovery)
import Federator.Env (Env, TLSSettings, applog, caStore, dnsResolver, runSettings, tls)
import Federator.Options (RunSettings)
import Federator.Remote (Remote, RemoteError (..), discoverAndCall, interpretRemote)
import Federator.Utils.PolysemyServerError (absorbServerError)
import Federator.Validation
import Imports
import Mu.GRpc.Client.Record (GRpcReply (..))
import Mu.GRpc.Server (msgProtoBuf, runGRpcAppTrans)
import Mu.Server (ServerError, ServerErrorIO, SingleServerT, singleService)
import qualified Mu.Server as Mu
import Network.HTTP2.Client.Exceptions (ClientError (..))
import Network.TLS
import Polysemy
import qualified Polysemy.Error as Polysemy
import Polysemy.IO (embedToMonadIO)
import qualified Polysemy.Input as Polysemy
import qualified Polysemy.Reader as Polysemy
import Polysemy.TinyLog (TinyLog)
import qualified Polysemy.TinyLog as Log
import Wire.API.Federation.GRPC.Client (GrpcClientErr (..))
import Wire.API.Federation.GRPC.Types
import Wire.Network.DNS.Effect (DNSLookup)
import qualified Wire.Network.DNS.Effect as Lookup
import Wire.Network.DNS.SRV (SrvTarget (..))

callOutward :: Members '[Remote, Polysemy.Reader RunSettings] r => FederatedRequest -> Sem r OutwardResponse
callOutward req = do
  case validateFederatedRequest req of
    Success vReq -> do
      allowedRemote <- federateWith (vDomain vReq)
      if allowedRemote
        then mkRemoteResponse <$> discoverAndCall vReq
        else pure $ mkOutwardErr FederationDeniedLocally ("federating with domain [" <> domainText (vDomain vReq) <> "] is not allowed (see federator configuration)")
    Failure errs ->
      pure $ mkOutwardErr InvalidRequest ("validation failed with: " <> Text.pack (show errs))

-- FUTUREWORK(federation): Make these errors less stringly typed
mkRemoteResponse :: Either RemoteError (GRpcReply InwardResponse) -> OutwardResponse
mkRemoteResponse reply =
  case reply of
    Right (GRpcOk (InwardResponseBody res)) ->
      OutwardResponseBody res
    Right (GRpcOk (InwardResponseError err)) -> OutwardResponseInwardError err
    Right (GRpcTooMuchConcurrency _) ->
      mkOutwardErr TooMuchConcurrency "Too much concurrency"
    Right (GRpcErrorCode code) ->
      mkOutwardErr GrpcError ("code: " <> Text.pack (show code))
    Right (GRpcErrorString msg) ->
      mkOutwardErr GrpcError (Text.pack msg)
    Right (GRpcClientError EarlyEndOfStream) -> mkOutwardErr GrpcError "Early end of stream"
    Left (RemoteErrorDiscoveryFailure domain err) ->
      case err of
        LookupErrorSrvNotAvailable _srvDomain ->
          mkOutwardErr RemoteNotFound ("domain=" <> domainText domain)
        LookupErrorDNSError dnsErr ->
          mkOutwardErr DiscoveryFailed ("domain=" <> domainText domain <> " error=" <> Text.decodeUtf8 dnsErr)
    Left (RemoteErrorClientFailure (SrvTarget host port) (GrpcClientErr reason)) ->
      mkOutwardErr
        GrpcError
        (reason <> " target=" <> Text.decodeUtf8 host <> ":" <> Text.pack (show port))
    Left (RemoteErrorTLSException (SrvTarget host port) exc) ->
      mkOutwardErr
        TLSFailure
        ( "Failed to establish TLS session with remote (target="
            <> Text.decodeUtf8 host
            <> ":"
            <> Text.pack (show port)
            <> "): "
            <> showTLSException exc
        )

showTLSException :: TLSException -> Text
showTLSException (Terminated _ reason err) = Text.pack reason <> ": " <> showTLSError err
showTLSException (HandshakeFailed err) = Text.pack "handshake failed: " <> showTLSError err
showTLSException ConnectionNotEstablished = Text.pack "connection not established"

showTLSError :: TLSError -> Text
showTLSError (Error_Misc msg) = Text.pack msg
showTLSError (Error_Protocol (msg, _, _)) = "protocol error: " <> Text.pack msg
showTLSError (Error_Certificate msg) = "certificate error: " <> Text.pack msg
showTLSError (Error_HandshakePolicy msg) = "handshake policy error: " <> Text.pack msg
showTLSError Error_EOF = "end-of-file error"
showTLSError (Error_Packet msg) = "packet error: " <> Text.pack msg
showTLSError (Error_Packet_unexpected actual expected) =
  "unexpected packet: " <> Text.pack expected <> ", " <> "got " <> Text.pack actual
showTLSError (Error_Packet_Parsing msg) = "packet parsing error: " <> Text.pack msg

mkOutwardErr :: OutwardErrorType -> Text -> OutwardResponse
mkOutwardErr typ msg = OutwardResponseError $ OutwardError typ msg

outward :: (Members '[Remote, Polysemy.Error ServerError, Polysemy.Reader RunSettings] r) => SingleServerT info Outward (Sem r) _
outward = singleService (Mu.method @"call" callOutward)

serveOutward :: Env -> Int -> IO ()
serveOutward env port = do
  runGRpcAppTrans msgProtoBuf port transformer outward
  where
    transformer ::
      Sem
        '[ Remote,
           DiscoverFederator,
           TinyLog,
           DNSLookup,
           Polysemy.Error ServerError,
           Polysemy.Reader RunSettings,
           Polysemy.Input TLSSettings,
           Embed IO,
           Embed Federator
         ]
        a ->
      ServerErrorIO a
    transformer action =
      runAppT env
        . runM -- Embed Federator
        . embedToMonadIO @Federator -- Embed IO
        . Polysemy.runInputSem (embed @IO (readIORef (view tls env))) -- Input TLSSettings
        . Polysemy.runReader (view runSettings env) -- Reader RunSettings
        . absorbServerError
        . Lookup.runDNSLookupWithResolver (view dnsResolver env)
        . Log.runTinyLog (view applog env)
        . runFederatorDiscovery
        . interpretRemote
        $ action
