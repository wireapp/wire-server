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
import Polysemy
import qualified Polysemy.Error as Polysemy
import Polysemy.IO (embedToMonadIO)
import qualified Polysemy.Input as Polysemy
import qualified Polysemy.Reader as Polysemy
import Polysemy.TinyLog (TinyLog)
import qualified Polysemy.TinyLog as Log
import Wire.API.Federation.GRPC.Types
import Wire.Network.DNS.Effect (DNSLookup)
import qualified Wire.Network.DNS.Effect as Lookup

callOutward :: Members '[Remote, Polysemy.Reader RunSettings] r => FederatedRequest -> Sem r OutwardResponse
callOutward req = do
  case validateFederatedRequest req of
    Success vReq -> do
      allowedRemote <- federateWith (vDomain vReq)
      if allowedRemote
        then mkRemoteResponse <$> discoverAndCall vReq
        else pure $ mkOutwardErr FederationDeniedLocally "federation-not-allowed" ("federating with domain [" <> domainText (vDomain vReq) <> "] is not allowed (see federator configuration)")
    Failure errs ->
      pure $ mkOutwardErr InvalidRequest "invalid-request-to-federator" ("validation failed with: " <> Text.pack (show errs))

-- FUTUREWORK(federation): Make these errors less stringly typed
mkRemoteResponse :: Either RemoteError (GRpcReply InwardResponse) -> OutwardResponse
mkRemoteResponse reply =
  case reply of
    Right (GRpcOk (InwardResponseBody res)) ->
      OutwardResponseBody res
    Right (GRpcOk (InwardResponseError err)) -> OutwardResponseInwardError err
    Right (GRpcTooMuchConcurrency _) ->
      mkOutwardErr RemoteFederatorError "too-much-concurrency" "Too much concurrency"
    Right (GRpcErrorCode grpcErr) ->
      mkOutwardErr RemoteFederatorError "grpc-error-code" ("code=" <> Text.pack (show grpcErr))
    Right (GRpcErrorString grpcErr) ->
      mkOutwardErr RemoteFederatorError "grpc-error-string" ("error=" <> Text.pack grpcErr)
    Right (GRpcClientError clientErr) ->
      mkOutwardErr RemoteFederatorError "grpc-client-error" ("error=" <> Text.pack (show clientErr))
    Left (RemoteErrorDiscoveryFailure domain err) ->
      case err of
        LookupErrorSrvNotAvailable _srvDomain ->
          mkOutwardErr RemoteNotFound "srv-record-not-found" ("domain=" <> domainText domain)
        LookupErrorDNSError dnsErr ->
          mkOutwardErr DiscoveryFailed "srv-lookup-dns-error" ("domain=" <> domainText domain <> "; error=" <> Text.decodeUtf8 dnsErr)
    Left (RemoteErrorClientFailure srvTarget cltErr) ->
      mkOutwardErr RemoteFederatorError "cannot-connect-to-remote-federator" ("target=" <> Text.pack (show srvTarget) <> "; error=" <> Text.pack (show cltErr))
    Left (RemoteErrorTLSException srvTarget exc) ->
      mkOutwardErr TLSFailure "tls-failure" ("Failed to establish TLS session with remote:  target=" <> Text.pack (show srvTarget) <> "; exception=" <> Text.pack (show exc))

mkOutwardErr :: OutwardErrorType -> Text -> Text -> OutwardResponse
mkOutwardErr typ label msg = OutwardResponseError $ OutwardError typ (Just $ ErrorPayload label msg)

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
