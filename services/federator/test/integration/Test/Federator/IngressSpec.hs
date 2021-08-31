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

module Test.Federator.IngressSpec where

import Bilge
import Control.Lens (view, (^.))
import Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Default (def)
import Data.Handle
import Data.LegalHold (UserLegalHoldStatus (UserLegalHoldNoConsent))
import Data.String.Conversions (cs)
import qualified Data.X509 as X509
import qualified Data.X509.Validation as X509
import Federator.Env (caStore)
import Federator.Options
import Federator.Remote (RemoteError, blessedCiphers, mkGrpcClient)
import Imports
import Mu.GRpc.Client.TyApps
import Network.GRPC.Client.Helpers (_grpcClientConfigTLS)
import qualified Network.TLS as TLS
import qualified Polysemy
import qualified Polysemy.Error as Polysemy
import qualified Polysemy.Reader as Polysemy
import Polysemy.TinyLog (discardLogs)
import Test.Federator.Util
import Test.Hspec
import Test.Tasty.HUnit (assertFailure)
import Util.Options (Endpoint (Endpoint))
import Wire.API.Federation.GRPC.Client (createGrpcClient)
import Wire.API.Federation.GRPC.Types hiding (body, path)
import qualified Wire.API.Federation.GRPC.Types as GRPC
import Wire.API.User
import Wire.Network.DNS.SRV

spec :: TestEnv -> Spec
spec env =
  describe "Ingress" $ do
    it "should be accessible using grpc client and forward to the local brig" $
      runTestFederator env $ do
        brig <- view teBrig <$> ask
        user <- randomUser brig
        hdl <- randomHandle
        _ <- putHandle brig (userId user) hdl

        let expectedProfile = (publicProfile user UserLegalHoldNoConsent) {profileHandle = Just (Handle hdl)}
        bdy <- asInwardBody =<< inwardBrigCallViaIngress "federation/get-user-by-handle" (encode hdl)
        liftIO $ bdy `shouldBe` expectedProfile

    it "should not be accessible without a client certificate" $
      runTestFederator env $ do
        brig <- view teBrig <$> ask
        user <- randomUser brig
        hdl <- randomHandle
        _ <- putHandle brig (userId user) hdl

        -- Create a client which has the right CA but not client certs
        Endpoint ingressHost ingressPort <- cfgNginxIngress . view teTstOpts <$> ask
        tlsSettings <- view teTLSSettings
        let cfg = grpcClientConfigSimple (cs ingressHost) (fromInteger $ toInteger ingressPort) True
            tlsConfig =
              (TLS.defaultParamsClient (cs ingressHost) (cs $ show ingressPort))
                { TLS.clientSupported =
                    def
                      { TLS.supportedCiphers = blessedCiphers,
                        -- FUTUREWORK: Figure out if we can drop TLS 1.2
                        TLS.supportedVersions = [TLS.TLS12, TLS.TLS13]
                      },
                  TLS.clientShared = def {TLS.sharedCAStore = tlsSettings ^. caStore},
                  TLS.clientHooks =
                    def
                      { TLS.onServerCertificate =
                          X509.validate X509.HashSHA256 X509.defaultHooks X509.defaultChecks
                      }
                }
        let cfg' = cfg {_grpcClientConfigTLS = Just tlsConfig}
        Right client <- createGrpcClient cfg'
        grpcReply <- inwardBrigCallViaIngressWithClient client "federation/get-user-by-handle" (encode hdl)
        liftIO $ case grpcReply of
          -- FUTUREWORK: Make it more obvious from nginx that this error is due
          -- to mTLS failure.
          GRpcErrorString err -> err `shouldBe` "GRPC status indicates failure: status-code=INTERNAL, status-message=\"HTTP Status 400\""
          _ -> assertFailure $ "Expect HTTP 400, got: " <> show grpcReply

inwardBrigCallViaIngress :: (MonadIO m, MonadHttp m, MonadReader TestEnv m, HasCallStack) => ByteString -> LBS.ByteString -> m (GRpcReply InwardResponse)
inwardBrigCallViaIngress requestPath payload = do
  Endpoint ingressHost ingressPort <- cfgNginxIngress . view teTstOpts <$> ask
  let target = SrvTarget (cs ingressHost) ingressPort
  runSettings <- optSettings . view teOpts <$> ask
  tlsSettings <- view teTLSSettings >>= newIORef
  c <-
    liftIO
      . Polysemy.runM
      . Polysemy.runError @RemoteError
      . discardLogs
      . Polysemy.runReader tlsSettings
      . Polysemy.runReader runSettings
      $ mkGrpcClient target
  client <- case c of
    Left clientErr -> liftIO $ assertFailure (show clientErr)
    Right cli -> pure cli
  inwardBrigCallViaIngressWithClient client requestPath payload

inwardBrigCallViaIngressWithClient :: (MonadIO m, MonadHttp m, MonadReader TestEnv m, HasCallStack) => GrpcClient -> ByteString -> LBS.ByteString -> m (GRpcReply InwardResponse)
inwardBrigCallViaIngressWithClient client requestPath payload = do
  originDomain <- cfgOriginDomain <$> view teTstOpts
  let brigCall =
        GRPC.Request
          { GRPC.component = Brig,
            GRPC.path = requestPath,
            GRPC.body = LBS.toStrict payload,
            GRPC.originDomain = originDomain
          }
  liftIO $ gRpcCall @'MsgProtoBuf @Inward @"Inward" @"call" client brigCall
