-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
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

import Control.Lens (view)
import Control.Monad.Catch (throwM)
import Control.Monad.Codensity
import Data.Aeson qualified as Aeson
import Data.Binary.Builder
import Data.Domain
import Data.Id
import Data.LegalHold (UserLegalHoldStatus (UserLegalHoldNoConsent))
import Data.String.Conversions
import Data.Text.Encoding qualified as Text
import Federator.Discovery
import Federator.Monitor (FederationSetupError)
import Federator.Monitor.Internal (mkSSLContextWithoutCert)
import Federator.Remote
import HTTP2.Client.Manager (http2ManagerWithSSLCtx)
import Imports
import Network.HTTP.Types qualified as HTTP
import OpenSSL.Session (SSLContext)
import Polysemy
import Polysemy.Embed
import Polysemy.Error
import Polysemy.Input
import Servant.Client.Core
import Test.Federator.Util
import Test.Hspec
import Util.Options (Endpoint (Endpoint))
import Wire.API.Federation.Client
import Wire.API.Federation.Component
import Wire.API.Federation.Domain
import Wire.API.User
import Wire.Network.DNS.SRV

-- | This module contains tests for the interface between federator and ingress.  Ingress is
-- mocked with nginz.
spec :: TestEnv -> Spec
spec env = do
  describe "Ingress" $ do
    it "should be accessible using http2 and forward to the local brig" $
      runTestFederator env $ do
        brig <- view teBrig <$> ask
        user <- randomUser brig

        let expectedProfile = mkUserProfile EmailVisibleToSelf user UserLegalHoldNoConsent
        runTestSem $ do
          resp <-
            liftToCodensity
              . assertNoError @RemoteError
              $ inwardBrigCallViaIngress
                "get-users-by-ids"
                (Aeson.fromEncoding (Aeson.toEncoding [userId user]))
          embed . lift @Codensity $ do
            bdy <- streamingResponseStrictBody resp
            let actualProfile = Aeson.decode (toLazyByteString bdy)
            responseStatusCode resp `shouldBe` HTTP.status200
            actualProfile `shouldBe` Just [expectedProfile]

  it "testRejectRequestsWithoutClientCertIngress" (testRejectRequestsWithoutClientCertIngress env)

-- @SF.Federation @TSFI.RESTfulAPI @S2 @S3 @S7
--
-- This test was primarily intended to test that federator is using the API right (header
-- name etc.), but it is also effectively testing that federator rejects clients without
-- certificates that have been validated by ingress.
--
-- We can't test end-to-end here: the TLS termination happens in k8s, and would have to be
-- tested there (and with a good emulation of the concrete configuration of the prod
-- system).
testRejectRequestsWithoutClientCertIngress :: TestEnv -> IO ()
testRejectRequestsWithoutClientCertIngress env = runTestFederator env $ do
  brig <- view teBrig <$> ask
  user <- randomUser brig
  hdl <- randomHandle
  _ <- putHandle brig (userId user) hdl

  settings <- view teSettings
  sslCtxWithoutCert <-
    either (throwM @_ @FederationSetupError) pure
      <=< runM
      . runEmbedded (liftIO @(TestFederator IO))
      . runError
      $ mkSSLContextWithoutCert settings
  runTestSem $ do
    r <-
      runError @RemoteError $
        inwardBrigCallViaIngressWithSettings
          sslCtxWithoutCert
          "get-user-by-handle"
          (Aeson.fromEncoding (Aeson.toEncoding hdl))
    liftToCodensity . embed $ case r of
      Right _ -> expectationFailure "Expected client certificate error, got response"
      Left (RemoteError {}) ->
        expectationFailure "Expected client certificate error, got remote error"
      Left (RemoteErrorResponse _ _ status _) -> status `shouldBe` HTTP.status400

-- @END

liftToCodensity :: (Member (Embed (Codensity IO)) r) => Sem (Embed IO ': r) a -> Sem r a
liftToCodensity = runEmbedded @IO @(Codensity IO) lift

runTestSem :: Sem '[Input TestEnv, Embed (Codensity IO)] a -> TestFederator IO a
runTestSem action = do
  e <- ask
  liftIO . lowerCodensity . runM . runInputConst e $ action

discoverConst :: SrvTarget -> Sem (DiscoverFederator ': r) a -> Sem r a
discoverConst target = interpret $ \case
  DiscoverFederator _ -> pure (Right target)
  DiscoverAllFederators _ -> pure (Right (pure target))

inwardBrigCallViaIngress ::
  (Members [Input TestEnv, Embed (Codensity IO), Error RemoteError] r) =>
  Text ->
  Builder ->
  Sem r StreamingResponse
inwardBrigCallViaIngress path payload = do
  sslCtx <- inputs (view teSSLContext)
  inwardBrigCallViaIngressWithSettings sslCtx path payload

inwardBrigCallViaIngressWithSettings ::
  (Members [Input TestEnv, Embed (Codensity IO), Error RemoteError] r) =>
  SSLContext ->
  Text ->
  Builder ->
  Sem r StreamingResponse
inwardBrigCallViaIngressWithSettings sslCtx requestPath payload =
  do
    Endpoint ingressHost ingressPort <- nginxIngress . view teTstOpts <$> input
    originDomain <- originDomain . view teTstOpts <$> input
    let target = SrvTarget (cs ingressHost) ingressPort
        headers = [(originDomainHeaderName, Text.encodeUtf8 originDomain)]
    mgr <- liftToCodensity . liftIO $ http2ManagerWithSSLCtx sslCtx
    liftToCodensity
      . runInputConst mgr
      . runInputConst (RequestId "N/A")
      . assertNoError @DiscoveryFailure
      . discoverConst target
      . interpretRemote
      $ discoverAndCall (Domain "example.com") Brig requestPath headers payload
