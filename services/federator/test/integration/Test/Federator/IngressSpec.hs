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
import Control.Monad.Codensity
import qualified Data.Aeson as Aeson
import Data.Binary.Builder
import Data.Domain
import Data.Handle
import Data.LegalHold (UserLegalHoldStatus (UserLegalHoldNoConsent))
import Data.String.Conversions (cs)
import qualified Data.Text.Encoding as Text
import qualified Data.X509 as X509
import Federator.Discovery
import Federator.Env
import Federator.Remote
import Imports
import qualified Network.HTTP.Types as HTTP
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
        hdl <- randomHandle
        _ <- putHandle brig (userId user) hdl

        let expectedProfile = (publicProfile user UserLegalHoldNoConsent) {profileHandle = Just (Handle hdl)}
        resp <-
          runTestSem
            . assertNoError @RemoteError
            $ inwardBrigCallViaIngress
              "get-user-by-handle"
              (Aeson.fromEncoding (Aeson.toEncoding hdl))
        liftIO $ do
          bdy <- streamingResponseStrictBody resp
          let actualProfile = Aeson.decode (toLazyByteString bdy)
          responseStatusCode resp `shouldBe` HTTP.status200
          actualProfile `shouldBe` Just expectedProfile

  -- @SF.Federation @TSFI.RESTfulAPI @S2 @S3 @S7
  --
  -- This test was primarily intended to test that federator is using the API right (header
  -- name etc.), but it is also effectively testing that federator rejects clients without
  -- certificates that have been validated by ingress.
  --
  -- We can't test end-to-end here: the TLS termination happens in k8s, and would have to be
  -- tested there (and with a good emulation of the concrete configuration of the prod
  -- system).
  it "rejectRequestsWithoutClientCertIngress" $
    runTestFederator env $ do
      brig <- view teBrig <$> ask
      user <- randomUser brig
      hdl <- randomHandle
      _ <- putHandle brig (userId user) hdl

      -- Remove client certificate from settings
      tlsSettings0 <- view teTLSSettings
      let tlsSettings =
            tlsSettings0
              { _creds = case _creds tlsSettings0 of
                  (_, privkey) -> (X509.CertificateChain [], privkey)
              }
      r <-
        runTestSem
          . runError @RemoteError
          $ inwardBrigCallViaIngressWithSettings
            tlsSettings
            "get-user-by-handle"
            (Aeson.fromEncoding (Aeson.toEncoding hdl))
      liftIO $ case r of
        Right _ -> expectationFailure "Expected client certificate error, got response"
        Left (RemoteError _ _) ->
          expectationFailure "Expected client certificate error, got remote error"
        Left (RemoteErrorResponse _ status _) -> status `shouldBe` HTTP.status400

-- FUTUREWORK: ORMOLU_DISABLE
-- @END
-- ORMOLU_ENABLE

runTestSem :: Sem '[Input TestEnv, Embed IO] a -> TestFederator IO a
runTestSem action = do
  e <- ask
  liftIO . runM . runInputConst e $ action

discoverConst :: SrvTarget -> Sem (DiscoverFederator ': r) a -> Sem r a
discoverConst target = interpret $ \case
  DiscoverFederator _ -> pure (Right target)
  DiscoverAllFederators _ -> pure (Right (pure target))

inwardBrigCallViaIngress ::
  Members [Input TestEnv, Embed IO, Error RemoteError] r =>
  Text ->
  Builder ->
  Sem r StreamingResponse
inwardBrigCallViaIngress path payload = do
  tlsSettings <- inputs (view teTLSSettings)
  inwardBrigCallViaIngressWithSettings tlsSettings path payload

inwardBrigCallViaIngressWithSettings ::
  Members [Input TestEnv, Embed IO, Error RemoteError] r =>
  TLSSettings ->
  Text ->
  Builder ->
  Sem r StreamingResponse
inwardBrigCallViaIngressWithSettings tlsSettings requestPath payload =
  do
    Endpoint ingressHost ingressPort <- cfgNginxIngress . view teTstOpts <$> input
    originDomain <- cfgOriginDomain . view teTstOpts <$> input
    let target = SrvTarget (cs ingressHost) ingressPort
        headers = [(originDomainHeaderName, Text.encodeUtf8 originDomain)]
    runInputConst tlsSettings
      . assertNoError @DiscoveryFailure
      . discoverConst target
      . runEmbedded @(Codensity IO) @IO lowerCodensity
      . interpretRemote
      $ discoverAndCall (Domain "example.com") Brig requestPath headers payload
