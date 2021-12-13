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

import Control.Lens (view)
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
import Polysemy.Error
import Polysemy.Input
import Test.Federator.Util
import Test.Hspec
import Util.Options (Endpoint (Endpoint))
import Wire.API.Federation.Component
import Wire.API.Federation.Domain
import Wire.API.User
import Wire.Network.DNS.SRV

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
        (status, resp) <-
          runTestSem
            . assertNoError @RemoteError
            $ inwardBrigCallViaIngress "get-user-by-handle" $
              (Aeson.fromEncoding (Aeson.toEncoding hdl))
        let actualProfile = Aeson.decode (toLazyByteString resp)
        liftIO $ do
          status `shouldBe` HTTP.status200
          actualProfile `shouldBe` (Just expectedProfile)

  it "should not be accessible without a client certificate" $
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
          $ inwardBrigCallViaIngressWithSettings tlsSettings "get-user-by-handle" $
            (Aeson.fromEncoding (Aeson.toEncoding hdl))
      liftIO $ case r of
        Right _ -> expectationFailure "Expected client certificate error, got response"
        Left (RemoteError _ _) ->
          expectationFailure "Expected client certificate error, got remote error"
        Left (RemoteErrorResponse _ status _) -> status `shouldBe` HTTP.status400

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
  Sem r (HTTP.Status, Builder)
inwardBrigCallViaIngress path payload = do
  tlsSettings <- inputs (view teTLSSettings)
  inwardBrigCallViaIngressWithSettings tlsSettings path payload

inwardBrigCallViaIngressWithSettings ::
  Members [Input TestEnv, Embed IO, Error RemoteError] r =>
  TLSSettings ->
  Text ->
  Builder ->
  Sem r (HTTP.Status, Builder)
inwardBrigCallViaIngressWithSettings tlsSettings requestPath payload =
  do
    Endpoint ingressHost ingressPort <- cfgNginxIngress . view teTstOpts <$> input
    originDomain <- cfgOriginDomain . view teTstOpts <$> input
    let target = SrvTarget (cs ingressHost) ingressPort
        headers = [(originDomainHeaderName, Text.encodeUtf8 originDomain)]
    runInputConst tlsSettings
      . assertNoError @DiscoveryFailure
      . discoverConst target
      . interpretRemote
      $ discoverAndCall (Domain "example.com") Brig requestPath headers payload
