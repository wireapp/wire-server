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

module Test.Federator.InwardSpec where

import Bilge
import Control.Lens (view)
import Data.Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Handle
import Data.LegalHold (UserLegalHoldStatus (UserLegalHoldNoConsent))
import qualified Data.Text as Text
import Federator.Options
import Imports
import Mu.GRpc.Client.TyApps
import Network.GRPC.Client.Helpers
import qualified Network.HTTP.Types as HTTP
import Test.Federator.Util
import Test.Hspec
import Test.Tasty.HUnit (assertFailure)
import Util.Options (Endpoint (Endpoint))
import Wire.API.Federation.GRPC.Client
import Wire.API.Federation.GRPC.Types hiding (body, path)
import qualified Wire.API.Federation.GRPC.Types as GRPC
import Wire.API.User

-- FUTUREWORK(federation): move these tests to brig-integration (benefit: avoid duplicating all of the brig helper code)

-- | Path covered by this test
--
--  +----------+
--  |federator-|          +------+--+
--  |integration  grpc    |federator|
--  |          |--------->+         +
--  |          |          +----+----+
--  +----------+               |
--                             | http
--                             v
--                         +---+--+
--                         | brig |
--                         |      |
--                         +------+
--
--  (ascii diagrams from asciiflow.com)
spec :: TestEnv -> Spec
spec env =
  describe "Inward" $ do
    it "should be able to call brig" $
      runTestFederator env $ do
        brig <- view teBrig <$> ask
        user <- randomUser brig
        hdl <- randomHandle
        _ <- putHandle brig (userId user) hdl

        let expectedProfile = (publicProfile user UserLegalHoldNoConsent) {profileHandle = Just (Handle hdl)}
        bdy <- asInwardBody =<< inwardBrigCall "federation/get-user-by-handle" (encode hdl)
        liftIO $ bdy `shouldBe` expectedProfile

    it "should give an InvalidEndpoint error on a 404 'no-endpoint' response from Brig" $
      runTestFederator env $ do
        err <- asInwardError =<< inwardBrigCall "federation/this-endpoint-does-not-exist" (encode Aeson.emptyObject)
        expectErr IInvalidEndpoint err

    -- Note: most tests for forbidden endpoints are in the unit tests of the sanitizePath function
    -- The integration tests are just another safeguard.
    it "should not accept invalid/disallowed paths" $
      runTestFederator env $ do
        let o = object ["name" .= ("fakeNewUser" :: Text)]
        err <- asInwardErrorUnsafe <$> inwardBrigCall "http://localhost:8080/i/users" (encode o)
        expectErr IForbiddenEndpoint err

    it "should only accept /federation/ paths" $
      runTestFederator env $ do
        let o = object ["name" .= ("fakeNewUser" :: Text)]
        err <- asInwardErrorUnsafe <$> inwardBrigCall "i/users" (encode o)
        expectErr IForbiddenEndpoint err

    it "should only accept /federation/ paths (also when there are /../ segments)" $
      runTestFederator env $ do
        let o = object ["name" .= ("fakeNewUser" :: Text)]
        err <- asInwardErrorUnsafe <$> inwardBrigCall "federation/../i/users" (encode o)
        expectErr IForbiddenEndpoint err

-- Utility functions
--
expectErr :: InwardErrorType -> InwardError -> TestFederator IO ()
expectErr expectedType err =
  unless (inwardErrorType err == expectedType)
    . liftIO
    $ assertFailure $ "expected type '" <> show expectedType <> "' but got " <> show err

inwardBrigCall :: (MonadIO m, MonadHttp m, MonadReader TestEnv m, HasCallStack) => ByteString -> LBS.ByteString -> m (GRpcReply InwardResponse)
inwardBrigCall requestPath payload = do
  c <- viewFederatorExternalClient
  originDomain <- cfgOriginDomain <$> view teTstOpts
  let brigCall =
        GRPC.Request
          { GRPC.component = Brig,
            GRPC.path = requestPath,
            GRPC.body = LBS.toStrict payload,
            GRPC.originDomain = originDomain
          }
  liftIO $ gRpcCall @'MsgProtoBuf @Inward @"Inward" @"call" c brigCall

viewFederatorExternalClient :: (MonadIO m, MonadHttp m, MonadReader TestEnv m, HasCallStack) => m GrpcClient
viewFederatorExternalClient = do
  Endpoint fedHost fedPort <- cfgFederatorExternal <$> view teTstOpts
  exampleCert <-
    (clientCertificate . optSettings <$> view teOpts) >>= \case
      Nothing -> liftIO $ assertFailure "No client certificate configured"
      Just certPath -> liftIO $ BS.readFile certPath
  let cfg =
        (grpcClientConfigSimple (Text.unpack fedHost) (fromIntegral fedPort) False)
          { _grpcClientConfigHeaders = [("X-SSL-Certificate", HTTP.urlEncode True exampleCert)]
          }
  client <- createGrpcClient cfg
  case client of
    Left clientErr -> liftIO $ assertFailure (show clientErr)
    Right cli -> pure cli

viewIngress :: (MonadReader TestEnv m, HasCallStack) => m Endpoint
viewIngress = cfgNginxIngress . view teTstOpts <$> ask
