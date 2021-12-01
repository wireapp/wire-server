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
import Bilge.Assert
import Control.Lens (view)
import Data.Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString as BS
import Data.ByteString.Conversion (toByteString')
import qualified Data.ByteString.Lazy as LBS
import Data.Handle
import Data.LegalHold (UserLegalHoldStatus (UserLegalHoldNoConsent))
import Data.Text.Encoding
import Federator.Options
import Imports
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai.Utilities.Error as E
import Test.Federator.Util
import Test.Hspec
import Util.Options (Endpoint (Endpoint))
import Wire.API.Federation.Domain
import Wire.API.User

-- FUTUREWORK(federation): move these tests to brig-integration (benefit: avoid duplicating all of the brig helper code)

-- | Path covered by this test
--
--  +----------+
--  |federator-|          +------+--+
--  |integration  http    |federator|
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
        bdy <-
          responseJsonError =<< inwardCall "/federation/brig/get-user-by-handle" (encode hdl)
            <!! const 200 === statusCode
        liftIO $ bdy `shouldBe` expectedProfile

    it "should return 404 'no-endpoint' response from Brig" $
      runTestFederator env $ do
        err <-
          responseJsonError =<< inwardCall "/federation/brig/this-endpoint-does-not-exist" (encode Aeson.emptyObject)
            <!! const 404 === statusCode
        liftIO $ E.label err `shouldBe` "no-endpoint"

    -- Note: most tests for forbidden endpoints are in the unit tests of ExternalService
    -- The integration tests are just another safeguard.
    it "should not accept invalid/disallowed paths" $
      runTestFederator env $ do
        let o = object ["name" .= ("fakeNewUser" :: Text)]
        inwardCall "/federation/brig/../i/users" (encode o)
          !!! const 403 === statusCode

    it "should only accept /federation/ paths" $
      runTestFederator env $ do
        let o = object ["name" .= ("fakeNewUser" :: Text)]
        inwardCall "/i/users" (encode o)
          !!! const 403 === statusCode

    -- Matching client certificates against domain names is better tested in
    -- unit tests.
    it "should reject requests without a client certificate" $
      runTestFederator env $ do
        originDomain <- cfgOriginDomain <$> view teTstOpts
        hdl <- randomHandle
        inwardCallWithHeaders
          "federation/brig/get-user-by-handle"
          [(originDomainHeaderName, toByteString' originDomain)]
          (encode hdl)
          !!! const 403 === statusCode

inwardCallWithHeaders ::
  (MonadIO m, MonadHttp m, MonadReader TestEnv m, HasCallStack) =>
  ByteString ->
  [HTTP.Header] ->
  LBS.ByteString ->
  m (Response (Maybe LByteString))
inwardCallWithHeaders requestPath hh payload = do
  Endpoint fedHost fedPort <- cfgFederatorExternal <$> view teTstOpts
  post
    ( host (encodeUtf8 fedHost)
        . port fedPort
        . path requestPath
        . foldr (uncurry (\k v r -> header k v . r)) id hh
        . bytes (toByteString' payload)
    )

inwardCall ::
  (MonadIO m, MonadHttp m, MonadReader TestEnv m, HasCallStack) =>
  ByteString ->
  LBS.ByteString ->
  m (Response (Maybe LByteString))
inwardCall requestPath payload = do
  Endpoint fedHost fedPort <- cfgFederatorExternal <$> view teTstOpts
  originDomain <- cfgOriginDomain <$> view teTstOpts
  clientCertFilename <- clientCertificate . optSettings . view teOpts <$> ask
  clientCert <- liftIO $ BS.readFile clientCertFilename
  post
    ( host (encodeUtf8 fedHost)
        . port fedPort
        . path requestPath
        . header "X-SSL-Certificate" (HTTP.urlEncode True clientCert)
        . header originDomainHeaderName (toByteString' originDomain)
        . bytes (toByteString' payload)
    )
