-- Disabling to stop warnings on HasCallStack
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

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

module Test.Federator.InwardSpec where

import Bilge
import Bilge.Assert
import Control.Lens (view)
import Data.Aeson
import Data.Aeson.Types qualified as Aeson
import Data.ByteString qualified as BS
import Data.ByteString.Conversion (toByteString')
import Data.ByteString.Lazy qualified as LBS
import Data.LegalHold (UserLegalHoldStatus (UserLegalHoldNoConsent))
import Data.Text.Encoding
import Federator.Options hiding (federatorExternal)
import Imports
import Network.HTTP.Types qualified as HTTP
import Test.Federator.Util
import Test.Hspec
import Test.QuickCheck (arbitrary, generate)
import Util.Options (Endpoint (Endpoint))
import Wire.API.Federation.API.Cargohold
import Wire.API.Federation.Domain
import Wire.API.User

-- FUTUREWORK(federation): move these tests to brig-integration (benefit: avoid duplicating all of the brig helper code)
-- FUTUREWORK(fisx): better yet, reorganize integration tests (or at least the helpers) so
-- they don't spread out over the different sevices.

-- | This module contains tests for the interface between federator and brig.  The tests call
-- federator directly, circumventing ingress:
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

        let expectedProfile = mkUserProfile EmailVisibleToSelf user UserLegalHoldNoConsent
        bdy <-
          responseJsonError
            =<< inwardCall "/federation/brig/get-users-by-ids" (encode [userId user])
              <!! do
                const 200 === statusCode

        liftIO $ bdy `shouldBe` [expectedProfile]

    it "testShouldRejectMissmatchingOriginDomainInward" (testShouldRejectMissmatchingOriginDomainInward env)

    it "should be able to call cargohold" $
      runTestFederator env $ do
        uid <- liftIO $ generate arbitrary
        key <- liftIO $ generate arbitrary
        let ga = GetAsset uid key Nothing
        inwardCall "/federation/cargohold/get-asset" (encode ga)
          !!! const 200 === statusCode

    it "should return 404 'no-endpoint' response from Brig" $
      runTestFederator env $ do
        resp <-
          inwardCall "/federation/brig/this-endpoint-does-not-exist" (encode Aeson.emptyObject)
            <!! const 404 === statusCode
        liftIO $ resp.responseBody `shouldBe` Nothing

    -- Note: most tests for forbidden endpoints are in the unit tests of ExternalService
    -- The integration tests are just another safeguard.
    it "should not accept invalid/disallowed paths" $
      runTestFederator env $ do
        let o = object ["name" .= ("fakeNewUser" :: Text)]
        inwardCall "/federation/brig/../i/users" (encode o)
          !!! const 404 === statusCode

    it "should only accept /federation/ paths" $
      runTestFederator env $ do
        let o = object ["name" .= ("fakeNewUser" :: Text)]
        inwardCall "/i/users" (encode o)
          !!! const 404 === statusCode

    it "testRejectRequestsWithoutClientCertInward" (testRejectRequestsWithoutClientCertInward env)

-- @SF.Federation @TSFI.RESTfulAPI @S2 @S3 @S7
--
-- This test is covered by the unit tests 'validateDomainCertWrongDomain' because
-- the domain matching is checked on certificate validation.
testShouldRejectMissmatchingOriginDomainInward :: TestEnv -> IO ()
testShouldRejectMissmatchingOriginDomainInward env = runTestFederator env $ pure ()

-- @END

-- @SF.Federation @TSFI.RESTfulAPI @S2 @S3 @S7
--
-- See related tests in unit tests (for matching client certificates against domain names)
-- and "IngressSpec".
testRejectRequestsWithoutClientCertInward :: TestEnv -> IO ()
testRejectRequestsWithoutClientCertInward env = runTestFederator env $ do
  originDomain <- originDomain <$> view teTstOpts
  hdl <- randomHandle
  inwardCallWithHeaders
    "federation/brig/get-user-by-handle"
    [(originDomainHeaderName, toByteString' originDomain)]
    (encode hdl)
    !!! const 400 === statusCode

-- @END

inwardCallWithHeaders ::
  (MonadIO m, MonadHttp m, MonadReader TestEnv m, HasCallStack) =>
  ByteString ->
  [HTTP.Header] ->
  LBS.ByteString ->
  m (Response (Maybe LByteString))
inwardCallWithHeaders requestPath hh payload = do
  Endpoint fedHost fedPort <- federatorExternal <$> view teTstOpts
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
  originDomain :: Text <- originDomain <$> view teTstOpts
  inwardCallWithOriginDomain (toByteString' originDomain) requestPath payload

inwardCallWithOriginDomain ::
  (MonadIO m, MonadHttp m, MonadReader TestEnv m, HasCallStack) =>
  ByteString ->
  ByteString ->
  LBS.ByteString ->
  m (Response (Maybe LByteString))
inwardCallWithOriginDomain originDomain requestPath payload = do
  Endpoint fedHost fedPort <- federatorExternal <$> view teTstOpts
  clientCertFilename <- clientCertificate . optSettings . view teOpts <$> ask
  clientCert <- liftIO $ BS.readFile clientCertFilename
  post
    ( host (encodeUtf8 fedHost)
        . port fedPort
        . path requestPath
        . header "X-SSL-Certificate" (HTTP.urlEncode True clientCert)
        . header originDomainHeaderName originDomain
        . bytes (toByteString' payload)
    )
