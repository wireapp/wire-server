{-# OPTIONS_GHC -Wno-orphans #-}

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

module Test.Federator.ExternalServer where

import qualified Data.ByteString as BS
import Data.Domain
import Data.String.Conversions (cs)
import Federator.ExternalServer (callLocal)
import Federator.Service (Service)
import Imports
import qualified Network.HTTP.Types as HTTP
import Polysemy (embed, runM)
import qualified Polysemy.Reader as Polysemy
import qualified Polysemy.TinyLog as TinyLog
import Test.Federator.Options (noClientCertSettings)
import Test.Federator.Validation (mockDiscoveryTrivial)
import Test.Polysemy.Mock (Mock (mock), evalMock)
import Test.Polysemy.Mock.TH (genMock)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, assertFailure, testCase)
import Wire.API.Federation.GRPC.Types
import qualified Wire.API.Federation.GRPC.Types as InwardError

genMock ''Service

tests :: TestTree
tests =
  testGroup "ExternalServer" $
    [ requestBrigSuccess,
      requestBrigFailure,
      requestGalleySuccess
    ]

requestBrigSuccess :: TestTree
requestBrigSuccess =
  testCase "should translate response from brig to 'InwardResponseBody' when response has status 200" $
    runM . evalMock @Service @IO $ do
      mockServiceCallReturns @IO (\_ _ _ _ -> pure (HTTP.ok200, Just "response body"))
      let request = Request Brig "/federation/get-user-by-handle" "\"foo\"" exampleDomain

      exampleCert <- embed $ BS.readFile "test/resources/unit/localhost.example.com.pem"
      res :: InwardResponse <-
        mock @Service @IO
          . TinyLog.discardLogs
          . mockDiscoveryTrivial
          . Polysemy.runReader noClientCertSettings
          $ callLocal (Just exampleCert) request
      actualCalls <- mockServiceCallCalls @IO
      let expectedCall = (Brig, "federation/get-user-by-handle", "\"foo\"", aValidDomain)
      embed $ assertEqual "one call to brig should be made" [expectedCall] actualCalls
      embed $ assertEqual "response should be success with correct body" (InwardResponseBody "response body") res

requestBrigFailure :: TestTree
requestBrigFailure =
  testCase "should translate response from brig to 'InwardResponseError' when response has status 404" $
    runM . evalMock @Service @IO $ do
      mockServiceCallReturns @IO (\_ _ _ _ -> pure (HTTP.notFound404, Just "response body"))
      let request = Request Brig "/federation/get-user-by-handle" "\"foo\"" exampleDomain

      exampleCert <- embed $ BS.readFile "test/resources/unit/localhost.example.com.pem"
      res <-
        mock @Service @IO
          . TinyLog.discardLogs
          . mockDiscoveryTrivial
          . Polysemy.runReader noClientCertSettings
          $ callLocal (Just exampleCert) request

      actualCalls <- mockServiceCallCalls @IO
      let expectedCall = (Brig, "federation/get-user-by-handle", "\"foo\"", aValidDomain)
      embed $ assertEqual "one call to brig should be made" [expectedCall] actualCalls
      embed $ case res of
        InwardResponseBody b -> assertFailure ("expecting error, got success: " <> cs b)
        InwardResponseError err -> assertEqual ("response should be InwardError 'IOther', but got:" <> show err) InwardError.IOther (inwardErrorType err)

requestGalleySuccess :: TestTree
requestGalleySuccess =
  testCase "should translate response from galley to 'InwardResponseBody' when response has status 200" $
    runM . evalMock @Service @IO $ do
      mockServiceCallReturns @IO (\_ _ _ _ -> pure (HTTP.ok200, Just "response body"))
      let request = Request Galley "federation/get-conversations" "{}" exampleDomain

      exampleCert <- embed $ BS.readFile "test/resources/unit/localhost.example.com.pem"
      res :: InwardResponse <-
        mock @Service @IO
          . TinyLog.discardLogs
          . mockDiscoveryTrivial
          . Polysemy.runReader noClientCertSettings
          $ callLocal (Just exampleCert) request
      actualCalls <- mockServiceCallCalls @IO
      let expectedCall = (Galley, "federation/get-conversations", "{}", aValidDomain)
      embed $ assertEqual "one call to brig should be made" [expectedCall] actualCalls
      embed $ assertEqual "response should be success with correct body" (InwardResponseBody "response body") res

exampleDomain :: Text
exampleDomain = "localhost.example.com"

aValidDomain :: Domain
aValidDomain = Domain exampleDomain
