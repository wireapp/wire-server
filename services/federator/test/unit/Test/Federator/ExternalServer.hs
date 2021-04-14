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

import Data.Domain (Domain (..))
import Federator.Brig (Brig)
import Federator.ExternalServer (callLocal)
import Federator.Options (FederationStrategy (AllowAll), RunSettings (..))
import Imports
import qualified Network.HTTP.Types as HTTP
import Polysemy (embed, runM)
import qualified Polysemy.Reader as Polysemy
import qualified Polysemy.TinyLog as Log
import qualified System.Logger as Logger
import Test.Polysemy.Mock (Mock (mock), evalMock)
import Test.Polysemy.Mock.TH (genMock)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)
import Wire.API.Federation.GRPC.Types

genMock ''Brig

genMock ''Log.TinyLog

-- TODO: replace (runTinyLog l) with this mock... somehow?

tests :: TestTree
tests =
  testGroup "ExternalServer" $
    [ requestBrigSuccess,
      requestBrigFailure
    ]

requestBrigSuccess :: TestTree
requestBrigSuccess =
  testCase "should translate response from brig to 'InwardResponseBody' when response has status 200" $
    runM . evalMock @Brig @IO $ do
      mockBrigCallReturns @IO (\_ _ _ -> pure (HTTP.ok200, Just "response body"))
      let request = Request Brig "/users" "\"foo\"" exampleDomain

      l <- Logger.create Logger.StdErr

      res :: InwardResponse <- mock @Brig @IO . Log.runTinyLog l . Polysemy.runReader allowAllSettings $ callLocal request
      actualCalls <- mockBrigCallCalls @IO
      let expectedCall = ("/users", "\"foo\"", aValidDomain)
      embed $ assertEqual "one call to brig should be made" [expectedCall] actualCalls
      embed $ assertEqual "response should be success with correct body" (InwardResponseBody "response body") res

requestBrigFailure :: TestTree
requestBrigFailure =
  testCase "should translate response from brig to 'InwardResponseBody' when response has status 404" $
    runM . evalMock @Brig @IO $ do
      mockBrigCallReturns @IO (\_ _ _ -> pure (HTTP.notFound404, Just "response body"))
      let request = Request Brig "/users" "\"foo\"" exampleDomain

      l <- Logger.create Logger.StdErr
      res <- mock @Brig @IO . Log.runTinyLog l . Polysemy.runReader allowAllSettings $ callLocal request

      actualCalls <- mockBrigCallCalls @IO
      let expectedCall = ("/users", "\"foo\"", aValidDomain)
      embed $ assertEqual "one call to brig should be made" [expectedCall] actualCalls
      embed $ assertEqual "response should be success with correct body" (InwardResponseErr "Invalid HTTP status from component: 404 Not Found") res

allowAllSettings :: RunSettings
allowAllSettings = RunSettings AllowAll

exampleDomain :: Text
exampleDomain = "some.example.com"

aValidDomain :: Domain
aValidDomain = Domain exampleDomain
