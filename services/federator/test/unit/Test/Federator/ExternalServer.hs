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

import Federator.Brig (Brig)
import Federator.ExternalServer (callLocal)
import Imports
import qualified Network.HTTP.Types as HTTP
import Polysemy (embed, runM)
import Test.Polysemy.Mock (Mock (mock), evalMock)
import Test.Polysemy.Mock.TH (genMock)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)
import Wire.API.Federation.GRPC.Types

genMock ''Brig

tests :: TestTree
tests =
  testGroup "ExternalServer" $
    [ requestBrigSuccess
    ]

requestBrigSuccess :: TestTree
requestBrigSuccess =
  testCase "should translate response from brig to 'InwardResponse'" $
    runM . evalMock @Brig @IO $ do
      mockBrigCallReturns @IO (\_ _ _ _ -> pure (HTTP.status200, Just "response body"))
      let request = Request Brig (HTTPMethod HTTP.GET) "/users" [QueryParam "handle" "foo"] mempty

      res <- mock @Brig @IO $ callLocal request

      actualCalls <- mockBrigCallCalls @IO
      let expectedCall = (HTTP.GET, "/users", [QueryParam "handle" "foo"], mempty)
      embed $ assertEqual "one call to brig should be made" [expectedCall] actualCalls
      embed $ assertEqual "response should be success with correct body" (InwardResponseHTTPResponse (HTTPResponse 200 "response body")) res
