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

module Test.Federator.Response (tests) where

import Data.Aeson qualified as Aeson
import Federator.Discovery
import Federator.Error.ServerError (ServerError (..))
import Federator.Remote
import Federator.Response (runWaiError)
import Federator.Validation
import Imports
import Network.HTTP.Types qualified as HTTP
import Network.Wai qualified as Wai
import Network.Wai.Utilities.Error qualified as Wai
import Network.Wai.Utilities.Server qualified as Wai
import Polysemy
import Polysemy.Error
import Test.Tasty
import Test.Tasty.HUnit
import Wire.API.Federation.Error
import Wire.Network.DNS.SRV
import Wire.Sem.Logger.TinyLog qualified as Log

tests :: TestTree
tests =
  testGroup
    "Wai Errors"
    [ testValidationError,
      testServerError,
      testDiscoveryFailure,
      testRemoteError
    ]

testValidationError :: TestTree
testValidationError =
  testCase "validation errors should be converted to wai error responses" $ do
    resp <-
      runM
        . Log.discardTinyLogs
        . runWaiError @ValidationError
        $ throw NoClientCertificate
    body <- Wai.lazyResponseBody resp
    let merr = Aeson.decode body
    Wai.responseStatus resp @?= HTTP.status403
    fmap Wai.label merr @?= Just "no-client-certificate"

testServerError :: TestTree
testServerError =
  testCase "server errors should be converted to wai error responses" $ do
    resp <-
      runM
        . Log.discardTinyLogs
        . runWaiError @ServerError
        $ throw InvalidRoute
    body <- Wai.lazyResponseBody resp
    let merr = Aeson.decode body
    Wai.responseStatus resp @?= HTTP.status403
    fmap Wai.label merr @?= Just "invalid-endpoint"

testDiscoveryFailure :: TestTree
testDiscoveryFailure =
  testCase "discovery failures should be converted to wai error responses" $ do
    resp <-
      runM
        . Log.discardTinyLogs
        . runWaiError @DiscoveryFailure
        $ throw (DiscoveryFailureDNSError "mock error")
    body <- Wai.lazyResponseBody resp
    let merr = Aeson.decode body
    Wai.responseStatus resp @?= HTTP.status400
    fmap Wai.label merr @?= Just "discovery-failure"

testRemoteError :: TestTree
testRemoteError =
  testCase "remote errors should be converted to wai error responses" $ do
    resp <-
      runM
        . Log.discardTinyLogs
        . runWaiError @RemoteError
        $ throw
          ( RemoteError
              (SrvTarget "example.com" 7777)
              FederatorClientNoStatusCode
          )
    body <- Wai.lazyResponseBody resp
    let merr = Aeson.decode body
    Wai.responseStatus resp @?= toEnum 533
    fmap Wai.label merr @?= Just "federation-http2-error"
