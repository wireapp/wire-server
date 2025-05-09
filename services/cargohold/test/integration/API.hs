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

module API (tests) where

import API.Util
import Bilge hiding (body)
import Bilge.Assert
import CargoHold.Types
import Control.Exception (throw)
import qualified Data.Aeson as Aeson
import Data.Domain
import Data.Id
import Data.Qualified
import Data.UUID.V4
import Federator.MockServer
import Imports hiding (head)
import Network.HTTP.Media ((//))
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai.Utilities.Error as Wai
import Test.Tasty
import Test.Tasty.HUnit
import TestSetup
import Wire.API.Federation.API.Cargohold

tests :: IO TestSetup -> TestTree
tests s =
  testGroup
    "API Integration"
    [ testGroup
        "remote"
        [ test s "federator failure on remote download" testRemoteDownloadFederationFailure
        ]
    ]

--------------------------------------------------------------------------------
-- Federation behaviour

-- This test doesn't fit in well with the `integration` style of tests. This is
-- because setting up the asset to have the weird state where the asset key exists,
-- but somehow the asset itself can't be downloaded is a bit beyond what the API
-- integration setup allows. So this specific test can stay here for now.
testRemoteDownloadFederationFailure :: TestM ()
testRemoteDownloadFederationFailure = do
  assetId <- liftIO $ Id <$> nextRandom
  uid <- liftIO $ Id <$> nextRandom
  let key = AssetKeyV3 assetId AssetPersistent
      qkey = Qualified key (Domain "faraway.example.com")
      respond req
        | frRPC req == "get-asset" =
            pure ("application" // "json", Aeson.encode (GetAssetResponse True))
        | otherwise = throw (MockErrorResponse HTTP.status500 "mock error")
  (resp, _) <-
    withMockFederator respond $ do
      responseJsonError
        =<< downloadAsset uid qkey () <!! do
          const 500 === statusCode
  liftIO $ do
    Wai.label resp @?= "mock-error"
    Wai.message resp @?= "Internal Server Error"
