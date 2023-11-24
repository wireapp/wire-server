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

module API.Federation (tests) where

import API.Util
import Bilge
import Bilge.Assert
import CargoHold.API.V3 (randToken)
import Conduit
import Control.Lens
import Data.Id
import Data.Qualified
import Data.UUID.V4
import Imports
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai.Utilities.Error as Wai
import Test.Tasty
import Test.Tasty.HUnit
import TestSetup
import Wire.API.Asset
import Wire.API.Federation.API
import Wire.API.Federation.API.Cargohold

tests :: IO TestSetup -> TestTree
tests s =
  testGroup
    "API Federation"
    [ testGroup
        "stream-asset"
        [ test s "stream asset not available" testStreamAssetNotAvailable,
          test s "stream asset wrong token" testStreamAssetWrongToken
        ]
    ]

testStreamAssetNotAvailable :: TestM ()
testStreamAssetNotAvailable = do
  uid <- liftIO $ Id <$> nextRandom
  token <- randToken

  assetId <- liftIO $ Id <$> nextRandom
  let key = AssetKeyV3 assetId AssetPersistent
  let ga =
        GetAsset
          { user = uid,
            token = Just token,
            key = key
          }
  err <- withFederationError $ do
    runFederationClient (unsafeFedClientIn @'Cargohold @"stream-asset" ga)
  liftIO $ do
    Wai.code err @?= HTTP.notFound404
    Wai.label err @?= "not-found"

testStreamAssetWrongToken :: TestM ()
testStreamAssetWrongToken = do
  -- Initial upload
  let bdy = (applicationOctetStream, "Hello World")
      settings = defAssetSettings & set setAssetRetention (Just AssetVolatile)
  uid <- randomUser
  ast :: Asset <-
    responseJsonError
      =<< uploadSimple (path "/assets/v3") uid settings bdy
        <!! const 201 === statusCode

  -- Call get-asset federation API with wrong (random) token
  tok <- randToken
  let key = view assetKey ast
  let ga =
        GetAsset
          { user = uid,
            token = Just tok,
            key = qUnqualified key
          }
  err <- withFederationError $ do
    runFederationClient (unsafeFedClientIn @'Cargohold @"stream-asset" ga)
  liftIO $ do
    Wai.code err @?= HTTP.notFound404
    Wai.label err @?= "not-found"
