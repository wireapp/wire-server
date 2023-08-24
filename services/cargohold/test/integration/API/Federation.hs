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
import Crypto.Random
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
import Wire.API.Routes.AssetBody

tests :: IO TestSetup -> TestTree
tests s =
  testGroup
    "API Federation"
    [ testGroup
        "get-asset"
        [ test s "private asset is available" (testGetAssetAvailable False),
          test s "public asset is available" (testGetAssetAvailable True),
          test s "not available" testGetAssetNotAvailable,
          test s "wrong token" testGetAssetWrongToken
        ],
      testGroup
        "stream-asset"
        [ test s "streaming large asset" testLargeAsset,
          test s "stream an asset" testStreamAsset,
          test s "stream asset not available" testStreamAssetNotAvailable,
          test s "stream asset wrong token" testStreamAssetWrongToken
        ]
    ]

testGetAssetAvailable :: Bool -> TestM ()
testGetAssetAvailable isPublicAsset = do
  -- Initial upload
  let bdy = (applicationOctetStream, "Hello World")
      settings =
        defAssetSettings
          & set setAssetRetention (Just AssetVolatile)
          & set setAssetPublic isPublicAsset
  uid <- liftIO $ Id <$> nextRandom
  ast :: Asset <-
    responseJsonError
      =<< uploadSimple (path "/assets/v3") uid settings bdy
        <!! const 201 === statusCode

  -- Call get-asset federation API
  let tok = view assetToken ast
  let key = view assetKey ast
  let ga =
        GetAsset
          { user = uid,
            token = tok,
            key = qUnqualified key
          }
  ok <-
    withFederationClient $
      available <$> runFederationClient (unsafeFedClientIn @'Cargohold @"get-asset" ga)

  -- check that asset is available
  liftIO $ ok @?= True

testGetAssetNotAvailable :: TestM ()
testGetAssetNotAvailable = do
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
  ok <-
    withFederationClient $
      available <$> runFederationClient (unsafeFedClientIn @'Cargohold @"get-asset" ga)

  -- check that asset is not available
  liftIO $ ok @?= False

testGetAssetWrongToken :: TestM ()
testGetAssetWrongToken = do
  -- Initial upload
  let bdy = (applicationOctetStream, "Hello World")
      settings = defAssetSettings & set setAssetRetention (Just AssetVolatile)
  uid <- liftIO $ Id <$> nextRandom
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
  ok <-
    withFederationClient $
      available <$> runFederationClient (unsafeFedClientIn @'Cargohold @"get-asset" ga)

  -- check that asset is not available
  liftIO $ ok @?= False

testLargeAsset :: TestM ()
testLargeAsset = do
  -- Initial upload
  let settings =
        defAssetSettings
          & set setAssetRetention (Just AssetVolatile)
  uid <- liftIO $ Id <$> nextRandom
  -- generate random bytes
  let size = 1024 * 1024
  bs <- liftIO $ getRandomBytes size

  ast :: Asset <-
    responseJsonError
      =<< uploadSimple (path "/assets/v3") uid settings (applicationOctetStream, bs)
        <!! const 201 === statusCode

  -- Call get-asset federation API
  let tok = view assetToken ast
  let key = view assetKey ast
  let ga =
        GetAsset
          { user = uid,
            token = tok,
            key = qUnqualified key
          }
  chunks <- withFederationClient $ do
    source <- getAssetSource <$> runFederationClient (unsafeFedClientIn @'Cargohold @"stream-asset" ga)
    liftIO . runResourceT $ connect source sinkList
  liftIO $ do
    let minNumChunks = 8
    assertBool
      ("Expected at least " <> show minNumChunks <> " chunks, got " <> show (length chunks))
      (length chunks > minNumChunks)
    mconcat chunks @?= bs

testStreamAsset :: TestM ()
testStreamAsset = do
  -- Initial upload
  let bdy = (applicationOctetStream, "Hello World")
      settings =
        defAssetSettings
          & set setAssetRetention (Just AssetVolatile)
  uid <- liftIO $ Id <$> nextRandom
  ast :: Asset <-
    responseJsonError
      =<< uploadSimple (path "/assets/v3") uid settings bdy
        <!! const 201 === statusCode

  -- Call get-asset federation API
  let tok = view assetToken ast
  let key = view assetKey ast
  let ga =
        GetAsset
          { user = uid,
            token = tok,
            key = qUnqualified key
          }
  respBody <- withFederationClient $ do
    source <- getAssetSource <$> runFederationClient (unsafeFedClientIn @'Cargohold @"stream-asset" ga)
    liftIO . runResourceT $ connect source sinkLazy
  liftIO $ respBody @?= "Hello World"

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
  uid <- liftIO $ Id <$> nextRandom
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
