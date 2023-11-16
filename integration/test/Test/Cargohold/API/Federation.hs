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

module Test.Cargohold.API.Federation where

import API.Brig qualified as BrigP
import API.BrigInternal qualified as BrigI
import API.Common qualified as API
import API.GalleyInternal qualified as GalleyI
import Control.Concurrent (threadDelay)
import Control.Lens
import Crypto.Random
import Data.Aeson.Types hiding ((.=))
import Data.Set qualified as Set
import Data.String.Conversions
import Data.UUID qualified as UUID
import Data.UUID.V4
import Data.UUID.V4 qualified as UUID
import Data.Vector.Internal.Check (HasCallStack)
import GHC.Stack
import SetupHelpers
import Test.Cargohold.API.Util
import Testlib.Assertions
import Testlib.Prelude

testGetAssetAvailablePrivate :: HasCallStack => App ()
testGetAssetAvailablePrivate = getAssetAvailable False

testGetAssetAvailablePublic :: HasCallStack => App ()
testGetAssetAvailablePublic = getAssetAvailable True

getAssetAvailable :: Bool -> App ()
getAssetAvailable isPublicAsset = do
  uid <- randomUser
  ast :: Asset <-
    responseJsonError
      =<< uploadAssetV3 uid isPublicAsset (Just AssetVolatile) applicationOctetStream bdy
      <!! const 201
      === statusCode

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

testGetAssetNotAvailable :: HasCallStack => App ()
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

testGetAssetWrongToken :: HasCallStack => App ()
testGetAssetWrongToken = do
  -- Initial upload
  let bdy = (applicationOctetStream, "Hello World")
      settings = defAssetSettings & set setAssetRetention (Just AssetVolatile)
  uid <- randomUser
  ast :: Asset <-
    responseJsonError
      =<< uploadSimple (path "/assets/v3") uid settings bdy
      <!! const 201
      === statusCode

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

testLargeAsset :: HasCallStack => App ()
testLargeAsset = do
  -- Initial upload
  let settings =
        defAssetSettings
          & set setAssetRetention (Just AssetVolatile)
  uid <- randomUser
  -- generate random bytes
  let size = 1024 * 1024
  bs <- liftIO $ getRandomBytes size

  ast :: Asset <-
    responseJsonError
      =<< uploadSimple (path "/assets/v3") uid settings (applicationOctetStream, bs)
      <!! const 201
      === statusCode

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

testStreamAsset :: HasCallStack => App ()
testStreamAsset = do
  -- Initial upload
  let bdy = (applicationOctetStream, "Hello World")
      settings =
        defAssetSettings
          & set setAssetRetention (Just AssetVolatile)
  uid <- randomUser
  ast :: Asset <-
    responseJsonError
      =<< uploadSimple (path "/assets/v3") uid settings bdy
      <!! const 201
      === statusCode

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

testStreamAssetNotAvailable :: HasCallStack => App ()
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

testStreamAssetWrongToken :: HasCallStack => App ()
testStreamAssetWrongToken = do
  -- Initial upload
  let bdy = (applicationOctetStream, "Hello World")
      settings = defAssetSettings & set setAssetRetention (Just AssetVolatile)
  uid <- randomUser
  ast :: Asset <-
    responseJsonError
      =<< uploadSimple (path "/assets/v3") uid settings bdy
      <!! const 201
      === statusCode

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
