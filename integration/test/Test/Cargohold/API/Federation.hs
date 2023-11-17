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

import API.Cargohold
import Control.Lens hiding ((.=))
import Crypto.Random
import Data.Aeson.Types hiding ((.=))
import Data.String.Conversions
import Data.Time
import Data.UUID.V4
import GHC.Stack
import SetupHelpers
import Test.Cargohold.API.Util
import Testlib.Assertions
import Testlib.Prelude

testGetAssetAvailablePrivate :: HasCallStack => App ()
testGetAssetAvailablePrivate = getAssetAvailable False

testGetAssetAvailablePublic :: HasCallStack => App ()
testGetAssetAvailablePublic = getAssetAvailable True

getAssetAvailable :: HasCallStack => Bool -> App ()
getAssetAvailable isPublicAsset = do
  uid <- randomUser OwnDomain def
  resp <- uploadAssetV3 uid isPublicAsset (Just assetVolatileSeconds) applicationOctetStream bdy
  resp.status `shouldMatchInt` 201
  ast <- resp.json

  -- Call get-asset federation API
  tok <- ast %. "token" & asString
  qKey <- ast %. "key"
  key <- qKey %. "id" & asString
  let ga =
        object
          [ "user" .= toJSON uid,
            "token" .= toJSON tok,
            "key" .= toJSON key
          ]
  ok <-
    withFederationClient $
      available <$> runFederationClient (unsafeFedClientIn @'Cargohold @"get-asset" ga)

  assertBool "check that asset is available" ok
  where
    assetVolatileSeconds :: NominalDiffTime
    assetVolatileSeconds = 28 * 24 * 3600 -- 28 days

testGetAssetNotAvailable :: HasCallStack => App ()
testGetAssetNotAvailable = do
  uid <- randomId
  token <- randToken

  assetId <- randomId
  let key = AssetKeyV3 assetId AssetPersistent
  let ga =
        object
          [ "user" .= _ uid,
            "token" .= _ (Just token),
            "key" .= _ key
          ]
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
  ast :: Value <-
    responseJsonError
      =<< uploadSimple (path "/assets/v3") uid settings bdy
      <!! const 201
      === statusCode

  -- Call get-asset federation API with wrong (random) token
  tok <- randToken
  let key = view assetKey ast
  let ga =
        object
          [ "user" .= _ uid,
            "token" .= _ (Just tok),
            "key" .= _ (qUnqualified key)
          ]
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

  ast :: Value <-
    responseJsonError
      =<< uploadSimple (path "/assets/v3") uid settings (applicationOctetStream, bs)
      <!! const 201
      === statusCode

  -- Call get-asset federation API
  tok <- ast %. "token" & asString
  key <- ast %. "key" & asString
  let ga =
        object
          [ "user" .= _ uid,
            "token" .= _ tok,
            "key" .= _ (qUnqualified key)
          ]
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
  ast :: Value <-
    responseJsonError
      =<< uploadSimple (path "/assets/v3") uid settings bdy
      <!! const 201
      === statusCode

  -- Call get-asset federation API
  tok <- ast %. "token" & asString
  key <- ast %. "key" & asString
  let ga =
        object
          [ "user" .= _ uid,
            "token" .= _ tok,
            "key" .= _ (qUnqualified key)
          ]
  respBody <- withFederationClient $ do
    source <- getAssetSource <$> runFederationClient (unsafeFedClientIn @'Cargohold @"stream-asset" ga)
    liftIO . runResourceT $ connect source sinkLazy
  liftIO $ respBody @?= "Hello World"

testStreamAssetNotAvailable :: HasCallStack => App ()
testStreamAssetNotAvailable = do
  uid <- liftIO $ Id <$> nextRandom
  token <- randToken

  assetId <- liftIO $ Id <$> nextRandom
  key <- ast %. "key" & asString
  let ga =
        object
          [ "user" .= _ uid,
            "token" .= _ (Just tok),
            "key" .= _ key
          ]
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
  ast :: Value <-
    responseJsonError
      =<< uploadSimple (path "/assets/v3") uid settings bdy
      <!! const 201
      === statusCode

  -- Call get-asset federation API with wrong (random) token
  tok <- randToken
  key <- ast %. "key" %. "id" & asString
  let ga =
        object
          [ "user" .= _ uid,
            "token" .= _ (Just tok),
            "key" .= _ key
          ]
  err <- withFederationError $ do
    runFederationClient (unsafeFedClientIn @'Cargohold @"stream-asset" ga)
  liftIO $ do
    Wai.code err @?= HTTP.notFound404
    Wai.label err @?= "not-found"
