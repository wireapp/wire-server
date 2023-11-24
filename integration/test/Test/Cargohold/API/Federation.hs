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
import SetupHelpers
import Test.Cargohold.API.Util
import Testlib.Prelude

testGetAssetAvailablePublic :: HasCallStack => App ()
testGetAssetAvailablePublic = getAssetAvailable True

testGetAssetAvailablePrivate :: HasCallStack => App ()
testGetAssetAvailablePrivate = getAssetAvailable False

getAssetAvailable :: HasCallStack => Bool -> App ()
getAssetAvailable isPublicAsset = do
  -- Initial upload
  let bdy = (applicationOctetStream, "Hello World")
      settings = object ["public" .= isPublicAsset, "retention" .= "volatile"]
  uid1 <- randomUser OwnDomain def
  uid2 <- randomUser OtherDomain def
  r1 <- uploadSimple uid1 settings bdy
  r1.status `shouldMatchInt` 201
  ast <- maybe (error "No JSON in the response") pure r1.jsonBody

  -- Call get-asset federation API
  -- Public assets don't have tokens, so don't explode if we can't get one.
  tok <-
    if isPublicAsset
      then pure $ Right ()
      else Left <$> (ast %. "token" & asString)
  res <- downloadAsset' uid2 r1.jsonBody tok
  res.status `shouldMatchInt` 200

testGetAssetNotAvailable :: HasCallStack => App ()
testGetAssetNotAvailable = do
  uid <- randomUser OwnDomain def
  userId <- uid %. "id" & asString
  token <- randomToken
  assetId <- randomId
  otherDomain <- make OtherDomain & asString
  let key = "3-2-" <> assetId
      -- Use a foreign domain so that it will go via federator
      ga = object ["user" .= userId, "token" .= token, "key" .= key, "domain" .= otherDomain]
  r <- downloadAsset' uid ga ga
  -- check that asset is not available
  r.status `shouldMatchInt` 404
  r.jsonBody %. "message" `shouldMatch` "Asset not found"

testGetAssetWrongToken :: HasCallStack => App ()
testGetAssetWrongToken = do
  -- Initial upload
  let bdy = (applicationOctetStream, "Hello World")
      -- Make it a public token so that other users can potentially
      -- grab it across federation instances
      settings = object ["public" .= True, "retention" .= "volatile"]
  uid1 <- randomUser OwnDomain def
  uid2 <- randomUser OtherDomain def
  userId2 <- uid2 %. "id" & asString
  domain <- make OwnDomain & asString
  r1 <- uploadSimple uid1 settings bdy
  r1.status `shouldMatchInt` 201
  key <- r1.jsonBody %. "key" & asString

  -- Call get-asset federation API with wrong (random) token
  -- Use uid2 so that this will go via federation
  tok <- randomToken
  let ga =
        object
          [ "user" .= userId2,
            "token" .= tok,
            "key" .= key,
            "domain" .= domain
          ]
  r2 <- downloadAsset' uid2 ga ga
  r2.status `shouldMatchInt` 404
  r2.jsonBody %. "message" `shouldMatch` "Asset not found"

-- testLargeAsset :: TestM ()
-- testLargeAsset = do
--   -- Initial upload
--   let settings =
--         defAssetSettings
--           & set setAssetRetention (Just AssetVolatile)
--   uid <- randomUser
--   -- generate random bytes
--   let size = 1024 * 1024
--   bs <- liftIO $ getRandomBytes size
--
--   ast :: Asset <-
--     responseJsonError
--       =<< uploadSimple (path "/assets/v3") uid settings (applicationOctetStream, bs)
--         <!! const 201 === statusCode
--
--   -- Call get-asset federation API
--   let tok = view assetToken ast
--   let key = view assetKey ast
--   let ga =
--         GetAsset
--           { user = uid,
--             token = tok,
--             key = qUnqualified key
--           }
--   chunks <- withFederationClient $ do
--     source <- getAssetSource <$> runFederationClient (unsafeFedClientIn @'Cargohold @"stream-asset" ga)
--     liftIO . runResourceT $ connect source sinkList
--   liftIO $ do
--     let minNumChunks = 8
--     assertBool
--       ("Expected at least " <> show minNumChunks <> " chunks, got " <> show (length chunks))
--       (length chunks > minNumChunks)
--     mconcat chunks @?= bs
--
-- testStreamAsset :: TestM ()
-- testStreamAsset = do
--   -- Initial upload
--   let bdy = (applicationOctetStream, "Hello World")
--       settings =
--         defAssetSettings
--           & set setAssetRetention (Just AssetVolatile)
--   uid <- randomUser
--   ast :: Asset <-
--     responseJsonError
--       =<< uploadSimple (path "/assets/v3") uid settings bdy
--         <!! const 201 === statusCode
--
--   -- Call get-asset federation API
--   let tok = view assetToken ast
--   let key = view assetKey ast
--   let ga =
--         GetAsset
--           { user = uid,
--             token = tok,
--             key = qUnqualified key
--           }
--   respBody <- withFederationClient $ do
--     source <- getAssetSource <$> runFederationClient (unsafeFedClientIn @'Cargohold @"stream-asset" ga)
--     liftIO . runResourceT $ connect source sinkLazy
--   liftIO $ respBody @?= "Hello World"
--
-- testStreamAssetNotAvailable :: TestM ()
-- testStreamAssetNotAvailable = do
--   uid <- liftIO $ Id <$> nextRandom
--   token <- randToken
--
--   assetId <- liftIO $ Id <$> nextRandom
--   let key = AssetKeyV3 assetId AssetPersistent
--   let ga =
--         GetAsset
--           { user = uid,
--             token = Just token,
--             key = key
--           }
--   err <- withFederationError $ do
--     runFederationClient (unsafeFedClientIn @'Cargohold @"stream-asset" ga)
--   liftIO $ do
--     Wai.code err @?= HTTP.notFound404
--     Wai.label err @?= "not-found"
--
-- testStreamAssetWrongToken :: TestM ()
-- testStreamAssetWrongToken = do
--   -- Initial upload
--   let bdy = (applicationOctetStream, "Hello World")
--       settings = defAssetSettings & set setAssetRetention (Just AssetVolatile)
--   uid <- randomUser
--   ast :: Asset <-
--     responseJsonError
--       =<< uploadSimple (path "/assets/v3") uid settings bdy
--         <!! const 201 === statusCode
--
--   -- Call get-asset federation API with wrong (random) token
--   tok <- randToken
--   let key = view assetKey ast
--   let ga =
--         GetAsset
--           { user = uid,
--             token = Just tok,
--             key = qUnqualified key
--           }
--   err <- withFederationError $ do
--     runFederationClient (unsafeFedClientIn @'Cargohold @"stream-asset" ga)
--   liftIO $ do
--     Wai.code err @?= HTTP.notFound404
--     Wai.label err @?= "not-found"
