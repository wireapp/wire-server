{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2021 Wire Swiss GmbH <opensource@wire.com>
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
import CargoHold.API.Error
import CargoHold.Types
import qualified CargoHold.Types.V3 as V3
import qualified Codec.MIME.Type as MIME
import Control.Exception (throw)
import Control.Lens hiding (sets)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as C8
import Data.ByteString.Conversion
import Data.Domain
import Data.Id
import Data.Qualified
import qualified Data.Text.Encoding.Error as Text
import qualified Data.Text.Lazy.Encoding as LText
import Data.Time.Clock
import Data.Time.Format
import Data.UUID.V4
import Federator.MockServer
import Imports hiding (head)
import Network.HTTP.Client (parseUrlThrow)
import Network.HTTP.Media ((//))
import qualified Network.HTTP.Types as HTTP
import Network.Wai.Utilities (Error (label))
import Test.Tasty
import Test.Tasty.HUnit
import TestSetup
import Wire.API.Federation.API.Cargohold
import Wire.API.Federation.Component

tests :: IO TestSetup -> TestTree
tests s =
  testGroup
    "API Integration"
    [ testGroup
        "simple"
        [ test s "roundtrip" testSimpleRoundtrip,
          test s "tokens" testSimpleTokens,
          test s "s3-upstream-closed" testSimpleS3ClosedConnectionReuse,
          test s "client-compatibility" testUploadCompatibility
        ],
      testGroup
        "remote"
        [ test s "remote download wrong domain" testRemoteDownloadWrongDomain,
          test s "remote download no asset" testRemoteDownloadNoAsset,
          test s "remote download" testRemoteDownload
        ]
    ]

--------------------------------------------------------------------------------
-- Simple (single-step) uploads

testSimpleRoundtrip :: TestM ()
testSimpleRoundtrip = do
  let def = V3.defAssetSettings
  let rets = [minBound ..]
  let sets = def : map (\r -> def & V3.setAssetRetention ?~ r) rets
  mapM_ simpleRoundtrip sets
  where
    simpleRoundtrip sets = do
      uid <- liftIO $ Id <$> nextRandom
      uid2 <- liftIO $ Id <$> nextRandom
      -- Initial upload
      let bdy = (applicationText, "Hello World")
      r1 <-
        uploadSimple (path "/assets/v3") uid sets bdy
          <!! const 201 === statusCode
      let loc = decodeHeaderOrFail "Location" r1 :: ByteString
      let Just ast = responseJsonMaybe @V3.Asset r1
      let Just tok = view V3.assetToken ast
      -- Check mandatory Date header
      let Just date = C8.unpack <$> lookup "Date" (responseHeaders r1)
      let utc = parseTimeOrError False defaultTimeLocale rfc822DateFormat date :: UTCTime
      -- Potentially check for the expires header
      when (isJust $ join (V3.assetRetentionSeconds <$> (sets ^. V3.setAssetRetention))) $ do
        liftIO $ assertBool "invalid expiration" (Just utc < view V3.assetExpires ast)
      -- Lookup with token and download via redirect.
      r2 <-
        downloadAsset uid loc (Just tok) <!! do
          const 302 === statusCode
          const Nothing === responseBody
      r3 <- flip get' id =<< parseUrlThrow (C8.unpack (getHeader' "Location" r2))
      liftIO $ do
        assertEqual "status" HTTP.status200 (responseStatus r3)
        assertEqual "content-type should always be application/octet-stream" (Just applicationOctetStream) (getContentType r3)
        assertEqual "token mismatch" tok (decodeHeaderOrFail "x-amz-meta-token" r3)
        assertEqual "user mismatch" uid (decodeHeaderOrFail "x-amz-meta-user" r3)
        assertEqual "data mismatch" (Just "Hello World") (responseBody r3)
      -- Delete (forbidden for other users)
      deleteAsset uid2 (view V3.assetKey ast) !!! const 403 === statusCode
      -- Delete (allowed for creator)
      deleteAsset uid (view V3.assetKey ast) !!! const 200 === statusCode
      r4 <- downloadAsset uid loc (Just tok) <!! const 404 === statusCode
      let Just date' = C8.unpack <$> lookup "Date" (responseHeaders r4)
      let utc' = parseTimeOrError False defaultTimeLocale rfc822DateFormat date' :: UTCTime
      liftIO $ assertBool "bad date" (utc' >= utc)

testSimpleTokens :: TestM ()
testSimpleTokens = do
  uid <- liftIO $ Id <$> nextRandom
  uid2 <- liftIO $ Id <$> nextRandom
  -- Initial upload
  let sets = V3.defAssetSettings & set V3.setAssetRetention (Just V3.AssetVolatile)
  let bdy = (applicationText, "Hello World")
  r1 <-
    uploadSimple (path "/assets/v3") uid sets bdy
      <!! const 201 === statusCode
  let loc = decodeHeaderOrFail "Location" r1 :: ByteString
  let Just ast = responseJsonMaybe @V3.Asset r1
  let key = view V3.assetKey ast
  let Just tok = view V3.assetToken ast
  -- No access without token from other user (opaque 404)
  downloadAsset uid2 loc ()
    !!! const 404 === statusCode
  -- No access with empty token query parameter from other user (opaque 404)
  downloadAsset uid2 loc (queryItem' "asset_token" Nothing)
    !!! const 404 === statusCode
  -- No access with wrong token (opaque 404)
  downloadAsset uid2 loc (Just (AssetToken "abc123"))
    !!! const 404 === statusCode
  -- No access with wrong token as query parameter (opaque 404)
  downloadAsset uid2 loc (queryItem "asset_token" "acb123")
    !!! const 404 === statusCode
  -- Token renewal fails if not done by owner
  postToken uid2 (qUnqualified key) !!! do
    const 403 === statusCode
    const (Just "unauthorised") === fmap label . responseJsonMaybe
  -- Token renewal succeeds if done by owner
  r2 <- postToken uid (qUnqualified key) <!! const 200 === statusCode
  let Just tok' = V3.newAssetToken <$> responseJsonMaybe r2
  liftIO $ assertBool "token unchanged" (tok /= tok')
  -- Download by owner with new token.
  r3 <-
    downloadAsset uid loc (Just tok') <!! do
      const 302 === statusCode
      const Nothing === responseBody
  r4 <- flip get' id =<< parseUrlThrow (C8.unpack (getHeader' "Location" r3))
  liftIO $ do
    assertEqual "status" HTTP.status200 (responseStatus r4)
    assertEqual "content-type should always be application/octet-stream" (Just applicationOctetStream) (getContentType r4)
    assertEqual "token mismatch" tok' (decodeHeaderOrFail "x-amz-meta-token" r4)
    assertEqual "user mismatch" uid (decodeHeaderOrFail "x-amz-meta-user" r4)
    assertEqual "data mismatch" (Just "Hello World") (responseBody r4)
  -- Verify access without token if the request comes from the creator.
  downloadAsset uid loc ()
    !!! const 302 === statusCode
  -- Verify access with new token from a different user.
  downloadAsset uid2 loc (Just tok')
    !!! const 302 === statusCode
  -- Verify access with new token as query parameter from a different user
  downloadAsset uid2 loc (queryItem "asset_token" (toByteString' tok'))
    !!! const 302 === statusCode
  -- Delete Token fails if not done by owner
  deleteToken uid2 (qUnqualified key) !!! do
    const 403 === statusCode
    const (Just "unauthorised") === fmap label . responseJsonMaybe
  -- Delete Token succeeds by owner
  deleteToken uid (qUnqualified key) !!! do
    const 200 === statusCode
    const Nothing === responseBody
  -- Access without token from different user (asset is now "public")
  downloadAsset uid2 loc () !!! do
    const 302 === statusCode
    const Nothing === responseBody

-- S3 closes idle connections after ~5 seconds, before the http-client 'Manager'
-- does. If such a closed connection is reused for an upload, no problems should
-- occur (i.e. the closed connection should be detected before sending any data).
testSimpleS3ClosedConnectionReuse :: TestM ()
testSimpleS3ClosedConnectionReuse = go >> wait >> go
  where
    wait = liftIO $ putStrLn "Waiting for S3 idle timeout ..." >> threadDelay 7000000
    go = do
      uid <- liftIO $ Id <$> nextRandom
      let sets = V3.defAssetSettings & set V3.setAssetRetention (Just V3.AssetVolatile)
      let part2 = (MIME.Type (MIME.Text "plain") [], C8.replicate 100000 'c')
      uploadSimple (path "/assets/v3") uid sets part2
        !!! const 201 === statusCode

--------------------------------------------------------------------------------
-- Client compatibility tests

-- Since the other tests use functions from the server code, it can happen that
-- an API change also changes the requests made here in the tests.
-- This test tries to prevent us from breaking the API without noticing.
--
-- The body is taken directly from a request made by the web app
-- (just replaced the content with a shorter one and updated the MD5 header).
testUploadCompatibility :: TestM ()
testUploadCompatibility = do
  uid <- liftIO $ Id <$> nextRandom
  -- Initial upload
  r1 <-
    uploadRaw (path "/assets/v3") uid exampleMultipart
      <!! const 201 === statusCode
  let loc = decodeHeaderOrFail "Location" r1 :: ByteString
  -- Lookup and download via redirect.
  r2 <-
    downloadAsset uid loc () <!! do
      const 302 === statusCode
      const Nothing === responseBody
  r3 <- flip get' id =<< parseUrlThrow (C8.unpack (getHeader' "Location" r2))
  liftIO $ do
    assertEqual "status" HTTP.status200 (responseStatus r3)
    assertEqual "content-type mismatch" (Just applicationOctetStream) (getContentType r3)
    assertEqual "user mismatch" uid (decodeHeaderOrFail "x-amz-meta-user" r3)
    assertEqual "data mismatch" (Just "test") (responseBody r3)
  where
    exampleMultipart :: LByteString
    exampleMultipart =
      "--FrontierIyj6RcVrqMcxNtMEWPsNpuPm325QsvWQ\r\n\
      \Content-Type: application/json;charset=utf-8\r\n\
      \Content-length: 37\r\n\
      \\r\n\
      \{\"public\":true,\"retention\":\"eternal\"}\r\n\
      \--FrontierIyj6RcVrqMcxNtMEWPsNpuPm325QsvWQ\r\n\
      \Content-Type: application/octet-stream\r\n\
      \Content-length: 4\r\n\
      \Content-MD5: CY9rzUYh03PK3k6DJie09g==\r\n\
      \\r\n\
      \test\r\n\
      \--FrontierIyj6RcVrqMcxNtMEWPsNpuPm325QsvWQ--\r\n\
      \\r\n"

--------------------------------------------------------------------------------
-- Federation behaviour

testRemoteDownloadWrongDomain :: TestM ()
testRemoteDownloadWrongDomain = do
  assetId <- liftIO $ Id <$> nextRandom
  uid <- liftIO $ Id <$> nextRandom

  let key = AssetKeyV3 assetId AssetPersistent
      qkey = Qualified key (Domain "invalid.example.com")
  downloadAsset uid qkey () !!! do
    const 422 === statusCode

testRemoteDownloadNoAsset :: TestM ()
testRemoteDownloadNoAsset = do
  assetId <- liftIO $ Id <$> nextRandom
  uid <- liftIO $ Id <$> nextRandom
  let key = AssetKeyV3 assetId AssetPersistent
      qkey = Qualified key (Domain "faraway.example.com")
      respond req
        | frRPC req == "get-asset" =
          pure ("application" // "json", Aeson.encode (GetAssetResponse False))
        | otherwise =
          throw
            . MockErrorResponse HTTP.status404
            . LText.decodeUtf8With Text.lenientDecode
            . Aeson.encode
            $ assetNotFound
  (_, reqs) <- withMockFederator respond $ do
    downloadAsset uid qkey () !!! do
      const 404 === statusCode
  liftIO $
    reqs
      @?= [ FederatedRequest
              { frOriginDomain = Domain "example.com",
                frTargetDomain = Domain "faraway.example.com",
                frComponent = Cargohold,
                frRPC = "get-asset",
                frBody = Aeson.encode (GetAsset uid key Nothing)
              }
          ]

testRemoteDownload :: TestM ()
testRemoteDownload = do
  assetId <- liftIO $ Id <$> nextRandom
  uid <- liftIO $ Id <$> nextRandom

  let key = AssetKeyV3 assetId AssetPersistent
      qkey = Qualified key (Domain "faraway.example.com")
      respond req
        | frRPC req == "get-asset" =
          pure ("application" // "json", Aeson.encode (GetAssetResponse True))
        | otherwise = pure ("application" // "octet-stream", "asset content")
  void $
    withMockFederator respond $ do
      downloadAsset uid qkey () !!! do
        const 200 === statusCode
        const (Just "asset content") === responseBody
