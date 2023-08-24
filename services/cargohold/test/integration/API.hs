{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

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
import CargoHold.API.Error
import CargoHold.Options (aws, s3DownloadEndpoint)
import CargoHold.Types
import qualified CargoHold.Types.V3 as V3
import qualified Codec.MIME.Type as MIME
import Control.Exception (throw)
import Control.Lens hiding (sets)
import qualified Data.Aeson as Aeson
import Data.ByteString.Builder
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
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Media ((//))
import qualified Network.HTTP.Types as HTTP
import Network.Wai.Utilities (Error (label))
import qualified Network.Wai.Utilities.Error as Wai
import Test.Tasty
import Test.Tasty.HUnit
import TestSetup
import Util.Options
import Wire.API.Federation.API.Cargohold
import Wire.API.Federation.Component

tests :: IO TestSetup -> TestTree
tests s =
  testGroup
    "API Integration"
    [ testGroup
        "simple"
        [ test s "roundtrip" testSimpleRoundtrip,
          test s "download with accept header" testDownloadWithAcceptHeader,
          test s "tokens" testSimpleTokens,
          test s "s3-upstream-closed" testSimpleS3ClosedConnectionReuse,
          test s "client-compatibility" testUploadCompatibility,
          test s "download url override" testDownloadURLOverride
        ],
      testGroup
        "remote"
        [ test s "remote download wrong domain" testRemoteDownloadWrongDomain,
          test s "remote download no asset" testRemoteDownloadNoAsset,
          test s "federator failure on remote download" testRemoteDownloadFederationFailure,
          test s "remote download" (testRemoteDownload "asset content"),
          test s "large remote download" $
            testRemoteDownload
              ( toLazyByteString
                  (mconcat (replicate 20000 (byteString "hello world\n")))
              )
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
      when (isJust $ V3.assetRetentionSeconds =<< (sets ^. V3.setAssetRetention)) $ do
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

testDownloadWithAcceptHeader :: TestM ()
testDownloadWithAcceptHeader = do
  assetId <- liftIO $ Id <$> nextRandom
  uid <- liftIO $ Id <$> nextRandom
  domain <- viewFederationDomain
  let key = AssetKeyV3 assetId AssetPersistent
      qkey = Qualified key domain
  downloadAssetWith (header "Accept" "image/jpeg") uid qkey ()
    !!! const 404 === statusCode

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

testDownloadURLOverride :: TestM ()
testDownloadURLOverride = do
  -- This is a .example domain, it shouldn't resolve. But it is also not
  -- supposed to be used by cargohold to make connections.
  let downloadEndpoint = "external-s3-url.example"
  withSettingsOverrides (aws . s3DownloadEndpoint ?~ AWSEndpoint downloadEndpoint True 443) $ do
    uid <- liftIO $ Id <$> nextRandom

    -- Upload, should work, shouldn't try to use the S3DownloadEndpoint
    let bdy = (applicationText, "Hello World")
    uploadRes <-
      uploadSimple (path "/assets/v3") uid V3.defAssetSettings bdy
        <!! const 201 === statusCode
    let loc = decodeHeaderOrFail "Location" uploadRes :: ByteString
    let Just ast = responseJsonMaybe @V3.Asset uploadRes
    let Just tok = view V3.assetToken ast

    -- Lookup with token and get download URL. Should return the
    -- S3DownloadEndpoint, but not try to use it.
    downloadURLRes <-
      downloadAsset uid loc (Just tok) <!! do
        const 302 === statusCode
        const Nothing === responseBody
    downloadURL <- parseUrlThrow (C8.unpack (getHeader' "Location" downloadURLRes))
    liftIO $ do
      assertEqual "download host" downloadEndpoint (HTTP.host downloadURL)
      assertEqual "download port" 443 (HTTP.port downloadURL)
      assertEqual "download secure" True (HTTP.secure downloadURL)

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
  localDomain <- viewFederationDomain
  liftIO $
    reqs
      @?= [ FederatedRequest
              { frOriginDomain = localDomain,
                frTargetDomain = Domain "faraway.example.com",
                frComponent = Cargohold,
                frRPC = "get-asset",
                frBody = Aeson.encode (GetAsset uid key Nothing)
              }
          ]

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
    Wai.message resp @?= "mock error"

testRemoteDownload :: LByteString -> TestM ()
testRemoteDownload assetContent = do
  assetId <- liftIO $ Id <$> nextRandom
  uid <- liftIO $ Id <$> nextRandom

  let key = AssetKeyV3 assetId AssetPersistent
      qkey = Qualified key (Domain "faraway.example.com")
      respond req
        | frRPC req == "get-asset" =
            pure ("application" // "json", Aeson.encode (GetAssetResponse True))
        | otherwise = pure ("application" // "octet-stream", assetContent)
  (_, reqs) <- withMockFederator respond $ do
    downloadAsset uid qkey () !!! do
      const 200 === statusCode
      const (Just assetContent) === responseBody

  localDomain <- viewFederationDomain
  let ga = Aeson.encode (GetAsset uid key Nothing)
  liftIO $
    reqs
      @?= [ FederatedRequest
              { frOriginDomain = localDomain,
                frTargetDomain = Domain "faraway.example.com",
                frComponent = Cargohold,
                frRPC = "get-asset",
                frBody = ga
              },
            FederatedRequest
              { frOriginDomain = localDomain,
                frTargetDomain = Domain "faraway.example.com",
                frComponent = Cargohold,
                frRPC = "stream-asset",
                frBody = ga
              }
          ]
