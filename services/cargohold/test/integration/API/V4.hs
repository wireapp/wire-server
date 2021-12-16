{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

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

module API.V4 (tests) where

import Bilge hiding (body)
import Bilge.Assert
import qualified Codec.MIME.Parse as MIME
import qualified Codec.MIME.Type as MIME
import Control.Lens hiding (sets)
import Data.ByteString.Builder
import qualified Data.ByteString.Char8 as C8
import Data.ByteString.Conversion
import qualified Data.ByteString.Lazy as Lazy
import Data.Id
import Data.Qualified
import Data.Text.Encoding (decodeLatin1)
import Data.Time.Clock
import Data.Time.Format
import qualified Data.UUID as UUID
import Data.UUID.V4
import Imports hiding (head)
import Network.HTTP.Client (parseUrlThrow)
import Network.HTTP.Types.Header
import Network.HTTP.Types.Method
import Network.HTTP.Types.Status (status200)
import Network.Wai.Utilities (Error (label))
import Test.Tasty
import Test.Tasty.HUnit
import TestSetup
import Wire.API.Asset

tests :: IO TestSetup -> TestTree
tests s =
  testGroup
    "API Integration v4"
    [ testGroup
        "simple"
        [ test s "roundtrip" testSimpleRoundtrip,
          test s "tokens" testSimpleTokens,
          test s "s3-upstream-closed" testSimpleS3ClosedConnectionReuse,
          test s "client-compatibility" testUploadCompatibility
        ]
    ]

--------------------------------------------------------------------------------
-- Simple (single-step) uploads

testSimpleRoundtrip :: TestSignature ()
testSimpleRoundtrip c = do
  let def = defAssetSettings
  let rets = [minBound ..]
  let sets = def : map (\r -> def & setAssetRetention ?~ r) rets
  mapM_ simpleRoundtrip sets
  where
    simpleRoundtrip sets = do
      uid <- liftIO $ Id <$> nextRandom
      uid2 <- liftIO $ Id <$> nextRandom
      -- Initial upload
      let bdy = (applicationText, "Hello World")
      r1 <-
        uploadSimple (c . path "/assets/v3") uid sets bdy
          <!! const 201 === statusCode
      let loc = decodeHeader "Location" r1 :: ByteString
      let Just ast = responseJsonMaybe @Asset r1
      let Just tok = view assetToken ast
      -- Check mandatory Date header
      let Just date = C8.unpack <$> lookup "Date" (responseHeaders r1)
      let utc = parseTimeOrError False defaultTimeLocale rfc822DateFormat date :: UTCTime
      -- Potentially check for the expires header
      when (isJust $ join (assetRetentionSeconds <$> (sets ^. setAssetRetention))) $ do
        liftIO $ assertBool "invalid expiration" (Just utc < view assetExpires ast)
      -- Lookup with token and download via redirect.
      r2 <-
        get (c . path loc . zUser uid . header "Asset-Token" (toByteString' tok) . noRedirect) <!! do
          const 302 === statusCode
          const Nothing === responseBody
      r3 <- flip get' id =<< parseUrlThrow (C8.unpack (getHeader' "Location" r2))
      liftIO $ do
        assertEqual "status" status200 (responseStatus r3)
        assertEqual "content-type should always be application/octet-stream" (Just applicationOctetStream) (getContentType r3)
        assertEqual "token mismatch" tok (decodeHeader "x-amz-meta-token" r3)
        assertEqual "user mismatch" uid (decodeHeader "x-amz-meta-user" r3)
        assertEqual "data mismatch" (Just "Hello World") (responseBody r3)
      -- Delete (forbidden for other users)
      deleteAsset c uid2 (view assetKey ast) !!! const 403 === statusCode
      -- Delete (allowed for creator)
      deleteAsset c uid (view assetKey ast) !!! const 200 === statusCode
      r4 <-
        get (c . path loc . zUser uid . header "Asset-Token" (toByteString' tok) . noRedirect)
          <!! const 404 === statusCode
      let Just date' = C8.unpack <$> lookup "Date" (responseHeaders r4)
      let utc' = parseTimeOrError False defaultTimeLocale rfc822DateFormat date' :: UTCTime
      liftIO $ assertBool "bad date" (utc' >= utc)

testSimpleTokens :: TestSignature ()
testSimpleTokens c = do
  uid <- liftIO $ Id <$> nextRandom
  uid2 <- liftIO $ Id <$> nextRandom
  -- Initial upload
  let sets = defAssetSettings & set setAssetRetention (Just AssetVolatile)
  let bdy = (applicationText, "Hello World")
  r1 <-
    uploadSimple (c . path "/assets/v3") uid sets bdy
      <!! const 201 === statusCode
  let loc = decodeHeader "Location" r1 :: ByteString
  let Just ast = responseJsonMaybe @Asset r1
  let key = view assetKey ast
  let Just tok = view assetToken ast
  -- No access without token from other user (opaque 404)
  get (c . path loc . zUser uid2 . noRedirect)
    !!! const 404 === statusCode
  -- No access with wrong token (opaque 404)
  get (c . path loc . zUser uid2 . header "Asset-Token" "acb123" . noRedirect)
    !!! const 404 === statusCode
  -- Token renewal fails if not done by owner
  post (c . paths ["assets", "v3", toByteString' (qUnqualified key), "token"] . zUser uid2) !!! do
    const 403 === statusCode
    const (Just "unauthorised") === fmap label . responseJsonMaybe
  -- Token renewal succeeds if done by owner
  r2 <-
    post (c . paths ["assets", "v3", toByteString' (qUnqualified key), "token"] . zUser uid)
      <!! const 200 === statusCode
  let Just tok' = newAssetToken <$> responseJsonMaybe r2
  liftIO $ assertBool "token unchanged" (tok /= tok')
  -- Download by owner with new token.
  r3 <-
    get (c . path loc . zUser uid . header "Asset-Token" (toByteString' tok') . noRedirect) <!! do
      const 302 === statusCode
      const Nothing === responseBody
  r4 <- flip get' id =<< parseUrlThrow (C8.unpack (getHeader' "Location" r3))
  liftIO $ do
    assertEqual "status" status200 (responseStatus r4)
    assertEqual "content-type should always be application/octet-stream" (Just applicationOctetStream) (getContentType r4)
    assertEqual "token mismatch" tok' (decodeHeader "x-amz-meta-token" r4)
    assertEqual "user mismatch" uid (decodeHeader "x-amz-meta-user" r4)
    assertEqual "data mismatch" (Just "Hello World") (responseBody r4)
  -- Verify access without token if the request comes from the creator.
  get (c . path loc . zUser uid . noRedirect)
    !!! const 302 === statusCode
  -- Verify access with new token from a different user.
  get (c . path loc . header "Asset-Token" (toByteString' tok') . zUser uid2 . noRedirect)
    !!! const 302 === statusCode
  -- Delete Token fails if not done by owner
  delete (c . paths ["assets", "v3", toByteString' (qUnqualified key), "token"] . zUser uid2) !!! do
    const 403 === statusCode
    const (Just "unauthorised") === fmap label . responseJsonMaybe
  -- Delete Token succeeds by owner
  delete (c . paths ["assets", "v3", toByteString' (qUnqualified key), "token"] . zUser uid) !!! do
    const 200 === statusCode
    const Nothing === responseBody
  -- Access without token from different user (asset is now "public")
  get (c . path loc . noRedirect . zUser uid2) !!! do
    const 302 === statusCode
    const Nothing === responseBody

-- S3 closes idle connections after ~5 seconds, before the http-client 'Manager'
-- does. If such a closed connection is reused for an upload, no problems should
-- occur (i.e. the closed connection should be detected before sending any data).
testSimpleS3ClosedConnectionReuse :: TestSignature ()
testSimpleS3ClosedConnectionReuse c = go >> wait >> go
  where
    wait = liftIO $ putStrLn "Waiting for S3 idle timeout ..." >> threadDelay 7000000
    go = do
      uid <- liftIO $ Id <$> nextRandom
      let sets = defAssetSettings & set setAssetRetention (Just AssetVolatile)
      let part2 = (MIME.Type (MIME.Text "plain") [], C8.replicate 100000 'c')
      uploadSimple (c . path "/assets/v3") uid sets part2
        !!! const 201 === statusCode

--------------------------------------------------------------------------------
-- Client compatibility tests

-- Since the other tests use functions from the server code, it can happen that
-- an API change also changes the requests made here in the tests.
-- This test tries to prevent us from breaking the API without noticing.
--
-- The body is taken directly from a request made by the web app
-- (just replaced the content with a shorter one and updated the MD5 header).
testUploadCompatibility :: TestSignature ()
testUploadCompatibility c = do
  uid <- liftIO $ Id <$> nextRandom
  -- Initial upload
  r1 <-
    uploadRaw (c . path "/assets/v3") uid exampleMultipart
      <!! const 201 === statusCode
  let loc = decodeHeader "Location" r1 :: ByteString
  -- Lookup and download via redirect.
  r2 <-
    get (c . path loc . zUser uid . noRedirect) <!! do
      const 302 === statusCode
      const Nothing === responseBody
  r3 <- flip get' id =<< parseUrlThrow (C8.unpack (getHeader' "Location" r2))
  liftIO $ do
    assertEqual "status" status200 (responseStatus r3)
    assertEqual "content-type mismatch" (Just applicationOctetStream) (getContentType r3)
    assertEqual "user mismatch" uid (decodeHeader "x-amz-meta-user" r3)
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

-- API Calls ------------------------------------------------------------------

uploadSimple ::
  CargoHold ->
  UserId ->
  AssetSettings ->
  (MIME.Type, ByteString) ->
  Http (Response (Maybe Lazy.ByteString))
uploadSimple c usr sets (ct, bs) =
  let mp = buildMultipartBody sets ct (Lazy.fromStrict bs)
   in uploadRaw c usr (toLazyByteString mp)

uploadRaw ::
  CargoHold ->
  UserId ->
  Lazy.ByteString ->
  Http (Response (Maybe Lazy.ByteString))
uploadRaw c usr bs =
  post $
    c
      . method POST
      . zUser usr
      . zConn "conn"
      . content "multipart/mixed"
      . lbytes bs

deleteAsset :: CargoHold -> UserId -> Qualified AssetKey -> Http (Response (Maybe Lazy.ByteString))
deleteAsset c u k = delete $ c . zUser u . paths ["assets", "v3", toByteString' (qUnqualified k)]

-- Utilities ------------------------------------------------------------------

decodeHeader :: FromByteString a => HeaderName -> Response b -> a
decodeHeader h =
  fromMaybe (error $ "decodeHeader: missing or invalid header: " ++ show h)
    . fromByteString
    . getHeader' h

getContentType :: Response a -> Maybe MIME.Type
getContentType = MIME.parseContentType . decodeLatin1 . getHeader' "Content-Type"

applicationText :: MIME.Type
applicationText = MIME.Type (MIME.Application "text") []

applicationOctetStream :: MIME.Type
applicationOctetStream = MIME.Type (MIME.Application "octet-stream") []

zUser :: UserId -> Request -> Request
zUser = header "Z-User" . UUID.toASCIIBytes . toUUID

zConn :: ByteString -> Request -> Request
zConn = header "Z-Connection"
