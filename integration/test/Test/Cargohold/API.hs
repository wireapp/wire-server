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

module Test.Cargohold.API where

import API.Cargohold
import qualified Codec.MIME.Type as MIME
import Control.Lens hiding (sets, (.=))
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (Pair)
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as LBS hiding (replicate)
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.CaseInsensitive (mk)
import Data.String.Conversions
import Data.Time (UTCTime, defaultTimeLocale, parseTimeOrError, rfc822DateFormat)
import Data.Time.Format.ISO8601 (formatParseM, iso8601Format)
import Network.HTTP.Client (parseUrlThrow)
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types as HTTP
import SetupHelpers (randomId, randomUser)
import Test.Cargohold.API.Util
import Testlib.Prelude
import UnliftIO.Concurrent

--------------------------------------------------------------------------------
-- Simple (single-step) uploads

testSimpleRoundtrip :: (HasCallStack) => App ()
testSimpleRoundtrip = do
  let def' = ["public" .= False]
      rets = ["eternal", "persistent", "volatile", "eternal-infrequent_access", "expiring"]
      sets' = fmap object $ def' : fmap (\r -> "retention" .= r : def') rets
  mapM_ simpleRoundtrip sets'
  where
    simpleRoundtrip :: (HasCallStack) => Value -> App ()
    simpleRoundtrip sets = do
      uid <- randomUser OwnDomain def
      userId1 <- uid %. "id" & asString
      uid2 <- randomUser OwnDomain def
      -- Initial upload
      let bdy = (applicationText, cs "Hello World")
      r1 <- uploadSimple uid sets bdy
      r1.status `shouldMatchInt` 201
      loc <- maybe (error "Could not find the Location header") (pure . cs @_ @String) $ lookup (mk $ cs "Location") r1.headers
      (tok, expires) <-
        (,)
          <$> asString (r1.json %. "token")
          <*> (lookupField r1.json "expires" >>= maybe (pure Nothing) (fmap pure . asString))
      -- Check mandatory Date header
      let Just date = C8.unpack <$> lookup (mk $ cs "Date") r1.headers
          utc = parseTimeOrError False defaultTimeLocale rfc822DateFormat date :: UTCTime
          parseTimeIso t = fromMaybe (error $ "Could not parse \"" <> t <> "\" as ISO8601") $ formatParseM (iso8601Format @UTCTime) t
          expires' = parseTimeIso <$> expires :: Maybe UTCTime
      -- Potentially check for the expires header
      case sets of
        Object o -> case KM.lookup (fromString "retention") o of
          Nothing -> pure ()
          Just r -> do
            r' <- asString r
            -- These retention policies never expire, so an expiration date isn't sent back
            unless (r' == "eternal" || r' == "persistent" || r' == "eternal-infrequent_access")
              $ assertBool "invalid expiration" (Just utc < expires')
        _ -> pure ()
      -- Lookup with token and download via redirect.
      r2 <- downloadAsset' uid loc tok
      r2.status `shouldMatchInt` 302
      cs @_ @String r2.body `shouldMatch` ""
      r3 <- flip get' id =<< parseUrlThrow (C8.unpack (getHeader' (mk $ cs "Location") r2))
      r3.status `shouldMatchInt` 200
      assertBool "content-type should always be application/octet-stream" $ Just applicationOctetStream == fmap MIME.mimeType (getContentType r3)
      assertBool "token mismatch" $ tok == decodeHeaderOrFail (mk $ cs "x-amz-meta-token") r3
      assertBool "user mismatch" $ userId1 == decodeHeaderOrFail (mk $ cs "x-amz-meta-user") r3
      assertBool "data mismatch" $ cs "Hello World" == r3.body
      -- Delete (forbidden for other users)
      deleteAsset uid2 r1.jsonBody >>= \r -> r.status `shouldMatchInt` 403
      -- Delete (allowed for creator)
      deleteAsset uid r1.jsonBody >>= \r -> r.status `shouldMatchInt` 200
      r4 <- downloadAsset' uid loc tok
      r4.status `shouldMatchInt` 404
      let Just date' = C8.unpack <$> lookup (mk $ cs "Date") r4.headers
          utc' = parseTimeOrError False defaultTimeLocale rfc822DateFormat date' :: UTCTime
      assertBool "bad date" (utc' >= utc)

testDownloadWithAcceptHeader :: (HasCallStack) => App ()
testDownloadWithAcceptHeader = do
  assetId <- randomId
  uid <- randomUser OwnDomain def
  domain <- make OwnDomain
  let key = "3-2-" <> assetId
      qkey = object ["domain" .= domain, "id" .= key]
  res <- downloadAssetWithQualifiedAssetKey (header "Accept" "image/jpeg") uid qkey ()
  res.status `shouldMatchInt` 404

queryItem :: ByteString -> Maybe ByteString -> HTTP.Request -> HTTP.Request
queryItem k v r =
  HTTP.setQueryString ((k, v) : queryItems) r
  where
    queryItems = HTTP.parseQuery $ HTTP.queryString r

get' :: HTTP.Request -> (HTTP.Request -> HTTP.Request) -> App Response
get' r f = submit "GET" $ f r

testSimpleTokens :: (HasCallStack) => App ()
testSimpleTokens = do
  uid <- randomUser OwnDomain def
  uid2 <- randomUser OwnDomain def
  -- Initial upload
  let sets = object ["public" .= False, "retention" .= "volatile"]
      bdy = (applicationText, cs "Hello World")
  r1 <- uploadSimple uid sets bdy
  r1.status `shouldMatchInt` 201
  loc <-
    maybe
      (assertFailure "Could not get \"Location\" header from the request")
      (pure . cs @_ @String)
      $ getHeader (mk $ cs "Location") r1
  (key, tok) <-
    (,)
      <$> asString (r1.json %. "key")
      <*> r1.json
      %. "token"
  -- No access without token from other user (opaque 404)
  downloadAsset' uid2 loc () >>= \r -> r.status `shouldMatchInt` 404
  -- No access with empty token query parameter from other user (opaque 404)
  downloadAsset' uid2 loc (queryItem (cs "asset_token") Nothing) >>= \r -> r.status `shouldMatchInt` 404
  -- No access with wrong token (opaque 404)
  downloadAsset' uid2 loc (header "Asset-Token" "abc123") >>= \r -> r.status `shouldMatchInt` 404
  -- No access with wrong token as query parameter (opaque 404)
  downloadAsset' uid2 loc (queryItem (cs "asset_token") $ pure $ cs "acb123") >>= \r -> r.status `shouldMatchInt` 404
  -- Token renewal fails if not done by owner
  postToken uid2 key >>= \r -> do
    r.status `shouldMatchInt` 403
    label <- traverse ((%. "label") >=> asString) r.jsonBody
    label `shouldMatch` "unauthorised"
  -- Token renewal succeeds if done by owner
  r2 <- postToken uid key
  r2.status `shouldMatchInt` 200
  tok' <- r2.jsonBody %. "token" & asString
  assertBool "token unchanged" (tok /= String (cs tok'))
  -- Download by owner with new token.
  r3 <- downloadAsset' uid loc tok'
  r3.status `shouldMatchInt` 302
  cs @_ @String r3.body `shouldMatch` ""
  r4 <- flip get' id =<< parseUrlThrow (C8.unpack (getHeader' (mk $ cs "Location") r3))
  r4.status `shouldMatchInt` 200
  let r4ContentType :: Maybe String
      r4ContentType = cs @_ @String <$> getHeader (mk $ cs "content-type") r4
  r4ContentType `shouldMatch` Just (cs @_ @String $ MIME.showMIMEType applicationOctetStream)
  let r4Tok :: Maybe String
      r4Tok = cs @_ @String <$> getHeader (mk $ cs "x-amz-meta-token") r4
  r4Tok `shouldMatch` Just tok'
  let r4User :: Maybe String
      r4User = cs @_ @String <$> getHeader (mk $ cs "x-amz-meta-user") r4
  r4User `shouldMatch` fmap Just (uid %. "id")
  cs @_ @String r4.body `shouldMatch` "Hello World"
  -- Verify access without token if the request comes from the creator.
  downloadAsset' uid loc () >>= \r -> r.status `shouldMatchInt` 302
  -- Verify access with new token from a different user.
  downloadAsset' uid2 loc tok' >>= \r -> r.status `shouldMatchInt` 302
  -- Verify access with new token as query parameter from a different user
  downloadAsset' uid2 loc (queryItem (cs "asset_token") (pure $ cs tok')) >>= \r ->
    r.status `shouldMatchInt` 302
  -- Delete Token fails if not done by owner
  deleteToken uid2 key >>= \r -> do
    r.status `shouldMatchInt` 403
    label' <- traverse ((%. "label") >=> asString) r.jsonBody
    label' `shouldMatch` "unauthorised"
  -- Delete Token succeeds by owner
  deleteToken uid key >>= \r -> do
    r.status `shouldMatchInt` 200
    cs @_ @String r.body `shouldMatch` ""
  -- Access without token from different user (asset is now "public")
  downloadAsset' uid2 loc () >>= \r -> do
    r.status `shouldMatchInt` 302
    cs @_ @String r.body `shouldMatch` ""

defAssetSettings' :: [Pair]
defAssetSettings' = ["public" .= False]

defAssetSettings :: Value
defAssetSettings = object defAssetSettings'

-- S3 closes idle connections after ~5 seconds, before the http-client 'Manager'
-- does. If such a closed connection is reused for an upload, no problems should
-- occur (i.e. the closed connection should be detected before sending any data).
testSimpleS3ClosedConnectionReuse :: (HasCallStack) => App ()
testSimpleS3ClosedConnectionReuse = go >> wait >> go
  where
    wait = liftIO $ putStrLn "Waiting for S3 idle timeout ..." >> threadDelay 7000000
    go = do
      uid <- randomUser OwnDomain def
      let sets = object $ defAssetSettings' <> ["retention" .= "volatile"]
      let part2 = (MIME.Text $ cs "plain", cs $ replicate 100000 'c')
      uploadSimple uid sets part2 >>= \r -> r.status `shouldMatchInt` 201

testDownloadURLOverride :: (HasCallStack) => App ()
testDownloadURLOverride = do
  -- This is a .example domain, it shouldn't resolve. But it is also not
  -- supposed to be used by cargohold to make connections.
  let downloadEndpoint = "external-s3-url.example"
      -- Stick the protocol on here, as the checks don't want to see it,
      -- they are just looking for the host name.
      f = setField "aws.s3DownloadEndpoint" ("https://" <> downloadEndpoint)
  startDynamicBackends [def {cargoholdCfg = f}] $ \[d] -> do
    -- withSettingsOverrides (aws . s3DownloadEndpoint ?~ AWSEndpoint downloadEndpoint True 443) $ do
    uid <- randomUser d def
    -- Upload, should work, shouldn't try to use the S3DownloadEndpoint
    let bdy = (applicationText, cs "Hello World")
    uploadRes <- uploadSimple uid defAssetSettings bdy
    uploadRes.status `shouldMatchInt` 201
    let loc = decodeHeaderOrFail (mk $ cs "Location") uploadRes :: String
    (_key, tok, _expires) <-
      (,,)
        <$> uploadRes.json
        %. "key"
        <*> (uploadRes.json %. "token" & asString)
        <*> lookupField uploadRes.json "expires"
    -- Lookup with token and get download URL. Should return the
    -- S3DownloadEndpoint, but not try to use it.
    downloadURLRes <- downloadAsset' uid loc tok
    downloadURLRes.status `shouldMatchInt` 302
    cs @_ @String downloadURLRes.body `shouldMatch` ""
    downloadURL <- parseUrlThrow (C8.unpack (getHeader' (mk $ cs "Location") downloadURLRes))
    downloadEndpoint `shouldMatch` cs @_ @String (HTTP.host downloadURL)
    HTTP.port downloadURL `shouldMatchInt` 443
    True `shouldMatch` (HTTP.secure downloadURL)

--------------------------------------------------------------------------------
-- Client compatibility tests

-- Since the other tests use functions from the server code, it can happen that
-- an API change also changes the requests made here in the tests.
-- This test tries to prevent us from breaking the API without noticing.
--
-- The body is taken directly from a request made by the web app
-- (just replaced the content with a shorter one and updated the MD5 header).
testUploadCompatibility :: (HasCallStack) => App ()
testUploadCompatibility = do
  uid <- randomUser OwnDomain def
  -- Initial upload
  r1 <- uploadRaw uid exampleMultipart
  r1.status `shouldMatchInt` 201
  let locHeader = mk $ cs "Location"
      loc = decodeHeaderOrFail @String locHeader r1
  -- Lookup and download via redirect.
  r2 <- downloadAsset' uid loc ()
  r2.status `shouldMatchInt` 302
  cs @_ @String r2.body `shouldMatch` ""
  r3 <- flip get' id =<< parseUrlThrow (C8.unpack (getHeader' locHeader r2))
  r3.status `shouldMatchInt` 200
  assertBool "Content types should match" $ getContentType r3 == Just applicationOctetStream'
  decodeHeaderOrFail @String (mk $ cs "x-amz-meta-user") r3 `shouldMatch` (uid %. "id")
  cs @_ @String r3.body `shouldMatch` Just "test"
  where
    exampleMultipart :: LBS.ByteString
    exampleMultipart =
      cs
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

testRemoteDownloadWrongDomain :: (HasCallStack) => App ()
testRemoteDownloadWrongDomain = do
  assetId <- randomId
  uid <- randomUser OwnDomain def
  let key = toJSON $ "3-2-" <> assetId
      qkey =
        object
          [ "key" .= key,
            "domain" .= "invalid.example.com"
          ]
  res <- downloadAsset' uid qkey ()
  res.status `shouldMatchInt` 422

testRemoteDownloadNoAsset :: (HasCallStack) => App ()
testRemoteDownloadNoAsset = do
  assetId <- randomId
  uid <- randomUser OwnDomain def
  otherDomain <- make OtherDomain & asString
  let key = "3-2-" <> assetId
      qkey =
        object
          [ "domain" .= otherDomain,
            "key" .= key
          ]
  res <- downloadAsset' uid qkey ()
  res.status `shouldMatchInt` 404

testRemoteDownloadShort :: (HasCallStack) => App ()
testRemoteDownloadShort = remoteDownload "asset content"

testRemoteDownloadLong :: (HasCallStack) => App ()
testRemoteDownloadLong = remoteDownload $ concat $ replicate 20000 $ "hello world\n"

remoteDownload :: (HasCallStack, ConvertibleStrings a L8.ByteString, ConvertibleStrings a String) => a -> App ()
remoteDownload content = do
  uid1 <- randomUser OwnDomain def
  uid2 <- randomUser OtherDomain def
  r1 <- uploadSimple uid1 settings (applicationOctetStream, cs content)
  r1.status `shouldMatchInt` 201
  let locHeader = mk $ cs "Location"
      loc = decodeHeaderOrFail @String locHeader r1
  -- Lookup and download via redirect.
  r2 <- downloadAsset' uid2 loc ()
  r2.status `shouldMatchInt` 200
  assertBool "Content types should match" $ getContentType r2 == Just applicationOctetStream'
  -- decodeHeaderOrFail @String (mk $ cs "x-amz-meta-user") r3 `shouldMatch` (uid %. "id")
  cs @_ @String r2.body `shouldMatch` Just (cs content :: String)
  where
    settings = object ["public" .= True]
