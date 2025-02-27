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

module Test.Cargohold.API.V3 where

import API.Cargohold
import Codec.MIME.Type (showMIMEType)
import Crypto.Random
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import Data.ByteString.Builder
import qualified Data.ByteString.Char8 as C8
import Data.CaseInsensitive
import Data.String.Conversions
import Data.Text.Encoding
import Data.Time.Clock (UTCTime)
import Data.Time.Format
import Data.Time.Format.ISO8601
import Network.HTTP.Client
import SetupHelpers
import Test.Cargohold.API.Util
import Testlib.Prelude

--------------------------------------------------------------------------------
-- Simple (single-step) uploads

testSimpleRoundtrip :: (HasCallStack) => App ()
testSimpleRoundtrip = do
  let defSettings = ["public" .= False]
      rets = ["eternal", "persistent", "volatile", "eternal-infrequent_access", "expiring"]
      allSets = fmap object $ (defSettings :) $ (\r -> ["retention" .= r]) <$> rets
  mapM_ simpleRoundtrip allSets
  where
    simpleRoundtrip :: (HasCallStack) => Value -> App ()
    simpleRoundtrip sets = do
      uid <- randomUser OwnDomain def
      uid2 <- randomUser OwnDomain def
      -- Initial upload
      let bdy = (applicationText, cs "Hello World")
      r1 <- uploadSimple uid sets bdy
      r1.status `shouldMatchInt` 201
      -- use v3 path instead of the one returned in the header
      (key, tok, expires) <-
        (,,)
          <$> r1.json
          %. "key"
          <*> (r1.json %. "token" >>= asString)
          <*> (lookupField r1.json "expires" >>= maybe (pure Nothing) (fmap pure . asString))
      -- Check mandatory Date header
      let Just date = C8.unpack <$> lookup (mk $ cs "Date") r1.headers
          parseTime = parseTimeOrError False defaultTimeLocale rfc822DateFormat
          parseTimeIso t = fromMaybe (error $ "Could not parse \"" <> t <> "\" as ISO8601") $ formatParseM (iso8601Format @UTCTime) t
          utc = parseTime date :: UTCTime
          expires' = parseTimeIso <$> expires :: Maybe UTCTime
      -- Potentially check for the expires header
      case sets of
        -- We don't care what the rentention value is here,
        -- we're just checking to see if other checks need
        -- to be done.
        Object o -> case KM.lookup (fromString "retention") o of
          Nothing -> pure ()
          Just r -> do
            r' <- asString r
            -- These retention policies never expire, so an expiration date isn't sent back
            unless (r' == "eternal" || r' == "persistent" || r' == "eternal-infrequent_access")
              $ assertBool "invalid expiration" (Just utc < expires')
        _ -> pure ()
      -- Lookup with token and download via redirect.
      r2 <- downloadAsset' uid r1.jsonBody tok
      r2.status `shouldMatchInt` 302
      assertBool "Response body should be empty" $ r2.body == mempty

      let locReq = C8.unpack (getHeader' (mk $ cs "Location") r2)
      req <- liftIO $ parseRequest locReq
      r3 <- submit "GET" req
      r3.status `shouldMatchInt` 200
      assertBool "content-type should always be application/octet-stream"
        $ getHeader (mk $ cs "content-type") r3
        == Just (encodeUtf8 $ showMIMEType applicationOctetStream)
      assertBool "Token mismatch" $ getHeader (mk $ cs "x-amz-meta-token") r3 == pure (cs tok)
      uid' <- uid %. "id" >>= asString
      assertBool "User mismatch" $ getHeader (mk $ cs "x-amz-meta-user") r3 == pure (cs uid')
      assertBool "Data mismatch" $ r3.body == cs "Hello World"
      -- Delete (forbidden for other users)
      deleteAssetV3 uid2 r1.jsonBody >>= \r -> r.status `shouldMatchInt` 403
      -- Delete (allowed for creator)
      deleteAssetV3 uid r1.jsonBody >>= \r -> r.status `shouldMatchInt` 200
      r4 <- downloadAsset' uid key tok
      r4.status `shouldMatchInt` 404
      let Just date' = C8.unpack <$> lookup (mk $ cs "Date") r4.headers
      let utc' = parseTimeOrError False defaultTimeLocale rfc822DateFormat date' :: UTCTime
      assertBool "bad date" (utc' >= utc)

-- | Simulates an interrupted upload, where the user sends less data than expected.
testUploadWrongContentLength :: (HasCallStack) => App ()
testUploadWrongContentLength = do
  uid <- randomUser OwnDomain def
  let size = 1024 * 1024
  bs :: LazyByteString <- BS.fromStrict <$> (liftIO . getRandomBytes) size
  let -- size + 17 would be sufficient, though I cannot really explain this
      -- number (16 + 1, probably.) The control sequence would be only 4 bytes,
      -- leaving 12 bytes to be explained. Adding some overhead for future
      -- stability.
      contentLengthDelta = size + 1024
      settings = object ["public" .= False, "retention" .= "volatile"]
      body =
        toLazyByteString
          $ beginMultipartBody settings applicationOctetStream' (fromIntegral contentLengthDelta)
          <> lazyByteString bs
          <> endMultipartBody'

  uploadRaw uid body >>= \resp -> do
    resp.status `shouldMatchInt` 400
    resp.jsonBody %. "label" `shouldMatch` "incomplete-body"
