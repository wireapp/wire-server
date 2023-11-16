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

import Control.Lens hiding (sets, (.=))
import Data.ByteString.Char8 qualified as C8
import Data.Time.Clock
import Data.Time.Format
import Network.HTTP.Client (parseUrlThrow, requestBody)
import Network.HTTP.Types.Status (status200)
import SetupHelpers
import Test.Cargohold.API.Util
import Testlib.Prelude
import Testlib.Types

--------------------------------------------------------------------------------
-- Simple (single-step) uploads

testSimpleRoundtrip :: HasCallStack => App ()
testSimpleRoundtrip = do
  let defSettings =
        [ "public" .= False
        ]
  let rets = ["eternal", "persistent", "volatile", "eternal-infrequent_access", "expiring"]
  let sets =
        fmap object $
          defSettings : map (\r -> defSettings <> ["retention" .= r]) rets
  mapM_ simpleRoundtrip sets
  where
    simpleRoundtrip sets = do
      uid <- randomUser OwnDomain def
      uid2 <- randomId
      -- Initial upload
      let bdy = (applicationText, "Hello World")
      r1 <-
        uploadSimple uid sets bdy
          <!! const 201
          === statusCode
      -- use v3 path instead of the one returned in the header
      (key, tok, expires) <-
        (,,)
          <$> r1.json %. "key"
          <*> r1.json %. "token"
          <*> r1.json %. "expires"
      -- Check mandatory Date header
      let Just date = C8.unpack <$> lookup "Date" (responseHeaders r1)
      let utc = parseTimeOrError False defaultTimeLocale rfc822DateFormat date :: UTCTime
      -- Potentially check for the expires header
      when (isJust $ assetRetentionSeconds =<< (sets ^. setAssetRetention)) $ do
        liftIO $ assertBool "invalid expiration" (Just utc < expires)
      -- Lookup with token and download via redirect.
      r2 <-
        downloadAsset uid key (Just tok) <!! do
          const 302 === statusCode
          const Nothing === responseBody
      r3 <- flip get' id =<< parseUrlThrow (C8.unpack (getHeader' "Location" r2))
      liftIO $ do
        assertEqual "status" status200 (responseStatus r3)
        assertEqual "content-type should always be application/octet-stream" (Just applicationOctetStream) (getContentType r3)
        assertEqual "token mismatch" tok (decodeHeaderOrFail "x-amz-meta-token" r3)
        assertEqual "user mismatch" uid (decodeHeaderOrFail "x-amz-meta-user" r3)
        assertEqual "data mismatch" (Just "Hello World") (responseBody r3)
      -- Delete (forbidden for other users)
      deleteAssetV3 uid2 key !!! const 403 === statusCode
      -- Delete (allowed for creator)
      deleteAssetV3 uid key !!! const 200 === statusCode
      r4 <-
        downloadAsset uid key (Just tok)
          <!! const 404
          === statusCode
      let Just date' = C8.unpack <$> lookup "Date" (responseHeaders r4)
      let utc' = parseTimeOrError False defaultTimeLocale rfc822DateFormat date' :: UTCTime
      liftIO $ assertBool "bad date" (utc' >= utc)
