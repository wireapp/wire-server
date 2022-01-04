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

module API.V3 (tests) where

import API.Util
import Bilge hiding (body)
import Bilge.Assert
import Control.Lens hiding (sets)
import qualified Data.ByteString.Char8 as C8
import Data.ByteString.Conversion
import Data.Id
import Data.Qualified
import Data.Time.Clock
import Data.Time.Format
import Data.UUID.V4
import Imports hiding (head)
import Network.HTTP.Client (parseUrlThrow)
import Network.HTTP.Types.Status (status200)
import Test.Tasty
import Test.Tasty.HUnit
import TestSetup
import Wire.API.Asset

tests :: IO TestSetup -> TestTree
tests s =
  testGroup
    "API Integration v3"
    [ testGroup
        "simple"
        [test s "roundtrip using v3 API" testSimpleRoundtrip]
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
      -- use v3 path instead of the one returned in the header
      let Just ast = responseJsonMaybe @Asset r1
      let loc = "/assets/v3/" <> toByteString' (qUnqualified (ast ^. assetKey))
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
        assertEqual "token mismatch" tok (decodeHeaderOrFail "x-amz-meta-token" r3)
        assertEqual "user mismatch" uid (decodeHeaderOrFail "x-amz-meta-user" r3)
        assertEqual "data mismatch" (Just "Hello World") (responseBody r3)
      -- Delete (forbidden for other users)
      deleteAssetV3 c uid2 (view assetKey ast) !!! const 403 === statusCode
      -- Delete (allowed for creator)
      deleteAssetV3 c uid (view assetKey ast) !!! const 200 === statusCode
      r4 <-
        get (c . path loc . zUser uid . header "Asset-Token" (toByteString' tok) . noRedirect)
          <!! const 404 === statusCode
      let Just date' = C8.unpack <$> lookup "Date" (responseHeaders r4)
      let utc' = parseTimeOrError False defaultTimeLocale rfc822DateFormat date' :: UTCTime
      liftIO $ assertBool "bad date" (utc' >= utc)
