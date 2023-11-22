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
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Char8 qualified as C8
import Data.CaseInsensitive
import Data.String.Conversions
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock (UTCTime)
import Data.Time.Format
import Network.HTTP.Client
import SetupHelpers
import Test.Cargohold.API.Util
import Testlib.Prelude

--------------------------------------------------------------------------------
-- Simple (single-step) uploads

testSimpleRoundtrip :: HasCallStack => App ()
testSimpleRoundtrip = do
  let defSettings =
        [ "public" .= False
        ]
  let rets = ["eternal", "persistent", "volatile", "eternal-infrequent_access", "expiring"]
  let allSets =
        fmap object $
          defSettings : fmap (\r -> defSettings <> ["retention" .= r]) rets
  mapM_ simpleRoundtrip allSets
  where
    simpleRoundtrip :: HasCallStack => Value -> App ()
    simpleRoundtrip sets = do
      uid <- randomUser OwnDomain def
      uid2 <- randomId
      -- Initial upload
      let bdy = (applicationText, "Hello World")
      r1 <- uploadSimple uid sets bdy
      r1.status `shouldMatchInt` 201
      print r1.jsonBody
      -- use v3 path instead of the one returned in the header
      (key, tok, expires) <-
        (,,)
          <$> r1.json %. "key"
          <*> (r1.json %. "token" >>= asString)
          <*> (lookupField r1.json "expires" >>= maybe (pure Nothing) (fmap pure . asString))
      -- Check mandatory Date header
      let Just date = C8.unpack <$> lookup (mk $ cs "Date") r1.headers
          parseTime = parseTimeOrError False defaultTimeLocale rfc822DateFormat
          utc = parseTime date :: UTCTime
          expires' = parseTime <$> expires :: Maybe UTCTime
      -- Potentially check for the expires header
      case sets of
        -- We don't care what the rentention value is here,
        -- we're just checking to see if other checks need
        -- to be done.
        Object o -> case KM.lookup (fromString "retention") o of
          Nothing -> pure ()
          Just _r -> do
            assertBool "invalid expiration" (Just utc < expires')
        _ -> pure ()
      -- Lookup with token and download via redirect.
      r2 <- downloadAsset' uid key tok
      print r2.body
      r2.status `shouldMatchInt` 302
      assertBool "Response body should be empty" $ r2.body == mempty

      let locReq = C8.unpack (getHeader' (mk $ cs "Location") r2)
      req <- liftIO $ parseRequest locReq
      r3 <- submit "GET" req
      r3.status `shouldMatchInt` 200
      assertBool "content-type should always be application/octet-stream" $
        getHeader (mk $ cs "content-type") r3 == Just (encodeUtf8 $ showMIMEType applicationOctetStream)
      assertBool "Token mismatch" $ getHeader (mk $ cs "x-amz-meta-token") r3 == pure (cs tok)
      uid' <- uid %. "id" >>= asString
      assertBool "User mismatch" $ getHeader (mk $ cs "x-amz-meta-user") r3 == pure (cs uid')
      assertBool "Data mismatch" $ r3.body == cs "Hello World"
      -- Delete (forbidden for other users)
      deleteAssetV3 uid2 key >>= \r -> r.status `shouldMatchInt` 403
      -- Delete (allowed for creator)
      deleteAssetV3 uid key >>= \r -> r.status `shouldMatchInt` 200
      r4 <- downloadAsset' uid key tok
      r4.status `shouldMatchInt` 404
      let Just date' = C8.unpack <$> lookup (mk $ cs "Date") r4.headers
      let utc' = parseTimeOrError False defaultTimeLocale rfc822DateFormat date' :: UTCTime
      assertBool "bad date" (utc' >= utc)
