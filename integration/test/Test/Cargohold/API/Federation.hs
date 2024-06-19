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
import qualified Codec.MIME.Type as MIME
import Control.Lens hiding ((.=))
import Crypto.Random (getRandomBytes)
import Data.ByteString.Builder
import Data.String.Conversions
import SetupHelpers
import Test.Cargohold.API.Util
import Testlib.Prelude

testGetAssetAvailablePublic :: (HasCallStack) => App ()
testGetAssetAvailablePublic = getAssetAvailable True

testGetAssetAvailablePrivate :: (HasCallStack) => App ()
testGetAssetAvailablePrivate = getAssetAvailable False

getAssetAvailable :: (HasCallStack) => Bool -> App ()
getAssetAvailable isPublicAsset = do
  -- Initial upload
  let bdy = (applicationOctetStream, cs "Hello World")
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

testGetAssetNotAvailable :: (HasCallStack) => App ()
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

testGetAssetWrongToken :: (HasCallStack) => App ()
testGetAssetWrongToken = do
  -- Initial upload
  let bdy = (applicationOctetStream, cs "Hello World")
      settings = object ["public" .= False, "retention" .= "volatile"]
  uid1 <- randomUser OwnDomain def
  uid2 <- randomUser OtherDomain def
  userId2 <- uid2 %. "id" & asString
  domain <- uid1 %. "qualified_id" %. "domain" & asString
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

testLargeAsset :: (HasCallStack) => App ()
testLargeAsset = do
  -- Initial upload
  let settings = object ["public" .= False, "retention" .= "volatile"]
  uid <- randomUser OwnDomain def
  domain <- uid %. "qualified_id" %. "domain" & asString
  uid2 <- randomUser OtherDomain def
  userId2 <- uid2 %. "id" & asString
  -- generate random bytes
  let size = 1024 * 1024
  bs :: ByteString <- liftIO $ getRandomBytes size
  let body = toLazyByteString $ buildMultipartBody' settings applicationOctetStream' (cs bs)
  r1 <- uploadRaw uid body
  r1.status `shouldMatchInt` 201
  tok <- r1.jsonBody %. "token" & asString
  key <- r1.jsonBody %. "key" & asString
  -- Call get-asset federation API
  let ga =
        object
          [ "user" .= userId2,
            "key" .= key,
            "domain" .= domain,
            "token" .= tok
          ]
  r2 <- downloadAsset' uid2 ga ga
  r2.status `shouldMatchInt` 200

testStreamAsset :: (HasCallStack) => App ()
testStreamAsset = do
  -- Initial upload
  uid <- randomUser OwnDomain def
  uid2 <- randomUser OtherDomain def
  userId <- uid %. "id" & asString
  domain <- uid %. "qualified_id" %. "domain" & asString
  r1 <- uploadSimple uid settings bdy
  r1.status `shouldMatchInt` 201

  -- Call get-asset federation API
  tok <- r1.jsonBody %. "token" & asString
  key <- r1.jsonBody %. "key" & asString
  let ga = object ["user" .= userId, "token" .= tok, "key" .= key, "domain" .= domain]
  r2 <- downloadAsset' uid2 ga ga
  r2.status `shouldMatchInt` 200
  cs @_ @String r2.body `shouldMatch` (snd bdy :: String)
  where
    bdy :: (ConvertibleStrings String a) => (MIME.MIMEType, a)
    bdy = (applicationOctetStream, cs "Hello World")
    settings = object ["public" .= False, "retention" .= "volatile"]

testStreamAssetNotAvailable :: (HasCallStack) => App ()
testStreamAssetNotAvailable = do
  uid <- randomUser OwnDomain def
  uid2 <- randomUser OtherDomain def
  userId <- uid2 %. "id" & asString
  domain <- uid %. "qualified_id" %. "domain" & asString
  token <- randomToken
  assetId <- randomId
  let key = "3-2-" <> assetId
      ga = object ["user" .= userId, "token" .= token, "key" .= key, "domain" .= domain]
  r <- downloadAsset' uid2 ga ga
  r.status `shouldMatchInt` 404
  r.jsonBody %. "message" `shouldMatch` "Asset not found"

testStreamAssetWrongToken :: (HasCallStack) => App ()
testStreamAssetWrongToken = do
  -- Initial upload
  uid <- randomUser OwnDomain def
  uid2 <- randomUser OtherDomain def
  userId2 <- uid2 %. "id" & asString
  domain <- uid %. "qualified_id" %. "domain" & asString
  r1 <- uploadSimple uid settings bdy
  r1.status `shouldMatchInt` 201

  -- Call get-asset federation API with wrong (random) token
  tok <- randomToken
  key <- r1.jsonBody %. "key" & asString
  let ga = object ["user" .= userId2, "token" .= tok, "key" .= key, "domain" .= domain]
  r2 <- downloadAsset' uid2 ga ga
  r2.status `shouldMatchInt` 404
  r2.jsonBody %. "message" `shouldMatch` "Asset not found"
  where
    bdy :: (ConvertibleStrings String a) => (MIME.MIMEType, a)
    bdy = (applicationOctetStream, cs "Hello World")
    settings = object ["public" .= False, "retention" .= "volatile"]
