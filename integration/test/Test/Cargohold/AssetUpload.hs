-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
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

module Test.Cargohold.AssetUpload where

import API.Cargohold
import qualified Data.ByteString.Char8 as BSC
import SetupHelpers
import Testlib.Prelude

testAssetUploadVerifiedUser :: (HasCallStack) => App ()
testAssetUploadVerifiedUser = do
  user <- randomUser OwnDomain def
  bindResponse (uploadSomeAsset user) $ \resp -> do
    resp.status `shouldMatchInt` 201

testAssetUploadUnknownUser :: (HasCallStack) => App ()
testAssetUploadUnknownUser = do
  uid <- randomId
  domain <- make OwnDomain
  let user =
        object
          [ "id" .= uid,
            "qualified_id"
              .= object
                [ "domain" .= domain,
                  "id" .= uid
                ]
          ]
  bindResponse (uploadSomeAsset user) $ \resp -> do
    resp.status `shouldMatchInt` 403

testUploadAssetEphemeralUser :: (HasCallStack) => App ()
testUploadAssetEphemeralUser = do
  user <- ephemeralUser OwnDomain

  key <- bindResponse (uploadSomeAsset user) $ \resp -> do
    resp.status `shouldMatchInt` 201
    resp.json %. "key"

  bindResponse (downloadAsset user user key "nginz-https.example.com" id) $ \resp -> do
    resp.status `shouldMatchInt` 200
    BSC.unpack resp.body `shouldMatch` "Hello World!"
