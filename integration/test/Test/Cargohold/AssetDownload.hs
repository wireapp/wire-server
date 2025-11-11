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

module Test.Cargohold.AssetDownload where

import API.Cargohold
import GHC.Stack
import SetupHelpers
import Testlib.Prelude

testDownloadAsset :: (HasCallStack) => App ()
testDownloadAsset = do
  user <- randomUser OwnDomain def

  key <- bindResponse (uploadSomeAsset user) $ \resp -> do
    resp.status `shouldMatchInt` 201
    resp.json %. "key"

  bindResponse (downloadAsset user user key "nginz-https.example.com" id) $ \resp -> do
    resp.status `shouldMatchInt` 200
    assertBool
      ("Expect 'Hello World!' as text asset content. Got: " ++ show resp.body)
      (resp.body == fromString "Hello World!")

testDownloadAssetMultiIngressS3DownloadUrl :: (HasCallStack) => App ()
testDownloadAssetMultiIngressS3DownloadUrl = do
  user <- randomUser OwnDomain def

  -- multi-ingress disabled
  key <- doUploadAsset user

  bindResponse (downloadAsset user user key "nginz-https.example.com" noRedirect) $ \resp -> do
    resp.status `shouldMatchInt` 302

  bindResponse (downloadAsset user user key "red.example.com" noRedirect) $ \resp -> do
    resp.status `shouldMatchInt` 302

  bindResponse (downloadAsset user user key "green.example.com" noRedirect) $ \resp -> do
    resp.status `shouldMatchInt` 302

  bindResponse (downloadAsset user user key "unknown.example.com" noRedirect) $ \resp -> do
    resp.status `shouldMatchInt` 302

  -- multi-ingress enabled
  withModifiedBackend modifyConfig $ \domain -> do
    user' <- randomUser domain def
    key' <- doUploadAsset user'

    bindResponse (downloadAsset user' user' key' "nginz-https.example.com" noRedirect) $ \resp -> do
      resp.status `shouldMatchInt` 404
      resp.json %. "label" `shouldMatch` "not-found"

    bindResponse (downloadAsset user' user' key' "red.example.com" noRedirect) $ \resp -> do
      resp.status `shouldMatchInt` 302
      locationHeaderHost resp `shouldMatch` "s3-download.red.example.com"

    bindResponse (downloadAsset user' user' key' "green.example.com" noRedirect) $ \resp -> do
      resp.status `shouldMatchInt` 302
      locationHeaderHost resp `shouldMatch` "s3-download.green.example.com"

    bindResponse (downloadAsset user' user' key' "unknown.example.com" noRedirect) $ \resp -> do
      resp.status `shouldMatchInt` 404
      resp.json %. "label" `shouldMatch` "not-found"
  where
    modifyConfig :: ServiceOverrides
    modifyConfig =
      def
        { cargoholdCfg =
            setField "aws.multiIngress"
              $ object
                [ "red.example.com" .= "http://s3-download.red.example.com",
                  "green.example.com" .= "http://s3-download.green.example.com"
                ]
        }

    doUploadAsset :: (HasCallStack) => Value -> App Value
    doUploadAsset user = bindResponse (uploadSomeAsset user) $ \resp -> do
      resp.status `shouldMatchInt` 201
      resp.json %. "key"
