module Test.AssetUpload where

import API.BrigInternal
import API.Cargohold
import SetupHelpers
import Testlib.Prelude

testAssetUploadUnverifiedUser :: (HasCallStack) => App ()
testAssetUploadUnverifiedUser = do
  user <- randomUser OwnDomain $ def {activate = False}
  bindResponse (uploadAsset user) $ \resp -> do
    resp.status `shouldMatchInt` 403

testAssetUploadVerifiedUser :: (HasCallStack) => App ()
testAssetUploadVerifiedUser = do
  user <- randomUser OwnDomain def
  bindResponse (uploadAsset user) $ \resp -> do
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
  bindResponse (uploadAsset user) $ \resp -> do
    resp.status `shouldMatchInt` 403
