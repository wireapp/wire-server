module Test.AssetDownload where

import API.Cargohold
import GHC.Stack
import SetupHelpers
import Testlib.Prelude

testDownloadAsset :: (HasCallStack) => App ()
testDownloadAsset = do
  user <- randomUser OwnDomain def

  key <- bindResponse (uploadAsset user) $ \resp -> do
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
    doUploadAsset user = bindResponse (uploadAsset user) $ \resp -> do
      resp.status `shouldMatchInt` 201
      resp.json %. "key"
