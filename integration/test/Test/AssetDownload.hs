module Test.AssetDownload where

import API.Cargohold
import GHC.Stack
import Network.HTTP.Client (Request (redirectCount))
import Network.HTTP.Client qualified as HTTP
import SetupHelpers
import Testlib.Prelude

testDownloadAsset :: HasCallStack => App ()
testDownloadAsset = do
  user <- randomUser OwnDomain def

  key <- bindResponse (uploadAsset user) $ \resp -> do
    resp.status `shouldMatchInt` 201
    resp.json %. "key"

  bindResponse (downloadAsset user user key id) $ \resp -> do
    resp.status `shouldMatchInt` 200
    assertBool
      ("Expect 'Hello World!' as text asset content. Got: " ++ show resp.body)
      (resp.body == fromString "Hello World!")

testDownloadAssetMultiIngressS3DownloadUrl :: HasCallStack => App ()
testDownloadAssetMultiIngressS3DownloadUrl = do
  user <- randomUser OwnDomain def

  -- multi-ingress disabled
  key <- doUploadAsset user
  checkAssetDownload user key

  withModifiedService Cargohold modifyConfig $ \_ -> do
    -- multi-ingress enabled
    key' <- doUploadAsset user
    checkAssetDownload user key'
  where
    checkAssetDownload :: HasCallStack => Value -> Value -> App ()
    checkAssetDownload user key = withModifiedService Cargohold modifyConfig $ \_ -> do
      bindResponse (downloadAsset user user key noRedirects) $ \resp -> do
        resp.status `shouldMatchInt` 404
      bindResponse (downloadAsset' user user key "red.example.com" noRedirects) $ \resp -> do
        resp.status `shouldMatchInt` 302
        locationHeaderHost resp `shouldMatch` "s3-download.red.example.com"
      bindResponse (downloadAsset' user user key "green.example.com" noRedirects) $ \resp -> do
        resp.status `shouldMatchInt` 302
        locationHeaderHost resp `shouldMatch` "s3-download.green.example.com"
      bindResponse (downloadAsset' user user key "unknown.example.com" noRedirects) $ \resp -> do
        resp.status `shouldMatchInt` 404
        resp.json %. "label" `shouldMatch` "not-found"

    noRedirects :: HTTP.Request -> HTTP.Request
    noRedirects req = (req {redirectCount = 0})

    modifyConfig :: Value -> App Value
    modifyConfig =
      setField "aws.multiIngress" $
        object
          [ "red.example.com" .= "http://s3-download.red.example.com",
            "green.example.com" .= "http://s3-download.green.example.com"
          ]

    doUploadAsset :: HasCallStack => Value -> App Value
    doUploadAsset user = bindResponse (uploadAsset user) $ \resp -> do
      resp.status `shouldMatchInt` 201
      resp.json %. "key"
