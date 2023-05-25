module Test.Cargohold where

import API.Cargohold
import qualified Data.ByteString.Char8 as C
import GHC.Stack
import Network.HTTP.Client (Request (redirectCount))
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Types
import Network.URI
import SetupHelpers
import Testlib.Prelude

testUploadAsset :: HasCallStack => App ()
testUploadAsset = do
  user <- randomUser OwnDomain def

  key <- bindResponse (uploadAsset user) $ \resp -> do
    resp.status `shouldMatchInt` 201
    resp.json %. "key"

  void . bindResponse (downloadAsset user key id) $ \resp -> do
    resp.status `shouldMatchInt` 200
    assertBool
      ("Expect 'Hello World!' as text asset content. Got: " ++ show resp.body)
      (resp.body == fromString "Hello World!")

testUploadAssetMultiIngressS3DownloadUrl :: HasCallStack => App ()
testUploadAssetMultiIngressS3DownloadUrl = do
  user <- randomUser OwnDomain def

  key <- bindResponse (uploadAsset user) $ \resp -> do
    resp.status `shouldMatchInt` 201
    resp.json %. "key"

  withModifiedService
    Cargohold
    modifyConfig
    $ do
      bindResponse (downloadAsset user key noRedirects) $ \resp -> do
        resp.status `shouldMatchInt` 404
      bindResponse (downloadAsset' user key "red.example.com" noRedirects) $ \resp -> do
        resp.status `shouldMatchInt` 302
        locationHeaderHost resp `shouldMatch` "s3-download.red.example.com"
      bindResponse (downloadAsset' user key "green.example.com" noRedirects) $ \resp -> do
        resp.status `shouldMatchInt` 302
        locationHeaderHost resp `shouldMatch` "s3-download.green.example.com"
      bindResponse (downloadAsset' user key "unknown.example.com" noRedirects) $ \resp -> do
        resp.status `shouldMatchInt` 404
        resp.json %. "label" `shouldMatch` "no-asset-endpoint"
  where
    noRedirects :: HTTP.Request -> HTTP.Request
    noRedirects req = (req {redirectCount = 0})

    modifyConfig :: Value -> App Value
    modifyConfig v =
      setField "aws.s3DownloadEndpoint" "http://s3-download.example.com" v
        >>= setField "aws.multiIngress" multiIngressConfig

    multiIngressConfig =
      object
        [ "red.example.com" .= "http://s3-download.red.example.com",
          "green.example.com" .= "http://s3-download.green.example.com"
        ]

    locationHeaderHost :: Response -> String
    locationHeaderHost resp =
      let location = C.unpack . snd . fromJust $ find (\(name, _) -> hLocation == name) resp.headers
          locationURI = fromJust $ parseURI location
          locationHost = fromJust $ locationURI & uriAuthority <&> uriRegName
       in locationHost
