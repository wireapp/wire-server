module Test.Cargohold where

import API.Cargohold
import GHC.Stack
import Testlib.Prelude
import SetupHelpers

testUploadAsset :: HasCallStack => App ()
testUploadAsset = do
  user <- randomUser ownDomain def

  key <- bindResponse (uploadAsset user) $ \resp -> do
    resp.status `shouldMatchInt` 201
    resp.json %. "key"

  void . bindResponse (downloadAsset user key) $ \resp -> do
    resp.status `shouldMatchInt` 200
    assertBool
      ("Expect 'Hello World!' as text asset content. Got: " ++ show resp.body)
      (resp.body == fromString "Hello World!")
