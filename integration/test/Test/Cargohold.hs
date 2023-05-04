module Test.Cargohold where

import API.Cargohold
import GHC.Stack
import Testlib.Prelude
import SetupHelpers

testUploadAsset :: HasCallStack => App ()
testUploadAsset = do
  user <- randomUser ownDomain def

  void . bindResponse (uploadAsset user) $ \resp -> do
    resp.status `shouldMatchInt` 201
