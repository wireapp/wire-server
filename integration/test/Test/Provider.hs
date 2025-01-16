module Test.Provider where

import API.Brig
import qualified API.Cargohold as Cargohold
import API.Common
import qualified API.Nginz as Nginz
import Data.String.Conversions (cs)
import SetupHelpers
import Testlib.Prelude

testProviderUploadAsset :: (HasCallStack) => App ()
testProviderUploadAsset = do
  alice <- randomUser OwnDomain def
  provider <- setupProvider alice def
  providerEmail <- provider %. "email" & asString
  pid <- provider %. "id" & asString
  -- test cargohold API
  bindResponse (Cargohold.uploadProviderAsset OwnDomain pid "profile pic") $ \resp -> do
    resp.status `shouldMatchInt` 201
  pw <- provider %. "password" & asString
  cookie <- loginProvider OwnDomain providerEmail pw
  -- test Nginz API
  bindResponse (Nginz.uploadProviderAsset OwnDomain (cs cookie) "another profile pic") $ \resp -> do
    resp.status `shouldMatchInt` 201
