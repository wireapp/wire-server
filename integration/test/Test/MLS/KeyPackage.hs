module Test.MLS.KeyPackage where

import API.Brig
import MLS.Util
import SetupHelpers
import Testlib.Prelude

testDeleteKeyPackages :: App ()
testDeleteKeyPackages = do
  alice <- randomUser OwnDomain def
  alice1 <- createMLSClient alice
  kps <- replicateM 3 (uploadNewKeyPackage alice1)

  -- add an extra non-existing key package to the delete request
  let kps' = "4B701F521EBE82CEC4AD5CB67FDD8E1C43FC4868DE32D03933CE4993160B75E8" : kps

  bindResponse (deleteKeyPackages alice1 kps') $ \resp -> do
    resp.status `shouldMatchInt` 201
  bindResponse (countKeyPackages alice1) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "count" `shouldMatchInt` 0
