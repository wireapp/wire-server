module Test.MLS.KeyPackage where

import API.Brig
import MLS.Util
import SetupHelpers
import Testlib.Prelude

testDeleteKeyPackages :: App ()
testDeleteKeyPackages = do
  alice <- randomUser OwnDomain def
  alice1 <- createMLSClient def alice
  kps <- replicateM 3 (uploadNewKeyPackage alice1)

  -- add an extra non-existing key package to the delete request
  let kps' = "4B701F521EBE82CEC4AD5CB67FDD8E1C43FC4868DE32D03933CE4993160B75E8" : kps

  bindResponse (deleteKeyPackages alice1 kps') $ \resp -> do
    resp.status `shouldMatchInt` 201

  bindResponse (countKeyPackages def alice1) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "count" `shouldMatchInt` 0

testKeyPackageMultipleCiphersuites :: App ()
testKeyPackageMultipleCiphersuites = do
  alice <- randomUser OwnDomain def
  [alice1, alice2] <- replicateM 2 (createMLSClient def alice)

  kp <- uploadNewKeyPackage alice2

  let suite = Ciphersuite "0xf031"
  setMLSCiphersuite suite
  void $ uploadNewKeyPackage alice2

  -- count key packages with default ciphersuite
  bindResponse (countKeyPackages def alice2) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "count" `shouldMatchInt` 1

  -- claim key packages with default ciphersuite
  bindResponse (claimKeyPackages def alice1 alice) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "key_packages.0.key_package_ref" `shouldMatch` kp

  -- count key package with the other ciphersuite
  bindResponse (countKeyPackages suite alice2) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "count" `shouldMatchInt` 1

testKeyPackageCount :: HasCallStack => Ciphersuite -> App ()
testKeyPackageCount cs = do
  alice <- randomUser OwnDomain def
  alice1 <- createMLSClient def alice

  bindResponse (countKeyPackages cs alice1) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "count" `shouldMatchInt` 0

  setMLSCiphersuite cs

  let count = 10
  kps <- map fst <$> replicateM count (generateKeyPackage alice1)
  void $ uploadKeyPackages alice1 kps >>= getBody 201

  bindResponse (countKeyPackages cs alice1) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "count" `shouldMatchInt` count

testUnsupportedCiphersuite :: HasCallStack => App ()
testUnsupportedCiphersuite = do
  setMLSCiphersuite (Ciphersuite "0x0002")
  bob <- randomUser OwnDomain def
  bob1 <- createMLSClient def bob
  (kp, _) <- generateKeyPackage bob1
  bindResponse (uploadKeyPackages bob1 [kp]) $ \resp -> do
    resp.status `shouldMatchInt` 400
    resp.json %. "label" `shouldMatch` "mls-protocol-error"
