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

  let count = 10
  kps <- map fst <$> replicateM count (generateKeyPackage alice1 cs)
  void $ uploadKeyPackages alice1 kps >>= getBody 201

  bindResponse (countKeyPackages cs alice1) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "count" `shouldMatchInt` count

testUnsupportedCiphersuite :: HasCallStack => App ()
testUnsupportedCiphersuite = do
  let suite = Ciphersuite "0x0002"
  setMLSCiphersuite suite
  bob <- randomUser OwnDomain def
  bob1 <- createMLSClient def bob
  (kp, _) <- generateKeyPackage bob1 suite
  bindResponse (uploadKeyPackages bob1 [kp]) $ \resp -> do
    resp.status `shouldMatchInt` 400
    resp.json %. "label" `shouldMatch` "mls-protocol-error"

testReplaceKeyPackages :: HasCallStack => App ()
testReplaceKeyPackages = do
  alice <- randomUser OwnDomain def
  [alice1, alice2] <- replicateM 2 $ createMLSClient def alice
  let suite = Ciphersuite "0xf031"

  let checkCount cs n =
        bindResponse (countKeyPackages cs alice1) $ \resp -> do
          resp.status `shouldMatchInt` 200
          resp.json %. "count" `shouldMatchInt` n

  -- setup: upload a batch of key packages for each ciphersuite
  void $
    replicateM 4 (fmap fst (generateKeyPackage alice1 def))
      >>= uploadKeyPackages alice1
      >>= getBody 201
  setMLSCiphersuite suite
  void $
    replicateM 5 (fmap fst (generateKeyPackage alice1 suite))
      >>= uploadKeyPackages alice1
      >>= getBody 201

  checkCount def 4
  checkCount suite 5

  do
    -- generate a new batch of key packages
    (kps, refs) <- unzip <$> replicateM 3 (generateKeyPackage alice1 suite)

    -- replace old key packages with new
    void $ replaceKeyPackages alice1 [suite] kps >>= getBody 201

    checkCount def 4
    checkCount suite 3

    -- claim all key packages one by one
    claimed <-
      replicateM 3 $
        bindResponse (claimKeyPackages suite alice2 alice) $ \resp -> do
          resp.status `shouldMatchInt` 200
          ks <- resp.json %. "key_packages" & asList
          k <- assertOne ks
          k %. "key_package_ref"

    refs `shouldMatchSet` claimed

    checkCount def 4
    checkCount suite 0

  do
    -- replenish key packages for the second ciphersuite
    void $
      replicateM 5 (fmap fst (generateKeyPackage alice1 suite))
        >>= uploadKeyPackages alice1
        >>= getBody 201

    checkCount def 4
    checkCount suite 5

    -- replace all key packages with fresh ones
    kps1 <- replicateM 2 (fmap fst (generateKeyPackage alice1 def))
    kps2 <- replicateM 2 (fmap fst (generateKeyPackage alice1 suite))

    void $ replaceKeyPackages alice1 [def, suite] (kps1 <> kps2) >>= getBody 201

    checkCount def 2
    checkCount suite 2
