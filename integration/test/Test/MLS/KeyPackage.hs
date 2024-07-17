-- FUTUREWORK:
-- GET /mls/key-packages/self/:client/count should be
-- tested with expired package

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

testKeyPackageUploadNoKey :: App ()
testKeyPackageUploadNoKey = do
  alice <- randomUser OwnDomain def
  alice1 <- createWireClient alice

  (kp, _) <- generateKeyPackage alice1

  -- if we upload a keypackage without a key,
  -- we get a bad request
  uploadKeyPackages alice1 [kp] `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 400

  -- there should be no keypackages after getting
  -- a rejection by the backend
  countKeyPackages def alice1 `bindResponse` \resp -> do
    resp.json %. "count" `shouldMatchInt` 0
    resp.status `shouldMatchInt` 200

testKeyPackageClaim :: App ()
testKeyPackageClaim = do
  alice <- randomUser OwnDomain def
  alices@[alice1, _alice2] <- replicateM 2 do
    createMLSClient def alice

  for_ alices \alicei -> replicateM 3 do
    uploadNewKeyPackage alicei

  bob <- randomUser OwnDomain def
  bobs <- replicateM 3 do
    createMLSClient def bob

  for_ bobs \bobi ->
    claimKeyPackages def bobi alice `bindResponse` \resp -> do
      ks <- resp.json %. "key_packages" & asList

      -- all of the keypackages should by issued by alice
      for_ ks \k ->
        (k %. "user") `shouldMatch` (alice %. "id")

      -- every claimed keypackage bundle should contain
      -- exactly one of each of the keypackages issued by
      -- alice
      for ks (%. "client")
        >>= (`shouldMatchSet` map (.client) alices)

      -- claiming keypckages should return 200
      resp.status `shouldMatchInt` 200

  -- bob has claimed all keypackages by alice, so there should
  -- be none left
  countKeyPackages def alice1 `bindResponse` \resp -> do
    resp.json %. "count" `shouldMatchInt` 0
    resp.status `shouldMatchInt` 200

testKeyPackageSelfClaim :: App ()
testKeyPackageSelfClaim = do
  alice <- randomUser OwnDomain def
  alices@[alice1, alice2] <- replicateM 2 do
    createMLSClient def alice
  for_ alices \alicei -> replicateM 3 do
    uploadNewKeyPackage alicei

  -- claim own keypackages
  claimKeyPackages def alice1 alice `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 200

    resp.json %. "key_packages"
      & asList
        -- the keypackage claimed by client 1 should be issued by
        -- client 2
        >>= \[v] -> v %. "client" `shouldMatch` alice2.client

  -- - the keypackages of client 1 (claimer) should still be there
  -- - two of the keypackages of client 2 (claimee) should be stil
  --   there
  for_ (zip alices [3, 2]) \(alicei, n) ->
    countKeyPackages def alicei `bindResponse` \resp -> do
      resp.json %. "count" `shouldMatchInt` n
      resp.status `shouldMatchInt` 200

  bob <- randomUser OwnDomain def
  bobs <- replicateM 2 do
    createMLSClient def bob

  -- skip own should only apply to own keypackages, hence
  -- bob claiming alices keypackages should work as normal
  a1s <- alice1 %. "client_id" & asString
  for_ bobs \bobi ->
    claimKeyPackagesWithParams def bobi alice [("skip_own", a1s)] `bindResponse` \resp -> do
      (resp.json %. "key_packages" & asList <&> length) `shouldMatchInt` 2
      resp.status `shouldMatchInt` 200

  -- alices keypackages should be gone after bob claimed them
  for_ (zip alices [1, 0]) \(alicei, n) ->
    countKeyPackages def alicei `bindResponse` \resp -> do
      resp.json %. "count" `shouldMatchInt` n
      resp.status `shouldMatchInt` 200

testKeyPackageRemoteClaim :: App ()
testKeyPackageRemoteClaim = do
  alice <- randomUser OwnDomain def
  alice1 <- createMLSClient def alice

  charlie <- randomUser OtherDomain def
  charlie1 <- createMLSClient def charlie

  refCharlie <- uploadNewKeyPackage charlie1
  refAlice <- uploadNewKeyPackage alice1

  -- the user should be able to claim the keypackage of
  -- a remote user and vice versa
  for_
    [ (alice1, charlie, charlie1, refCharlie),
      (charlie1, alice, alice1, refAlice)
    ]
    \(claimer, claimee, uploader, reference) -> do
      claimKeyPackages def claimer claimee `bindResponse` \resp -> do
        -- make sure that the reference match on the keypackages
        [kp] <- resp.json %. "key_packages" & asList
        kp %. "key_package_ref" `shouldMatch` reference
        resp.status `shouldMatchInt` 200

      -- the key package of the uploading client should be gone
      -- after claiming
      countKeyPackages def uploader `bindResponse` \resp -> do
        resp.json %. "count" `shouldMatchInt` 0
        resp.status `shouldMatchInt` 200

testKeyPackageCount :: (HasCallStack) => Ciphersuite -> App ()
testKeyPackageCount cs = do
  setMLSCiphersuite cs
  alice <- randomUser OwnDomain def
  alice1 <- createMLSClient def alice

  bindResponse (countKeyPackages cs alice1) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "count" `shouldMatchInt` 0

  let count = 10
  kps <- map fst <$> replicateM count (generateKeyPackage alice1)
  void $ uploadKeyPackages alice1 kps >>= getBody 201

  bindResponse (countKeyPackages cs alice1) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "count" `shouldMatchInt` count

testUnsupportedCiphersuite :: (HasCallStack) => App ()
testUnsupportedCiphersuite = do
  let suite = Ciphersuite "0x0003"
  setMLSCiphersuite suite
  bob <- randomUser OwnDomain def
  bob1 <- createMLSClient def bob
  (kp, _) <- generateKeyPackage bob1
  bindResponse (uploadKeyPackages bob1 [kp]) $ \resp -> do
    resp.status `shouldMatchInt` 400
    resp.json %. "label" `shouldMatch` "mls-protocol-error"

testReplaceKeyPackages :: (HasCallStack) => App ()
testReplaceKeyPackages = do
  alice <- randomUser OwnDomain def
  [alice1, alice2] <- replicateM 2 $ createMLSClient def alice
  let suite = Ciphersuite "0xf031"

  let checkCount cs n =
        bindResponse (countKeyPackages cs alice1) $ \resp -> do
          resp.status `shouldMatchInt` 200
          resp.json %. "count" `shouldMatchInt` n

  -- setup: upload a batch of key packages for each ciphersuite
  void
    $ replicateM 4 (fmap fst (generateKeyPackage alice1))
    >>= uploadKeyPackages alice1
    >>= getBody 201
  setMLSCiphersuite suite
  void
    $ replicateM 5 (fmap fst (generateKeyPackage alice1))
    >>= uploadKeyPackages alice1
    >>= getBody 201

  checkCount def 4
  checkCount suite 5

  do
    -- generate a new batch of key packages
    (kps, refs) <- unzip <$> replicateM 3 (generateKeyPackage alice1)

    -- replace old key packages with new
    void $ replaceKeyPackages alice1 [suite] kps >>= getBody 201

    checkCount def 4
    checkCount suite 3

    -- claim all key packages one by one
    claimed <-
      replicateM 3
        $ bindResponse (claimKeyPackages suite alice2 alice)
        $ \resp -> do
          resp.status `shouldMatchInt` 200
          ks <- resp.json %. "key_packages" & asList
          k <- assertOne ks
          k %. "key_package_ref"

    refs `shouldMatchSet` claimed

    checkCount def 4
    checkCount suite 0

  do
    -- replenish key packages for the second ciphersuite
    void
      $ replicateM 5 (fmap fst (generateKeyPackage alice1))
      >>= uploadKeyPackages alice1
      >>= getBody 201

    checkCount def 4
    checkCount suite 5

    -- replace all key packages with fresh ones
    setMLSCiphersuite def
    kps1 <- replicateM 2 (fmap fst (generateKeyPackage alice1))
    setMLSCiphersuite suite
    kps2 <- replicateM 2 (fmap fst (generateKeyPackage alice1))

    void $ replaceKeyPackages alice1 [def, suite] (kps1 <> kps2) >>= getBody 201

    checkCount def 2
    checkCount suite 2

  do
    setMLSCiphersuite def
    defKeyPackages <- replicateM 3 (fmap fst (generateKeyPackage alice1))
    setMLSCiphersuite suite
    suiteKeyPackages <- replicateM 3 (fmap fst (generateKeyPackage alice1))

    void
      $ replaceKeyPackages' alice1 (Just []) []
      `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 201

    void
      $ replaceKeyPackages' alice1 Nothing defKeyPackages
      `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 201

    checkCount def 3
    checkCount suite 2

    let testErrorCases :: (HasCallStack) => Maybe [Ciphersuite] -> [ByteString] -> App ()
        testErrorCases ciphersuites keyPackages = do
          void
            $ replaceKeyPackages' alice1 ciphersuites keyPackages
            `bindResponse` \resp -> do
              resp.status `shouldMatchInt` 400
              resp.json %. "label" `shouldMatch` "mls-protocol-error"
          checkCount def 3
          checkCount suite 2

    testErrorCases (Just []) defKeyPackages
    testErrorCases (Just []) suiteKeyPackages
    testErrorCases Nothing []
    testErrorCases Nothing suiteKeyPackages
    testErrorCases Nothing (suiteKeyPackages <> defKeyPackages)

    testErrorCases (Just [suite]) defKeyPackages
    testErrorCases (Just [suite]) (suiteKeyPackages <> defKeyPackages)
    testErrorCases (Just [suite]) []

    testErrorCases (Just [def]) suiteKeyPackages
    testErrorCases (Just [def]) (suiteKeyPackages <> defKeyPackages)
    testErrorCases (Just [def]) []
