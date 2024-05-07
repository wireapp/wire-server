{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Test.PushToken where

import API.Common
import API.Gundeck
import SetupHelpers
import Testlib.Prelude

testRegisterPushToken :: App ()
testRegisterPushToken = do
  alice <- randomUser OwnDomain def
  aliceC2 <- randomClientId
  aliceC1 <- randomClientId

  -- TODO(elland): remove magic numbers;
  -- GCM uses 16 bytes, APNS uses 32
  -- Client 1 with 4 tokens
  c1Apns1 <- randomSnsToken 32
  c1Apns1Overlap <- randomSnsToken 32
  c1Apns2 <- randomSnsToken 32
  c1Gcm1 <- randomSnsToken 16

  -- Client 2 with 1 token
  c2Apns1 <- randomSnsToken 32
  c2Gcm1 <- randomSnsToken 16
  c2Gcm1Overlap <- randomSnsToken 16

  let apnsToken = PushToken "APNS_SANDBOX" "test"
  let gcmToken = PushToken "GCM" "test"

  let c1Apns1Token = apnsToken c1Apns1 aliceC1
  let c1Apns1OverlapToken = apnsToken c1Apns1Overlap aliceC1
  let c1Apns2Token = (apnsToken c1Apns2 aliceC1 :: PushToken) {app = "com.wire.ent"} -- diff app prevents overlap
  let c1Gcm1Token = gcmToken c1Gcm1 aliceC1

  let c2Apns1Token = apnsToken c2Apns1 aliceC2
  let c2Gcm1Token = gcmToken c2Gcm1 aliceC2
  let c2Gcm1OverlapToken = gcmToken c2Gcm1Overlap aliceC2

  -- Register non-overlapping tokens for client 1
  void $ getJSON 201 =<< (postPushToken alice c1Apns1Token)
  void $ getJSON 201 =<< (postPushToken alice c1Apns2Token)
  void $ getJSON 201 =<< (postPushToken alice c1Gcm1Token)

  -- register non-overlapping tokens for client 2
  void $ getJSON 201 =<< (postPushToken alice c2Apns1Token)
  void $ getJSON 201 =<< (postPushToken alice c2Gcm1Token)

  bindResponse (listPushTokens alice) \resp -> do
    resp.status `shouldMatchInt` 200
    allTokens <- resp.json %. "tokens"
    allTokens `shouldMatchSet` [c1Apns1Token, c1Apns2Token, c1Gcm1Token, c2Apns1Token, c2Gcm1Token]

  -- Resistering an overlapping token overwrites it.
