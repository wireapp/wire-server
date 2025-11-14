{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

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

  -- Client 1 with 4 tokens
  c1Apns1 <- randomSnsToken APNS
  c1Apns1Overlap <- randomSnsToken APNS
  c1Apns2 <- randomSnsToken APNS
  c1Gcm1 <- randomSnsToken GCM

  -- Client 2 with 1 token
  c2Apns1 <- randomSnsToken APNS
  c2Gcm1 <- randomSnsToken GCM
  c2Gcm1Overlap <- randomSnsToken GCM

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
  assertStatus 201 =<< (postPushToken alice c1Apns1Token)
  assertStatus 201 =<< (postPushToken alice c1Apns2Token)
  assertStatus 201 =<< (postPushToken alice c1Gcm1Token)

  -- register non-overlapping tokens for client 2
  assertStatus 201 =<< (postPushToken alice c2Apns1Token)
  assertStatus 201 =<< (postPushToken alice c2Gcm1Token)

  bindResponse (listPushTokens alice) \resp -> do
    resp.status `shouldMatchInt` 200
    allTokens <- resp.json %. "tokens"
    allTokens
      `shouldMatchSet` [ c1Apns1Token,
                         c1Apns2Token,
                         c1Gcm1Token,
                         c2Apns1Token,
                         c2Gcm1Token
                       ]

  -- Resistering an overlapping token overwrites it.
  assertStatus 201 =<< postPushToken alice c1Apns1OverlapToken
  assertStatus 201 =<< postPushToken alice c2Gcm1OverlapToken

  bindResponse (listPushTokens alice) \resp -> do
    resp.status `shouldMatchInt` 200
    allTokens <- resp.json %. "tokens"
    allTokens
      `shouldMatchSet` [ c1Apns1OverlapToken,
                         c1Apns2Token,
                         c1Gcm1Token,
                         c2Apns1Token,
                         c2Gcm1OverlapToken
                       ]

  -- Push tokens are deleted alongside clients
  assertStatus 200 =<< unregisterClient alice aliceC1
  assertStatus 200 =<< unregisterClient alice aliceC2

  bindResponse (listPushTokens alice) \resp -> do
    resp.status `shouldMatchInt` 200
    allTokens <- resp.json %. "tokens"
    allTokens
      `shouldMatchSet` ([] :: [PushToken])

testVoipTokenRegistrationFails :: App ()
testVoipTokenRegistrationFails = do
  alice <- randomUser OwnDomain def
  aliceC2 <- randomClientId

  token <- randomSnsToken APNS
  let apnsVoipToken = PushToken "APNS_VOIP_SANDBOX" "test" token aliceC2
  postPushToken alice apnsVoipToken `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 400
    resp.json %. "label" `shouldMatch` "apns-voip-not-supported"
