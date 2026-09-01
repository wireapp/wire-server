{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
module Test.Wire.API.Golden.Manual.ConversationProtocol (testPairs) where

import Data.Time
import Imports
import Wire.API.Conversation.Protocol
import Wire.API.MLS.CipherSuite
import Wire.API.MLS.Group

testPairs :: [(Protocol, FilePath)]
testPairs =
  [ (testObject_ProtocolProteus, "testObject_ProtocolProteus.json"),
    (testObject_ProtocolMLS1, "testObject_ProtocolMLS1.json"),
    (testObject_ProtocolMLS2, "testObject_ProtocolMLS2.json"),
    (testObject_ProtocolMLS3, "testObject_ProtocolMLS3.json")
  ]

testObject_ProtocolProteus :: Protocol
testObject_ProtocolProteus = ProtocolProteus

testObject_ProtocolMLS1 :: Protocol
testObject_ProtocolMLS1 =
  ProtocolMLS $
    ConversationMLSData (GroupId "test") Nothing

testObject_ProtocolMLS2 :: Protocol
testObject_ProtocolMLS2 =
  ProtocolMLS $
    ConversationMLSData
      (GroupId "test")
      (Just $ ActiveMLSConversationData (Epoch 42) timestamp MLS_128_DHKEMX25519_AES128GCM_SHA256_Ed25519)

testObject_ProtocolMLS3 :: Protocol
testObject_ProtocolMLS3 =
  ProtocolMLS $
    ConversationMLSData
      (GroupId "test")
      (Just $ ActiveMLSConversationData (Epoch 0) timestamp MLS_128_DHKEMX25519_AES128GCM_SHA256_Ed25519)

timestamp :: UTCTime
timestamp = UTCTime (fromGregorian 2023 1 17) (secondsToDiffTime 42)
