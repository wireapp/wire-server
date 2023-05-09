{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Test.MLS where

import qualified API.Galley as Public
import SetupHelpers
import Testlib.Prelude

testMixedProtocolUpgrade :: HasCallStack => App ()
testMixedProtocolUpgrade = do
  [alice, bob] <- createAndConnectUsers [ownDomain, ownDomain]

  qcnv <- bindResponseR (Public.postConversation alice noValue Public.defProteus {Public.qualifiedUsers = [bob]}) $ \resp -> do
    resp.status `shouldMatchInt` 201

  withWebSocket alice $ \wsAlice -> do
    bindResponse (Public.putConversationProtocol bob qcnv noValue "mixed") $ \resp -> do
      resp.status `shouldMatchInt` 200
      resp %. "conversation" `shouldMatch` (qcnv %. "id")
      resp %. "data.protocol" `shouldMatch` "mixed"

    n <- awaitMatch 3 (\value -> nPayload value %. "type" `isEqual` "conversation.protocol-update") wsAlice
    nPayload n %. "data.protocol" `shouldMatch` "mixed"

  bindResponse (Public.getConversation alice qcnv) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp %. "protocol" `shouldMatch` "mixed"

  bindResponse (Public.putConversationProtocol alice qcnv noValue "mixed") $ \resp -> do
    resp.status `shouldMatchInt` 204
