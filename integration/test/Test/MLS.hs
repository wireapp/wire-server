{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Test.MLS where

import API.Galley
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8 as B8
import MLS.Util
import SetupHelpers
import Testlib.Prelude

testMixedProtocolUpgrade :: HasCallStack => App ()
testMixedProtocolUpgrade = ptestMixedProtocolUpgrade ownDomain

testMixedProtocolUpgradeFed :: HasCallStack => App ()
testMixedProtocolUpgradeFed = ptestMixedProtocolUpgrade otherDomain

ptestMixedProtocolUpgrade :: (HasCallStack, MakesValue domain) => domain -> App ()
ptestMixedProtocolUpgrade secondDomain = do
  [alice, bob, charlie] <- do
    d <- ownDomain
    d2 <- secondDomain & asString
    createAndConnectUsers [d, d2, d2]

  qcnv <- postConversation alice defProteus {qualifiedUsers = [bob]} >>= getJSON 201

  withWebSockets [alice, charlie] $ \websockets -> do
    bindResponse (putConversationProtocol bob qcnv "mixed") $ \resp -> do
      resp.status `shouldMatchInt` 200
      resp.json %. "conversation" `shouldMatch` (qcnv %. "id")
      resp.json %. "data.protocol" `shouldMatch` "mixed"

    for_ websockets $ \ws -> do
      n <- awaitMatch 3 (\value -> nPayload value %. "type" `isEqual` "conversation.protocol-update") ws
      nPayload n %. "data.protocol" `shouldMatch` "mixed"

  bindResponse (getConversation alice qcnv) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "protocol" `shouldMatch` "mixed"

  bindResponse (putConversationProtocol alice qcnv "mixed") $ \resp -> do
    resp.status `shouldMatchInt` 204

testAddUser :: HasCallStack => App ()
testAddUser = do
  [alice, bob] <- createAndConnectUsers [ownDomain, ownDomain]

  [alice1, bob1, bob2] <- traverse createMLSClient [alice, bob, bob]

  traverse_ uploadNewKeyPackage [bob1, bob2]

  (_, qcnv) <- setupMLSGroup alice1

  resp <- createAddCommit alice1 [bob] >>= sendAndConsumeCommitBundle
  events <- resp %. "events" & asList
  do
    event <- assertOne events
    shouldMatch (event %. "qualified_conversation") qcnv
    shouldMatch (event %. "type") "conversation.member-join"
    shouldMatch (event %. "from") (objId alice)
    members <- event %. "data" %. "users" & asList
    memberQids <- for members $ \mem -> mem %. "qualified_id"
    bobQid <- bob %. "qualified_id"
    shouldMatch memberQids [bobQid]

  -- check that bob can now see the conversation
  convs <- getAllConvs bob
  convIds <- traverse (%. "qualified_id") convs
  void $
    assertBool
      "Users added to an MLS group should find it when listing conversations"
      (qcnv `elem` convIds)

testCreateSubConv :: HasCallStack => App ()
testCreateSubConv = do
  alice <- randomUser ownDomain def
  alice1 <- createMLSClient alice
  (_, conv) <- setupMLSGroup alice1
  bindResponse (getSubConversation alice conv "conference") $ \resp -> do
    resp.status `shouldMatchInt` 200
    let tm = resp.json %. "epoch_timestamp"
    tm `shouldMatch` Null

testCreateSubConvProteus :: App ()
testCreateSubConvProteus = do
  alice <- randomUser ownDomain def
  conv <- bindResponse (postConversation alice defProteus) $ \resp -> do
    resp.status `shouldMatchInt` 201
    resp.json
  bindResponse (getSubConversation alice conv "conference") $ \resp ->
    resp.status `shouldMatchInt` 404

-- FUTUREWORK: New clients should be adding themselves via external commits, and
-- they shouldn't be added by another client. Change the test so external
-- commits are used.
testSelfConversation :: App ()
testSelfConversation = do
  alice <- randomUser ownDomain def
  creator : others <- traverse createMLSClient (replicate 3 alice)
  traverse_ uploadNewKeyPackage others
  void $ setupMLSSelfGroup creator
  commit <- createAddCommit creator [alice]
  welcome <- assertOne (toList commit.welcome)

  withWebSockets others $ \wss -> do
    void $ sendAndConsumeCommitBundle commit
    let isWelcome n = nPayload n %. "type" `isEqual` "conversation.mls-welcome"
    for_ wss $ \ws -> do
      n <- awaitMatch 3 isWelcome ws
      shouldMatch (nPayload n %. "conversation") (objId alice)
      shouldMatch (nPayload n %. "from") (objId alice)
      shouldMatch (nPayload n %. "data") (B8.unpack (Base64.encode welcome))

testJoinSubConv :: App ()
testJoinSubConv = do
  [alice, bob] <- createAndConnectUsers [ownDomain, ownDomain]
  [alice1, bob1, bob2] <- traverse createMLSClient [alice, bob, bob]
  traverse_ uploadNewKeyPackage [bob1, bob2]
  (_, qcnv) <- setupMLSGroup alice1
  void $ createAddCommit alice1 [bob] >>= sendAndConsumeCommitBundle

  sub <- bindResponse (getSubConversation bob qcnv "conference") $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json
  resetGroup bob1 sub

  -- bob adds his first client to the subconversation
  void $ createPendingProposalCommit bob1 >>= sendAndConsumeCommitBundle
  sub' <- bindResponse (getSubConversation bob qcnv "conference") $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json
  do
    tm <- sub' %. "epoch_timestamp"
    assertBool "Epoch timestamp should not be null" (tm /= Null)

  -- now alice joins with her own client
  void $
    createExternalCommit alice1 Nothing
      >>= sendAndConsumeCommitBundle
