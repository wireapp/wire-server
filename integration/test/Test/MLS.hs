{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Test.MLS where

import API.Brig (claimKeyPackages)
import API.Galley
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text.Encoding as T
import MLS.Util
import SetupHelpers
import Testlib.Prelude

testMixedProtocolUpgrade :: HasCallStack => Domain -> App ()
testMixedProtocolUpgrade secondDomain = do
  (alice, tid) <- createTeam OwnDomain
  [bob, charlie] <- replicateM 2 (randomUser secondDomain def)
  connectUsers [alice, bob, charlie]

  qcnv <-
    postConversation
      alice
      defProteus
        { qualifiedUsers = [bob, charlie],
          team = Just tid
        }
      >>= getJSON 201

  bindResponse (putConversationProtocol bob qcnv "mls") $ \resp -> do
    resp.status `shouldMatchInt` 403

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

  bindResponse (putConversationProtocol bob qcnv "proteus") $ \resp -> do
    resp.status `shouldMatchInt` 403

  bindResponse (putConversationProtocol bob qcnv "invalid") $ \resp -> do
    resp.status `shouldMatchInt` 400

testMixedProtocolNonTeam :: HasCallStack => Domain -> App ()
testMixedProtocolNonTeam secondDomain = do
  [alice, bob] <- createAndConnectUsers [OwnDomain, secondDomain]
  qcnv <-
    postConversation alice defProteus {qualifiedUsers = [bob]}
      >>= getJSON 201

  bindResponse (putConversationProtocol bob qcnv "mixed") $ \resp -> do
    resp.status `shouldMatchInt` 403

testMixedProtocolAddUsers :: HasCallStack => Domain -> App ()
testMixedProtocolAddUsers secondDomain = do
  (alice, tid) <- createTeam OwnDomain
  [bob, charlie] <- replicateM 2 (randomUser secondDomain def)
  connectUsers [alice, bob, charlie]

  qcnv <-
    postConversation alice defProteus {qualifiedUsers = [bob], team = Just tid}
      >>= getJSON 201

  bindResponse (putConversationProtocol bob qcnv "mixed") $ \resp -> do
    resp.status `shouldMatchInt` 200

  [alice1, bob1] <- traverse createMLSClient [alice, bob]

  bindResponse (getConversation alice qcnv) $ \resp -> do
    resp.status `shouldMatchInt` 200
    createGroup alice1 resp.json

  traverse_ uploadNewKeyPackage [bob1]

  withWebSockets [alice, bob] $ \wss -> do
    mp <- createAddCommit alice1 [bob]
    welcome <- assertJust "should have welcome" mp.welcome
    void $ sendAndConsumeCommitBundle mp
    for_ wss $ \ws -> do
      n <- awaitMatch 3 (\n -> nPayload n %. "type" `isEqual` "conversation.mls-welcome") ws
      nPayload n %. "data" `shouldMatch` T.decodeUtf8 (Base64.encode welcome)

testMixedProtocolUserLeaves :: HasCallStack => Domain -> App ()
testMixedProtocolUserLeaves secondDomain = do
  (alice, tid) <- createTeam OwnDomain
  bob <- randomUser secondDomain def
  connectUsers [alice, bob]

  qcnv <-
    postConversation alice defProteus {qualifiedUsers = [bob], team = Just tid}
      >>= getJSON 201

  bindResponse (putConversationProtocol bob qcnv "mixed") $ \resp -> do
    resp.status `shouldMatchInt` 200

  [alice1, bob1] <- traverse createMLSClient [alice, bob]

  bindResponse (getConversation alice qcnv) $ \resp -> do
    resp.status `shouldMatchInt` 200
    createGroup alice1 resp.json

  traverse_ uploadNewKeyPackage [bob1]

  mp <- createAddCommit alice1 [bob]
  void $ sendAndConsumeCommitBundle mp

  withWebSocket alice $ \ws -> do
    bindResponse (removeConversationMember bob qcnv) $ \resp ->
      resp.status `shouldMatchInt` 200

    n <- awaitMatch 3 (\n -> nPayload n %. "type" `isEqual` "conversation.mls-message-add") ws

    msg <- asByteString (nPayload n %. "data") >>= showMessage alice1
    let leafIndexBob = 1
    msg %. "message.content.body.Proposal.Remove.removed" `shouldMatchInt` leafIndexBob
    msg %. "message.content.sender.External" `shouldMatchInt` 0

testMixedProtocolAddPartialClients :: HasCallStack => Domain -> App ()
testMixedProtocolAddPartialClients secondDomain = do
  (alice, tid) <- createTeam OwnDomain
  bob <- randomUser secondDomain def
  connectUsers [alice, bob]

  qcnv <-
    postConversation alice defProteus {qualifiedUsers = [bob], team = Just tid}
      >>= getJSON 201

  bindResponse (putConversationProtocol bob qcnv "mixed") $ \resp -> do
    resp.status `shouldMatchInt` 200

  [alice1, bob1, bob2] <- traverse createMLSClient [alice, bob, bob]

  bindResponse (getConversation alice qcnv) $ \resp -> do
    resp.status `shouldMatchInt` 200
    createGroup alice1 resp.json

  traverse_ uploadNewKeyPackage [bob1, bob1, bob2, bob2]

  -- create add commit for only one of bob's two clients
  do
    bundle <- claimKeyPackages alice1 bob >>= getJSON 200
    kps <- unbundleKeyPackages bundle
    kp1 <- assertOne (filter ((== bob1) . fst) kps)
    mp <- createAddCommitWithKeyPackages alice1 [kp1]
    void $ sendAndConsumeCommitBundle mp

  -- this tests that bob's backend has a mapping of group id to the remote conv
  -- this test is only interesting when bob is on OtherDomain
  do
    bundle <- claimKeyPackages bob1 bob >>= getJSON 200
    kps <- unbundleKeyPackages bundle
    kp2 <- assertOne (filter ((== bob2) . fst) kps)
    mp <- createAddCommitWithKeyPackages bob1 [kp2]
    void $ postMLSCommitBundle mp.sender (mkBundle mp) >>= getJSON 201

testMixedProtocolRemovePartialClients :: HasCallStack => Domain -> App ()
testMixedProtocolRemovePartialClients secondDomain = do
  (alice, tid) <- createTeam OwnDomain
  bob <- randomUser secondDomain def
  connectUsers [alice, bob]

  qcnv <-
    postConversation alice defProteus {qualifiedUsers = [bob], team = Just tid}
      >>= getJSON 201

  bindResponse (putConversationProtocol bob qcnv "mixed") $ \resp -> do
    resp.status `shouldMatchInt` 200

  [alice1, bob1, bob2] <- traverse createMLSClient [alice, bob, bob]

  bindResponse (getConversation alice qcnv) $ \resp -> do
    resp.status `shouldMatchInt` 200
    createGroup alice1 resp.json

  traverse_ uploadNewKeyPackage [bob1, bob2]
  void $ createAddCommit alice1 [bob] >>= sendAndConsumeCommitBundle
  mp <- createRemoveCommit alice1 [bob1]

  void $ postMLSCommitBundle mp.sender (mkBundle mp) >>= getJSON 201

testMixedProtocolAppMessagesAreDenied :: HasCallStack => Domain -> App ()
testMixedProtocolAppMessagesAreDenied secondDomain = do
  (alice, tid) <- createTeam OwnDomain
  bob <- randomUser secondDomain def
  connectUsers [alice, bob]

  qcnv <-
    postConversation alice defProteus {qualifiedUsers = [bob], team = Just tid}
      >>= getJSON 201

  bindResponse (putConversationProtocol bob qcnv "mixed") $ \resp -> do
    resp.status `shouldMatchInt` 200

  [alice1, bob1] <- traverse createMLSClient [alice, bob]

  traverse_ uploadNewKeyPackage [bob1]

  bindResponse (getConversation alice qcnv) $ \resp -> do
    resp.status `shouldMatchInt` 200
    createGroup alice1 resp.json

  void $ createAddCommit alice1 [bob] >>= sendAndConsumeCommitBundle

  mp <- createApplicationMessage bob1 "hello, world"
  bindResponse (postMLSMessage mp.sender mp.message) $ \resp -> do
    resp.status `shouldMatchInt` 422
    resp.json %. "label" `shouldMatch` "mls-unsupported-message"

testMLSProtocolUpgrade :: HasCallStack => Domain -> App ()
testMLSProtocolUpgrade secondDomain = do
  (alice, bob, conv) <- simpleMixedConversationSetup secondDomain

  -- alice creates MLS group and bob joins
  [alice1, bob1] <- traverse createMLSClient [alice, bob]
  createGroup alice1 conv
  void $ createPendingProposalCommit alice1 >>= sendAndConsumeCommitBundle
  void $ createExternalCommit bob1 Nothing >>= sendAndConsumeCommitBundle

  supportMLS alice
  bindResponse (putConversationProtocol bob conv "mls") $ \resp -> do
    -- TODO: should this return a different error?
    resp.status `shouldMatchInt` 403
  bindResponse (getConversation alice conv) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "protocol" `shouldMatch` "mixed"

  supportMLS bob
  bindResponse (putConversationProtocol bob conv "mls") $ \resp -> do
    resp.status `shouldMatchInt` 200
  bindResponse (getConversation alice conv) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "protocol" `shouldMatch` "mls"

testAddUser :: HasCallStack => App ()
testAddUser = do
  [alice, bob] <- createAndConnectUsers [OwnDomain, OwnDomain]

  [alice1, bob1, bob2] <- traverse createMLSClient [alice, bob, bob]

  traverse_ uploadNewKeyPackage [bob1, bob2]

  (_, qcnv) <- createNewGroup alice1

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

testRemoteAddUser :: HasCallStack => App ()
testRemoteAddUser = do
  [alice, bob, charlie] <- createAndConnectUsers [OwnDomain, OtherDomain, OwnDomain]
  [alice1, bob1, charlie1] <- traverse createMLSClient [alice, bob, charlie]
  traverse_ uploadNewKeyPackage [bob1, charlie1]
  (_, conv) <- createNewGroup alice1
  void $ createAddCommit alice1 [bob] >>= sendAndConsumeCommitBundle
  bindResponse (updateConversationMember alice1 conv bob "wire_admin") $ \resp ->
    resp.status `shouldMatchInt` 200

  mp <- createAddCommit bob1 [charlie]
  -- Support for remote admins is not implemeted yet, but this shows that add
  -- proposal is being applied action
  bindResponse (postMLSCommitBundle mp.sender (mkBundle mp)) $ \resp -> do
    resp.status `shouldMatchInt` 500
    resp.json %. "label" `shouldMatch` "federation-not-implemented"

testCreateSubConv :: HasCallStack => App ()
testCreateSubConv = do
  alice <- randomUser OwnDomain def
  alice1 <- createMLSClient alice
  (_, conv) <- createNewGroup alice1
  bindResponse (getSubConversation alice conv "conference") $ \resp -> do
    resp.status `shouldMatchInt` 200
    let tm = resp.json %. "epoch_timestamp"
    tm `shouldMatch` Null

testCreateSubConvProteus :: App ()
testCreateSubConvProteus = do
  alice <- randomUser OwnDomain def
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
  alice <- randomUser OwnDomain def
  creator : others <- traverse createMLSClient (replicate 3 alice)
  traverse_ uploadNewKeyPackage others
  (_, cnv) <- createSelfGroup creator
  commit <- createAddCommit creator [alice]
  welcome <- assertOne (toList commit.welcome)

  withWebSockets others $ \wss -> do
    void $ sendAndConsumeCommitBundle commit
    let isWelcome n = nPayload n %. "type" `isEqual` "conversation.mls-welcome"
    for_ wss $ \ws -> do
      n <- awaitMatch 3 isWelcome ws
      shouldMatch (nPayload n %. "conversation") (objId cnv)
      shouldMatch (nPayload n %. "from") (objId alice)
      shouldMatch (nPayload n %. "data") (B8.unpack (Base64.encode welcome))

testJoinSubConv :: App ()
testJoinSubConv = do
  [alice, bob] <- createAndConnectUsers [OwnDomain, OwnDomain]
  [alice1, bob1, bob2] <- traverse createMLSClient [alice, bob, bob]
  traverse_ uploadNewKeyPackage [bob1, bob2]
  (_, qcnv) <- createNewGroup alice1
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

testDeleteParentOfSubConv :: HasCallStack => Domain -> App ()
testDeleteParentOfSubConv secondDomain = do
  (alice, tid) <- createTeam OwnDomain
  bob <- randomUser secondDomain def
  connectUsers [alice, bob]

  [alice1, bob1] <- traverse createMLSClient [alice, bob]
  traverse_ uploadNewKeyPackage [alice1, bob1]
  (_, qcnv) <- createNewGroup alice1
  void $ createAddCommit alice1 [bob] >>= sendAndConsumeCommitBundle

  sub <- bindResponse (getSubConversation bob qcnv "conference") $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json
  resetGroup bob1 sub

  -- bob adds his client to the subconversation
  void $ createPendingProposalCommit bob1 >>= sendAndConsumeCommitBundle

  -- alice joins with her own client
  void $ createExternalCommit alice1 Nothing >>= sendAndConsumeCommitBundle

  -- bob sends a message to the subconversation
  do
    mp <- createApplicationMessage bob1 "hello, alice"
    void . bindResponse (postMLSMessage mp.sender mp.message) $ \resp -> do
      resp.status `shouldMatchInt` 201

  -- alice sends a message to the subconversation
  do
    mp <- createApplicationMessage bob1 "hello, bob"
    void . bindResponse (postMLSMessage mp.sender mp.message) $ \resp -> do
      resp.status `shouldMatchInt` 201

  -- alice deletes main conversation
  void . bindResponse (deleteTeamConv tid qcnv alice) $ \resp -> do
    resp.status `shouldMatchInt` 200

  -- bob fails to send a message to the subconversation
  do
    mp <- createApplicationMessage bob1 "hello, alice"
    void . bindResponse (postMLSMessage mp.sender mp.message) $ \resp -> do
      resp.status `shouldMatchInt` 404
      case secondDomain of
        OwnDomain -> resp.json %. "label" `shouldMatch` "no-conversation"
        OtherDomain -> resp.json %. "label" `shouldMatch` "no-conversation-member"

  -- alice fails to send a message to the subconversation
  do
    mp <- createApplicationMessage alice1 "hello, bob"
    void . bindResponse (postMLSMessage mp.sender mp.message) $ \resp -> do
      resp.status `shouldMatchInt` 404
      resp.json %. "label" `shouldMatch` "no-conversation"

-- | FUTUREWORK: Don't allow partial adds, not even in the first commit
testFirstCommitAllowsPartialAdds :: HasCallStack => App ()
testFirstCommitAllowsPartialAdds = do
  alice <- randomUser OwnDomain def

  [alice1, alice2, alice3] <- traverse createMLSClient [alice, alice, alice]
  traverse_ uploadNewKeyPackage [alice1, alice2, alice2, alice3, alice3]

  (_, _qcnv) <- createNewGroup alice1

  bundle <- claimKeyPackages alice1 alice >>= getJSON 200
  kps <- unbundleKeyPackages bundle

  -- first commit only adds kp for alice2 (not alice2 and alice3)
  mp <- createAddCommitWithKeyPackages alice1 (filter ((== alice2) . fst) kps)
  bindResponse (postMLSCommitBundle mp.sender (mkBundle mp)) $ \resp -> do
    resp.status `shouldMatchInt` 409
    resp.json %. "label" `shouldMatch` "mls-client-mismatch"

testAddUserPartial :: HasCallStack => App ()
testAddUserPartial = do
  [alice, bob, charlie] <- createAndConnectUsers (replicate 3 OwnDomain)

  -- Bob has 3 clients, Charlie has 2
  alice1 <- createMLSClient alice
  bobClients@[_bob1, _bob2, bob3] <- replicateM 3 (createMLSClient bob)
  charlieClients <- replicateM 2 (createMLSClient charlie)

  -- Only the first 2 clients of Bob's have uploaded key packages
  traverse_ uploadNewKeyPackage (take 2 bobClients <> charlieClients)

  -- alice adds bob's first 2 clients
  void $ createNewGroup alice1

  -- alice sends a commit now, and should get a conflict error
  kps <- fmap concat . for [bob, charlie] $ \user -> do
    bundle <- claimKeyPackages alice1 user >>= getJSON 200
    unbundleKeyPackages bundle
  mp <- createAddCommitWithKeyPackages alice1 kps

  -- before alice can commit, bob3 uploads a key package
  void $ uploadNewKeyPackage bob3

  err <- postMLSCommitBundle mp.sender (mkBundle mp) >>= getJSON 409
  err %. "label" `shouldMatch` "mls-client-mismatch"

-- | admin removes user from a conversation but doesn't list all clients
testRemoveClientsIncomplete :: HasCallStack => App ()
testRemoveClientsIncomplete = do
  [alice, bob] <- createAndConnectUsers [OwnDomain, OwnDomain]

  [alice1, bob1, bob2] <- traverse createMLSClient [alice, bob, bob]
  traverse_ uploadNewKeyPackage [bob1, bob2]
  void $ createNewGroup alice1
  void $ createAddCommit alice1 [bob] >>= sendAndConsumeCommitBundle
  mp <- createRemoveCommit alice1 [bob1]

  err <- postMLSCommitBundle mp.sender (mkBundle mp) >>= getJSON 409
  err %. "label" `shouldMatch` "mls-client-mismatch"
