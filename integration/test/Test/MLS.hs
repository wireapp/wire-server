{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Test.MLS where

import API.Brig (claimKeyPackages, deleteClient)
import API.Galley
import Data.ByteString.Base64 qualified as Base64
import Data.ByteString.Char8 qualified as B8
import Data.Text.Encoding qualified as T
import MLS.Util
import SetupHelpers
import Testlib.Prelude

testSendMessageNoReturnToSender :: HasCallStack => App ()
testSendMessageNoReturnToSender = do
  [alice, bob] <- createAndConnectUsers [OwnDomain, OwnDomain]
  [alice1, alice2, bob1, bob2] <- traverse (createMLSClient def) [alice, alice, bob, bob]
  traverse_ uploadNewKeyPackage [alice2, bob1, bob2]
  void $ createNewGroup alice1
  void $ createAddCommit alice1 [alice, bob] >>= sendAndConsumeCommitBundle

  -- alice1 sends a message to the conversation, all clients but alice1 receive
  -- the message
  withWebSockets [alice1, alice2, bob1, bob2] $ \(wsSender : wss) -> do
    mp <- createApplicationMessage alice1 "hello, bob"
    void . bindResponse (postMLSMessage mp.sender mp.message) $ \resp -> do
      resp.status `shouldMatchInt` 201
    for_ wss $ \ws -> do
      n <- awaitMatch 3 (\n -> nPayload n %. "type" `isEqual` "conversation.mls-message-add") ws
      nPayload n %. "data" `shouldMatch` T.decodeUtf8 (Base64.encode mp.message)
    expectFailure (const $ pure ()) $
      awaitMatch
        3
        ( \n ->
            liftM2
              (&&)
              (nPayload n %. "type" `isEqual` "conversation.mls-message-add")
              (nPayload n %. "data" `isEqual` T.decodeUtf8 (Base64.encode mp.message))
        )
        wsSender

testStaleApplicationMessage :: HasCallStack => Domain -> App ()
testStaleApplicationMessage otherDomain = do
  [alice, bob, charlie, dave, eve] <-
    createAndConnectUsers [OwnDomain, otherDomain, OwnDomain, OwnDomain, OwnDomain]
  [alice1, bob1, charlie1] <- traverse (createMLSClient def) [alice, bob, charlie]
  traverse_ uploadNewKeyPackage [bob1, charlie1]
  void $ createNewGroup alice1

  -- alice adds bob first
  void $ createAddCommit alice1 [bob] >>= sendAndConsumeCommitBundle

  -- bob prepares some application messages
  [msg1, msg2] <- replicateM 2 $ createApplicationMessage bob1 "hi alice"

  -- alice adds charlie and dave with different commits
  void $ createAddCommit alice1 [charlie] >>= sendAndConsumeCommitBundle
  void $ createAddCommit alice1 [dave] >>= sendAndConsumeCommitBundle

  -- bob's application messages still go through
  void $ postMLSMessage bob1 msg1.message >>= getJSON 201

  -- alice adds eve
  void $ createAddCommit alice1 [eve] >>= sendAndConsumeCommitBundle

  -- bob's application messages are now rejected
  void $ postMLSMessage bob1 msg2.message >>= getJSON 409

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

  [alice1, bob1] <- traverse (createMLSClient def) [alice, bob]

  bindResponse (getConversation alice qcnv) $ \resp -> do
    resp.status `shouldMatchInt` 200
    createGroup alice1 resp.json

  traverse_ uploadNewKeyPackage [bob1]

  withWebSocket bob $ \ws -> do
    mp <- createAddCommit alice1 [bob]
    welcome <- assertJust "should have welcome" mp.welcome
    void $ sendAndConsumeCommitBundle mp
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

  [alice1, bob1] <- traverse (createMLSClient def) [alice, bob]

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

  [alice1, bob1, bob2] <- traverse (createMLSClient def) [alice, bob, bob]

  bindResponse (getConversation alice qcnv) $ \resp -> do
    resp.status `shouldMatchInt` 200
    createGroup alice1 resp.json

  traverse_ uploadNewKeyPackage [bob1, bob1, bob2, bob2]

  -- create add commit for only one of bob's two clients
  do
    bundle <- claimKeyPackages def alice1 bob >>= getJSON 200
    kps <- unbundleKeyPackages bundle
    kp1 <- assertOne (filter ((== bob1) . fst) kps)
    mp <- createAddCommitWithKeyPackages alice1 [kp1]
    void $ sendAndConsumeCommitBundle mp

  -- this tests that bob's backend has a mapping of group id to the remote conv
  -- this test is only interesting when bob is on OtherDomain
  do
    bundle <- claimKeyPackages def bob1 bob >>= getJSON 200
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

  [alice1, bob1, bob2] <- traverse (createMLSClient def) [alice, bob, bob]

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

  [alice1, bob1] <- traverse (createMLSClient def) [alice, bob]

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
  charlie <- randomUser OwnDomain def

  -- alice creates MLS group and bob joins
  [alice1, bob1, charlie1] <- traverse (createMLSClient def) [alice, bob, charlie]
  createGroup alice1 conv
  void $ createPendingProposalCommit alice1 >>= sendAndConsumeCommitBundle
  void $ createExternalCommit bob1 Nothing >>= sendAndConsumeCommitBundle

  -- charlie is added to the group
  void $ uploadNewKeyPackage charlie1
  void $ createAddCommit alice1 [charlie] >>= sendAndConsumeCommitBundle

  supportMLS alice
  bindResponse (putConversationProtocol bob conv "mls") $ \resp -> do
    resp.status `shouldMatchInt` 400
    resp.json %. "label" `shouldMatch` "mls-migration-criteria-not-satisfied"
  bindResponse (getConversation alice conv) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "protocol" `shouldMatch` "mixed"

  supportMLS bob

  withWebSockets [alice1, bob1] $ \wss -> do
    bindResponse (putConversationProtocol bob conv "mls") $ \resp -> do
      resp.status `shouldMatchInt` 200
    for_ wss $ \ws -> do
      let isMessage n = nPayload n %. "type" `isEqual` "conversation.mls-message-add"
      n <- awaitMatch 3 isMessage ws
      msg <- asByteString (nPayload n %. "data") >>= showMessage alice1
      let leafIndexCharlie = 2
      msg %. "message.content.body.Proposal.Remove.removed" `shouldMatchInt` leafIndexCharlie
      msg %. "message.content.sender.External" `shouldMatchInt` 0

  bindResponse (getConversation alice conv) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "protocol" `shouldMatch` "mls"

testAddUserSimple :: HasCallStack => Ciphersuite -> CredentialType -> App ()
testAddUserSimple suite ctype = do
  setMLSCiphersuite suite
  [alice, bob] <- createAndConnectUsers [OwnDomain, OwnDomain]
  [alice1, bob1, bob2] <- traverse (createMLSClient def {credType = ctype}) [alice, bob, bob]

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
  [alice1, bob1, charlie1] <- traverse (createMLSClient def) [alice, bob, charlie]
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

testRemoteRemoveClient :: HasCallStack => App ()
testRemoteRemoveClient = do
  [alice, bob] <- createAndConnectUsers [OwnDomain, OtherDomain]
  [alice1, bob1] <- traverse (createMLSClient def) [alice, bob]
  void $ uploadNewKeyPackage bob1
  (_, conv) <- createNewGroup alice1
  void $ createAddCommit alice1 [bob] >>= sendAndConsumeCommitBundle

  withWebSocket alice $ \wsAlice -> do
    void $ deleteClient bob bob1.client >>= getBody 200
    let predicate n = nPayload n %. "type" `isEqual` "conversation.mls-message-add"
    n <- awaitMatch 5 predicate wsAlice
    shouldMatch (nPayload n %. "conversation") (objId conv)
    shouldMatch (nPayload n %. "from") (objId bob)

    msg <- asByteString (nPayload n %. "data") >>= showMessage alice1
    let leafIndexBob = 1
    msg %. "message.content.body.Proposal.Remove.removed" `shouldMatchInt` leafIndexBob
    msg %. "message.content.sender.External" `shouldMatchInt` 0

testCreateSubConv :: HasCallStack => Ciphersuite -> App ()
testCreateSubConv suite = do
  setMLSCiphersuite suite
  alice <- randomUser OwnDomain def
  alice1 <- createMLSClient def alice
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
  creator : others <- traverse (createMLSClient def) (replicate 3 alice)
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
  [alice1, bob1, bob2] <- traverse (createMLSClient def) [alice, bob, bob]
  traverse_ uploadNewKeyPackage [bob1, bob2]
  (_, qcnv) <- createNewGroup alice1
  void $ createAddCommit alice1 [bob] >>= sendAndConsumeCommitBundle
  void $ createSubConv bob1 "conference"

  -- bob adds his first client to the subconversation
  void $ createPendingProposalCommit bob1 >>= sendAndConsumeCommitBundle
  sub' <- getSubConversation bob qcnv "conference" >>= getJSON 200
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

  [alice1, bob1] <- traverse (createMLSClient def) [alice, bob]
  traverse_ uploadNewKeyPackage [alice1, bob1]
  (_, qcnv) <- createNewGroup alice1
  void $ createAddCommit alice1 [bob] >>= sendAndConsumeCommitBundle
  void $ createSubConv bob1 "conference"

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

  [alice1, alice2, alice3] <- traverse (createMLSClient def) [alice, alice, alice]
  traverse_ uploadNewKeyPackage [alice1, alice2, alice2, alice3, alice3]

  (_, _qcnv) <- createNewGroup alice1

  bundle <- claimKeyPackages def alice1 alice >>= getJSON 200
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
  alice1 <- createMLSClient def alice
  bobClients@[_bob1, _bob2, bob3] <- replicateM 3 (createMLSClient def bob)
  charlieClients <- replicateM 2 (createMLSClient def charlie)

  -- Only the first 2 clients of Bob's have uploaded key packages
  traverse_ uploadNewKeyPackage (take 2 bobClients <> charlieClients)

  -- alice adds bob's first 2 clients
  void $ createNewGroup alice1

  -- alice sends a commit now, and should get a conflict error
  kps <- fmap concat . for [bob, charlie] $ \user -> do
    bundle <- claimKeyPackages def alice1 user >>= getJSON 200
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

  [alice1, bob1, bob2] <- traverse (createMLSClient def) [alice, bob, bob]
  traverse_ uploadNewKeyPackage [bob1, bob2]
  void $ createNewGroup alice1
  void $ createAddCommit alice1 [bob] >>= sendAndConsumeCommitBundle
  mp <- createRemoveCommit alice1 [bob1]

  err <- postMLSCommitBundle mp.sender (mkBundle mp) >>= getJSON 409
  err %. "label" `shouldMatch` "mls-client-mismatch"

testAdminRemovesUserFromConv :: HasCallStack => App ()
testAdminRemovesUserFromConv = do
  [alice, bob] <- createAndConnectUsers [OwnDomain, OwnDomain]
  [alice1, bob1, bob2] <- traverse (createMLSClient def) [alice, bob, bob]
  void $ createWireClient bob
  traverse_ uploadNewKeyPackage [bob1, bob2]
  (gid, qcnv) <- createNewGroup alice1
  void $ createAddCommit alice1 [bob] >>= sendAndConsumeCommitBundle
  events <- createRemoveCommit alice1 [bob1, bob2] >>= sendAndConsumeCommitBundle

  do
    event <- assertOne =<< asList (events %. "events")
    event %. "qualified_conversation" `shouldMatch` qcnv
    event %. "type" `shouldMatch` "conversation.member-leave"
    event %. "from" `shouldMatch` objId alice
    members <- event %. "data" %. "qualified_user_ids" & asList
    bobQid <- bob %. "qualified_id"
    shouldMatch members [bobQid]

  convs <- getAllConvs bob
  convIds <- traverse (%. "qualified_id") convs
  clients <- bindResponse (getGroupClients alice gid) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "client_ids" & asList
  void $ assertOne clients
  assertBool
    "bob is not longer part of conversation after the commit"
    (qcnv `notElem` convIds)

testLocalWelcome :: HasCallStack => App ()
testLocalWelcome = do
  users@[alice, bob] <- createAndConnectUsers [OwnDomain, OwnDomain]

  [alice1, bob1] <- traverse (createMLSClient def) users

  void $ uploadNewKeyPackage bob1

  (_, qcnv) <- createNewGroup alice1

  commit <- createAddCommit alice1 [bob]
  Just welcome <- pure commit.welcome

  es <- withWebSocket bob1 $ \wsBob -> do
    es <- sendAndConsumeCommitBundle commit
    let isWelcome n = nPayload n %. "type" `isEqual` "conversation.mls-welcome"

    n <- awaitMatch 5 isWelcome wsBob

    shouldMatch (nPayload n %. "conversation") (objId qcnv)
    shouldMatch (nPayload n %. "from") (objId alice)
    shouldMatch (nPayload n %. "data") (B8.unpack (Base64.encode welcome))
    pure es

  event <- assertOne =<< asList (es %. "events")
  event %. "type" `shouldMatch` "conversation.member-join"
  event %. "conversation" `shouldMatch` objId qcnv
  addedUser <- (event %. "data.users") >>= asList >>= assertOne
  objQid addedUser `shouldMatch` objQid bob

testStaleCommit :: HasCallStack => App ()
testStaleCommit = do
  (alice : users) <- createAndConnectUsers (replicate 5 OwnDomain)
  let (users1, users2) = splitAt 2 users

  (alice1 : clients) <- traverse (createMLSClient def) (alice : users)
  traverse_ uploadNewKeyPackage clients
  void $ createNewGroup alice1

  gsBackup <- getClientGroupState alice1

  -- add the first batch of users to the conversation
  void $ createAddCommit alice1 users1 >>= sendAndConsumeCommitBundle

  -- now roll back alice1 and try to add the second batch of users
  setClientGroupState alice1 gsBackup

  mp <- createAddCommit alice1 users2
  bindResponse (postMLSCommitBundle mp.sender (mkBundle mp)) $ \resp -> do
    resp.status `shouldMatchInt` 409
    resp.json %. "label" `shouldMatch` "mls-stale-message"

testPropInvalidEpoch :: HasCallStack => App ()
testPropInvalidEpoch = do
  users@[_alice, bob, charlie, dee] <- createAndConnectUsers (replicate 4 OwnDomain)
  [alice1, bob1, charlie1, dee1] <- traverse (createMLSClient def) users
  void $ createNewGroup alice1

  -- Add bob -> epoch 1
  void $ uploadNewKeyPackage bob1
  gsBackup <- getClientGroupState alice1
  void $ createAddCommit alice1 [bob] >>= sendAndConsumeCommitBundle
  gsBackup2 <- getClientGroupState alice1

  -- try to send a proposal from an old epoch (0)
  do
    setClientGroupState alice1 gsBackup
    void $ uploadNewKeyPackage dee1
    [prop] <- createAddProposals alice1 [dee]
    bindResponse (postMLSMessage alice1 prop.message) $ \resp -> do
      resp.status `shouldMatchInt` 409
      resp.json %. "label" `shouldMatch` "mls-stale-message"

  -- try to send a proposal from a newer epoch (2)
  do
    void $ uploadNewKeyPackage dee1
    void $ uploadNewKeyPackage charlie1
    setClientGroupState alice1 gsBackup2
    void $ createAddCommit alice1 [charlie] -- --> epoch 2
    [prop] <- createAddProposals alice1 [dee]
    bindResponse (postMLSMessage alice1 prop.message) $ \resp -> do
      resp.status `shouldMatchInt` 409
      resp.json %. "label" `shouldMatch` "mls-stale-message"
    -- remove charlie from users expected to get a welcome message
    modifyMLSState $ \mls -> mls {newMembers = mempty}

  -- alice send a well-formed proposal and commits it
  void $ uploadNewKeyPackage dee1
  setClientGroupState alice1 gsBackup2
  createAddProposals alice1 [dee] >>= traverse_ sendAndConsumeMessage
  void $ createPendingProposalCommit alice1 >>= sendAndConsumeCommitBundle

--- | This test submits a ReInit proposal, which is currently ignored by the
-- backend, in order to check that unsupported proposal types are accepted.
testPropUnsupported :: HasCallStack => App ()
testPropUnsupported = do
  users@[_alice, bob] <- createAndConnectUsers (replicate 2 OwnDomain)
  [alice1, bob1] <- traverse (createMLSClient def) users
  void $ uploadNewKeyPackage bob1
  void $ createNewGroup alice1
  void $ createAddCommit alice1 [bob] >>= sendAndConsumeCommitBundle

  mp <- createReInitProposal alice1

  -- we cannot consume this message, because the membership tag is fake
  void $ postMLSMessage mp.sender mp.message >>= getJSON 201

testAddUserBareProposalCommit :: HasCallStack => App ()
testAddUserBareProposalCommit = do
  [alice, bob] <- createAndConnectUsers (replicate 2 OwnDomain)
  [alice1, bob1] <- traverse (createMLSClient def) [alice, bob]
  (_, qcnv) <- createNewGroup alice1
  void $ uploadNewKeyPackage bob1
  void $ createAddCommit alice1 [] >>= sendAndConsumeCommitBundle

  createAddProposals alice1 [bob]
    >>= traverse_ sendAndConsumeMessage
  commit <- createPendingProposalCommit alice1
  void $ assertJust "Expected welcome" commit.welcome
  void $ sendAndConsumeCommitBundle commit

  -- check that bob can now see the conversation
  convs <- getAllConvs bob
  convIds <- traverse (%. "qualified_id") convs
  void $
    assertBool
      "Users added to an MLS group should find it when listing conversations"
      (qcnv `elem` convIds)

testPropExistingConv :: HasCallStack => App ()
testPropExistingConv = do
  [alice, bob] <- createAndConnectUsers (replicate 2 OwnDomain)
  [alice1, bob1] <- traverse (createMLSClient def) [alice, bob]
  void $ uploadNewKeyPackage bob1
  void $ createNewGroup alice1
  void $ createAddCommit alice1 [] >>= sendAndConsumeCommitBundle
  res <- createAddProposals alice1 [bob] >>= traverse sendAndConsumeMessage >>= assertOne
  shouldBeEmpty (res %. "events")

testCommitNotReferencingAllProposals :: HasCallStack => App ()
testCommitNotReferencingAllProposals = do
  users@[_alice, bob, charlie] <- createAndConnectUsers (replicate 3 OwnDomain)

  [alice1, bob1, charlie1] <- traverse (createMLSClient def) users
  void $ createNewGroup alice1
  traverse_ uploadNewKeyPackage [bob1, charlie1]
  void $ createAddCommit alice1 [] >>= sendAndConsumeCommitBundle

  gsBackup <- getClientGroupState alice1

  -- create proposals for bob and charlie
  createAddProposals alice1 [bob, charlie]
    >>= traverse_ sendAndConsumeMessage

  -- now create a commit referencing only the first proposal
  setClientGroupState alice1 gsBackup
  commit <- createPendingProposalCommit alice1

  -- send commit and expect and error
  bindResponse (postMLSCommitBundle alice1 (mkBundle commit)) $ \resp -> do
    resp.status `shouldMatchInt` 400
    resp.json %. "label" `shouldMatch` "mls-commit-missing-references"

testUnsupportedCiphersuite :: HasCallStack => App ()
testUnsupportedCiphersuite = do
  setMLSCiphersuite (Ciphersuite "0x0002")
  alice <- randomUser OwnDomain def
  alice1 <- createMLSClient def alice
  void $ createNewGroup alice1

  mp <- createPendingProposalCommit alice1

  bindResponse (postMLSCommitBundle alice1 (mkBundle mp)) $ \resp -> do
    resp.status `shouldMatchInt` 400
    resp.json %. "label" `shouldMatch` "mls-protocol-error"
