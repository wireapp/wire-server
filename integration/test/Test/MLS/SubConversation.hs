module Test.MLS.SubConversation where

import API.Galley
import MLS.Util
import Notifications
import SetupHelpers
import Testlib.Prelude

testJoinSubConv :: App ()
testJoinSubConv = do
  [alice, bob] <- createAndConnectUsers [OwnDomain, OwnDomain]
  [alice1, bob1, bob2] <- traverse (createMLSClient def) [alice, bob, bob]
  traverse_ uploadNewKeyPackage [bob1, bob2]
  (_, qcnv) <- createNewGroup alice1

  void $ createAddCommit alice1 [bob] >>= sendAndConsumeCommitBundle
  createSubConv bob1 "conference"

  -- bob adds his first client to the subconversation
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
  (alice, tid, _) <- createTeam OwnDomain 1
  bob <- randomUser secondDomain def
  connectUsers [alice, bob]

  [alice1, bob1] <- traverse (createMLSClient def) [alice, bob]
  traverse_ uploadNewKeyPackage [alice1, bob1]
  (_, qcnv) <- createNewGroup alice1
  void $ createAddCommit alice1 [bob] >>= sendAndConsumeCommitBundle

  -- bob creates a subconversation and adds his own client
  createSubConv bob1 "conference"

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
  withWebSocket bob $ \ws -> do
    void . bindResponse (deleteTeamConv tid qcnv alice) $ \resp -> do
      resp.status `shouldMatchInt` 200
    void $ awaitMatch isConvDeleteNotif ws

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

testDeleteSubConversation :: HasCallStack => Domain -> App ()
testDeleteSubConversation otherDomain = do
  [alice, bob] <- createAndConnectUsers [OwnDomain, otherDomain]
  charlie <- randomUser OwnDomain def
  [alice1, bob1] <- traverse (createMLSClient def) [alice, bob]
  void $ uploadNewKeyPackage bob1
  (_, qcnv) <- createNewGroup alice1
  void $ createAddCommit alice1 [bob] >>= sendAndConsumeCommitBundle

  createSubConv alice1 "conference1"
  sub1 <- getSubConversation alice qcnv "conference1" >>= getJSON 200
  void $ deleteSubConversation charlie sub1 >>= getBody 403
  void $ deleteSubConversation alice sub1 >>= getBody 200

  createSubConv alice1 "conference2"
  sub2 <- getSubConversation alice qcnv "conference2" >>= getJSON 200
  void $ deleteSubConversation bob sub2 >>= getBody 200

  sub2' <- getSubConversation alice1 qcnv "conference2" >>= getJSON 200
  sub2 `shouldNotMatch` sub2'

data LeaveSubConvVariant = AliceLeaves | BobLeaves

instance HasTests x => HasTests (LeaveSubConvVariant -> x) where
  mkTests m n s f x =
    mkTests m (n <> "[leaver=alice]") s f (x AliceLeaves)
      <> mkTests m (n <> "[leaver=bob]") s f (x BobLeaves)

testLeaveSubConv :: HasCallStack => LeaveSubConvVariant -> App ()
testLeaveSubConv variant = do
  [alice, bob, charlie] <- createAndConnectUsers [OwnDomain, OwnDomain, OtherDomain]
  clients@[alice1, bob1, bob2, charlie1] <- traverse (createMLSClient def) [alice, bob, bob, charlie]
  traverse_ uploadNewKeyPackage [bob1, bob2, charlie1]
  void $ createNewGroup alice1

  withWebSockets [bob, charlie] $ \wss -> do
    void $ createAddCommit alice1 [bob, charlie] >>= sendAndConsumeCommitBundle
    traverse_ (awaitMatch isMemberJoinNotif) wss

  createSubConv bob1 "conference"
  void $ createExternalCommit alice1 Nothing >>= sendAndConsumeCommitBundle
  void $ createExternalCommit bob2 Nothing >>= sendAndConsumeCommitBundle
  void $ createExternalCommit charlie1 Nothing >>= sendAndConsumeCommitBundle

  -- a member leaves the subconversation
  let (firstLeaver, idxFirstLeaver) = case variant of
        BobLeaves -> (bob1, 0)
        AliceLeaves -> (alice1, 1)
  let idxCharlie1 = 3

  let others = filter (/= firstLeaver) clients
  withWebSockets others $ \wss -> do
    leaveCurrentConv firstLeaver

    for_ (zip others wss) $ \(cid, ws) -> do
      msg <- consumeMessage cid Nothing ws
      msg %. "message.content.body.Proposal.Remove.removed" `shouldMatchInt` idxFirstLeaver
      msg %. "message.content.sender.External" `shouldMatchInt` 0

  withWebSockets (tail others) $ \wss -> do
    -- a member commits the pending proposal
    void $ createPendingProposalCommit (head others) >>= sendAndConsumeCommitBundle
    traverse_ (awaitMatch isNewMLSMessageNotif) wss

    -- send an application message
    void $ createApplicationMessage (head others) "good riddance" >>= sendAndConsumeMessage
    traverse_ (awaitMatch isNewMLSMessageNotif) wss

  -- check that only 3 clients are left in the subconv
  do
    conv <- getCurrentConv (head others)
    mems <- conv %. "members" & asList
    length mems `shouldMatchInt` 3

  -- charlie1 leaves
  let others' = filter (/= charlie1) others
  withWebSockets others' $ \wss -> do
    leaveCurrentConv charlie1

    for_ (zip others' wss) $ \(cid, ws) -> do
      msg <- consumeMessage cid Nothing ws
      msg %. "message.content.body.Proposal.Remove.removed" `shouldMatchInt` idxCharlie1
      msg %. "message.content.sender.External" `shouldMatchInt` 0

  -- a member commits the pending proposal
  void $ createPendingProposalCommit (head others') >>= sendAndConsumeCommitBundle

  -- check that only 2 clients are left in the subconv
  do
    conv <- getCurrentConv (head others)
    mems <- conv %. "members" & asList
    length mems `shouldMatchInt` 2
