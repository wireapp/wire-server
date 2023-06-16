module Test.MLS.SubConversation where

import API.Galley
import MLS.Util
import SetupHelpers
import Testlib.Prelude

testJoinSubConv :: App ()
testJoinSubConv = do
  [alice, bob] <- createAndConnectUsers [OwnDomain, OwnDomain]
  [alice1, bob1, bob2] <- traverse createMLSClient [alice, bob, bob]
  traverse_ uploadNewKeyPackage [bob1, bob2]
  (_, qcnv) <- createNewGroup alice1
  void $ createAddCommit alice1 [bob] >>= sendAndConsumeCommitBundle
  createSubConv bob1 "conference"

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

  [alice1, bob1] <- traverse createMLSClient [alice, bob]
  traverse_ uploadNewKeyPackage [alice1, bob1]
  (_, qcnv) <- createNewGroup alice1
  void $ createAddCommit alice1 [bob] >>= sendAndConsumeCommitBundle
  createSubConv bob1 "conference"

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

testDeleteSubConversation :: HasCallStack => Domain -> App ()
testDeleteSubConversation otherDomain = do
  [alice, bob] <- createAndConnectUsers [OwnDomain, otherDomain]
  charlie <- randomUser OwnDomain def
  [alice1, bob1] <- traverse createMLSClient [alice, bob]
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
