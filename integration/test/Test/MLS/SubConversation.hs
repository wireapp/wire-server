module Test.MLS.SubConversation where

import API.Galley
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT (runMaybeT))
import qualified Data.Set as Set
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
  void
    $ createExternalCommit alice1 Nothing
    >>= sendAndConsumeCommitBundle

testJoinOne2OneSubConv :: App ()
testJoinOne2OneSubConv = do
  [alice, bob] <- createAndConnectUsers [OwnDomain, OwnDomain]
  [alice1, bob1, bob2] <- traverse (createMLSClient def) [alice, bob, bob]
  traverse_ uploadNewKeyPackage [bob1, bob2]
  conv <- getMLSOne2OneConversation alice bob >>= getJSON 200
  resetGroup alice1 conv

  void $ createAddCommit alice1 [bob] >>= sendAndConsumeCommitBundle
  createSubConv bob1 "conference"

  -- bob adds his first client to the subconversation
  sub' <- getSubConversation bob conv "conference" >>= getJSON 200
  do
    tm <- sub' %. "epoch_timestamp"
    assertBool "Epoch timestamp should not be null" (tm /= Null)

  -- now alice joins with her own client
  void
    $ createExternalCommit alice1 Nothing
    >>= sendAndConsumeCommitBundle

testDeleteParentOfSubConv :: (HasCallStack) => Domain -> App ()
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

testDeleteSubConversation :: (HasCallStack) => Domain -> App ()
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

data Leaver = Alice | Bob
  deriving stock (Generic)

testLeaveSubConv :: (HasCallStack) => Leaver -> App ()
testLeaveSubConv leaver = do
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
  let (firstLeaver, idxFirstLeaver) = case leaver of
        Bob -> (bob1, 0)
        Alice -> (alice1, 1)
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

testCreatorRemovesUserFromParent :: App ()
testCreatorRemovesUserFromParent = do
  [alice, bob, charlie] <- createAndConnectUsers [OwnDomain, OwnDomain, OtherDomain]
  [alice1, bob1, bob2, charlie1, charlie2] <- traverse (createMLSClient def) [alice, bob, bob, charlie, charlie]
  traverse_ uploadNewKeyPackage [bob1, bob2, charlie1, charlie2]
  (_, qcnv) <- createNewGroup alice1

  _ <- createAddCommit alice1 [bob, charlie] >>= sendAndConsumeCommitBundle

  -- save the state of the parent group
  parentState <- getMLSState
  -- switch to the subgroup
  let subConvName = "conference"
  createSubConv alice1 subConvName

  for_ [bob1, bob2, charlie1, charlie2] \c ->
    createExternalCommit c Nothing >>= sendAndConsumeCommitBundle
  -- save the state of the subgroup and switch to the parent context
  childState <- getMLSState <* setMLSState parentState
  withWebSockets [alice1, charlie1, charlie2] \wss -> do
    removeCommitEvents <- createRemoveCommit alice1 [bob1, bob2] >>= sendAndConsumeCommitBundle
    modifyMLSState $ \s -> s {members = s.members Set.\\ Set.fromList [bob1, bob2]}

    removeCommitEvents %. "events.0.type" `shouldMatch` "conversation.member-leave"
    removeCommitEvents %. "events.0.data.reason" `shouldMatch` "removed"
    removeCommitEvents %. "events.0.from" `shouldMatch` alice1.user

    for_ wss \ws -> do
      n <- awaitMatch isConvLeaveNotif ws
      n %. "payload.0.data.reason" `shouldMatch` "removed"
      n %. "payload.0.from" `shouldMatch` alice1.user

    setMLSState childState
    let idxBob1 :: Int = 1
        idxBob2 :: Int = 2
    for_ ((,) <$> [idxBob1, idxBob2] <*> [alice1, charlie1, charlie2] `zip` wss) \(idx, (consumer, ws)) -> do
      msg <-
        awaitMatch
          do
            \n ->
              isJust <$> runMaybeT do
                msg <- lift $ n %. "payload.0.data" & asByteString >>= showMessage alice1
                guard =<< lift do
                  isNewMLSMessageNotif n

                prop <-
                  maybe mzero pure =<< lift do
                    lookupField msg "message.content.body.Proposal"

                lift do
                  (== idx) <$> (prop %. "Remove.removed" & asInt)
          ws
      msg %. "payload.0.data"
        & asByteString
          >>= mlsCliConsume consumer

    -- remove bob from the child state
    modifyMLSState $ \s -> s {members = s.members Set.\\ Set.fromList [bob1, bob2]}

    _ <- createPendingProposalCommit alice1 >>= sendAndConsumeCommitBundle

    getSubConversation bob qcnv subConvName >>= flip withResponse \resp ->
      assertBool "access to the conversation for bob should be denied" (resp.status == 403)

    for_ [charlie, alice] \m -> do
      resp <- getSubConversation m qcnv subConvName
      assertBool "alice and charlie should have access to the conversation" (resp.status == 200)
      mems <- resp.jsonBody %. "members" & asList
      mems `shouldMatchSet` ((renameField "id" "user_id" <=< make) `traverse` [alice1, charlie1, charlie2])

testResendingProposals :: (HasCallStack) => App ()
testResendingProposals = do
  [alice, bob, charlie] <- createAndConnectUsers [OwnDomain, OwnDomain, OtherDomain]
  [alice1, alice2, bob1, bob2, bob3, charlie1] <-
    traverse
      (createMLSClient def)
      [alice, alice, bob, bob, bob, charlie]
  traverse_ uploadNewKeyPackage [alice2, bob1, bob2, bob3, charlie1]

  (_, conv) <- createNewGroup alice1
  void $ createAddCommit alice1 [alice, bob, charlie] >>= sendAndConsumeCommitBundle

  createSubConv alice1 "conference"

  void $ createExternalCommit alice2 Nothing >>= sendAndConsumeCommitBundle
  void $ createExternalCommit bob1 Nothing >>= sendAndConsumeCommitBundle
  void $ createExternalCommit bob2 Nothing >>= sendAndConsumeCommitBundle
  void $ createExternalCommit bob3 Nothing >>= sendAndConsumeCommitBundle

  leaveCurrentConv bob1
  leaveCurrentConv bob2
  leaveCurrentConv bob3

  mls <- getMLSState
  withWebSockets (charlie1 : toList mls.members) \wss -> do
    void $ createExternalCommit charlie1 Nothing >>= sendAndConsumeCommitBundle

    -- consume proposals after backend resends them
    for_ wss \ws -> do
      replicateM 3 do
        msg <- consumeMessage (fromJust ws.client) Nothing ws
        msg %. "message.content.sender.External" `shouldMatchInt` 0

  void $ createPendingProposalCommit alice1 >>= sendAndConsumeCommitBundle

  sub <- getSubConversation alice1 conv "conference" >>= getJSON 200
  let members =
        map
          ( \cid ->
              object
                [ "client_id" .= cid.client,
                  "user_id" .= cid.user,
                  "domain" .= cid.domain
                ]
          )
          [alice1, alice2, charlie1]
  sub %. "members" `shouldMatchSet` members
