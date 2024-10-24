{-# OPTIONS_GHC -fmax-errors=10 #-}

module Test.MLS.SubConversation where

import API.Galley
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT (runMaybeT))
import qualified Data.Map as Map
import qualified Data.Set as Set
import MLS.Util
import Notifications
import SetupHelpers
import Test.MLS.One2One
import Testlib.Prelude

testJoinSubConv :: App ()
testJoinSubConv = do
  [alice, bob] <- createAndConnectUsers [OwnDomain, OwnDomain]
  [alice1, bob1, bob2] <- traverse (createMLSClient def def) [alice, bob, bob]
  traverse_ (uploadNewKeyPackage def) [bob1, bob2]
  convId <- createNewGroup def alice1

  void $ createAddCommit alice1 convId [bob] >>= sendAndConsumeCommitBundle
  void $ createSubConv def convId bob1 "conference"

  -- bob adds his first client to the subconversation
  sub' <- getSubConversation bob convId "conference" >>= getJSON 200
  subConvId <- objConvId sub'
  do
    tm <- sub' %. "epoch_timestamp"
    assertBool "Epoch timestamp should not be null" (tm /= Null)

  -- now alice joins with her own client
  void
    $ createExternalCommit subConvId alice1 Nothing
    >>= sendAndConsumeCommitBundle

testJoinOne2OneSubConv :: App ()
testJoinOne2OneSubConv = do
  [alice, bob] <- createAndConnectUsers [OwnDomain, OwnDomain]
  [alice1, bob1, bob2] <- traverse (createMLSClient def def) [alice, bob, bob]
  traverse_ (uploadNewKeyPackage def) [bob1, bob2]
  one2OneConv <- getMLSOne2OneConversation alice bob >>= getJSON 200
  one2OneConvId <- objConvId (one2OneConv %. "conversation")
  resetOne2OneGroup def alice1 one2OneConv

  void $ createAddCommit alice1 one2OneConvId [bob] >>= sendAndConsumeCommitBundle
  createOne2OneSubConv def one2OneConvId bob1 "conference" (one2OneConv %. "public_keys")

  -- bob adds his first client to the subconversation
  sub' <- getSubConversation bob one2OneConvId "conference" >>= getJSON 200
  subConvId <- objConvId sub'
  do
    tm <- sub' %. "epoch_timestamp"
    assertBool "Epoch timestamp should not be null" (tm /= Null)

  -- now alice joins with her own client
  void
    $ createExternalCommit subConvId alice1 Nothing
    >>= sendAndConsumeCommitBundle

testLeaveOne2OneSubConv :: One2OneScenario -> Leaver -> App ()
testLeaveOne2OneSubConv scenario leaver = do
  -- set up 1-1 conversation
  alice <- randomUser OwnDomain def
  let otherDomain = one2OneScenarioUserDomain scenario
      convDomain = one2OneScenarioConvDomain scenario
  bob <- createMLSOne2OnePartner otherDomain alice convDomain
  [alice1, bob1] <- traverse (createMLSClient def def) [alice, bob]
  void $ uploadNewKeyPackage def bob1
  one2OneConv <- getMLSOne2OneConversation alice bob >>= getJSON 200
  one2OneConvId <- objConvId $ one2OneConv %. "conversation"
  resetOne2OneGroup def alice1 one2OneConv
  void $ createAddCommit alice1 one2OneConvId [bob] >>= sendAndConsumeCommitBundle

  -- create and join subconversation
  createOne2OneSubConv def one2OneConvId alice1 "conference" (one2OneConv %. "public_keys")
  subConvId <- getSubConvId bob one2OneConvId "conference"

  void $ createExternalCommit subConvId bob1 Nothing >>= sendAndConsumeCommitBundle

  -- one of the two clients leaves
  let (leaverClient, leaverIndex, remainingClient) = case leaver of
        Alice -> (alice1, 0, bob1)
        Bob -> (bob1, 1, alice1)

  withWebSocket remainingClient $ \ws -> do
    leaveConv subConvId leaverClient
    msg <- consumeMessage subConvId def remainingClient Nothing ws
    msg %. "message.content.body.Proposal.Remove.removed" `shouldMatchInt` leaverIndex
    msg %. "message.content.sender.External" `shouldMatchInt` 0

  -- the other client commits the pending proposal
  void $ createPendingProposalCommit subConvId remainingClient >>= sendAndConsumeCommitBundle

testDeleteParentOfSubConv :: (HasCallStack) => Domain -> App ()
testDeleteParentOfSubConv secondDomain = do
  (alice, tid, _) <- createTeam OwnDomain 1
  bob <- randomUser secondDomain def
  connectUsers [alice, bob]

  [alice1, bob1] <- traverse (createMLSClient def def) [alice, bob]
  traverse_ (uploadNewKeyPackage def) [alice1, bob1]
  convId <- createNewGroup def alice1
  void $ createAddCommit alice1 convId [bob] >>= sendAndConsumeCommitBundle

  -- bob creates a subconversation and adds his own client
  createSubConv def convId bob1 "conference"
  subConvId <- getSubConvId bob convId "conference"

  -- alice joins with her own client
  void $ createExternalCommit subConvId alice1 Nothing >>= sendAndConsumeCommitBundle

  -- bob sends a message to the subconversation
  do
    mp <- createApplicationMessage subConvId bob1 "hello, alice"
    void . bindResponse (postMLSMessage mp.sender mp.message) $ \resp -> do
      resp.status `shouldMatchInt` 201

  -- alice sends a message to the subconversation
  do
    mp <- createApplicationMessage subConvId bob1 "hello, bob"
    void . bindResponse (postMLSMessage mp.sender mp.message) $ \resp -> do
      resp.status `shouldMatchInt` 201

  -- alice deletes main conversation
  withWebSocket bob $ \ws -> do
    void . bindResponse (deleteTeamConv tid (convIdToQidObject convId) alice) $ \resp -> do
      resp.status `shouldMatchInt` 200
    void $ awaitMatch isConvDeleteNotif ws

  -- bob fails to send a message to the subconversation
  do
    mp <- createApplicationMessage subConvId bob1 "hello, alice"
    void . bindResponse (postMLSMessage mp.sender mp.message) $ \resp -> do
      resp.status `shouldMatchInt` 404
      case secondDomain of
        OwnDomain -> resp.json %. "label" `shouldMatch` "no-conversation"
        OtherDomain -> resp.json %. "label" `shouldMatch` "no-conversation-member"

  -- alice fails to send a message to the subconversation
  do
    mp <- createApplicationMessage subConvId alice1 "hello, bob"
    void . bindResponse (postMLSMessage mp.sender mp.message) $ \resp -> do
      resp.status `shouldMatchInt` 404
      resp.json %. "label" `shouldMatch` "no-conversation"

testDeleteSubConversation :: (HasCallStack) => Domain -> App ()
testDeleteSubConversation otherDomain = do
  [alice, bob] <- createAndConnectUsers [OwnDomain, otherDomain]
  charlie <- randomUser OwnDomain def
  [alice1, bob1] <- traverse (createMLSClient def def) [alice, bob]
  void $ uploadNewKeyPackage def bob1
  convId <- createNewGroup def alice1
  void $ createAddCommit alice1 convId [bob] >>= sendAndConsumeCommitBundle

  createSubConv def convId alice1 "conference1"
  sub1 <- getSubConversation alice convId "conference1" >>= getJSON 200
  void $ deleteSubConversation charlie sub1 >>= getBody 403
  void $ deleteSubConversation alice sub1 >>= getBody 200

  createSubConv def convId alice1 "conference2"
  sub2 <- getSubConversation alice convId "conference2" >>= getJSON 200
  void $ deleteSubConversation bob sub2 >>= getBody 200

  sub2' <- getSubConversation alice1 convId "conference2" >>= getJSON 200
  sub2 `shouldNotMatch` sub2'

data Leaver = Alice | Bob
  deriving stock (Generic)

testLeaveSubConv :: (HasCallStack) => Leaver -> App ()
testLeaveSubConv leaver = do
  [alice, bob, charlie] <- createAndConnectUsers [OwnDomain, OwnDomain, OtherDomain]
  clients@[alice1, bob1, bob2, charlie1] <- traverse (createMLSClient def def) [alice, bob, bob, charlie]
  traverse_ (uploadNewKeyPackage def) [bob1, bob2, charlie1]
  convId <- createNewGroup def alice1

  withWebSockets [bob, charlie] $ \wss -> do
    void $ createAddCommit alice1 convId [bob, charlie] >>= sendAndConsumeCommitBundle
    traverse_ (awaitMatch isMemberJoinNotif) wss

  createSubConv def convId bob1 "conference"
  subConvId <- getSubConvId bob convId "conference"
  void $ createExternalCommit subConvId alice1 Nothing >>= sendAndConsumeCommitBundle
  void $ createExternalCommit subConvId bob2 Nothing >>= sendAndConsumeCommitBundle
  void $ createExternalCommit subConvId charlie1 Nothing >>= sendAndConsumeCommitBundle

  -- a member leaves the subconversation
  let (firstLeaver, idxFirstLeaver) = case leaver of
        Bob -> (bob1, 0)
        Alice -> (alice1, 1)
  let idxCharlie1 = 3

  let others = filter (/= firstLeaver) clients
  withWebSockets others $ \wss -> do
    leaveConv subConvId firstLeaver

    for_ (zip others wss) $ \(cid, ws) -> do
      msg <- consumeMessage subConvId def cid Nothing ws
      msg %. "message.content.body.Proposal.Remove.removed" `shouldMatchInt` idxFirstLeaver
      msg %. "message.content.sender.External" `shouldMatchInt` 0

  withWebSockets (tail others) $ \wss -> do
    -- a member commits the pending proposal
    void $ createPendingProposalCommit subConvId (head others) >>= sendAndConsumeCommitBundle
    traverse_ (awaitMatch isNewMLSMessageNotif) wss

    -- send an application message
    void $ createApplicationMessage subConvId (head others) "good riddance" >>= sendAndConsumeMessage
    traverse_ (awaitMatch isNewMLSMessageNotif) wss

  -- check that only 3 clients are left in the subconv
  do
    conv <- getConv subConvId (head others)
    mems <- conv %. "members" & asList
    length mems `shouldMatchInt` 3

  -- charlie1 leaves
  let others' = filter (/= charlie1) others
  withWebSockets others' $ \wss -> do
    leaveConv subConvId charlie1

    for_ (zip others' wss) $ \(cid, ws) -> do
      msg <- consumeMessage subConvId def cid Nothing ws
      msg %. "message.content.body.Proposal.Remove.removed" `shouldMatchInt` idxCharlie1
      msg %. "message.content.sender.External" `shouldMatchInt` 0

  -- a member commits the pending proposal
  void $ createPendingProposalCommit subConvId (head others') >>= sendAndConsumeCommitBundle

  -- check that only 2 clients are left in the subconv
  do
    conv <- getConv subConvId (head others)
    mems <- conv %. "members" & asList
    length mems `shouldMatchInt` 2

testCreatorRemovesUserFromParent :: App ()
testCreatorRemovesUserFromParent = do
  [alice, bob, charlie] <- createAndConnectUsers [OwnDomain, OwnDomain, OtherDomain]
  addUsersToFailureContext [("alice", alice), ("bob", bob), ("charlie", charlie)] $ do
    [alice1, bob1, bob2, charlie1, charlie2] <- traverse (createMLSClient def def) [alice, bob, bob, charlie, charlie]
    traverse_ (uploadNewKeyPackage def) [bob1, bob2, charlie1, charlie2]
    convId <- createNewGroup def alice1

    _ <- createAddCommit alice1 convId [bob, charlie] >>= sendAndConsumeCommitBundle

    -- save the state of the parent group
    let subConvName = "conference"
    createSubConv def convId alice1 subConvName
    subConvId <- getSubConvId alice convId "conference"

    for_ [bob1, bob2, charlie1, charlie2] \c ->
      createExternalCommit subConvId c Nothing >>= sendAndConsumeCommitBundle

    withWebSockets [alice1, charlie1, charlie2] \wss -> do
      removeCommitEvents <- createRemoveCommit alice1 convId [bob1, bob2] >>= sendAndConsumeCommitBundle
      modifyMLSState $ \s ->
        s
          { convs =
              Map.adjust
                (\conv -> conv {members = conv.members Set.\\ Set.fromList [bob1, bob2]})
                convId
                s.convs
          }

      removeCommitEvents %. "events.0.type" `shouldMatch` "conversation.member-leave"
      removeCommitEvents %. "events.0.data.reason" `shouldMatch` "removed"
      removeCommitEvents %. "events.0.from" `shouldMatch` alice1.user

      for_ wss \ws -> do
        n <- awaitMatch isConvLeaveNotif ws
        n %. "payload.0.data.reason" `shouldMatch` "removed"
        n %. "payload.0.from" `shouldMatch` alice1.user

      let idxBob1 :: Int = 1
          idxBob2 :: Int = 2
      for_ ((,) <$> [idxBob1, idxBob2] <*> wss) \(idx, ws) -> do
        msg <-
          awaitMatch
            do
              \n ->
                isJust <$> runMaybeT do
                  msg <- lift $ n %. "payload.0.data" & asByteString >>= showMessage def alice1
                  guard =<< lift do
                    isNewMLSMessageNotif n

                  prop <-
                    maybe mzero pure =<< lift do
                      lookupField msg "message.content.body.Proposal"

                  lift do
                    (== idx) <$> (prop %. "Remove.removed" & asInt)
            ws
        for_ ws.client $ \consumer ->
          msg %. "payload.0.data" & asByteString >>= mlsCliConsume subConvId def consumer

      -- remove bob from the child state
      modifyMLSState $ \s ->
        s
          { convs =
              Map.adjust
                (\conv -> conv {members = conv.members Set.\\ Set.fromList [bob1, bob2]})
                subConvId
                s.convs
          }

      _ <- createPendingProposalCommit subConvId alice1 >>= sendAndConsumeCommitBundle

      getSubConversation bob convId subConvName >>= flip withResponse \resp ->
        assertBool "access to the conversation for bob should be denied" (resp.status == 403)

      for_ [charlie, alice] \m -> do
        resp <- getSubConversation m convId subConvName
        assertBool "alice and charlie should have access to the conversation" (resp.status == 200)
        mems <- resp.jsonBody %. "members" & asList
        mems `shouldMatchSet` ((renameField "id" "user_id" <=< make) `traverse` [alice1, charlie1, charlie2])

testResendingProposals :: (HasCallStack) => App ()
testResendingProposals = do
  [alice, bob, charlie] <- createAndConnectUsers [OwnDomain, OwnDomain, OtherDomain]
  [alice1, alice2, bob1, bob2, bob3, charlie1] <-
    traverse
      (createMLSClient def def)
      [alice, alice, bob, bob, bob, charlie]
  traverse_ (uploadNewKeyPackage def) [alice2, bob1, bob2, bob3, charlie1]

  conv <- createNewGroup def alice1
  void $ createAddCommit alice1 conv [alice, bob, charlie] >>= sendAndConsumeCommitBundle

  createSubConv def conv alice1 "conference"
  subConvId <- getSubConvId alice conv "conference"

  void $ createExternalCommit subConvId alice2 Nothing >>= sendAndConsumeCommitBundle
  void $ createExternalCommit subConvId bob1 Nothing >>= sendAndConsumeCommitBundle
  void $ createExternalCommit subConvId bob2 Nothing >>= sendAndConsumeCommitBundle
  void $ createExternalCommit subConvId bob3 Nothing >>= sendAndConsumeCommitBundle

  leaveConv subConvId bob1
  leaveConv subConvId bob2
  leaveConv subConvId bob3

  subConv <- getMLSConv subConvId
  withWebSockets (charlie1 : toList subConv.members) \wss -> do
    void $ createExternalCommit subConvId charlie1 Nothing >>= sendAndConsumeCommitBundle

    -- consume proposals after backend resends them
    for_ wss \ws -> do
      replicateM 3 do
        msg <- consumeMessage subConvId def (fromJust ws.client) Nothing ws
        msg %. "message.content.sender.External" `shouldMatchInt` 0

  void $ createPendingProposalCommit subConvId alice1 >>= sendAndConsumeCommitBundle

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
