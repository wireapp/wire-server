-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2023 Wire Swiss GmbH <opensource@wire.com>
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

module Test.MLS.One2One where

import API.Brig
import API.Galley
import Control.Concurrent.Async
import Control.Concurrent.MVar
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8 as B8
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Read as T
import MLS.Util
import Notifications
import SetupHelpers
import Test.Version
import Testlib.Prelude

testGetMLSOne2One :: HasCallStack => Version5 -> Domain -> App ()
testGetMLSOne2One v otherDomain = withVersion5 v $ do
  [alice, bob] <- createAndConnectUsers [OwnDomain, otherDomain]

  let assertConvData conv = do
        conv %. "epoch" `shouldMatchInt` 0
        case v of
          Version5 -> conv %. "cipher_suite" `shouldMatchInt` 1
          NoVersion5 -> assertFieldMissing conv "cipher_suite"

  conv <- getMLSOne2OneConversation alice bob >>= getJSON 200
  conv %. "type" `shouldMatchInt` 2
  shouldBeEmpty (conv %. "members.others")

  conv %. "members.self.conversation_role" `shouldMatch` "wire_member"
  conv %. "members.self.qualified_id" `shouldMatch` (alice %. "qualified_id")
  assertConvData conv

  convId <- conv %. "qualified_id"

  -- check that the conversation has the same ID on the other side
  conv2 <- bindResponse (getMLSOne2OneConversation bob alice) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json

  conv2 %. "type" `shouldMatchInt` 2
  conv2 %. "qualified_id" `shouldMatch` convId
  assertConvData conv2

testMLSOne2OneOtherMember :: HasCallStack => One2OneScenario -> App ()
testMLSOne2OneOtherMember scenario = do
  alice <- randomUser OwnDomain def
  let otherDomain = one2OneScenarioUserDomain scenario
      convDomain = one2OneScenarioConvDomain scenario
  bob <- createMLSOne2OnePartner otherDomain alice convDomain
  conv <- getMLSOne2OneConversation alice bob >>= getJSON 200
  do
    convId <- conv %. "qualified_id"
    bobConv <- getMLSOne2OneConversation bob alice >>= getJSON 200
    convId `shouldMatch` (bobConv %. "qualified_id")

  [alice1, bob1] <- traverse (createMLSClient def) [alice, bob]
  traverse_ uploadNewKeyPackage [bob1]
  resetGroup alice1 conv
  withWebSocket bob1 $ \ws -> do
    commit <- createAddCommit alice1 [bob]
    void $ sendAndConsumeCommitBundle commit
    let isMessage n = nPayload n %. "type" `isEqual` "conversation.mls-welcome"
    n <- awaitMatch isMessage ws
    nPayload n %. "data" `shouldMatch` B8.unpack (Base64.encode (fold commit.welcome))

  -- Make sure the membership info is OK both for the MLS 1-to-1 endpoint and
  -- for the general conversation fetching endpoint.
  let assertOthers other resp = do
        bdy <- getJSON 200 resp
        othersObj <- bdy %. "members.others" & asList
        otherActual <- assertOne othersObj
        otherActual %. "qualified_id" `shouldMatch` (other %. "qualified_id")
  forM_ [(alice, bob), (bob, alice)] $ \(self, other) -> do
    getMLSOne2OneConversation self other `bindResponse` assertOthers other
    getConversation self conv `bindResponse` assertOthers other

testGetMLSOne2OneUnconnected :: HasCallStack => Domain -> App ()
testGetMLSOne2OneUnconnected otherDomain = do
  [alice, bob] <- for [OwnDomain, otherDomain] $ \domain -> randomUser domain def

  bindResponse (getMLSOne2OneConversation alice bob) $ \resp ->
    resp.status `shouldMatchInt` 403

testMLSOne2OneBlocked :: HasCallStack => Domain -> App ()
testMLSOne2OneBlocked otherDomain = do
  [alice, bob] <- for [OwnDomain, otherDomain] $ flip randomUser def
  void $ postConnection bob alice >>= getBody 201
  void $ putConnection alice bob "blocked" >>= getBody 200
  void $ getMLSOne2OneConversation alice bob >>= getJSON 403
  void $ getMLSOne2OneConversation bob alice >>= getJSON 403

-- | Alice and Bob are initially connected, but then Alice blocks Bob.
testMLSOne2OneBlockedAfterConnected :: HasCallStack => One2OneScenario -> App ()
testMLSOne2OneBlockedAfterConnected scenario = do
  alice <- randomUser OwnDomain def
  let otherDomain = one2OneScenarioUserDomain scenario
      convDomain = one2OneScenarioConvDomain scenario
  bob <- createMLSOne2OnePartner otherDomain alice convDomain
  conv <- getMLSOne2OneConversation alice bob >>= getJSON 200
  convId <- conv %. "qualified_id"
  do
    bobConv <- getMLSOne2OneConversation bob alice >>= getJSON 200
    convId `shouldMatch` (bobConv %. "qualified_id")

  [alice1, bob1] <- traverse (createMLSClient def) [alice, bob]
  traverse_ uploadNewKeyPackage [bob1]
  resetGroup alice1 conv
  commit <- createAddCommit alice1 [bob]
  withWebSocket bob1 $ \ws -> do
    void $ sendAndConsumeCommitBundle commit
    let isMessage n = nPayload n %. "type" `isEqual` "conversation.mls-welcome"
    n <- awaitMatch isMessage ws
    nPayload n %. "data" `shouldMatch` B8.unpack (Base64.encode (fold commit.welcome))

  withWebSocket bob1 $ \ws -> do
    -- Alice blocks Bob
    void $ putConnection alice bob "blocked" >>= getBody 200
    -- There is also a proteus 1-to-1 conversation. Neither it nor the MLS
    -- 1-to-1 conversation should get any events.
    awaitAnyEvent 2 ws `shouldMatch` (Nothing :: Maybe Value)
    -- Alice is not in the MLS 1-to-1 conversation given that she has blocked
    -- Bob.
    void $ getMLSOne2OneConversation alice bob >>= getJSON 403

  mp <- createApplicationMessage bob1 "hello, world, again"
  withWebSocket alice1 $ \ws -> do
    void $ postMLSMessage mp.sender mp.message >>= getJSON 201
    awaitAnyEvent 2 ws `shouldMatch` (Nothing :: Maybe Value)

-- | Alice and Bob are initially connected, then Alice blocks Bob, and finally
-- Alice unblocks Bob.
testMLSOne2OneUnblocked :: HasCallStack => One2OneScenario -> App ()
testMLSOne2OneUnblocked scenario = do
  alice <- randomUser OwnDomain def
  let otherDomain = one2OneScenarioUserDomain scenario
      convDomain = one2OneScenarioConvDomain scenario
  bob <- createMLSOne2OnePartner otherDomain alice convDomain
  conv <- getMLSOne2OneConversation alice bob >>= getJSON 200
  do
    convId <- conv %. "qualified_id"
    bobConv <- getMLSOne2OneConversation bob alice >>= getJSON 200
    convId `shouldMatch` (bobConv %. "qualified_id")

  [alice1, bob1] <- traverse (createMLSClient def) [alice, bob]
  traverse_ uploadNewKeyPackage [bob1]
  resetGroup alice1 conv
  withWebSocket bob1 $ \ws -> do
    commit <- createAddCommit alice1 [bob]
    void $ sendAndConsumeCommitBundle commit
    let isMessage n = nPayload n %. "type" `isEqual` "conversation.mls-welcome"
    n <- awaitMatch isMessage ws
    nPayload n %. "data" `shouldMatch` B8.unpack (Base64.encode (fold commit.welcome))

  -- Alice blocks Bob
  void $ putConnection alice bob "blocked" >>= getBody 200
  void $ getMLSOne2OneConversation alice bob >>= getJSON 403

  -- Reset the group membership in the test setup as only 'bob1' is left in
  -- reality, even though the test state believes 'alice1' is still part of the
  -- conversation.
  modifyMLSState $ \s -> s {members = Set.singleton bob1}

  -- Bob creates a new client and adds it to the one-to-one conversation just so
  -- that the epoch advances.
  bob2 <- createMLSClient def bob
  traverse_ uploadNewKeyPackage [bob2]
  void $ createAddCommit bob1 [bob] >>= sendAndConsumeCommitBundle

  -- Alice finally unblocks Bob
  void $ putConnection alice bob "accepted" >>= getBody 200
  void $ getMLSOne2OneConversation alice bob >>= getJSON 200

  -- Alice rejoins via an external commit
  void $ createExternalCommit alice1 Nothing >>= sendAndConsumeCommitBundle

  -- Check that an application message can get to Bob
  withWebSockets [bob1, bob2] $ \wss -> do
    mp <- createApplicationMessage alice1 "hello, I've always been here"
    void $ sendAndConsumeMessage mp
    let isMessage n = nPayload n %. "type" `isEqual` "conversation.mls-message-add"
    forM_ wss $ \ws -> do
      n <- awaitMatch isMessage ws
      nPayload n %. "data" `shouldMatch` B8.unpack (Base64.encode mp.message)

testGetMLSOne2OneSameTeam :: App ()
testGetMLSOne2OneSameTeam = do
  (alice, _, _) <- createTeam OwnDomain 1
  bob <- addUserToTeam alice
  void $ getMLSOne2OneConversation alice bob >>= getJSON 200

data One2OneScenario
  = -- | Both users are local
    One2OneScenarioLocal
  | -- | One user is remote, conversation is local
    One2OneScenarioLocalConv
  | -- | One user is remote, conversation is remote
    One2OneScenarioRemoteConv

instance TestCases One2OneScenario where
  testCases =
    [ MkTestCase "[domain=own]" One2OneScenarioLocal,
      MkTestCase "[domain=other;conv=own]" One2OneScenarioLocalConv,
      MkTestCase "[domain=other;conv=other]" One2OneScenarioRemoteConv
    ]

one2OneScenarioUserDomain :: One2OneScenario -> Domain
one2OneScenarioUserDomain One2OneScenarioLocal = OwnDomain
one2OneScenarioUserDomain _ = OtherDomain

one2OneScenarioConvDomain :: One2OneScenario -> Domain
one2OneScenarioConvDomain One2OneScenarioLocal = OwnDomain
one2OneScenarioConvDomain One2OneScenarioLocalConv = OwnDomain
one2OneScenarioConvDomain One2OneScenarioRemoteConv = OtherDomain

testMLSOne2One :: HasCallStack => Ciphersuite -> One2OneScenario -> App ()
testMLSOne2One suite scenario = do
  setMLSCiphersuite suite
  alice <- randomUser OwnDomain def
  let otherDomain = one2OneScenarioUserDomain scenario
      convDomain = one2OneScenarioConvDomain scenario
  bob <- createMLSOne2OnePartner otherDomain alice convDomain
  [alice1, bob1] <- traverse (createMLSClient def) [alice, bob]
  traverse_ uploadNewKeyPackage [bob1]

  conv <- getMLSOne2OneConversation alice bob >>= getJSON 200
  resetGroup alice1 conv

  commit <- createAddCommit alice1 [bob]
  withWebSocket bob1 $ \ws -> do
    void $ sendAndConsumeCommitBundle commit

    let isWelcome n = nPayload n %. "type" `isEqual` "conversation.mls-welcome"
    n <- awaitMatch isWelcome ws
    nPayload n %. "data" `shouldMatch` B8.unpack (Base64.encode (fold commit.welcome))

    void $ awaitMatch isMemberJoinNotif ws

  withWebSocket bob1 $ \ws -> do
    mp <- createApplicationMessage alice1 "hello, world"
    void $ sendAndConsumeMessage mp
    let isMessage n = nPayload n %. "type" `isEqual` "conversation.mls-message-add"
    n <- awaitMatch isMessage ws
    nPayload n %. "data" `shouldMatch` B8.unpack (Base64.encode mp.message)

  void $ createPendingProposalCommit alice1 >>= sendAndConsumeCommitBundle

  conv' <- getMLSOne2OneConversation alice bob >>= getJSON 200
  (suiteCode, _) <- assertOne $ T.hexadecimal (T.pack suite.code)
  conv' %. "cipher_suite" `shouldMatchInt` suiteCode

-- | This test verifies that one-to-one conversations are created inside the
-- commit lock. There used to be an issue where a conversation could be
-- partially created at the time of setting its ciphersuite, resulting in an
-- incomplete database entry that would prevent further uses of the
-- conversation.
testMLSGhostOne2OneConv :: App ()
testMLSGhostOne2OneConv = do
  [alice, bob] <- createAndConnectUsers [OwnDomain, OwnDomain]
  [alice1, bob1, bob2] <- traverse (createMLSClient def) [alice, bob, bob]
  traverse_ uploadNewKeyPackage [bob1, bob2]
  conv <- getMLSOne2OneConversation alice bob >>= getJSON 200
  resetGroup alice1 conv

  doneVar <- liftIO $ newEmptyMVar
  let checkConversation =
        liftIO (tryReadMVar doneVar) >>= \case
          Nothing -> do
            bindResponse (getConversation alice conv) $ \resp ->
              resp.status `shouldMatchOneOf` [404 :: Int, 403, 200]
            checkConversation
          Just _ -> pure ()
  checkConversationIO <- appToIO checkConversation

  createCommit <-
    appToIO $
      void $
        createAddCommit alice1 [bob]
          >>= sendAndConsumeCommitBundle

  liftIO $ withAsync checkConversationIO $ \a -> do
    createCommit
    liftIO $ putMVar doneVar ()
    wait a
