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
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8 as B8
import MLS.Util
import Notifications
import SetupHelpers
import Testlib.Prelude

testGetMLSOne2One :: HasCallStack => Domain -> App ()
testGetMLSOne2One otherDomain = do
  [alice, bob] <- createAndConnectUsers [OwnDomain, otherDomain]

  conv <- getMLSOne2OneConversation alice bob >>= getJSON 200
  conv %. "type" `shouldMatchInt` 2
  shouldBeEmpty (conv %. "members.others")

  conv %. "members.self.conversation_role" `shouldMatch` "wire_member"
  conv %. "members.self.qualified_id" `shouldMatch` (alice %. "qualified_id")

  convId <- conv %. "qualified_id"

  -- check that the conversation has the same ID on the other side
  conv2 <- bindResponse (getMLSOne2OneConversation bob alice) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json

  conv2 %. "type" `shouldMatchInt` 2
  conv2 %. "qualified_id" `shouldMatch` convId
  conv2 %. "epoch" `shouldMatch` (conv %. "epoch")

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
  let otherDomain = one2OneScenarioDomain scenario
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

one2OneScenarioDomain :: One2OneScenario -> Domain
one2OneScenarioDomain One2OneScenarioLocal = OwnDomain
one2OneScenarioDomain _ = OtherDomain

one2OneScenarioConvDomain :: One2OneScenario -> Domain
one2OneScenarioConvDomain One2OneScenarioLocal = OwnDomain
one2OneScenarioConvDomain One2OneScenarioLocalConv = OwnDomain
one2OneScenarioConvDomain One2OneScenarioRemoteConv = OtherDomain

testMLSOne2One :: HasCallStack => One2OneScenario -> App ()
testMLSOne2One scenario = do
  alice <- randomUser OwnDomain def
  let otherDomain = one2OneScenarioDomain scenario
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
