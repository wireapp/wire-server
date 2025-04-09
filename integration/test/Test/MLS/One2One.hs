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
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Read as T
import MLS.Util
import Notifications
import SetupHelpers
import Test.Version
import Testlib.Prelude
import Testlib.VersionedFed

testGetMLSOne2OneLocalV5 :: (HasCallStack) => App ()
testGetMLSOne2OneLocalV5 = withVersion5 Version5 $ do
  [alice, bob] <- createAndConnectUsers [OwnDomain, OwnDomain]
  let assertConvData conv = do
        conv %. "epoch" `shouldMatchInt` 0
        conv %. "cipher_suite" `shouldMatchInt` 1

  convId <-
    getMLSOne2OneConversationLegacy alice bob `bindResponse` \resp -> do
      conv <- getJSON 200 resp
      conv %. "type" `shouldMatchInt` 2
      shouldBeEmpty (conv %. "members.others")

      conv %. "members.self.conversation_role" `shouldMatch` "wire_member"
      conv %. "members.self.qualified_id" `shouldMatch` (alice %. "qualified_id")
      assertConvData conv

      conv %. "qualified_id"

  -- check that the conversation has the same ID on the other side
  conv2 <- bindResponse (getMLSOne2OneConversationLegacy bob alice) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json

  conv2 %. "type" `shouldMatchInt` 2
  conv2 %. "qualified_id" `shouldMatch` convId
  assertConvData conv2

testGetMLSOne2OneRemoteV5 :: (HasCallStack) => App ()
testGetMLSOne2OneRemoteV5 = withVersion5 Version5 $ do
  [alice, bob] <- createAndConnectUsers [OwnDomain, OtherDomain]
  getMLSOne2OneConversationLegacy alice bob `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 400
    resp.jsonBody %. "label" `shouldMatch` "mls-federated-one2one-not-supported"

  getMLSOne2OneConversationLegacy bob alice `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 400
    resp.jsonBody %. "label" `shouldMatch` "mls-federated-one2one-not-supported"

testGetMLSOne2One :: (HasCallStack) => Domain -> App ()
testGetMLSOne2One bobDomain = do
  [alice, bob] <- createAndConnectUsers [OwnDomain, bobDomain]
  bobDomainStr <- asString bobDomain
  let assertConvData conv = do
        conv %. "epoch" `shouldMatchInt` 0
        assertFieldMissing conv "cipher_suite"

  mlsOne2OneConv <-
    getMLSOne2OneConversation alice bob `bindResponse` \resp -> do
      one2oneConv <- getJSON 200 resp
      convOwnerDomain <- asString $ one2oneConv %. "conversation.qualified_id.domain"
      let user = if convOwnerDomain == bobDomainStr then bob else alice
      ownerDomainPublicKeys <- getMLSPublicKeys user >>= getJSON 200

      one2oneConv %. "public_keys" `shouldMatch` ownerDomainPublicKeys

      conv <- one2oneConv %. "conversation"
      conv %. "type" `shouldMatchInt` 2
      shouldBeEmpty (conv %. "members.others")
      conv %. "members.self.conversation_role" `shouldMatch` "wire_member"
      conv %. "members.self.qualified_id" `shouldMatch` (alice %. "qualified_id")
      assertConvData conv

      pure one2oneConv

  -- check that the conversation has the same ID on the other side
  mlsOne2OneConv2 <- bindResponse (getMLSOne2OneConversation bob alice) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json

  conv2 <- mlsOne2OneConv2 %. "conversation"
  conv2 %. "type" `shouldMatchInt` 2
  conv2 %. "qualified_id" `shouldMatch` (mlsOne2OneConv %. "conversation.qualified_id")
  mlsOne2OneConv2 %. "public_keys" `shouldMatch` (mlsOne2OneConv %. "public_keys")
  assertConvData conv2

testMLSOne2OneOtherMember :: (HasCallStack) => One2OneScenario -> App ()
testMLSOne2OneOtherMember scenario = do
  alice <- randomUser OwnDomain def
  let otherDomain = one2OneScenarioUserDomain scenario
      convDomain = one2OneScenarioConvDomain scenario
  bob <- createMLSOne2OnePartner otherDomain alice convDomain
  one2OneConv <- getMLSOne2OneConversation alice bob >>= getJSON 200
  one2OneConvId <- objConvId $ one2OneConv %. "conversation"
  do
    convId <- one2OneConv %. "conversation.qualified_id"
    bobOne2OneConv <- getMLSOne2OneConversation bob alice >>= getJSON 200
    convId `shouldMatch` (bobOne2OneConv %. "conversation.qualified_id")

  [alice1, bob1] <- traverse (createMLSClient def) [alice, bob]
  void $ uploadNewKeyPackage def bob1
  resetOne2OneGroup def alice1 one2OneConv
  withWebSocket bob1 $ \ws -> do
    commit <- createAddCommit alice1 one2OneConvId [bob]
    void $ sendAndConsumeCommitBundle commit
    let isMessage n = nPayload n %. "type" `isEqual` "conversation.mls-welcome"
    n <- awaitMatch isMessage ws
    nPayload n %. "data" `shouldMatch` B8.unpack (Base64.encode (fold commit.welcome))

  -- Make sure the membership info is OK both for the MLS 1-to-1 endpoint and
  -- for the general conversation fetching endpoint.
  let assertOthers :: (HasCallStack, MakesValue other, MakesValue retrievedConv) => other -> retrievedConv -> App ()
      assertOthers other retrievedConv = do
        othersObj <- retrievedConv %. "members.others" & asList
        otherActual <- assertOne othersObj
        otherActual %. "qualified_id" `shouldMatch` (other %. "qualified_id")
  forM_ [(alice, bob), (bob, alice)] $ \(self, other) -> do
    getMLSOne2OneConversation self other `bindResponse` \resp -> do
      retrievedConv <- getJSON 200 resp >>= (%. "conversation")
      assertOthers other retrievedConv
    getConversation self (one2OneConv %. "conversation") `bindResponse` \resp -> do
      retrievedConv <- getJSON 200 resp
      assertOthers other retrievedConv

testMLSOne2OneRemoveClientLocalV5 :: App ()
testMLSOne2OneRemoveClientLocalV5 = withVersion5 Version5 $ do
  [alice, bob] <- createAndConnectUsers [OwnDomain, OwnDomain]
  conv <- getMLSOne2OneConversationLegacy alice bob >>= getJSON 200

  [alice1, bob1] <- traverse (createMLSClient def) [alice, bob]
  void $ uploadNewKeyPackage def bob1
  convId <- objConvId conv
  createGroup def alice1 convId

  void $ createAddCommit alice1 convId [bob] >>= sendAndConsumeCommitBundle

  withWebSocket alice $ \wsAlice -> do
    _ <- deleteClient bob bob1.client >>= getBody 200
    let predicate n = nPayload n %. "type" `isEqual` "conversation.mls-message-add"
    n <- awaitMatch predicate wsAlice
    shouldMatch (nPayload n %. "conversation") (objId conv)
    shouldMatch (nPayload n %. "from") (objId bob)

    mlsMsg <- asByteString (nPayload n %. "data")

    -- Checks that the remove proposal is consumable by alice
    void $ mlsCliConsume convId def alice1 mlsMsg

    parsedMsg <- showMessage def alice1 mlsMsg
    let leafIndexBob = 1
    -- msg `shouldMatch` "foo"
    parsedMsg %. "message.content.body.Proposal.Remove.removed" `shouldMatchInt` leafIndexBob
    parsedMsg %. "message.content.sender.External" `shouldMatchInt` 0

testGetMLSOne2OneUnconnected :: (HasCallStack) => Domain -> App ()
testGetMLSOne2OneUnconnected otherDomain = do
  [alice, bob] <- for [OwnDomain, otherDomain] $ \domain -> randomUser domain def

  bindResponse (getMLSOne2OneConversation alice bob) $ \resp ->
    resp.status `shouldMatchInt` 403

testMLSOne2OneBlocked :: (HasCallStack) => Domain -> App ()
testMLSOne2OneBlocked otherDomain = do
  [alice, bob] <- for [OwnDomain, otherDomain] $ flip randomUser def
  void $ postConnection bob alice >>= getBody 201
  void $ putConnection alice bob "blocked" >>= getBody 200
  void $ getMLSOne2OneConversation alice bob >>= getJSON 403
  void $ getMLSOne2OneConversation bob alice >>= getJSON 403

-- | Alice and Bob are initially connected, but then Alice blocks Bob.
testMLSOne2OneBlockedAfterConnected :: (HasCallStack) => One2OneScenario -> App ()
testMLSOne2OneBlockedAfterConnected scenario = do
  alice <- randomUser OwnDomain def
  let otherDomain = one2OneScenarioUserDomain scenario
      convDomain = one2OneScenarioConvDomain scenario
  bob <- createMLSOne2OnePartner otherDomain alice convDomain
  one2OneConv <- getMLSOne2OneConversation alice bob >>= getJSON 200
  one2OneConvId <- objConvId $ one2OneConv %. "conversation"
  convId <- one2OneConv %. "conversation.qualified_id"
  do
    bobConv <- getMLSOne2OneConversation bob alice >>= getJSON 200
    convId `shouldMatch` (bobConv %. "conversation.qualified_id")

  [alice1, bob1] <- traverse (createMLSClient def) [alice, bob]
  void $ uploadNewKeyPackage def bob1
  resetOne2OneGroup def alice1 one2OneConv
  commit <- createAddCommit alice1 one2OneConvId [bob]
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

  mp <- createApplicationMessage one2OneConvId bob1 "hello, world, again"
  withWebSocket alice1 $ \ws -> do
    void $ postMLSMessage mp.sender mp.message >>= getJSON 201
    awaitAnyEvent 2 ws `shouldMatch` (Nothing :: Maybe Value)

-- | Alice and Bob are initially connected, then Alice blocks Bob, and finally
-- Alice unblocks Bob.
testMLSOne2OneUnblocked :: (HasCallStack) => One2OneScenario -> App ()
testMLSOne2OneUnblocked scenario = do
  alice <- randomUser OwnDomain def
  let otherDomain = one2OneScenarioUserDomain scenario
      convDomain = one2OneScenarioConvDomain scenario
  bob <- createMLSOne2OnePartner otherDomain alice convDomain
  one2OneConv <- getMLSOne2OneConversation alice bob >>= getJSON 200
  one2OneConvId <- objConvId $ one2OneConv %. "conversation"
  do
    convId <- one2OneConv %. "conversation.qualified_id"
    bobConv <- getMLSOne2OneConversation bob alice >>= getJSON 200
    convId `shouldMatch` (bobConv %. "conversation.qualified_id")

  [alice1, bob1] <- traverse (createMLSClient def) [alice, bob]
  void $ uploadNewKeyPackage def bob1
  resetOne2OneGroup def alice1 one2OneConv
  withWebSocket bob1 $ \ws -> do
    commit <- createAddCommit alice1 one2OneConvId [bob]
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
  modifyMLSState $ \s -> s {convs = Map.adjust (\conv -> conv {members = Set.singleton bob1}) one2OneConvId s.convs}

  -- Bob creates a new client and adds it to the one-to-one conversation just so
  -- that the epoch advances.
  bob2 <- createMLSClient def bob
  void $ uploadNewKeyPackage def bob2
  void $ createAddCommit bob1 one2OneConvId [bob] >>= sendAndConsumeCommitBundle

  -- Alice finally unblocks Bob
  void $ putConnection alice bob "accepted" >>= getBody 200
  void $ getMLSOne2OneConversation alice bob >>= getJSON 200

  -- Alice rejoins via an external commit
  void $ createExternalCommit one2OneConvId alice1 Nothing >>= sendAndConsumeCommitBundle

  -- Check that an application message can get to Bob
  withWebSockets [bob1, bob2] $ \wss -> do
    mp <- createApplicationMessage one2OneConvId alice1 "hello, I've always been here"
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
  mkTestCases =
    pure
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

testMLSOne2One :: (HasCallStack) => Ciphersuite -> One2OneScenario -> App ()
testMLSOne2One suite scenario = do
  alice <- randomUser OwnDomain def
  let otherDomain = one2OneScenarioUserDomain scenario
      convDomain = one2OneScenarioConvDomain scenario
  bob <- createMLSOne2OnePartner otherDomain alice convDomain
  [alice1, bob1] <- traverse (createMLSClient def {ciphersuites = [suite]}) [alice, bob]
  void $ uploadNewKeyPackage suite bob1

  one2OneConv <- getMLSOne2OneConversation alice bob >>= getJSON 200
  one2OneConvId <- objConvId $ one2OneConv %. "conversation"
  resetOne2OneGroup suite alice1 one2OneConv

  commit <- createAddCommit alice1 one2OneConvId [bob]
  withWebSocket bob1 $ \ws -> do
    void $ sendAndConsumeCommitBundle commit

    let isWelcome n = nPayload n %. "type" `isEqual` "conversation.mls-welcome"
    n <- awaitMatch isWelcome ws
    nPayload n %. "data" `shouldMatch` B8.unpack (Base64.encode (fold commit.welcome))

    void $ awaitMatch isMemberJoinNotif ws

  withWebSocket bob1 $ \ws -> do
    mp <- createApplicationMessage one2OneConvId alice1 "hello, world"
    void $ sendAndConsumeMessage mp
    let isMessage n = nPayload n %. "type" `isEqual` "conversation.mls-message-add"
    n <- awaitMatch isMessage ws
    nPayload n %. "data" `shouldMatch` B8.unpack (Base64.encode mp.message)

  -- Send another commit. This verifies that the backend has correctly updated
  -- the cipersuite of this conversation.
  void $ createPendingProposalCommit one2OneConvId alice1 >>= sendAndConsumeCommitBundle

  one2OneConv' <- getMLSOne2OneConversation alice bob >>= getJSON 200
  (suiteCode, _) <- assertOne $ T.hexadecimal (T.pack suite.code)
  one2OneConv' %. "conversation.cipher_suite" `shouldMatchInt` suiteCode

-- | This test verifies that one-to-one conversations are created inside the
-- commit lock. There used to be an issue where a conversation could be
-- partially created at the time of setting its ciphersuite, resulting in an
-- incomplete database entry that would prevent further uses of the
-- conversation.
testMLSGhostOne2OneConv :: App ()
testMLSGhostOne2OneConv = do
  [alice, bob] <- createAndConnectUsers [OwnDomain, OwnDomain]
  [alice1, bob1, bob2] <- traverse (createMLSClient def) [alice, bob, bob]
  traverse_ (uploadNewKeyPackage def) [bob1, bob2]
  one2OneConv <- getMLSOne2OneConversation alice bob >>= getJSON 200
  one2OneConvId <- objConvId $ one2OneConv %. "conversation"
  resetOne2OneGroup def alice1 one2OneConv

  doneVar <- liftIO $ newEmptyMVar
  let checkConversation =
        liftIO (tryReadMVar doneVar) >>= \case
          Nothing -> do
            bindResponse (getConversation alice (one2OneConv %. "conversation")) $ \resp ->
              resp.status `shouldMatchOneOf` [404 :: Int, 403, 200]

            checkConversation
          Just _ -> pure ()
  checkConversationIO <- appToIO checkConversation

  createCommit <-
    appToIO
      $ void
      $ createAddCommit alice1 one2OneConvId [bob]
      >>= sendAndConsumeCommitBundle

  liftIO $ withAsync checkConversationIO $ \a -> do
    createCommit
    liftIO $ putMVar doneVar ()
    wait a

-- [NOTE: Federated 1:1 MLS Conversations]
-- 1:1 Conversations shouldn't work when there is no way for the creator to know
-- the MLS public keys of the backend which will host this conversation. In
-- federation API V2, this will always work and has been tested above. When one
-- of the backends doesn't support federation API v2, the 1:1 conversation can
-- still be created but only by the user whose backend hosts this conversation.

-- | See Note: [Federated 1:1 MLS Conversations]
-- To run locally this test requires federation-v1 docker containers to be up and running.
-- See `deploy/dockerephemeral/run.sh` and comment on `StaticFedDomain` in `Testlib/VersionedFed.hs` for more details.
testMLSFederationV1ConvOnOldBackend :: (HasCallStack) => FedDomain 1 -> App ()
testMLSFederationV1ConvOnOldBackend domain = do
  let cs = Ciphersuite "0x0001"
  alice <- randomUser OwnDomain def
  let createBob = do
        bobCandidate <- randomUser domain def
        connectUsers [alice, bobCandidate]
        getMLSOne2OneConversation alice bobCandidate `bindResponse` \resp -> do
          if resp.status == 533
            then pure bobCandidate
            else createBob

  bob <- createBob
  [alice1, bob1] <- traverse (createMLSClient def {ciphersuites = [cs]}) [alice, bob]
  void $ uploadNewKeyPackage cs alice1

  -- Alice cannot start this conversation because it would exist on Bob's
  -- backend and Alice cannot get the MLS public keys of that backend.
  getMLSOne2OneConversation alice bob `bindResponse` \resp -> do
    fedError <- getJSON 533 resp
    fedError %. "label" `shouldMatch` "federation-version-error"

  conv <- getMLSOne2OneConversationLegacy bob alice >>= getJSON 200
  convId <- objConvId conv
  keys <- getMLSPublicKeys bob >>= getJSON 200
  resetOne2OneGroupGeneric cs bob1 conv keys

  withWebSocket alice1 $ \wsAlice -> do
    commit <- createAddCommit bob1 convId [alice]
    void $ sendAndConsumeCommitBundle commit

    let isMessage n = nPayload n %. "type" `isEqual` "conversation.mls-welcome"
    n <- awaitMatch isMessage wsAlice
    nPayload n %. "data" `shouldMatch` B8.unpack (Base64.encode (fold commit.welcome))

  withWebSocket bob1 $ \wsBob -> do
    _ <- deleteClient alice alice1.client >>= getBody 200

    let predicate n = nPayload n %. "type" `isEqual` "conversation.mls-message-add"
    n <- awaitMatch predicate wsBob
    shouldMatch (nPayload n %. "conversation") (objId conv)
    shouldMatch (nPayload n %. "from") (objId alice)

    mlsMsg <- asByteString (nPayload n %. "data")

    -- Checks that the remove proposal is consumable by bob
    void $ mlsCliConsume convId cs bob1 mlsMsg

    parsedMsg <- showMessage cs bob1 mlsMsg
    let leafIndexAlice = 1
    parsedMsg %. "message.content.body.Proposal.Remove.removed" `shouldMatchInt` leafIndexAlice
    parsedMsg %. "message.content.sender.External" `shouldMatchInt` 0

-- | See Note: Federated 1:1 MLS Conversations
-- To run locally this test requires federation-v1 docker containers to be up and running.
-- See `deploy/dockerephemeral/run.sh` and comment on `StaticFedDomain` in `Testlib/VersionedFed.hs` for more details.
testMLSFederationV1ConvOnNewBackend :: (HasCallStack) => FedDomain 1 -> App ()
testMLSFederationV1ConvOnNewBackend domain = do
  let cs = Ciphersuite "0x0001"
  alice <- randomUser OwnDomain def
  let createBob = do
        bobCandidate <- randomUser domain def
        connectUsers [alice, bobCandidate]
        getMLSOne2OneConversation alice bobCandidate `bindResponse` \resp -> do
          if resp.status == 200
            then pure bobCandidate
            else createBob

  bob <- createBob
  [alice1, bob1] <- traverse (createMLSClient def {ciphersuites = [cs]}) [alice, bob]
  void $ uploadNewKeyPackage cs bob1

  -- Bob cannot start this conversation because it would exist on Alice's
  -- backend and Bob cannot get the MLS public keys of that backend.
  getMLSOne2OneConversationLegacy bob alice `bindResponse` \resp -> do
    fedError <- getJSON 533 resp
    fedError %. "label" `shouldMatch` "federation-remote-error"

  one2OneConv <- getMLSOne2OneConversation alice bob >>= getJSON 200
  one2OneConvId <- objConvId $ one2OneConv %. "conversation"
  conv <- one2OneConv %. "conversation"
  resetOne2OneGroup cs alice1 one2OneConv

  withWebSocket bob1 $ \wsBob -> do
    commit <- createAddCommit alice1 one2OneConvId [bob]
    void $ sendAndConsumeCommitBundle commit

    let isMessage n = nPayload n %. "type" `isEqual` "conversation.mls-welcome"
    n <- awaitMatch isMessage wsBob
    nPayload n %. "data" `shouldMatch` B8.unpack (Base64.encode (fold commit.welcome))

  withWebSocket alice1 $ \wsAlice -> do
    _ <- deleteClient bob bob1.client >>= getBody 200

    let predicate n = nPayload n %. "type" `isEqual` "conversation.mls-message-add"
    n <- awaitMatch predicate wsAlice
    shouldMatch (nPayload n %. "conversation") (objId conv)
    shouldMatch (nPayload n %. "from") (objId bob)

    mlsMsg <- asByteString (nPayload n %. "data")

    -- Checks that the remove proposal is consumable by bob
    void $ mlsCliConsume one2OneConvId cs alice1 mlsMsg

    parsedMsg <- showMessage cs alice1 mlsMsg
    let leafIndexBob = 1
    parsedMsg %. "message.content.body.Proposal.Remove.removed" `shouldMatchInt` leafIndexBob
    parsedMsg %. "message.content.sender.External" `shouldMatchInt` 0
