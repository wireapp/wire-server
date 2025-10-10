-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
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

module Test.MLS.Reset where

import API.Galley
import MLS.Util
import Notifications (isConvResetNotif)
import SetupHelpers
import Testlib.Prelude
import Testlib.VersionedFed

testResetGroupConversation :: (HasCallStack) => Domain -> App ()
testResetGroupConversation domain = do
  [alice, bob, charlie] <- createAndConnectUsers [make OwnDomain, make domain, make OwnDomain]
  [alice1, bob1] <- traverse (createMLSClient def) [alice, bob]
  void $ uploadNewKeyPackage def bob1
  conv <- createNewGroup def alice1
  void $ createAddCommit alice1 conv [bob] >>= sendAndConsumeCommitBundle
  mlsConv <- getMLSConv conv

  resetConversation alice mlsConv.groupId 0 >>= assertStatus 409
  resetConversation bob mlsConv.groupId 0 >>= assertStatus 409
  resetConversation charlie mlsConv.groupId mlsConv.epoch >>= assertStatus 404

  conv' <- withWebSocket alice $ \ws -> do
    resetConversation bob mlsConv.groupId mlsConv.epoch >>= assertStatus 200
    conv' <- getConversation alice conv >>= getJSON 200

    e <- awaitMatch isConvResetNotif ws
    e %. "payload.0.data.group_id" `shouldMatch` mlsConv.groupId
    e %. "payload.0.data.new_group_id" `shouldMatch` (conv' %. "group_id")

    pure conv'

  conv' %. "group_id" `shouldNotMatch` (mlsConv.groupId :: String)
  conv' %. "epoch" `shouldMatchInt` 0
  otherMember <- assertOne =<< asList (conv' %. "members.others")
  otherMember %. "qualified_id" `shouldMatch` (bob %. "qualified_id")

testResetSelfConversation :: (HasCallStack) => App ()
testResetSelfConversation = do
  alice <- randomUser OwnDomain def
  [alice1, alice2, alice3] <- traverse (createMLSClient def) (replicate 3 alice)
  void $ uploadNewKeyPackage def alice2
  (_, conv) <- createSelfGroup def alice1
  convId <- objConvId conv
  void $ createAddCommit alice1 convId [alice] >>= sendAndConsumeCommitBundle
  mlsConv <- getMLSConv convId

  conv' <- resetMLSConversation alice alice1 conv
  conv' %. "group_id" `shouldNotMatch` (mlsConv.groupId :: String)
  conv' %. "epoch" `shouldMatchInt` 0
  convId' <- objConvId conv'

  void $ uploadNewKeyPackage def alice3
  void $ createSelfGroup def alice1
  void $ createAddCommit alice1 convId' [alice] >>= sendAndConsumeCommitBundle

  conv'' <- getConversation alice convId' >>= getJSON 200
  conv'' %. "epoch" `shouldMatchInt` 1
  conv'' %. "group_id" `shouldMatch` (conv' %. "group_id")

testResetOne2OneConversation :: (HasCallStack) => App ()
testResetOne2OneConversation = do
  [alice, bob] <- createAndConnectUsers [OwnDomain, OtherDomain]
  [alice1, bob1] <- traverse (createMLSClient def) [alice, bob]
  void . replicateM 2 $ uploadNewKeyPackage def bob1
  otherDomain <- asString OtherDomain
  conv <- getMLSOne2OneConversation alice bob >>= getJSON 200
  convOwnerDomain <- asString $ conv %. "conversation.qualified_id.domain"
  let (user, cid) = if convOwnerDomain == otherDomain then (bob, bob1) else (alice, alice1)
  convId <- objConvId (conv %. "conversation")

  resetOne2OneGroup def alice1 conv
  void $ createAddCommit alice1 convId [bob] >>= sendAndConsumeCommitBundle
  void $ createPendingProposalCommit convId alice1 >>= sendAndConsumeCommitBundle
  mlsConv <- getMLSConv convId

  conv' <- resetMLSConversation user cid (conv %. "conversation")
  conv' %. "group_id" `shouldNotMatch` (mlsConv.groupId :: String)
  conv' %. "epoch" `shouldMatchInt` 0
  convId' <- objConvId conv'
  resetOne2OneGroupGeneric def alice1 conv' (conv %. "public_keys")

  void $ createAddCommit alice1 convId' [bob] >>= sendAndConsumeCommitBundle

  conv'' <- getConversation user convId >>= getJSON 200
  conv'' %. "epoch" `shouldMatchInt` 1
  conv'' %. "group_id" `shouldMatch` (conv' %. "group_id")

testResetMixedConversation :: (HasCallStack) => Domain -> App ()
testResetMixedConversation domain = do
  -- create mixed conversation
  (alice, tid, []) <- createTeam OwnDomain 1
  bob <- randomUser domain def
  connectTwoUsers alice bob
  convId0 <-
    postConversation alice defProteus {qualifiedUsers = [bob], team = Just tid}
      >>= getJSON 201
      >>= objConvId
  void $ putConversationProtocol bob convId0 "mixed" >>= getJSON 200
  convId <- getConversation alice convId0 >>= getJSON 200 >>= objConvId

  -- add clients
  [alice1, bob1] <- traverse (createMLSClient def) [alice, bob]
  createGroup def alice1 convId
  void $ uploadNewKeyPackage def bob1
  void
    $ createAddCommit alice1 convId [bob]
    >>= sendAndConsumeCommitBundleWithProtocol MLSProtocolMixed
  conv <- getConversation alice convId >>= getJSON 200

  -- reset
  groupId <- asString $ conv %. "group_id"
  epoch <- asInt $ conv %. "epoch"
  resetConversation bob groupId (fromIntegral epoch) >>= assertStatus 200

  conv' <- getConversation alice conv >>= getJSON 200
  conv' %. "group_id" `shouldNotMatch` groupId
  conv' %. "epoch" `shouldMatchInt` 0
  otherMember <- assertOne =<< asList (conv' %. "members.others")
  otherMember %. "qualified_id" `shouldMatch` (bob %. "qualified_id")

testResetWithLegacyBackend :: (HasCallStack) => AnyFedDomain -> App ()
testResetWithLegacyBackend domain = when (unFedDomain domain > 0) $ do
  let suite = Ciphersuite "0x0001"
  [alice, bob, charlie] <- createAndConnectUsers [make OwnDomain, make OtherDomain, make domain]
  [alice1, bob1, charlie1] <-
    traverse
      (createMLSClient def {ciphersuites = [suite]})
      [alice, bob, charlie]
  traverse_ (uploadNewKeyPackage suite) [bob1, charlie1]
  conv <- createNewGroup suite alice1
  void $ createAddCommit alice1 conv [bob, charlie] >>= sendAndConsumeCommitBundle
  mlsConv <- getMLSConv conv

  -- reset conversation
  resetConversation bob mlsConv.groupId mlsConv.epoch >>= assertStatus 200
  groupId' <- do
    conv' <- getConversation alice conv >>= getJSON 200
    conv' %. "group_id" `shouldNotMatch` (mlsConv.groupId :: String)
    conv' %. "epoch" `shouldMatchInt` 0
    otherMember <- assertOne =<< asList (conv' %. "members.others")
    otherMember %. "qualified_id" `shouldMatch` (bob %. "qualified_id")
    conv' %. "group_id" & asString
  convId' <- resetGroup alice1 mlsConv.convId groupId'

  -- charlie cannot rejoin
  void $ uploadNewKeyPackage suite charlie1
  mp <- createAddCommit alice1 convId' [charlie]
  bindResponse (postMLSCommitBundle mp.sender (mkBundle mp)) $ \resp -> do
    resp.status `shouldMatchInt` 400
    resp.json %. "label" `shouldMatch` "mls-group-id-not-supported"

  do
    conv' <- getConversation alice conv >>= getJSON 200
    conv' %. "group_id" `shouldNotMatch` (mlsConv.groupId :: String)
    conv' %. "epoch" `shouldMatchInt` 0
    otherMember <- assertOne =<< asList (conv' %. "members.others")
    otherMember %. "qualified_id" `shouldMatch` (bob %. "qualified_id")
