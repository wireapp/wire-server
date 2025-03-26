{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

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

module Test.Channels where

import API.Common (randomName)
import API.Galley
import API.GalleyInternal hiding (setTeamFeatureConfig)
import GHC.Stack
import MLS.Util
import Notifications (isChannelAddPermissionUpdate)
import SetupHelpers
import Testlib.JSON
import Testlib.Prelude

testCreateChannelEveryone :: (HasCallStack) => App ()
testCreateChannelEveryone = do
  (owner, tid, mem : _) <- createTeam OwnDomain 2
  partner <- createTeamMember owner def {role = "partner"}
  ownerClient <- createMLSClient def def owner
  memClient <- createMLSClient def def mem
  partnerClient <- createMLSClient def def partner
  for_ [memClient, ownerClient, partnerClient] (uploadNewKeyPackage def)
  setTeamFeatureLockStatus owner tid "channels" "unlocked"
  void $ setTeamFeatureConfig owner tid "channels" (config "everyone")
  assertCreateChannelSuccess ownerClient tid
  assertCreateChannelSuccess memClient tid
  assertCreateChannelSuccess partnerClient tid

testCreateChannelMembersOnly :: (HasCallStack) => App ()
testCreateChannelMembersOnly = do
  (owner, tid, mem : _) <- createTeam OwnDomain 2
  partner <- createTeamMember owner def {role = "partner"}
  ownerClient <- createMLSClient def def owner
  memClient <- createMLSClient def def mem
  partnerClient <- createMLSClient def def partner
  for_ [memClient, ownerClient, partnerClient] (uploadNewKeyPackage def)
  setTeamFeatureLockStatus owner tid "channels" "unlocked"
  void $ setTeamFeatureConfig owner tid "channels" (config "team-members")
  assertCreateChannelSuccess ownerClient tid
  assertCreateChannelSuccess memClient tid
  assertCreateChannelFailure "operation-denied" partnerClient tid

testCreateChannelAdminsOnly :: (HasCallStack) => App ()
testCreateChannelAdminsOnly = do
  (owner, tid, mem : _) <- createTeam OwnDomain 2
  partner <- createTeamMember owner def {role = "partner"}
  ownerClient <- createMLSClient def def owner
  memClient <- createMLSClient def def mem
  partnerClient <- createMLSClient def def partner
  for_ [memClient, ownerClient, partnerClient] (uploadNewKeyPackage def)
  setTeamFeatureLockStatus owner tid "channels" "unlocked"
  void $ setTeamFeatureConfig owner tid "channels" (config "admins")
  assertCreateChannelSuccess ownerClient tid
  assertCreateChannelFailure "operation-denied" memClient tid
  assertCreateChannelFailure "operation-denied" partnerClient tid

testCreateChannelFeatureDisabled :: (HasCallStack) => App ()
testCreateChannelFeatureDisabled = do
  (owner, tid, _) <- createTeam OwnDomain 1
  ownerClient <- createMLSClient def def owner
  void $ uploadNewKeyPackage def ownerClient
  assertCreateChannelFailure "channels-not-enabled" ownerClient tid

testCreateChannelNonTeamConvNotAllowed :: (HasCallStack) => App ()
testCreateChannelNonTeamConvNotAllowed = do
  user <- randomUser OwnDomain def
  userClient <- createMLSClient def def user
  void $ uploadNewKeyPackage def userClient
  postConversation userClient defMLS {groupConvType = Just "channel"} `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 403
    resp.json %. "label" `shouldMatch` "operation-denied"

testCreateChannelProteusNotAllowed :: (HasCallStack) => App ()
testCreateChannelProteusNotAllowed = do
  (owner, tid, _) <- createTeam OwnDomain 1
  setTeamFeatureLockStatus owner tid "channels" "unlocked"
  void $ setTeamFeatureConfig owner tid "channels" (config "everyone")
  postConversation owner defProteus {groupConvType = Just "channel", team = Just tid} `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 403
    resp.json %. "label" `shouldMatch` "not-mls-conversation"

assertCreateChannelSuccess :: (HasCallStack) => ClientIdentity -> String -> App ()
assertCreateChannelSuccess client tid = do
  conv <- postConversation client defMLS {groupConvType = Just "channel", team = Just tid} >>= getJSON 201
  conv %. "group_conv_type" `shouldMatch` "channel"

assertCreateChannelFailure :: (HasCallStack) => String -> ClientIdentity -> String -> App ()
assertCreateChannelFailure label client tid = do
  postConversation client defMLS {groupConvType = Just "channel", team = Just tid} `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 403
    resp.json %. "label" `shouldMatch` label

config :: String -> Value
config perms =
  object
    [ "status" .= "enabled",
      "config"
        .= object
          [ "allowed_to_create_channels" .= perms,
            "allowed_to_open_channels" .= perms
          ]
    ]

testTeamAdminPermissions :: (HasCallStack) => App ()
testTeamAdminPermissions = do
  (owner, tid, mem : nonAdmin : mems) <- createTeam OwnDomain 10
  clients@(ownerClient : memClient : nonAdminClient : _) <- for (owner : mem : nonAdmin : mems) $ createMLSClient def def
  for_ clients (uploadNewKeyPackage def)
  setTeamFeatureLockStatus owner tid "channels" "unlocked"
  void $ setTeamFeatureConfig owner tid "channels" (config "everyone")

  -- a member creates a channel
  conv <- postConversation memClient defMLS {groupConvType = Just "channel", team = Just tid} >>= getJSON 201
  convId <- objConvId conv
  createGroup def memClient convId

  -- other team members are added to the channel
  void $ createAddCommit memClient convId [owner, nonAdmin] >>= sendAndConsumeCommitBundle
  bindResponse (getConversation mem (convIdToQidObject convId)) $ \resp -> do
    resp.status `shouldMatchInt` 200
    members <- resp.json %. "members" %. "others" & asList
    for members (\m -> m %. "id") `shouldMatchSet` (for [owner, nonAdmin] (\m -> m %. "id"))
    for_ members $ \m -> do
      m %. "conversation_role" `shouldMatch` "wire_member"

  let otherMembers = mems `zip` drop 3 clients

  assertChannelAdminPermission convId conv mem memClient (head otherMembers) owner
  assertChannelAdminPermission convId conv owner ownerClient (otherMembers !! 1) mem
  assertNoChannelAdminPermission convId conv nonAdmin nonAdminClient (otherMembers !! 2) ownerClient
  -- make nonAdmin a team admin
  updateTeamMember tid owner nonAdmin Admin >>= assertSuccess
  assertChannelAdminPermission convId conv nonAdmin nonAdminClient (otherMembers !! 3) mem
  -- make nonAdmin a team member again
  updateTeamMember tid owner nonAdmin Member >>= assertSuccess
  assertNoChannelAdminPermission convId conv nonAdmin nonAdminClient (otherMembers !! 4) ownerClient
  -- finally make them admin again and check that they can delete the conversation
  updateTeamMember tid owner nonAdmin Admin >>= assertSuccess
  deleteTeamConv tid conv nonAdmin >>= assertSuccess
  where
    assertChannelAdminPermission :: (HasCallStack) => ConvId -> Value -> Value -> ClientIdentity -> (Value, ClientIdentity) -> Value -> App ()
    assertChannelAdminPermission convId conv user userClient (userToAdd, userToAddClient) userToUpdate = do
      newName <- randomName
      changeConversationName user conv newName >>= assertSuccess
      updateReceiptMode user conv (42 :: Int) >>= assertSuccess
      updateMessageTimer user conv 1000 >>= assertSuccess
      updateAccess user conv (["access" .= ["code", "invite"], "access_role" .= ["team_member", "guest"]]) >>= assertSuccess
      updateConversationMember user conv userToUpdate "wire_member" >>= assertSuccess
      updateConversationSelf user conv (object ["otr_archived" .= True]) >>= assertSuccess
      postConversationCode user conv Nothing Nothing >>= assertSuccess
      getConversationCode user conv Nothing >>= assertSuccess
      deleteConversationCode user conv >>= assertSuccess
      updateChannelAddPermission user conv "everyone" >>= assertSuccess
      bindResponse (getConversation user conv) $ \resp -> do
        resp.status `shouldMatchInt` 200
        resp.json %. "name" `shouldMatch` newName
        resp.json %. "receipt_mode" `shouldMatchInt` 42
        resp.json %. "message_timer" `shouldMatchInt` 1000
        asList (resp.json %. "access_role") `shouldMatchSet` ["team_member", "guest"]
        resp.json %. "members.self.otr_archived" `shouldMatch` True
        resp.json %. "add_permission" `shouldMatch` "everyone"
      -- we need to reset the add permission to admins for the next assertions to be meaningful
      updateChannelAddPermission user conv "admins" >>= assertSuccess
      void $ createAddCommit userClient convId [userToAdd] >>= sendAndConsumeCommitBundle
      void $ createRemoveCommit userClient convId [userToAddClient] >>= sendAndConsumeCommitBundle

    assertNoChannelAdminPermission :: (HasCallStack) => ConvId -> Value -> Value -> ClientIdentity -> (Value, ClientIdentity) -> ClientIdentity -> App ()
    assertNoChannelAdminPermission convId conv user userClient (userToAdd, _) userToUpdate = do
      newName <- randomName
      changeConversationName user conv newName `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 403
        resp.json %. "label" `shouldMatch` "action-denied"
      updateReceiptMode user conv (41 :: Int) `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 403
        resp.json %. "label" `shouldMatch` "action-denied"
      updateMessageTimer user conv 2000 `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 403
        resp.json %. "label" `shouldMatch` "action-denied"
      updateAccess user conv (["access" .= ["code"], "access_role" .= ["team_member", "guest"]]) `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 403
        resp.json %. "label" `shouldMatch` "action-denied"
      updateConversationMember user conv userToUpdate "wire_member" `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 403
        resp.json %. "label" `shouldMatch` "action-denied"
      tid <- user %. "team" & asString
      deleteTeamConv tid conv user `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 403
        resp.json %. "label" `shouldMatch` "action-denied"
      updateChannelAddPermission user conv "everyone" `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 403
        resp.json %. "label" `shouldMatch` "action-denied"
      updateConversationSelf user conv (object ["otr_archived" .= True]) >>= assertSuccess
      -- since the mls test client cannot handle failed commits, we need to restore the state manually
      mlsState <- getMLSState
      createAddCommit userClient convId [userToAdd] >>= \mp -> postMLSCommitBundle userClient (mkBundle mp) >>= assertStatus 403
      modifyMLSState (const mlsState)
      createRemoveCommit userClient convId [userToUpdate] >>= \mp -> postMLSCommitBundle userClient (mkBundle mp) >>= assertStatus 403
      modifyMLSState (const mlsState)

testUpdateAddPermissions :: (HasCallStack) => App ()
testUpdateAddPermissions = do
  (alice, tid, bob : chaz : _) <- createTeam OwnDomain 3
  clients@(aliceClient : _) <- for [alice, bob, chaz] $ createMLSClient def def
  for_ clients (uploadNewKeyPackage def)
  setTeamFeatureLockStatus alice tid "channels" "unlocked"
  void $ setTeamFeatureConfig alice tid "channels" (config "everyone")

  conv <- postConversation alice defMLS {groupConvType = Just "channel", team = Just tid} >>= getJSON 201
  convId <- objConvId conv
  createGroup def aliceClient convId

  bindResponse (getConversation alice (convIdToQidObject convId)) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "add_permission" `shouldMatch` "everyone"

  void $ createAddCommit aliceClient convId [bob, chaz] >>= sendAndConsumeCommitBundle
  void $ withWebSockets [alice, bob, chaz] $ \wss -> do
    updateChannelAddPermission alice conv "admins" >>= assertSuccess
    for_ wss $ \ws -> awaitMatch isChannelAddPermissionUpdate ws

testAddPermissionEveryone :: (HasCallStack) => App ()
testAddPermissionEveryone = do
  (alice, tid, bob : chaz : delia : eric : _) <- createTeam OwnDomain 5
  gunther <- randomUser OwnDomain def
  clients@(aliceClient : bobClient : chazClient : _ : _ : guntherClient : _) <- for [alice, bob, chaz, delia, eric, gunther] $ createMLSClient def def
  connectTwoUsers bob gunther
  connectTwoUsers gunther eric
  for_ clients (uploadNewKeyPackage def)
  setTeamFeatureLockStatus alice tid "channels" "unlocked"
  void $ setTeamFeatureConfig alice tid "channels" (config "everyone")
  conv <- postConversation alice defMLS {groupConvType = Just "channel", team = Just tid} >>= getJSON 201
  convId <- objConvId conv
  createGroup def aliceClient convId
  void $ createAddCommit aliceClient convId [bob] >>= sendAndConsumeCommitBundle

  bindResponse (getConversation alice (convIdToQidObject convId)) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "add_permission" `shouldMatch` "everyone"
    members <- resp.json %. "members" %. "others" & asList
    for members (\m -> m %. "id") `shouldMatchSet` (for [bob] (\m -> m %. "id"))
    for_ members $ \m -> do
      m %. "conversation_role" `shouldMatch` "wire_member"

  assertAddSuccess convId bobClient (chaz, chazClient)
  -- guests can be added
  assertAddSuccess convId bobClient (gunther, guntherClient)
  -- but guests are not allowed to add other members even when the add permission is set to everyone
  assertAddFailure convId guntherClient eric
  -- set permissions back to admins
  updateChannelAddPermission alice conv "admins" >>= assertSuccess
  assertAddFailure convId bobClient delia
  where
    assertAddSuccess :: (HasCallStack) => ConvId -> ClientIdentity -> (Value, ClientIdentity) -> App ()
    assertAddSuccess convId userClient (userToAdd, userToAddClient) = do
      void $ createAddCommit userClient convId [userToAdd] >>= sendAndConsumeCommitBundle
      mlsState <- getMLSState
      -- they cant remove, though
      createRemoveCommit userClient convId [userToAddClient] >>= \mp -> postMLSCommitBundle userClient (mkBundle mp) >>= assertStatus 403
      modifyMLSState (const mlsState)

    assertAddFailure :: (HasCallStack) => ConvId -> ClientIdentity -> Value -> App ()
    assertAddFailure convId userClient userToAdd = do
      mlsState <- getMLSState
      createAddCommit userClient convId [userToAdd] >>= \mp -> postMLSCommitBundle userClient (mkBundle mp) >>= assertStatus 403
      modifyMLSState (const mlsState)

testFederatedChannel :: (HasCallStack) => App ()
testFederatedChannel = do
  (alice, teamAlice, anton : _) <- createTeam OwnDomain 2
  (bärbel, _, bob : _) <- createTeam OtherDomain 2
  connectTwoUsers alice bärbel
  connectTwoUsers alice bob
  clients@(aliceClient : _ : bärbelClient : _) <- for [alice, anton, bärbel, bob] $ createMLSClient def def
  for_ clients (uploadNewKeyPackage def)

  setTeamFeatureLockStatus alice teamAlice "channels" "unlocked"
  void $ setTeamFeatureConfig alice teamAlice "channels" (config "everyone")
  conv <- postConversation alice defMLS {groupConvType = Just "channel", team = Just teamAlice} >>= getJSON 201
  convId <- objConvId conv
  createGroup def aliceClient convId
  void $ createAddCommit aliceClient convId [anton, bärbel] >>= sendAndConsumeCommitBundle

  bindResponse (getConversation alice (convIdToQidObject convId)) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "add_permission" `shouldMatch` "everyone"
    members <- resp.json %. "members" %. "others" & asList
    for members (\m -> m %. "id") `shouldMatchSet` (for [anton, bärbel] (\m -> m %. "id"))
    for_ members $ \m -> do
      m %. "conversation_role" `shouldMatch` "wire_member"

  -- remote user gets the event
  void $ withWebSockets [bärbel] $ \wss -> do
    updateChannelAddPermission alice conv "admins" >>= assertSuccess
    for_ wss $ \ws -> awaitMatch isChannelAddPermissionUpdate ws

  -- even when the remote member is promoted to a conversation admin they can cant add other members, because this is not implemented yet
  updateConversationMember alice conv bärbel "wire_admin" >>= assertSuccess
  assertAddFails convId bärbelClient bob
  where
    assertAddFails :: (HasCallStack) => ConvId -> ClientIdentity -> Value -> App ()
    assertAddFails convId userClient userToAdd = do
      mp <- createAddCommit userClient convId [userToAdd]
      postMLSCommitBundle userClient (mkBundle mp) `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 500
        resp.json %. "label" `shouldMatch` "federation-not-implemented"
