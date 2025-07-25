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
import API.GalleyInternal hiding (getConversation, setTeamFeatureConfig)
import qualified API.GalleyInternal as I
import GHC.Stack
import MLS.Util
import Notifications (isChannelAddPermissionUpdate, isMemberJoinNotif, isWelcomeNotif)
import SetupHelpers
import Testlib.JSON
import Testlib.Prelude
import Testlib.VersionedFed (FedDomain)

testCreateChannelEveryone :: (HasCallStack) => App ()
testCreateChannelEveryone = do
  (owner, tid, mem : otherTeamMembers) <- createTeam OwnDomain 4
  partner <- createTeamMember owner def {role = "partner"}
  ownerClient <- createMLSClient def owner
  memClient <- createMLSClient def mem
  partnerClient <- createMLSClient def partner
  otherClients <- for otherTeamMembers $ createMLSClient def
  replicateM_ 3 $ for_ (memClient : ownerClient : partnerClient : otherClients) (uploadNewKeyPackage def)
  setTeamFeatureLockStatus owner tid "channels" "unlocked"
  void $ setTeamFeatureConfig owner tid "channels" (config "everyone")
  assertCreateChannelSuccess_ ownerClient tid otherTeamMembers
  assertCreateChannelSuccess_ memClient tid otherTeamMembers
  assertCreateChannelSuccess_ partnerClient tid otherTeamMembers

testCreateChannelMembersOnly :: (HasCallStack) => App ()
testCreateChannelMembersOnly = do
  (owner, tid, mem : otherTeamMembers) <- createTeam OwnDomain 4
  partner <- createTeamMember owner def {role = "partner"}
  ownerClient <- createMLSClient def owner
  memClient <- createMLSClient def mem
  partnerClient <- createMLSClient def partner
  otherClients <- for otherTeamMembers $ createMLSClient def
  replicateM_ 3 $ for_ (memClient : ownerClient : partnerClient : otherClients) (uploadNewKeyPackage def)
  setTeamFeatureLockStatus owner tid "channels" "unlocked"
  void $ setTeamFeatureConfig owner tid "channels" (config "team-members")
  assertCreateChannelSuccess_ ownerClient tid otherTeamMembers
  assertCreateChannelSuccess_ memClient tid otherTeamMembers
  assertCreateChannelFailure "operation-denied" partnerClient tid

testCreateChannelAdminsOnly :: (HasCallStack) => App ()
testCreateChannelAdminsOnly = do
  (owner, tid, mem : otherTeamMembers) <- createTeam OwnDomain 4
  partner <- createTeamMember owner def {role = "partner"}
  ownerClient <- createMLSClient def owner
  memClient <- createMLSClient def mem
  partnerClient <- createMLSClient def partner
  otherClients <- for otherTeamMembers $ createMLSClient def
  replicateM_ 3 $ for_ (memClient : ownerClient : partnerClient : otherClients) (uploadNewKeyPackage def)
  setTeamFeatureLockStatus owner tid "channels" "unlocked"
  void $ setTeamFeatureConfig owner tid "channels" (config "admins")
  assertCreateChannelSuccess_ ownerClient tid otherTeamMembers
  assertCreateChannelFailure "operation-denied" memClient tid
  assertCreateChannelFailure "operation-denied" partnerClient tid

testCreateChannelFeatureDisabled :: (HasCallStack) => App ()
testCreateChannelFeatureDisabled = do
  (owner, tid, _) <- createTeam OwnDomain 1
  ownerClient <- createMLSClient def owner
  void $ uploadNewKeyPackage def ownerClient
  assertCreateChannelFailure "channels-not-enabled" ownerClient tid

testCreateChannelNonTeamConvNotAllowed :: (HasCallStack) => App ()
testCreateChannelNonTeamConvNotAllowed = do
  user <- randomUser OwnDomain def
  userClient <- createMLSClient def user
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

assertCreateChannelSuccess_ :: (HasCallStack) => ClientIdentity -> String -> [Value] -> App ()
assertCreateChannelSuccess_ client tid members = void $ assertCreateChannelSuccess client tid members

assertCreateChannelSuccess :: (HasCallStack) => ClientIdentity -> String -> [Value] -> App Value
assertCreateChannelSuccess client tid members = do
  conv <-
    postConversation
      client
      defMLS {groupConvType = Just "channel", team = Just tid, addPermission = Just "admins"}
      >>= getJSON 201
  conv %. "group_conv_type" `shouldMatch` "channel"
  convId <- objConvId conv
  createGroup def client convId
  resp <- createAddCommit client convId members >>= sendAndConsumeCommitBundle
  (resp %. "events.0.data.user_ids" & asList) `shouldMatchSet` (for members (%. "id"))
  pure conv

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
  clients@(ownerClient : memClient : nonAdminClient : _) <- for (owner : mem : nonAdmin : mems) $ createMLSClient def
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
  clients@(aliceClient : _) <- for [alice, bob, chaz] $ createMLSClient def
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

testSetAddPermissionOnChannelCreation :: (HasCallStack) => App ()
testSetAddPermissionOnChannelCreation = do
  (alice, tid, _) <- createTeam OwnDomain 1
  aliceClient <- createMLSClient def alice
  void $ uploadNewKeyPackage def aliceClient
  setTeamFeatureLockStatus alice tid "channels" "unlocked"
  void $ setTeamFeatureConfig alice tid "channels" (config "everyone")

  conv <- postConversation alice defMLS {groupConvType = Just "channel", team = Just tid, addPermission = Just "admins"} >>= getJSON 201
  convId <- objConvId conv
  bindResponse (getConversation alice (convIdToQidObject convId)) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "add_permission" `shouldMatch` "admins"

testAddPermissionEveryone :: (HasCallStack) => App ()
testAddPermissionEveryone = do
  (alice, tid, bob : chaz : delia : eric : _) <- createTeam OwnDomain 5
  gunther <- randomUser OwnDomain def
  clients@(aliceClient : bobClient : chazClient : _ : _ : guntherClient : _) <- for [alice, bob, chaz, delia, eric, gunther] $ createMLSClient def
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
  clients@(aliceClient : _ : bärbelClient : _) <- for [alice, anton, bärbel, bob] $ createMLSClient def
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

-- if the federation queue gets stuck, the second test run will fail
-- therefore this test verifies that a notification that cannot be parsed by the remote
-- backend does not block the queue
testWithOldBackendVersion :: (HasCallStack) => FedDomain 1 -> App ()
testWithOldBackendVersion fedDomain = replicateM_ 2 do
  let cs = Ciphersuite "0x0001"
  (bärbel, tid, _) <- createTeam OwnDomain 2
  horst <- randomUser fedDomain def
  connectTwoUsers bärbel horst

  bärbelClient <- createMLSClient def {ciphersuites = [cs]} bärbel
  void $ uploadNewKeyPackage cs bärbelClient
  horstClient <- createMLSClient def {ciphersuites = [cs]} horst
  void $ uploadNewKeyPackage cs horstClient

  setTeamFeatureLockStatus bärbel tid "channels" "unlocked"
  void $ setTeamFeatureConfig bärbel tid "channels" (config "everyone")
  conv <- postConversation bärbel defMLS {groupConvType = Just "channel", team = Just tid} >>= getJSON 201
  convId <- objConvId conv
  createGroup cs bärbelClient convId
  void $ createAddCommit bärbelClient convId [horst] >>= sendAndConsumeCommitBundle

  -- this will trigger a notification that the old backend cannot parse
  updateChannelAddPermission bärbel conv "admins" >>= assertSuccess

testAddPermissionAdminExternalPartner :: (HasCallStack) => App ()
testAddPermissionAdminExternalPartner = do
  _testAddtermissionExternalPartner "admins" $ \partnerClient convId mems -> do
    commit <- createAddCommit partnerClient convId mems
    postMLSCommitBundle partnerClient (mkBundle commit) `bindResponse` \resp -> do
      resp.status `shouldMatchInt` 403
      resp.json %. "label" `shouldMatch` "action-denied"

testAddPermissionEveryoneExternalPartner :: (HasCallStack) => App ()
testAddPermissionEveryoneExternalPartner = do
  _testAddtermissionExternalPartner "everyone" $ \partnerClient convId mems -> do
    resp <- createAddCommit partnerClient convId mems >>= sendAndConsumeCommitBundle
    (resp %. "events.0.data.user_ids" & asList) `shouldMatchSet` (for mems (%. "id"))

_testAddtermissionExternalPartner :: (HasCallStack) => String -> (ClientIdentity -> ConvId -> [Value] -> App ()) -> App ()
_testAddtermissionExternalPartner addPermission assertion = do
  (owner, tid, mems) <- createTeam OwnDomain 3
  setTeamFeatureLockStatus owner tid "channels" "unlocked"
  void $ setTeamFeatureConfig owner tid "channels" (config "everyone")
  partner <- createTeamMember owner def {role = "partner"}
  clients@(ownerClient : partnerClient : _) <- for (owner : partner : mems) $ createMLSClient def
  for_ clients (uploadNewKeyPackage def)
  let p =
        defMLS
          { groupConvType = Just "channel",
            team = Just tid,
            addPermission = Just addPermission
          }
  conv <- postConversation owner p >>= getJSON 201
  convId <- objConvId conv
  createGroup def ownerClient convId
  void $ createAddCommit ownerClient convId [partner] >>= sendAndConsumeCommitBundle
  assertion partnerClient convId mems

testTeamAdminCanCreateChannelWithoutJoining :: (HasCallStack) => App ()
testTeamAdminCanCreateChannelWithoutJoining = do
  (owner, tid, _) <- createTeam OwnDomain 1

  setTeamFeatureLockStatus owner tid "channels" "unlocked"
  void $ setTeamFeatureConfig owner tid "channels" (config "everyone")

  conv <-
    postConversation owner defMLS {groupConvType = Just "channel", team = Just tid, skipCreator = Just True} `bindResponse` \resp -> do
      resp.status `shouldMatchInt` 201
      resp.json %. "members.others" `shouldMatch` ([] :: [Value])
      pure resp.json

  I.getConversation conv `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 200

testNonTeamAdminCannotAddMembersWithoutJoining :: (HasCallStack) => App ()
testNonTeamAdminCannotAddMembersWithoutJoining = do
  (owner, tid, mems@(m1 : m2 : m3 : _)) <- createTeam OwnDomain 4
  cs <- for mems $ createMLSClient def
  for_ cs $ uploadNewKeyPackage def

  setTeamFeatureLockStatus owner tid "channels" "unlocked"
  void $ setTeamFeatureConfig owner tid "channels" (config "admins")

  channel <- postConversation owner defMLS {groupConvType = Just "channel", team = Just tid, skipCreator = Just True} >>= getJSON 201

  addMembers m1 channel def {users = [m1, m2, m3], role = Just "wire_member"} `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 404
    resp.json %. "label" `shouldMatch` "no-conversation"

testTeamAdminCanChangeChannelNameWithoutJoining :: (HasCallStack) => App ()
testTeamAdminCanChangeChannelNameWithoutJoining = do
  (owner, tid, mem : _) <- createTeam OwnDomain 2
  setTeamFeatureLockStatus owner tid "channels" "unlocked"
  void $ setTeamFeatureConfig owner tid "channels" (config "everyone")
  conv <-
    postConversation
      owner
      defMLS {name = Just "foo", groupConvType = Just "channel", team = Just tid, skipCreator = Just True}
      >>= getJSON 201
  I.getConversation conv `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "name" `shouldMatch` "foo"
  newName <- randomName
  changeConversationName owner conv newName >>= assertSuccess
  I.getConversation conv `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "name" `shouldMatch` newName
  changeConversationName mem conv newName `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 404
    resp.json %. "label" `shouldMatch` "no-conversation"
  I.getConversation conv `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "name" `shouldMatch` newName

testTeamAdminCanAddMembersWithoutJoining :: (HasCallStack) => App ()
testTeamAdminCanAddMembersWithoutJoining = do
  (owner, tid, mems@(m1 : m2 : m3 : m4 : m5 : _)) <- createTeam OwnDomain 6
  cs@(c1 : c2 : c3 : c4 : c5 : _) <- for mems $ createMLSClient def
  for_ cs $ uploadNewKeyPackage def

  setTeamFeatureLockStatus owner tid "channels" "unlocked"
  void $ setTeamFeatureConfig owner tid "channels" (config "admins")

  -- the team admin creates a channel without joining
  channel <- postConversation owner defMLS {groupConvType = Just "channel", team = Just tid, skipCreator = Just True} >>= getJSON 201
  convId <- objConvId channel

  withWebSockets [c1, c2, c3, c4, c5] $ \[ws1, ws2, ws3, ws4, ws5] -> do
    -- the team admin adds members to the channel
    addMembersToChannel owner channel def {users = [m1, m2, m3], role = Just "wire_member"} `bindResponse` \resp -> do
      resp.status `shouldMatchInt` 200

    -- the members are added to the backend conversation
    I.getConversation channel `bindResponse` \resp -> do
      resp.status `shouldMatchInt` 200
      convMems <- resp.json %. "members.others" & asList
      for [m1, m2, m3] (\m -> m %. "id") `shouldMatchSet` (for convMems (\m -> m %. "id"))

    do
      -- client if m1 receives the member join notification
      notif <- awaitMatch isMemberJoinNotif ws1
      -- if add_type is external_add ...
      notif %. "payload.0.data.add_type" `shouldMatch` "external_add"
      qconv <- notif %. "payload.0.qualified_conversation"
      membersToAdd <- notif %. "payload.0.data.users" & asList

      conv <- getConversation m1 qconv >>= getJSON 200
      -- ... and the epoch is 0
      conv %. "epoch" `shouldMatchInt` 0
      -- the client creates the MLS group and adds everyone else
      createGroupForChannel def c1 convId membersToAdd
      void $ createAddCommit c1 convId membersToAdd >>= sendAndConsumeCommitBundle

    -- the members that were added receive a welcome message
    for_ [ws2, ws3] $ \ws -> do
      awaitMatch isWelcomeNotif ws

    -- the team admin adds another member to the channel
    addMembersToChannel owner channel def {users = [m4, m5], role = Just "wire_member"} `bindResponse` \resp -> do
      resp.status `shouldMatchInt` 200

    notif <- awaitMatch isMemberJoinNotif ws4
    notif %. "payload.0.data.add_type" `shouldMatch` "external_add"

    -- c4 adds itself with an external add
    void $ createExternalCommit convId c4 Nothing >>= sendAndConsumeCommitBundle
    -- add others via normal commit
    membersToAdd <- others m4 notif
    void $ createAddCommit c4 convId membersToAdd >>= sendAndConsumeCommitBundle
    -- m5 receives welcome message
    void $ awaitMatch isWelcomeNotif ws5
  where
    others self memberJoinNotif = do
      allUsers <- memberJoinNotif %. "payload.0.data.users" & asList
      selfQid <- self %. "qualified_id"
      filterM (\m -> (/= selfQid) <$> (m %. "qualified_id")) allUsers

testAdminCanRemoveMemberWithoutJoining :: (HasCallStack) => App ()
testAdminCanRemoveMemberWithoutJoining = do
  (owner, tid, mems@(m1 : m2 : m3 : _)) <- createTeam OwnDomain 4
  cs@(c1 : c2 : c3 : _) <- for mems $ createMLSClient def
  for_ cs $ uploadNewKeyPackage def

  setTeamFeatureLockStatus owner tid "channels" "unlocked"
  void $ setTeamFeatureConfig owner tid "channels" (config "everyone")

  -- a channel is created by a team member
  channel <- assertCreateChannelSuccess c1 tid [m2]
  convId <- objConvId channel
  I.getConversation channel `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 200
    convMems <- resp.json %. "members.others" & asList
    for [m1, m2] (%. "id") `shouldMatchSet` (for convMems (%. "id"))

  withWebSockets [c1, c2, c3] $ \[ws1, _ws2, ws3] -> do
    -- the team admin removes a member from the channel without joining
    removeMemberFromChannel owner channel m2

    I.getConversation channel `bindResponse` \resp -> do
      resp.status `shouldMatchInt` 200
      convMems <- resp.json %. "members.others" & asList
      for [m1] (%. "id") `shouldMatchSet` (for convMems (%. "id"))

    -- the client of m1 receives a notification, creates a pending proposal, sends it, and consumes messages
    awaitAndProcessRemoveProposal convId c1 ws1 1

    -- the team admin now removes the last remaining member
    removeMemberFromChannel owner channel m1

    I.getConversation channel `bindResponse` \resp -> do
      resp.status `shouldMatchInt` 200
      shouldBeEmpty $ resp.json %. "members.others" & asList

    -- now there is no one left to create and submit the pending proposal
    -- the team admin adds another member to the channel again
    addMembersToChannel owner channel def {users = [m3], role = Just "wire_member"} `bindResponse` \resp -> do
      resp.status `shouldMatchInt` 200

    I.getConversation channel `bindResponse` \resp -> do
      resp.status `shouldMatchInt` 200
      convMems <- resp.json %. "members.others" & asList
      for [m3] (%. "id") `shouldMatchSet` (for convMems (%. "id"))

    -- m3 receives a member-join notification and joins via external commit
    notif <- awaitMatch isMemberJoinNotif ws3
    notif %. "payload.0.data.add_type" `shouldMatch` "external_add"

    -- c3 adds itself with an external add
    void $ createExternalCommit convId c3 Nothing >>= sendAndConsumeCommitBundle

    -- now m3 receives the pending remove proposal and processes it
    awaitAndProcessRemoveProposal convId c3 ws3 0
  where
    awaitAndProcessRemoveProposal :: (HasCallStack) => ConvId -> ClientIdentity -> WebSocket -> Int -> App ()
    awaitAndProcessRemoveProposal convId cid ws index = do
      e <- awaitAnyEvent 1 ws
      msgData <- e %. "payload.0.data" & asByteString
      msg <- showMessage def cid msgData
      msg %. "message.content.body.Proposal.Remove.removed" `shouldMatchInt` index
      void $ mlsCliConsume convId def cid msgData
      r <- createPendingProposalCommit convId cid >>= sendAndConsumeCommitBundle
      shouldBeEmpty $ r %. "events"

testTeamAdminCanGetChannelData :: (HasCallStack) => App ()
testTeamAdminCanGetChannelData = do
  (owner, tid, mem : _) <- createTeam OwnDomain 2
  setTeamFeatureLockStatus owner tid "channels" "unlocked"
  void $ setTeamFeatureConfig owner tid "channels" (config "everyone")
  chan <-
    postConversation
      owner
      defMLS {groupConvType = Just "channel", team = Just tid, skipCreator = Just True}
      >>= getJSON 201
  chan %. "group_conv_type" `shouldMatch` "channel"

  -- The admin can get channel data without joining
  getConversation owner chan `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "members.others" `shouldMatch` ([] :: [Value])
    lookupField resp.json "members.self" `shouldMatch` (Nothing :: Maybe Value)

  -- A team member cannot get channel data without joining
  getConversation mem chan `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 403
    resp.json %. "label" `shouldMatch` "access-denied"

  -- The admin cannot get data of a conversation that is not a channel
  conv <- postConversation mem defMLS {team = Just tid} >>= getJSON 201
  conv %. "group_conv_type" `shouldMatch` "group_conversation"
  getConversation owner conv `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 403
    resp.json %. "label" `shouldMatch` "access-denied"
