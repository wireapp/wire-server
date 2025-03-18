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
import MLS.Util (createAddCommit, createGroup, createMLSClient, sendAndConsumeCommitBundle, uploadNewKeyPackage)
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
  (owner, tid, mem : otherMem : mems) <- createTeam OwnDomain 4
  clients@(_ : memClient : _) <- for (owner : mem : otherMem : mems) $ createMLSClient def def
  for_ clients (uploadNewKeyPackage def)
  setTeamFeatureLockStatus owner tid "channels" "unlocked"
  void $ setTeamFeatureConfig owner tid "channels" (config "everyone")

  -- a member creates a channel
  conv <- postConversation memClient defMLS {groupConvType = Just "channel", team = Just tid} >>= getJSON 201
  convId <- objConvId conv
  createGroup def memClient convId

  -- other team members are added to the channel
  void $ createAddCommit memClient convId (owner : otherMem : mems) >>= sendAndConsumeCommitBundle
  bindResponse (getConversation mem (convIdToQidObject convId)) $ \resp -> do
    resp.status `shouldMatchInt` 200
    members <- resp.json %. "members" %. "others" & asList
    for members (\m -> m %. "id") `shouldMatchSet` (for (owner : otherMem : mems) (\m -> m %. "id"))
    for_ members $ \m -> do
      m %. "conversation_role" `shouldMatch` "wire_member"

  assertChannelAdminPermission mem conv
  assertChannelAdminPermission owner conv
  assertNoChannelAdminPermission otherMem conv
  -- make otherMem a team admin
  updateTeamMember tid owner otherMem Admin >>= assertSuccess
  assertChannelAdminPermission otherMem conv
  -- make otherMem a team member again
  updateTeamMember tid owner otherMem Member >>= assertSuccess
  assertNoChannelAdminPermission otherMem conv
  where
    assertChannelAdminPermission :: (HasCallStack) => Value -> Value -> App ()
    assertChannelAdminPermission mem conv = do
      newName <- randomName
      changeConversationName mem conv newName >>= assertSuccess
      bindResponse (getConversation mem conv) $ \resp -> do
        resp.status `shouldMatchInt` 200
        resp.json %. "name" `shouldMatch` newName

    assertNoChannelAdminPermission :: (HasCallStack) => Value -> Value -> App ()
    assertNoChannelAdminPermission mem conv = do
      newName <- randomName
      changeConversationName mem conv newName `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 403
        resp.json %. "label" `shouldMatch` "action-denied"
