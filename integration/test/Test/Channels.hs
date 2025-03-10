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

import API.Galley
import API.GalleyInternal hiding (setTeamFeatureConfig)
import GHC.Stack
import MLS.Util (createMLSClient, uploadNewKeyPackage)
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
  assertCreateChannelFailure partnerClient tid

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
  assertCreateChannelFailure memClient tid
  assertCreateChannelFailure partnerClient tid

testCreateChannelFeatureDisabled :: (HasCallStack) => App ()
testCreateChannelFeatureDisabled = do
  (owner, tid, _) <- createTeam OwnDomain 1
  ownerClient <- createMLSClient def def owner
  void $ uploadNewKeyPackage def ownerClient
  assertCreateChannelFailure ownerClient tid

testCreateChannelNonTeamConvNotAllowed :: (HasCallStack) => App ()
testCreateChannelNonTeamConvNotAllowed = do
  (owner, tid, _) <- createTeam OwnDomain 1
  ownerClient <- createMLSClient def def owner
  void $ uploadNewKeyPackage def ownerClient
  setTeamFeatureLockStatus owner tid "channels" "unlocked"
  void $ setTeamFeatureConfig owner tid "channels" (config "everyone")
  postConversation ownerClient defMLS {groupConvType = Just "channel"} `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 403
    resp.json %. "label" `shouldMatch` "operation-denied"

testCreateChannelProteusNotAllowed :: (HasCallStack) => App ()
testCreateChannelProteusNotAllowed = do
  (owner, tid, _) <- createTeam OwnDomain 1
  postConversation owner defProteus {groupConvType = Just "channel", team = Just tid} `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 403
    resp.json %. "label" `shouldMatch` "operation-denied"

assertCreateChannelSuccess :: (HasCallStack) => ClientIdentity -> String -> App ()
assertCreateChannelSuccess client tid = do
  conv <- postConversation client defMLS {groupConvType = Just "channel", team = Just tid} >>= getJSON 201
  conv %. "group_conv_type" `shouldMatch` "channel"

assertCreateChannelFailure :: (HasCallStack) => ClientIdentity -> String -> App ()
assertCreateChannelFailure client tid = do
  postConversation client defMLS {groupConvType = Just "channel", team = Just tid} `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 403
    resp.json %. "label" `shouldMatch` "operation-denied"

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
