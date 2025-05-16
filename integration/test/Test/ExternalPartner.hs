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

module Test.ExternalPartner where

import API.Galley
import GHC.Stack
import MLS.Util
import SetupHelpers
import Testlib.Prelude

testExternalPartnerPermissions :: (HasCallStack) => App ()
testExternalPartnerPermissions = do
  (owner, tid, u1 : u2 : u3 : _) <- createTeam OwnDomain 4

  partner <- createTeamMember owner def {role = "partner"}

  -- a partner should not be able to create conversation with 2 additional users or more
  void $ postConversation partner (defProteus {team = Just tid, qualifiedUsers = [u1, u2]}) >>= getJSON 403

  do
    -- a partner can create a one to one conversation with a user from the same team
    conv <- postConversation partner (defProteus {team = Just tid, qualifiedUsers = [u1]}) >>= getJSON 201

    -- they should not be able to add another team member to the one to one conversation
    bindResponse (addMembers partner conv def {users = [u2]}) $ \resp -> do
      resp.status `shouldMatchInt` 403

    -- the other member in the conversation gets deleted
    deleteUser u1

    -- now they still should not be able to add another member
    bindResponse (addMembers partner conv def {users = [u2]}) $ \resp -> do
      resp.status `shouldMatchInt` 403

  do
    -- also an external partner cannot add someone to a conversation, even if it is empty
    conv <- postConversation partner (defProteus {team = Just tid}) >>= getJSON 201
    bindResponse (addMembers partner conv def {users = [u3]}) $ \resp -> do
      resp.status `shouldMatchInt` 403

testExternalPartnerPermissionsMls :: (HasCallStack) => App ()
testExternalPartnerPermissionsMls = do
  -- external partners should not be able to create (MLS) conversations
  (owner, _, _) <- createTeam OwnDomain 2
  bobExt <- createTeamMember owner def {role = "partner"}
  bobExtClient <- createMLSClient def bobExt
  bindResponse (postConversation bobExtClient defMLS) $ \resp -> do
    resp.status `shouldMatchInt` 403

testExternalPartnerPermissionMlsOne2One :: (HasCallStack) => App ()
testExternalPartnerPermissionMlsOne2One = do
  (owner, _, alice : _) <- createTeam OwnDomain 2
  bobExternal <- createTeamMember owner def {role = "partner"}
  void $ getMLSOne2OneConversation alice bobExternal >>= getJSON 200

testExternalPartnerPermissionsConvName :: (HasCallStack) => App ()
testExternalPartnerPermissionsConvName = do
  (owner, tid, u1 : _) <- createTeam OwnDomain 2

  partner <- createTeamMember owner def {role = "partner"}

  conv <- postConversation partner (defProteus {team = Just tid, qualifiedUsers = [u1]}) >>= getJSON 201

  bindResponse (changeConversationName partner conv "new name") $ \resp -> do
    resp.status `shouldMatchInt` 403

testExternalPartnerCannotBecomeConversationAdmin :: (HasCallStack) => App ()
testExternalPartnerCannotBecomeConversationAdmin = do
  (owner, tid, tm1 : tm2 : _) <- createTeam OwnDomain 3
  partner <- createTeamMember owner def {role = "partner"}
  conv <- postConversation owner (defProteus {team = Just tid, qualifiedUsers = [partner, tm1], newUsersRole = "wire_admin"}) >>= getJSON 201

  bindResponse (getConversation owner conv) $ \resp -> do
    resp.status `shouldMatchInt` 200
    members <- resp.json %. "members.others" & asList
    for_ members $ \m -> do
      m %. "conversation_role" `shouldMatch` "wire_admin"

  bindResponse (addMembers partner conv def {users = [tm2]}) $ \resp -> do
    resp.status `shouldMatchInt` 403
    resp.json %. "label" `shouldMatch` "invalid-op"
