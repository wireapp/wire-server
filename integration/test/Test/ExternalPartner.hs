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

-- import API.Brig (getConnection)
import API.Galley
import GHC.Stack
import SetupHelpers
import Testlib.Prelude

testExternalPartnerPermissions :: HasCallStack => App ()
testExternalPartnerPermissions = do
  (owner, tid, u1 : u2 : u3 : _) <- createTeam OwnDomain 6

  partner <- createTeamMemberWithRole owner tid "partner"

  -- a partner should not be able to create conversation with more than 2 users
  void $ postConversation partner (defProteus {team = Just tid, qualifiedUsers = [u1, u2]}) >>= getJSON 403

  -- a partner can create a one to one conversation with a user from the same team
  do
    conv <- postConversation partner (defProteus {team = Just tid, qualifiedUsers = [u1]}) >>= getJSON 201

    -- they should not be able to add another team member to the one to one conversation
    bindResponse (addMembers partner conv def {users = [u2]}) $ \resp -> do
      resp.status `shouldMatchInt` 403

    -- the other member in the conversation gets deleted
    deleteUser u1

    -- now they still should not be able to add another member
    bindResponse (addMembers partner conv def {users = [u2]}) $ \resp -> do
      resp.status `shouldMatchInt` 403

  -- they should be able to create an empty conversation and add a member
  -- because this is the conversation creation flow for MLS conversations
  do
    conv <- postConversation partner (defProteus {team = Just tid}) >>= getJSON 201
    bindResponse (addMembers partner conv def {users = [u3]}) $ \resp -> do
      resp.status `shouldMatchInt` 200

    -- now they should not be able to add another member
    bindResponse (addMembers partner conv def {users = [u2]}) $ \resp -> do
      resp.status `shouldMatchInt` 403

    -- the other member in the conversation gets deleted
    deleteUser u3

    -- now they still should not be able to add another member
    bindResponse (addMembers partner conv def {users = [u2]}) $ \resp -> do
      resp.status `shouldMatchInt` 403

testExternalPartnerPermissionsConvName :: HasCallStack => App ()
testExternalPartnerPermissionsConvName = do
  (owner, tid, u1 : _) <- createTeam OwnDomain 2

  partner <- createTeamMemberWithRole owner tid "partner"

  conv <- postConversation partner (defProteus {team = Just tid, qualifiedUsers = [u1]}) >>= getJSON 201

  bindResponse (changeConversationName partner conv "new name") $ \resp -> do
    resp.status `shouldMatchInt` 403
