-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2024 Wire Swiss GmbH <opensource@wire.com>
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

module Test.Teams where

import API.Brig
import API.BrigInternal (createUser, getInvitationCode, refreshIndex)
import API.Galley (getTeamMembers)
import SetupHelpers
import Testlib.JSON
import Testlib.Prelude

testInvitePersonalUserToTeam :: (HasCallStack) => App ()
testInvitePersonalUserToTeam = do
  (owner, tid, tm : _) <- createTeam OwnDomain 2
  ownerId <- owner %. "id" & asString
  user <- createUser OwnDomain def >>= getJSON 201
  uid <- user %. "id" >>= asString
  email <- user %. "email" >>= asString
  inv <- postInvitation owner (PostInvitation $ Just email) >>= getJSON 201
  code <- getInvitationCode owner inv >>= getJSON 200 >>= (%. "code") & asString
  acceptTeamInvitation user code >>= assertSuccess
  bindResponse (getSelf user) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "team" `shouldMatch` tid
  -- a team member can now find the former personal user in the team
  bindResponse (getTeamMembers tm tid) $ \resp -> do
    resp.status `shouldMatchInt` 200
    members <- resp.json %. "members" >>= asList
    ids <- for members ((%. "user") >=> asString)
    ids `shouldContain` [uid]
  -- the former personal user can now see other team members
  bindResponse (getTeamMembers user tid) $ \resp -> do
    resp.status `shouldMatchInt` 200
    members <- resp.json %. "members" >>= asList
    ids <- for members ((%. "user") >=> asString)
    tmId <- tm %. "id" & asString
    ids `shouldContain` [ownerId]
    ids `shouldContain` [tmId]
  -- the former personal user can now search for the owner
  bindResponse (searchContacts user (owner %. "name") OwnDomain) $ \resp -> do
    resp.status `shouldMatchInt` 200
    documents <- resp.json %. "documents" >>= asList
    ids <- for documents ((%. "id") >=> asString)
    ids `shouldContain` [ownerId]
  refreshIndex OwnDomain
  -- a team member can now search for the former personal user
  bindResponse (searchContacts tm (user %. "name") OwnDomain) $ \resp -> do
    resp.status `shouldMatchInt` 200
    document <- resp.json %. "documents" >>= asList >>= assertOne
    document %. "id" `shouldMatch` uid
    document %. "team" `shouldMatch` tid

  -- void $ assertFailure "TODO(leif): verify user events"
  -- void $ assertFailure "TODO(leif): verify team admin gets events"
