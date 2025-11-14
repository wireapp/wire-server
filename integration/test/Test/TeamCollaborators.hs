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

module Test.TeamCollaborators where

import API.Brig
import API.Galley
import qualified API.GalleyInternal as Internal
import Data.Tuple.Extra
import Notifications (isConvLeaveNotif, isTeamCollaboratorAddedNotif, isTeamCollaboratorRemovedNotif, isTeamMemberLeaveNotif)
import SetupHelpers
import Testlib.Prelude

testCreateTeamCollaborator :: (HasCallStack) => App ()
testCreateTeamCollaborator = do
  (owner, team, [alice]) <- createTeam OwnDomain 2

  -- At the time of writing, it wasn't clear if this should be a bot instead.
  user <- randomUser OwnDomain def
  (_, userId) <- objQid user
  withWebSockets [owner, alice] $ \[wsOwner, wsAlice] -> do
    addTeamCollaborator
      owner
      team
      user
      [ "create_team_conversation",
        "implicit_connection"
      ]
      >>= assertSuccess

    let checkEvent :: (MakesValue a) => a -> App ()
        checkEvent evt = do
          evt %. "payload.0.data.permissions" `shouldMatch` ["create_team_conversation", "implicit_connection"]
          evt %. "payload.0.data.user" `shouldMatch` userId
          evt %. "payload.0.team" `shouldMatch` team
          evt %. "transient" `shouldMatch` False

    awaitMatch isTeamCollaboratorAddedNotif wsOwner >>= checkEvent
    assertNoEvent 1 wsAlice

  bindResponse (getAllTeamCollaborators owner team) $ \resp -> do
    resp.status `shouldMatchInt` 200
    res <- (resp.jsonBody & asList) <&> assertOne
    res %. "user" `shouldMatch` userId
    res %. "team" `shouldMatch` team
    res %. "permissions" `shouldMatch` ["create_team_conversation", "implicit_connection"]

testTeamCollaboratorEndpointsForbiddenForOtherTeams :: (HasCallStack) => App ()
testTeamCollaboratorEndpointsForbiddenForOtherTeams = do
  (owner, _team, _members) <- createTeam OwnDomain 2
  (_owner2, team2, _members2) <- createTeam OwnDomain 0

  -- At the time of writing, it wasn't clear if this should be a bot instead.
  user <- randomUser OwnDomain def
  addTeamCollaborator
    owner
    team2
    user
    [ "create_team_conversation",
      "implicit_connection"
    ]
    >>= assertStatus 403

  getAllTeamCollaborators owner team2 >>= assertStatus 403

testCreateTeamCollaboratorPostTwice :: (HasCallStack) => App ()
testCreateTeamCollaboratorPostTwice = do
  (owner, team, _members) <- createTeam OwnDomain 2

  -- At the time of writing, it wasn't clear if this should be a bot instead.
  user <- randomUser OwnDomain def
  let add =
        addTeamCollaborator
          owner
          team
          user
          [ "create_team_conversation",
            "implicit_connection"
          ]
  bindResponse add assertSuccess
  bindResponse add $ assertStatus 409

testCollaboratorCanCreateTeamConv :: (HasCallStack) => TaggedBool "collaborator-has-team" -> App ()
testCollaboratorCanCreateTeamConv (TaggedBool collaboratorHasTeam) = do
  (owner, team, _) <- createTeam OwnDomain 1
  (_, nonCollaboratingTeam, _) <- createTeam OwnDomain 1
  collaborator <-
    if collaboratorHasTeam
      then head . thd3 <$> createTeam OwnDomain 2
      else randomUser OwnDomain def

  addTeamCollaborator owner team collaborator ["create_team_conversation"]
    >>= assertSuccess

  postConversation collaborator (defMLS {team = Just nonCollaboratingTeam}) `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 403
    resp.json %. "label" `shouldMatch` "no-team-member"

  postConversation collaborator (defMLS {team = Just team}) `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 201
    resp.json %. "team" `shouldMatch` team

testImplicitConnectionAllowed :: (HasCallStack) => App ()
testImplicitConnectionAllowed = do
  (owner, team, [alice]) <- createTeam OwnDomain 2

  -- At the time of writing, it wasn't clear if this should be a bot instead.
  bob <- randomUser OwnDomain def
  addTeamCollaborator
    owner
    team
    bob
    ["implicit_connection"]
    >>= assertSuccess

  postOne2OneConversation bob alice team "chit-chat" >>= assertSuccess

  getMLSOne2OneConversation bob alice >>= assertSuccess

  -- Connecting should work the other way round as well.
  postOne2OneConversation alice bob team "chat-chit" >>= assertSuccess

  getMLSOne2OneConversation alice bob >>= assertSuccess

testImplicitConnectionNotConfigured :: (HasCallStack) => App ()
testImplicitConnectionNotConfigured = do
  (owner, team, [alice]) <- createTeam OwnDomain 2

  -- At the time of writing, it wasn't clear if this should be a bot instead.
  bob <- randomUser OwnDomain def
  addTeamCollaborator
    owner
    team
    bob
    []
    >>= assertSuccess

  postOne2OneConversation bob alice team "chit-chat" >>= assertLabel 403 "operation-denied"

  -- Team members can create 1:1s with all collaborators, regardless of the
  -- collaborators' permissions.
  postOne2OneConversation alice bob team "chat-chit" >>= assertSuccess

  getMLSOne2OneConversation alice bob >>= assertSuccess

testImplicitConnectionNoCollaborator :: (HasCallStack) => App ()
testImplicitConnectionNoCollaborator = do
  (_owner0, team0, [alice]) <- createTeam OwnDomain 2
  (owner1, team1, _users1) <- createTeam OwnDomain 2

  -- At the time of writing, it wasn't clear if this should be a bot instead.
  bob <- randomUser OwnDomain def
  addTeamCollaborator
    owner1
    team1
    bob
    ["implicit_connection"]
    >>= assertSuccess

  -- Alice and Bob aren't connected at all.
  postOne2OneConversation bob alice team0 "chit-chat" >>= assertLabel 403 "no-team-member"

testRemoveCollaboratorInTeamsO2O :: (HasCallStack) => App ()
testRemoveCollaboratorInTeamsO2O = do
  (owner0, team0, [alice]) <- createTeam OwnDomain 2
  (owner1, team1, [bob]) <- createTeam OwnDomain 2

  -- At the time of writing, it wasn't clear if this should be a bot instead.
  charlie <- randomUser OwnDomain def
  addTeamCollaborator owner0 team0 charlie ["implicit_connection"] >>= assertSuccess
  addTeamCollaborator owner1 team1 charlie ["implicit_connection"] >>= assertSuccess

  convId <-
    postOne2OneConversation charlie alice team0 "chit-chat" `bindResponse` \resp -> do
      resp.status `shouldMatchInt` 201
      resp.json %. "qualified_id"
  postOne2OneConversation charlie bob team1 "chit-chat" >>= assertSuccess
  Internal.getConversation convId >>= assertSuccess

  removeTeamCollaborator owner0 team0 charlie >>= assertSuccess

  getMLSOne2OneConversation charlie alice >>= assertLabel 403 "not-connected"
  postOne2OneConversation charlie alice team0 "chit-chat" >>= assertLabel 403 "no-team-member"
  Internal.getConversation convId >>= assertLabel 404 "no-conversation"
  getMLSOne2OneConversation charlie bob >>= assertSuccess

testRemoveCollaboratorInO2OConnected :: (HasCallStack) => App ()
testRemoveCollaboratorInO2OConnected = do
  (owner0, team0, [alice]) <- createTeam OwnDomain 2

  -- At the time of writing, it wasn't clear if this should be a bot instead.
  bob <- randomUser OwnDomain def
  connectTwoUsers alice bob

  addTeamCollaborator owner0 team0 bob ["implicit_connection"] >>= assertSuccess

  postOne2OneConversation bob alice team0 "chit-chat" >>= assertSuccess

  removeTeamCollaborator owner0 team0 bob >>= assertSuccess

  getMLSOne2OneConversation bob alice >>= assertSuccess

testRemoveCollaboratorInO2O :: (HasCallStack) => App ()
testRemoveCollaboratorInO2O = do
  (owner0, team0, [alice]) <- createTeam OwnDomain 2

  -- At the time of writing, it wasn't clear if this should be a bot instead.
  bob <- randomUser OwnDomain def
  addTeamCollaborator owner0 team0 bob ["implicit_connection"] >>= assertSuccess

  teamConvId <-
    postOne2OneConversation bob alice team0 "chit-chat" `bindResponse` \resp -> do
      resp.status `shouldMatchInt` 201
      resp.json %. "qualified_id"
  Internal.getConversation teamConvId >>= assertSuccess

  connectTwoUsers alice bob
  personalConvId <- postConversation alice defProteus {qualifiedUsers = [bob]} >>= getJSON 201
  Internal.getConversation personalConvId >>= assertSuccess

  removeTeamCollaborator owner0 team0 bob >>= assertSuccess

  postOne2OneConversation bob alice team0 "chit-chat" >>= assertLabel 403 "no-team-member"
  Internal.getConversation teamConvId >>= assertLabel 404 "no-conversation"

  getMLSOne2OneConversation bob alice >>= assertSuccess
  Internal.getConversation personalConvId >>= assertSuccess

testRemoveCollaboratorInTeamConversation :: (HasCallStack) => App ()
testRemoveCollaboratorInTeamConversation = do
  (owner, team, [alice, bob]) <- createTeam OwnDomain 3

  conv <-
    postConversation
      owner
      defProteus {team = Just team, qualifiedUsers = [alice, bob]}
      >>= getJSON 201

  withWebSockets [owner, alice, bob] $ \[wsOwner, wsAlice, wsBob] -> do
    removeTeamCollaborator owner team bob >>= assertSuccess

    bobId <- bob %. "qualified_id"
    bobUnqualifiedId <- bobId %. "id"
    let checkLeaveEvent :: (MakesValue a, HasCallStack) => a -> App ()
        checkLeaveEvent evt = do
          evt %. "payload.0.data.user" `shouldMatch` bobUnqualifiedId
          evt %. "payload.0.team" `shouldMatch` team
        checkRemoveEvent :: (MakesValue a, HasCallStack) => a -> App ()
        checkRemoveEvent evt = do
          evt %. "payload.0.data.user" `shouldMatch` bobUnqualifiedId
          evt %. "payload.0.team" `shouldMatch` team
        checkConvLeaveEvent :: (MakesValue a, HasCallStack) => a -> App ()
        checkConvLeaveEvent evt = do
          evt %. "payload.0.data.qualified_user_ids" `shouldMatch` [bobId]
          evt %. "payload.0.team" `shouldMatch` team

    awaitMatch isTeamMemberLeaveNotif wsOwner >>= checkLeaveEvent
    awaitMatch isTeamMemberLeaveNotif wsAlice >>= checkRemoveEvent
    awaitMatch isTeamMemberLeaveNotif wsBob >>= checkLeaveEvent
    awaitMatch isTeamCollaboratorRemovedNotif wsOwner >>= checkRemoveEvent
    awaitMatch isConvLeaveNotif wsAlice >>= checkConvLeaveEvent

  getConversation alice conv `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 200
    otherMember <- assertOne =<< asList (resp.json %. "members.others")
    otherMember %. "qualified_id" `shouldNotMatch` (bob %. "qualified_id")

  getConversation bob conv `bindResponse` \resp -> do
    -- should be 404
    resp.status `shouldMatchInt` 403
    resp.json %. "label" `shouldMatch` "access-denied"

  Internal.getConversation conv `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 200
    otherMembers <- asList (resp.json %. "members.others")
    traverse (%. "qualified_id") otherMembers `shouldMatchSet` traverse (%. "qualified_id") [owner, alice]

testUpdateCollaborator :: (HasCallStack) => App ()
testUpdateCollaborator = do
  (owner, team, [alice]) <- createTeam OwnDomain 2

  -- At the time of writing, it wasn't clear if this should be a bot instead.
  bob <- randomUser OwnDomain def
  addTeamCollaborator
    owner
    team
    bob
    ["implicit_connection"]
    >>= assertSuccess
  postOne2OneConversation bob alice team "chit-chat" >>= assertSuccess

  updateTeamCollaborator
    owner
    team
    bob
    ["create_team_conversation", "implicit_connection"]
    >>= assertSuccess
  postOne2OneConversation bob alice team "chit-chat" >>= assertSuccess

  updateTeamCollaborator
    owner
    team
    bob
    []
    >>= assertSuccess
  postOne2OneConversation bob alice team "chit-chat" >>= assertLabel 403 "operation-denied"
