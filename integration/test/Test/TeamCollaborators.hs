module Test.TeamCollaborators where

import API.Brig
import API.Galley
import Data.Tuple.Extra
import Notifications (isTeamCollaboratorAddedNotif)
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

testRemoveMemberInO2O :: (HasCallStack) => App ()
testRemoveMemberInO2O = do
  (owner0, team0, [alice]) <- createTeam OwnDomain 2
  (owner1, team1, [bob]) <- createTeam OwnDomain 2

  -- At the time of writing, it wasn't clear if this should be a bot instead.
  charlie <- randomUser OwnDomain def
  addTeamCollaborator owner0 team0 charlie ["implicit_connection"] >>= assertSuccess
  addTeamCollaborator owner1 team1 charlie ["implicit_connection"] >>= assertSuccess

  postOne2OneConversation charlie alice team0 "chit-chat" >>= assertSuccess
  postOne2OneConversation charlie bob team1 "chit-chat" >>= assertSuccess

  removeTeamCollaborator owner0 team0 charlie >>= assertSuccess

  getMLSOne2OneConversation charlie alice >>= assertLabel 403 "not-connected"
  postOne2OneConversation charlie alice team0 "chit-chat" >>= assertLabel 403 "no-team-member"
  getMLSOne2OneConversation charlie bob >>= assertSuccess

testRemoveMemberInTeamConversation :: (HasCallStack) => App ()
testRemoveMemberInTeamConversation = do
  (owner, team, [alice, bob]) <- createTeam OwnDomain 3

  aliceId <- alice %. "qualified_id"
  bobId <- bob %. "qualified_id"
  conv <-
    postConversation
      owner
      defProteus {team = Just team, skipCreator = Just True, qualifiedUsers = [aliceId, bobId]}
      >>= getJSON 201

  removeTeamCollaborator owner team bob >>= assertSuccess

  getConversation alice conv `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 200

  getConversation bob conv `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 403
