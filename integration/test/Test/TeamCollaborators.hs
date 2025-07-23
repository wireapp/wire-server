module Test.TeamCollaborators where

import API.Brig
import API.Galley
import Notifications (isTeamCollaboratorAddedNotif)
import SetupHelpers
import Testlib.Prelude

testCreateTeamCollaborator :: (HasCallStack) => App ()
testCreateTeamCollaborator = do
  (owner, team, [alice]) <- createTeam OwnDomain 2

  -- At the time of writing, it wasn't clear if this should be a bot instead.
  userId <- randomUser OwnDomain def >>= asString . (%. "id")
  withWebSockets [owner, alice] $ \[wsOwner, wsAlice] -> do
    addTeamCollaborator
      owner
      team
      userId
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
    awaitMatch isTeamCollaboratorAddedNotif wsAlice >>= checkEvent

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
  userId <- randomUser OwnDomain def >>= asString . (%. "id")
  addTeamCollaborator
    owner
    team2
    userId
    [ "create_team_conversation",
      "implicit_connection"
    ]
    >>= assertStatus 403

  getAllTeamCollaborators owner team2 >>= assertStatus 403

testCreateTeamCollaboratorPostTwice :: (HasCallStack) => App ()
testCreateTeamCollaboratorPostTwice = do
  (owner, team, _members) <- createTeam OwnDomain 2

  -- At the time of writing, it wasn't clear if this should be a bot instead.
  userId <- randomUser OwnDomain def >>= asString . (%. "id")
  let add =
        addTeamCollaborator
          owner
          team
          userId
          [ "create_team_conversation",
            "implicit_connection"
          ]
  bindResponse add assertSuccess
  bindResponse add $ assertStatus 409

testImplicitConnection :: (HasCallStack) => App ()
testImplicitConnection = do
  (owner, team, [alice]) <- createTeam OwnDomain 2

  -- At the time of writing, it wasn't clear if this should be a bot instead.
  bob <- randomUser OwnDomain def
  userId <- bob %. "id" >>= asString
  addTeamCollaborator
    owner
    team
    userId
    ["implicit_connection"]
    >>= assertSuccess

  postOne2OneConversation bob alice team "chit-chat" >>= assertSuccess
