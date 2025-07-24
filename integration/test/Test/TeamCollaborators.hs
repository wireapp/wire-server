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

-- Question about channels: Do collaborators get to create them when all team members are allowed to create them?
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
