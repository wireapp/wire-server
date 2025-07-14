module Test.TeamCollaborators where

import API.Brig
import Notifications (isTeamCollaboratorAddedNotif)
import SetupHelpers
import Testlib.Prelude

testCreateTeamCollaborator :: (HasCallStack) => App ()
testCreateTeamCollaborator = do
  (owner, team, [alice]) <- createTeam OwnDomain 2

  -- TODO: Just creating any user might be wrong. Should this be a bot?
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

    void $ awaitMatch isTeamCollaboratorAddedNotif wsOwner
    void $ awaitMatch isTeamCollaboratorAddedNotif wsAlice

  bindResponse (getAllTeamCollaborators owner team) $ \resp -> do
    resp.status `shouldMatchInt` 200
    res <- (resp.jsonBody & asList) <&> assertOne
    res %. "user" `shouldMatch` userId
    res %. "team" `shouldMatch` team
    res %. "permissions" `shouldMatch` ["create_team_conversation", "implicit_connection"]
