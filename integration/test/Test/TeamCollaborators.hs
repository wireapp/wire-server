module Test.TeamCollaborators where

import API.Brig
import SetupHelpers
import Testlib.Prelude

testCreateTeamCollaborator :: (HasCallStack) => App ()
testCreateTeamCollaborator = do
  (owner, team, []) <- createTeam OwnDomain 0

  -- TODO: Just creating any user might be wrong. Should this be a bot?
  userId <- randomUser OwnDomain def >>= asString . (%. "id")
  addTeamCollaborator
    owner
    team
    userId
    [ "create_team_conversation",
      "implicit_connection"
    ]
    >>= assertSuccess

  bindResponse (getAllTeamCollaborators owner team) $ \resp -> do
    -- TODO: Assert more here
    resp.status `shouldMatchInt` 200
    res <- (resp.jsonBody & asList) <&> assertOne
    res %. "user" `shouldMatch` userId
    res %. "team" `shouldMatch` team
    res %. "permissions" `shouldMatch` ["create_team_conversation", "implicit_connection"]
