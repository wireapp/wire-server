module Test.TeamCollaborators where

import API.Brig
import SetupHelpers
import Testlib.Prelude

testCreateTeamCollaborator :: (HasCallStack) => App ()
testCreateTeamCollaborator = do
  (owner, team, []) <- createTeam OwnDomain 0

  -- TODO: Just creating any user might be wrong. Should this be a bot?
  userId <- randomUser OwnDomain def >>= asString . (%. "id")
  addTeamCollaborator owner team userId [] >>= assertSuccess
  bindResponse (getAllTeamCollaborators owner team) $ \resp -> do
    -- TODO: Assert more here
    resp.status `shouldMatchInt` 200
    printJSON resp.jsonBody
