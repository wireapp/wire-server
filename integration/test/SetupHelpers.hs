module SetupHelpers where

import qualified API.BrigInternal as Internal
import Data.Aeson
import Data.Default
import Imports
import Testlib.App

randomUser :: Internal.CreateUser -> App Value
randomUser cu = bindResponse (Internal.createUser cu) $ \resp -> do
  resp.status `shouldMatchInt` 201
  resp.json

-- | returns (user, team id)
createTeam :: App (Value, String)
createTeam = do
  res <- Internal.createUser def {Internal.team = True}
  user <- res.json
  tid <- user %. "team" & asString
  -- TODO
  -- SQS.assertTeamActivate "create team" tid
  -- refreshIndex
  pure (user, tid)
