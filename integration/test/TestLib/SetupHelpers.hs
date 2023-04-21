module TestLib.SetupHelpers where

import qualified API
import Data.Aeson
import Data.Default
import Imports
import TestLib.App

randomUser :: API.CreateUser -> App Value
randomUser cu = bindResponse (API.createUser cu) $ \resp -> do
  resp.status `shouldMatch` 201
  resp.json

-- | returns (user, team id)
createTeam :: App (Value, String)
createTeam = do
  res <- API.createUser def {API.team = True}
  user <- res.json
  tid <- user %. "team" & asString
  -- TODO
  -- SQS.assertTeamActivate "create team" tid
  -- refreshIndex
  pure (user, tid)
