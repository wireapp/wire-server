module SetupHelpers where

import qualified API.Brig as Public
import qualified API.BrigInternal as Internal
import Data.Aeson
import Data.Default
import Data.Function
import GHC.Stack
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

connectUsersB2B ::
  ( HasCallStack,
    MakesValue alice,
    MakesValue bob
  ) =>
  alice ->
  bob ->
  App ()
connectUsersB2B alice bob = do
  bindResponse (Public.postConnection alice bob) (\resp -> resp.status `shouldMatchInt` 201)
  bindResponse (withTwo (Public.putConnection bob alice "accepted")) (\resp -> resp.status `shouldMatchInt` 200)
