module SetupHelpers where

import qualified API.Brig as Public
import qualified API.BrigInternal as Internal
import API.Galley
import Data.Aeson
import Data.Default
import Data.Function
import GHC.Stack
import Testlib.Prelude

randomUser :: (HasCallStack, MakesValue domain) => domain -> Internal.CreateUser -> App Value
randomUser domain cu = bindResponse (Internal.createUser domain cu) $ \resp -> do
  resp.status `shouldMatchInt` 201
  resp.json

-- | returns (user, team id)
createTeam :: (HasCallStack, MakesValue domain) => domain -> App (Value, String)
createTeam domain = do
  res <- Internal.createUser domain def {Internal.team = True}
  user <- res.json
  tid <- user %. "team" & asString
  -- TODO
  -- SQS.assertTeamActivate "create team" tid
  -- refreshIndex
  pure (user, tid)

connectUsers ::
  ( HasCallStack,
    MakesValue alice,
    MakesValue bob
  ) =>
  alice ->
  bob ->
  App ()
connectUsers alice bob = do
  bindResponse (Public.postConnection alice bob) (\resp -> resp.status `shouldMatchInt` 201)
  bindResponse (Public.putConnection bob alice "accepted") (\resp -> resp.status `shouldMatchInt` 200)

createAndConnectUsers :: (HasCallStack, MakesValue domain) => [domain] -> App [Value]
createAndConnectUsers domains = do
  users <- for domains (flip randomUser def)
  let userPairs = do
        t <- tails users
        (a, others) <- maybeToList (uncons t)
        b <- others
        pure (a, b)
  for_ userPairs (uncurry connectUsers)
  pure users

getAllConvs :: (HasCallStack, MakesValue u) => u -> App [Value]
getAllConvs u = do
  page <- bindResponse (listConversationIds u def) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json
  ids <- page %. "qualified_conversations" & asList
  result <- bindResponse (listConversations u ids) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json
  result %. "found" & asList

resetFedConns :: (HasCallStack, MakesValue owndom) => owndom -> App ()
resetFedConns owndom = do
  bindResponse (Internal.readFedConns owndom) $ \resp -> do
    rdoms :: [String] <- do
      rawlist <- resp.json %. "remotes" & asList
      (asString . (%. "domain")) `mapM` rawlist
    Internal.deleteFedConn' owndom `mapM_` rdoms
