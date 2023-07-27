module SetupHelpers where

import API.Brig qualified as Public
import API.BrigInternal qualified as Internal
import API.Galley
import Control.Concurrent (threadDelay)
import Data.Aeson hiding ((.=))
import Data.Default
import Data.Function
import Data.UUID.V4 (nextRandom)
import GHC.Stack
import Testlib.Prelude
import Testlib.ResourcePool (remoteDomains)

-- | `n` should be 2 x `setFederationDomainConfigsUpdateFreq` in the config
connectAllDomainsAndWaitToSync :: HasCallStack => Int -> [String] -> App ()
connectAllDomainsAndWaitToSync n domains = do
  sequence_ [Internal.createFedConn x (Internal.FedConn y "full_search") | x <- domains, y <- domains, x /= y]
  liftIO $ threadDelay (n * 1000 * 1000) -- wait for federation status to be updated

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

randomId :: HasCallStack => App String
randomId = do
  liftIO (show <$> nextRandom)

randomUserId :: (HasCallStack, MakesValue domain) => domain -> App Value
randomUserId domain = do
  d <- make domain
  uid <- randomId
  pure $ object ["id" .= uid, "domain" .= d]

addFullSearchFor :: [String] -> Value -> App Value
addFullSearchFor domains val =
  modifyField
    "optSettings.setFederationDomainConfigs"
    ( \configs -> do
        cfg <- assertJust "" configs
        xs <- cfg & asList
        pure (xs <> [object ["domain" .= domain, "search_policy" .= "full_search"] | domain <- domains])
    )
    val

fullSearchWithAll :: ServiceOverrides
fullSearchWithAll =
  def
    { dbBrig = \val -> do
        ownDomain <- asString =<< val %. "optSettings.setFederationDomain"
        addFullSearchFor (remoteDomains ownDomain) val
    }
