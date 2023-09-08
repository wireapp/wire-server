{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module SetupHelpers where

import API.Brig
import API.Brig qualified as Brig
import API.BrigInternal
import API.BrigInternal qualified as Internal
import API.Galley
import Control.Concurrent (threadDelay)
import Control.Monad.Reader
import Data.Aeson hiding ((.=))
import Data.Aeson.Types qualified as Aeson
import Data.Default
import Data.Function
import Data.List qualified as List
import Data.UUID.V1 (nextUUID)
import Data.UUID.V4 (nextRandom)
import GHC.Stack
import Testlib.Prelude

randomUser :: (HasCallStack, MakesValue domain) => domain -> CreateUser -> App Value
randomUser domain cu = bindResponse (createUser domain cu) $ \resp -> do
  resp.status `shouldMatchInt` 201
  resp.json

-- | `n` should be 2 x `setFederationDomainConfigsUpdateFreq` in the config
connectAllDomainsAndWaitToSync :: HasCallStack => Int -> [String] -> App ()
connectAllDomainsAndWaitToSync n domains = do
  sequence_ [createFedConn x (FedConn y "full_search") | x <- domains, y <- domains, x /= y]
  liftIO $ threadDelay (n * 1000 * 1000) -- wait for federation status to be updated

deleteUser :: (HasCallStack, MakesValue user) => user -> App ()
deleteUser user = bindResponse (Brig.deleteUser user) $ \resp -> do
  resp.status `shouldMatchInt` 200

-- | returns (user, team id)
createTeam :: (HasCallStack, MakesValue domain) => domain -> App (Value, String)
createTeam domain = do
  res <- createUser domain def {team = True}
  user <- res.json
  tid <- user %. "team" & asString
  -- TODO
  -- SQS.assertTeamActivate "create team" tid
  -- refreshIndex
  pure (user, tid)

connectUsers2 ::
  ( HasCallStack,
    MakesValue alice,
    MakesValue bob
  ) =>
  alice ->
  bob ->
  App ()
connectUsers2 alice bob = do
  bindResponse (postConnection alice bob) (\resp -> resp.status `shouldMatchInt` 201)
  bindResponse (putConnection bob alice "accepted") (\resp -> resp.status `shouldMatchInt` 200)

connectUsers :: HasCallStack => [Value] -> App ()
connectUsers users = traverse_ (uncurry connectUsers2) $ do
  t <- tails users
  (a, others) <- maybeToList (uncons t)
  b <- others
  pure (a, b)

createAndConnectUsers :: (HasCallStack, MakesValue domain) => [domain] -> App [Value]
createAndConnectUsers domains = do
  users <- for domains (flip randomUser def)
  connectUsers users
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

-- | Setup a team user, another user, connect the two, create a proteus
-- conversation, upgrade to mixed. Return the two users and the conversation.
simpleMixedConversationSetup ::
  (HasCallStack, MakesValue domain) =>
  domain ->
  App (Value, Value, Value)
simpleMixedConversationSetup secondDomain = do
  (alice, tid) <- createTeam OwnDomain
  bob <- randomUser secondDomain def
  connectUsers [alice, bob]

  conv <-
    postConversation alice defProteus {qualifiedUsers = [bob], team = Just tid}
      >>= getJSON 201

  bindResponse (putConversationProtocol bob conv "mixed") $ \resp -> do
    resp.status `shouldMatchInt` 200

  conv' <- getConversation alice conv >>= getJSON 200

  pure (alice, bob, conv')

supportMLS :: (HasCallStack, MakesValue u) => u -> App ()
supportMLS u = do
  prots <- bindResponse (getUserSupportedProtocols u u) $ \resp -> do
    resp.status `shouldMatchInt` 200
    prots <- resp.json & asList
    traverse asString prots
  let prots' = "mls" : prots
  bindResponse (putUserSupportedProtocols u prots') $ \resp ->
    resp.status `shouldMatchInt` 200

addUserToTeam :: (HasCallStack, MakesValue u) => u -> App Value
addUserToTeam u = do
  inv <- postInvitation u def >>= getJSON 201
  email <- inv %. "email" & asString
  resp <- getInvitationCode u inv >>= getJSON 200
  code <- resp %. "code" & asString
  addUser u def {email = Just email, teamCode = Just code} >>= getJSON 201

resetFedConns :: (HasCallStack, MakesValue owndom) => owndom -> App ()
resetFedConns owndom = do
  bindResponse (readFedConns owndom) $ \resp -> do
    rdoms :: [String] <- do
      rawlist <- resp.json %. "remotes" & asList
      (asString . (%. "domain")) `mapM` rawlist
    deleteFedConn' owndom `mapM_` rdoms

-- | Create a user on the given domain, such that the 1-1 conversation with
-- 'other' resides on 'convDomain'. This connects the two users as a side-effect.
createMLSOne2OnePartner :: MakesValue user => Domain -> user -> Domain -> App Value
createMLSOne2OnePartner domain other convDomain = loop
  where
    loop = do
      u <- randomUser domain def
      connectUsers2 u other
      conv <- getMLSOne2OneConversation other u >>= getJSON 200

      desiredConvDomain <- make convDomain & asString
      actualConvDomain <- conv %. "qualified_id.domain" & asString

      if desiredConvDomain == actualConvDomain
        then pure u
        else loop

randomId :: HasCallStack => App String
randomId = liftIO (show <$> nextRandom)

randomUUIDv1 :: HasCallStack => App String
randomUUIDv1 = liftIO (show . fromJust <$> nextUUID)

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
    { brigCfg = \val -> do
        ownDomain <- asString =<< val %. "optSettings.setFederationDomain"
        env <- ask
        let remoteDomains = List.delete ownDomain $ [env.domain1, env.domain2] <> env.dynamicDomains
        addFullSearchFor remoteDomains val
    }

withFederatingBackendsAllowDynamic :: HasCallStack => Int -> ((String, String, String) -> App a) -> App a
withFederatingBackendsAllowDynamic n k = do
  let setFederationConfig =
        setField "optSettings.setFederationStrategy" "allowDynamic"
          >=> removeField "optSettings.setFederationDomainConfigs"
          >=> setField "optSettings.setFederationDomainConfigsUpdateFreq" (Aeson.Number 1)
  startDynamicBackends
    [ def {brigCfg = setFederationConfig},
      def {brigCfg = setFederationConfig},
      def {brigCfg = setFederationConfig}
    ]
    $ \dynDomains -> do
      domains@[domainA, domainB, domainC] <- pure dynDomains
      sequence_ [Internal.createFedConn x (Internal.FedConn y "full_search") | x <- domains, y <- domains, x /= y]
      liftIO $ threadDelay (n * 1000 * 1000) -- wait for federation status to be updated
      k (domainA, domainB, domainC)
