{-# OPTIONS_GHC -Wno-ambiguous-fields #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module SetupHelpers where

import API.Brig
import API.BrigInternal
import API.Common
import API.Galley
import Control.Monad.Reader
import Data.Aeson hiding ((.=))
import Data.Aeson.Types qualified as Aeson
import Data.Default
import Data.Function
import Data.UUID.V1 (nextUUID)
import Data.UUID.V4 (nextRandom)
import GHC.Stack
import Testlib.Prelude

randomUser :: (HasCallStack, MakesValue domain) => domain -> CreateUser -> App Value
randomUser domain cu = bindResponse (createUser domain cu) $ \resp -> do
  resp.status `shouldMatchInt` 201
  resp.json

deleteUser :: (HasCallStack, MakesValue user) => user -> App ()
deleteUser user = bindResponse (API.Brig.deleteUser user) $ \resp -> do
  resp.status `shouldMatchInt` 200

-- | returns (user, team id)
createTeam :: (HasCallStack, MakesValue domain) => domain -> Int -> App (Value, String, [Value])
createTeam domain memberCount = do
  res <- createUser domain def {team = True}
  owner <- res.json
  tid <- owner %. "team" & asString
  members <- for [2 .. memberCount] $ \_ -> createTeamMember owner tid
  pure (owner, tid, members)

createTeamMember ::
  (HasCallStack, MakesValue inviter) =>
  inviter ->
  String ->
  App Value
createTeamMember inviter tid = do
  newUserEmail <- randomEmail
  let invitationJSON = ["role" .= "member", "email" .= newUserEmail]
  invitationReq <-
    baseRequest inviter Brig Versioned $
      joinHttpPath ["teams", tid, "invitations"]
  invitation <- getJSON 201 =<< submit "POST" (addJSONObject invitationJSON invitationReq)
  invitationId <- objId invitation
  invitationCodeReq <-
    rawBaseRequest inviter Brig Unversioned "/i/teams/invitation-code"
      <&> addQueryParams [("team", tid), ("invitation_id", invitationId)]
  invitationCode <- bindResponse (submit "GET" invitationCodeReq) $ \res -> do
    res.status `shouldMatchInt` 200
    res.json %. "code" & asString
  let registerJSON =
        [ "name" .= newUserEmail,
          "email" .= newUserEmail,
          "password" .= defPassword,
          "team_code" .= invitationCode
        ]
  registerReq <-
    rawBaseRequest inviter Brig Versioned "/register"
      <&> addJSONObject registerJSON
  getJSON 201 =<< submit "POST" registerReq

connectTwoUsers ::
  ( HasCallStack,
    MakesValue alice,
    MakesValue bob
  ) =>
  alice ->
  bob ->
  App ()
connectTwoUsers alice bob = do
  bindResponse (postConnection alice bob) (\resp -> resp.status `shouldMatchInt` 201)
  bindResponse (putConnection bob alice "accepted") (\resp -> resp.status `shouldMatchInt` 200)

connectUsers :: HasCallStack => [Value] -> App ()
connectUsers users = traverse_ (uncurry connectTwoUsers) $ do
  t <- tails users
  (a, others) <- maybeToList (uncons t)
  b <- others
  pure (a, b)

createAndConnectUsers :: (HasCallStack, MakesValue domain) => [domain] -> App [Value]
createAndConnectUsers domains = do
  users <- for domains (flip randomUser def)
  connectUsers users
  pure users

createUsers :: (HasCallStack, MakesValue domain) => [domain] -> App [Value]
createUsers domains = for domains (flip randomUser def)

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
  (alice, tid, _) <- createTeam OwnDomain 1
  bob <- randomUser secondDomain def
  connectUsers [alice, bob]

  conv <-
    postConversation alice defProteus {qualifiedUsers = [bob], team = Just tid}
      >>= getJSON 201

  bindResponse (putConversationProtocol bob conv "mixed") $ \resp -> do
    resp.status `shouldMatchInt` 200

  modifyMLSState $ \mls -> mls {protocol = MLSProtocolMixed}

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

-- | Create a user on the given domain, such that the 1-1 conversation with
-- 'other' resides on 'convDomain'. This connects the two users as a side-effect.
createMLSOne2OnePartner :: MakesValue user => Domain -> user -> Domain -> App Value
createMLSOne2OnePartner domain other convDomain = loop
  where
    loop = do
      u <- randomUser domain def
      connectTwoUsers u other
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

withFederatingBackendsAllowDynamic :: HasCallStack => ((String, String, String) -> App a) -> App a
withFederatingBackendsAllowDynamic k = do
  let setFederationConfig =
        setField "optSettings.setFederationStrategy" "allowDynamic"
          >=> setField "optSettings.setFederationDomainConfigsUpdateFreq" (Aeson.Number 1)
  startDynamicBackends
    [ def {brigCfg = setFederationConfig},
      def {brigCfg = setFederationConfig},
      def {brigCfg = setFederationConfig}
    ]
    $ \[domainA, domainB, domainC] -> k (domainA, domainB, domainC)
