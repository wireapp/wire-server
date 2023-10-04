{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module SetupHelpers where

import API.Brig qualified as Brig
import API.BrigInternal qualified as Internal
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

randomUser :: (HasCallStack, MakesValue domain) => domain -> Internal.CreateUser -> App Value
randomUser domain cu = bindResponse (Internal.createUser domain cu) $ \resp -> do
  resp.status `shouldMatchInt` 201
  resp.json

deleteUser :: (HasCallStack, MakesValue user) => user -> App ()
deleteUser user = bindResponse (Brig.deleteUser user) $ \resp -> do
  resp.status `shouldMatchInt` 200

-- | returns (user, team id)
createTeam :: (HasCallStack, MakesValue domain) => domain -> Int -> App (Value, String, [Value])
createTeam domain memberCount = do
  res <- Internal.createUser domain def {Internal.team = True}
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
  bindResponse (Brig.postConnection alice bob) (\resp -> resp.status `shouldMatchInt` 201)
  bindResponse (Brig.putConnection bob alice "accepted") (\resp -> resp.status `shouldMatchInt` 200)

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
    $ \[domainA, domainB, domainC] ->
      k (domainA, domainB, domainC)
