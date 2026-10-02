{-# OPTIONS -Wno-incomplete-patterns -Wno-ambiguous-fields #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

module Test.Apps where

import API.Brig as Brig
import qualified API.BrigInternal as BrigI
import API.Common
import API.Galley
import API.Gundeck
import Data.Aeson.QQ.Simple
import qualified Data.Map as Map
import qualified Data.Set as Set
import MLS.Util
import Notifications
import SetupHelpers
import System.Random (randomRIO)
import Testlib.Prelude

testCreateGetApp :: (HasCallStack) => Domain -> App ()
testCreateGetApp sameOrOtherDomain = do
  domainA <- make OwnDomain
  domainB <- make sameOrOtherDomain

  (owner, tid, [regularMember]) <- createTeam domainA 2
  let new :: NewApp =
        def
          { name = "chappie",
            description = "some description of this app",
            category = "ai"
          }

  -- Regular team member can't create apps
  bindResponse (createApp regularMember tid new) $ \resp -> do
    resp.status `shouldMatchInt` 403
    resp.json %. "label" `shouldMatch` "app-no-permission"

  -- Get the last team notification ID before creating the app
  lastTeamNotif <- bindResponse (getTeamNotifications regularMember Nothing) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "notifications.-1.id" & asString

  -- Owner can create an app
  (appId, cookie) <- withWebSockets [owner, regularMember] \[wsOwner, wsRegularMember] -> do
    bindResponse (createApp owner tid new) $ \resp -> do
      resp.status `shouldMatchInt` 200
      appId <- resp.json %. "user.id" & asString
      cookie <- resp.json %. "cookie" & asString
      _ <- do
        let predicate payload = do
              typ <- payload %. "payload.0.type" & asString
              pure $ typ == "team.member-join"
        void $ awaitMatch predicate wsOwner
        void $ assertNoEvent 5 wsRegularMember
      pure (appId, cookie)

  -- Verify that the team.member-join event is in the team notifications queue
  bindResponse (getTeamNotifications regularMember (Just lastTeamNotif)) $ \resp -> do
    resp.status `shouldMatchInt` 200
    -- First notification is `lastTeamNotif`, we can ignore that one.
    resp.json %. "notifications.1.payload.0.type" `shouldMatch` "team.member-join"
    resp.json %. "notifications.1.payload.0.team" `shouldMatch` tid
    resp.json %. "notifications.1.payload.0.data.user" `shouldMatch` appId

  -- App user should have type "app"
  let appIdObject = object ["domain" .= domainA, "id" .= appId]
  bindResponse (getUser owner appIdObject) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "type" `shouldMatch` "app"

  -- getApp, getApps
  bindResponse (getApp owner tid appId) $ \resp -> do
    resp.status `shouldMatchInt` 200
  bindResponse (getApps owner tid) $ \resp -> do
    resp.status `shouldMatchInt` 200
    void $ resp.json & asList >>= assertOne
  bindResponse (createApp owner tid (new {name = "fmappie"})) $ \resp -> do
    resp.status `shouldMatchInt` 200
  bindResponse (getApps owner tid) $ \resp -> do
    resp.status `shouldMatchInt` 200
    apps <- resp.json & asList
    name1 <- apps %. "0.name"
    name2 <- apps %. "1.name"
    [name1, name2] `shouldMatchSet` ["chappie", "fmappie"]

  -- Creator should have type "regular"
  bindResponse (getUser owner owner) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "type" `shouldMatch` "regular"

  void $ bindResponse (renewToken domainA cookie) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "user" `shouldMatch` appId
    resp.json %. "token_type" `shouldMatch` "Bearer"
    resp.json %. "access_token" & asString

  -- Get app for the app created above succeeds
  void $ getApp regularMember tid appId `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 200
    (resp.json %. "name") `shouldMatch` "chappie"
    (resp.json %. "app.description") `shouldMatch` "some description of this app"
    (resp.json %. "app.category") `shouldMatch` "ai"

  -- A teamless user can't get the app
  outsideUser <- randomUser domainB (def {BrigI.team = False})
  bindResponse (getApp outsideUser tid appId) $ \resp -> do
    resp.status `shouldMatchInt` 404

  (owner2, tid2, [regularMember2]) <- createTeam domainB 2
  bindResponse (getApp owner2 tid appId) $ \resp -> resp.status `shouldMatchInt` 404
  bindResponse (getApp owner2 tid2 appId) $ \resp -> resp.status `shouldMatchInt` 404
  bindResponse (getApp regularMember2 tid appId) $ \resp -> resp.status `shouldMatchInt` 404

  -- Get app on remote apps gives 404 not found.
  void $ getApp owner2 tid appId `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 404
    resp.json %. "label" `shouldMatch` "not-found"

  -- Category can be any text; sanitization must happen by clients.
  void $ bindResponse (createApp owner tid new {category = "notinenum"}) $ \resp -> do
    resp.status `shouldMatchInt` 200
    deleteTeamMember tid owner (resp.json %. "user") >>= assertSuccess

testRefreshAppCookie :: (HasCallStack) => App ()
testRefreshAppCookie = do
  (alice, tid, [bob]) <- createTeam OwnDomain 2
  charlie <- randomUser OwnDomain def

  let new = def {name = "flexo"} :: NewApp
      goodPassword = Just (object ["password" .= defPassword])

  (appId, cookie) <- bindResponse (createApp alice tid new) $ \resp -> do
    resp.status `shouldMatchInt` 200
    appId <- resp.json %. "user.id" & asString
    cookie <- resp.json %. "cookie" & asString
    pure (appId, cookie)

  bindResponse (refreshAppCookie bob tid appId goodPassword) $ \resp -> do
    resp.status `shouldMatchInt` 403
    resp.json %. "label" `shouldMatch` "app-no-permission"

  bindResponse (refreshAppCookie charlie tid appId goodPassword) $ \resp -> do
    resp.status `shouldMatchInt` 403
    resp.json %. "label" `shouldMatch` "app-no-permission"

  forM_
    [ (Nothing, 415),
      (Just Null, 400),
      (Just (object []), 403),
      (Just (object ["password" .= "this is not a good password"]), 403)
    ]
    $ \(badPassword, stat) -> do
      -- the status codes and error labels differ here, but the
      -- important thing is that the request fails.
      refreshAppCookie alice tid appId badPassword >>= assertStatus stat

  cookie' <- bindResponse (refreshAppCookie alice tid appId goodPassword) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "cookie" & asString

  renewToken OwnDomain cookie `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 403

  renewToken OwnDomain cookie' `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "user" `shouldMatch` appId
    resp.json %. "token_type" `shouldMatch` "Bearer"
    void $ resp.json %. "access_token" & asString

testDeleteAppFromTeam :: (HasCallStack) => App ()
testDeleteAppFromTeam = do
  domain <- make OwnDomain
  (owner, tid, [regularMember]) <- createTeam domain 2
  let new = def {name = "chappie"} :: NewApp
  appId <- bindResponse (createApp owner tid new) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "user.id" & asString

  let appIdObject = object ["domain" .= domain, "id" .= appId]

  withWebSockets [owner, regularMember] \[wsOwner, wsRegularMember] -> do
    bindResponse (deleteTeamMember tid owner appIdObject) $ \resp -> do
      resp.status `shouldMatchInt` 202
      let predicate payload = do
            typ <- payload %. "payload.0.type" & asString
            pure $ typ == "team.member-leave"
      void $ awaitMatch predicate wsOwner
      void $ awaitMatch predicate wsRegularMember

  eventually $ do
    -- Check StoredApp is gone
    bindResponse (getApp owner tid appId) $ \resp -> do
      resp.status `shouldMatchInt` 404

    -- Check StoredUser is deleted (via public API)
    bindResponse (getUser owner appIdObject) $ \resp -> do
      resp.status `shouldMatchInt` 200
      resp.json %. "deleted" `shouldMatch` True

    -- Check StoredUser is gone (via internal API)
    bindResponse (BrigI.getUsersId domain [appId]) $ \resp -> do
      resp.status `shouldMatchInt` 200
      resp.json `shouldMatch` ([] :: [Value])

testPutApp :: (HasCallStack) => App ()
testPutApp = do
  domain <- make OwnDomain
  (owner, tid, [regularMember]) <- createTeam domain 2
  let new = def {name = "choppie"} :: NewApp
  (app, appId) <- bindResponse (createApp owner tid new) $ \resp -> do
    resp.status `shouldMatchInt` 200
    (,)
      <$> (resp.json %. "user")
      <*> (resp.json %. "user.id")

  let Object appMetadata =
        [aesonQQ|
        {
          "accent_id": 126,
          "assets": [
            {
              "key": "3-1-47de4580-ae51-4650-acbb-d10c028cb0ac",
              "size": "preview",
              "type": "image"
            }
          ],
          "name": "Appy McApp",
          "category": "security",
          "description": "This is the best app ever."
        }|]

  withWebSockets [owner, regularMember, app] \[wsOwner, wsRegularMember, wsApp] -> do
    bindResponse (putAppMetadata tid owner appId (Object appMetadata)) $ \resp -> do
      resp.status `shouldMatchInt` 200
    notifOwner <- awaitMatch isUserUpdatedNotif wsOwner
    notifMember <- awaitMatch isUserUpdatedNotif wsRegularMember
    notifApp <- awaitMatch isUserUpdatedNotif wsApp
    notifOwner %. "payload.0.user.id" `shouldMatch` appId
    length (nub [notifOwner, notifMember, notifApp]) `shouldMatchInt` 1

  bindResponse (getApp owner tid appId) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json
      `shouldMatchShapeLenient` SObject
        [ ("accent_id", SNumber),
          ("assets", SArray (SObject [("key", SString), ("size", SString), ("type", SString)])),
          ("name", SString),
          ("app", SObject [("category", SString), ("description", SString)])
        ]

  let badAppId = "5e002eca-114f-11f1-b5a3-7306b8837f91"
  bindResponse (putAppMetadata tid owner badAppId (Object appMetadata)) $ \resp -> do
    resp.status `shouldMatchInt` 404

-- | FUTUREWORK: 'Test.Apps.testFindApp',
-- 'Test.Apps.testRetrieveUsersIncludingApps',
-- 'Test.Search.checkUserSearch' have some overlap, or at least could
-- be re-ordered for clarity.
testRetrieveUsersIncludingApps :: (HasCallStack) => App ()
testRetrieveUsersIncludingApps = do
  let userShape =
        SObject
          [ ("accent_id", SNumber),
            ("assets", SArray SAny),
            ("id", SString),
            ("name", SString),
            ("qualified_id", SObject [("domain", SString), ("id", SString)]),
            ("searchable", SBool),
            ("supported_protocols", SArray SString),
            ("team", SString),
            ("type", SString)
          ]
      memberShape =
        SObject
          [ ("created_at", SString),
            ("created_by", SString),
            ("legalhold_status", SString),
            ("permissions", SObject [("copy", SNumber), ("self", SNumber)]),
            ("user", SString)
          ]
      appShape =
        SObject
          [ ("category", SString),
            ("description", SString)
          ]
      appWithIdShape =
        SObject
          [ ("id", SString),
            ("app", appShape)
          ]
      searchResultShape =
        SObject
          [ ("accent_id", SNumber),
            ("handle", SAny), -- sometimes "string", but always "null" for apps
            ("id", SString),
            ("name", SString),
            ("qualified_id", SObject [("domain", SString), ("id", SString)]),
            ("team", SString),
            ("type", SString)
          ]
      listResultShape =
        SObject
          [ ("accent_id", SNumber),
            ("assets", SArray SAny),
            ("app", SAny),
            ("id", SString),
            ("legalhold_status", SString),
            ("name", SString),
            ("picture", SArray SAny),
            ("qualified_id", SObject [("domain", SString), ("id", SString)]),
            ("searchable", SBool),
            ("supported_protocols", SArray SString),
            ("team", SString),
            ("type", SString)
          ]

  domain <- make OwnDomain
  (owner, tid, [regular]) <- createTeam domain 2

  -- [`POST /teams/:tid/apps`](https://staging-nginz-https.zinfra.io/v15/api/swagger-ui/#/default/create-app) (route id: "create-app")
  let new = def {name = "chippie"} :: NewApp
  appCreated <- bindResponse (createApp owner tid new) $ \resp -> do
    resp.status `shouldMatchInt` 200
    pure resp.json
  appCreated
    `shouldMatchShapeLenient` SObject
      [ ("cookie", SString),
        ("user", userShape)
      ]
  appId <- appCreated %. "user.id" & asString

  -- [`GET /teams/:tid/members`](https://staging-nginz-https.zinfra.io/v15/api/swagger-ui/#/default/get-team-members) (route id: "get-team-members")
  getTeamMembers owner tid `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "hasMore" `shouldMatch` False
    mems <- resp.json %. "members" >>= asList
    memIds <- (asString . (%. "user")) `mapM` mems
    memIds
      `shouldMatchSet` sequence
        [ pure appId,
          asString $ regular %. "qualified_id.id",
          asString $ owner %. "qualified_id.id"
        ]

  -- [`GET /teams/:tid/members/:uid`](https://staging-nginz-https.zinfra.io/v15/api/swagger-ui/#/default/get-team-member) (route id: "get-team-member")
  getTeamMember owner tid appId `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "user" `shouldMatch` appId
    resp.json `shouldMatchShapeLenient` memberShape

  -- [`GET /teams/:tid/apps`](https://staging-nginz-https.zinfra.io/v15/api/swagger-ui/#/default/get-apps) (route id: "get-apps")
  getApps owner tid `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 200
    apps <- resp.json & maybe (error "this shouldn't happen") pure
    apps `shouldMatchShapeLenient` SArray appWithIdShape

  -- [`GET /teams/:tid/apps/:uid`](https://staging-nginz-https.zinfra.io/v15/api/swagger-ui/#/default/get-app) (route id: "get-app")
  getApp owner tid appId `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json `shouldMatchShapeLenient` userShape

  -- [`POST /list-users`](https://staging-nginz-https.zinfra.io/v15/api/swagger-ui/#/default/list-users-by-ids-or-handles) (route id: "list-users-by-ids-or-handles")
  listUsers owner [appCreated %. "user"] `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "found.0" `shouldMatchShapeLenient` listResultShape

  -- [`GET /search/contacts`](https://staging-nginz-https.zinfra.io/v15/api/swagger-ui/#/default/search-contacts) (route id: "search-contacts")
  putSelf owner (def {name = Just "name-A1"}) >>= assertSuccess
  putSelf regular (def {name = Just "name-A2"}) >>= assertSuccess
  putSelf (appCreated %. "user") (def {name = Just "name-A3"}) >>= assertSuccess
  eventually
    $ searchContacts owner "name" domain
    `bindResponse` \resp -> do
      resp.status `shouldMatchInt` 200
      hits :: [Value] <- resp.json %. "documents" & asList
      length hits `shouldMatchInt` 2 -- owner doesn't find itself
      (`shouldMatchShapeLenient` searchResultShape) `mapM_` hits

testCrossTeamAppConversation :: (HasCallStack) => Domain -> App ()
testCrossTeamAppConversation sameOrOtherDomain = do
  domainA <- make OwnDomain
  domainB <- make sameOrOtherDomain
  (ownerA, tidA, [m1]) <- createTeam domainA 2
  (ownerB, tidB, [m2]) <- createTeam domainB 2

  -- Create app A1 (member of team A)
  let newAppA1 = def {name = "app-a1"} :: NewApp
  appA1 <- bindResponse (createApp ownerA tidA newAppA1) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "user"

  -- Create app A2 (member of team B)
  let newAppA2 = def {name = "app-a2"} :: NewApp
  appA2 <- bindResponse (createApp ownerB tidB newAppA2) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "user"

  -- Create MLS clients for M1 and A1 (both on domainA)
  [m1c, appA1c] <- traverse (createMLSClient def) [m1, appA1]
  traverse_ (uploadNewKeyPackage def) [m1c, appA1c]

  -- M1 tries to connect to app A2 from team B => should fail
  -- Apps cannot create connections accross teams
  bindResponse (postConnection m1 appA2) $ \resp -> do
    resp.status `shouldMatchInt` 400
    resp.json %. "label" `shouldMatch` "invalid-user"

  -- M1 creates an MLS team conversation
  convId <- createNewGroupWith def m1c defMLS {team = Just tidA}

  -- M1 adds A1 to the conversation
  void $ createAddCommit m1c convId [appA1] >>= sendAndConsumeCommitBundle

  -- M1 connects to M2 from team B (cross-team/cross-domain)
  postConnection m1 m2 >>= assertSuccess
  putConnection m2 m1 "accepted" >>= assertSuccess

  -- Create MLS client for M2 (on domainB) and add to conversation
  m2c <- createMLSClient def m2
  void $ uploadNewKeyPackage def m2c
  void $ createAddCommit m1c convId [m2] >>= sendAndConsumeCommitBundle

  -- Create app A3 (on domainA, team A)
  let newAppA3 = def {name = "app-a3"} :: NewApp
  appA3 <- bindResponse (createApp ownerA tidA newAppA3) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "user"

  -- Create MLS client for A3 and add to conversation
  appA3c <- createMLSClient def appA3
  void $ uploadNewKeyPackage def appA3c
  void $ createAddCommit m1c convId [appA3] >>= sendAndConsumeCommitBundle

  void $ createApplicationMessage convId m1c "hello from M1" >>= sendAndConsumeMessage
  void $ createApplicationMessage convId appA1c "hello from A1" >>= sendAndConsumeMessage
  void $ createApplicationMessage convId appA3c "hello from A3" >>= sendAndConsumeMessage
  void $ createApplicationMessage convId m2c "hello from M2" >>= sendAndConsumeMessage

-- | FUTUREWORK: 'Test.Apps.testFindApp',
-- 'Test.Apps.testRetrieveUsersIncludingApps',
-- 'Test.Search.checkUserSearch' have some overlap, or at least could
-- be re-ordered for clarity.
testFindApp :: (HasCallStack) => Domain -> App ()
testFindApp sameOrOtherDomain = do
  domainA <- make OwnDomain
  domainB <- make sameOrOtherDomain

  (ownerA1, tidA1, [regularMemberA1]) <- createTeam domainA 2
  let newAppA1 :: NewApp = def {name = "app A1", description = ""}
  (appA1Id) <- bindResponse (createApp ownerA1 tidA1 newAppA1) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "user.id" & asString
  BrigI.refreshIndex domainA

  (ownerA2, _, [regularMemberA2]) <- createTeam domainA 2
  (ownerB1, _, [regularMemberB1]) <- createTeam domainB 2

  let foundUserType :: (HasCallStack) => SearchContactsCfg -> [String] -> App ()
      foundUserType cfg uids =
        searchContactsWith cfg `bindResponse` \resp -> do
          resp.status `shouldMatchInt` 200
          foundDocs :: [Value] <- resp.json %. "documents" >>= asList
          ((%. "id") `mapM` foundDocs) `shouldMatch` uids

  searchTerm <- asString newAppA1.name
  domain <- asString domainA

  -- App is findable from /search/contacts inside the own team.
  forM_ [ownerA1, regularMemberA1]
    $ \user -> do
      foundUserType SearchContactsCfg {types = Nothing, ..} [appA1Id]
      foundUserType SearchContactsCfg {types = Just [], ..} [appA1Id]
      foundUserType SearchContactsCfg {types = Just ["app"], ..} [appA1Id]
      foundUserType SearchContactsCfg {types = Just ["app", "regular"], ..} [appA1Id]
      foundUserType SearchContactsCfg {types = Just ["regular"], ..} []

  -- App user is *not* findable from other team.
  forM_ [ownerA2, regularMemberA2, ownerB1, regularMemberB1]
    $ \user -> do
      foundUserType SearchContactsCfg {types = Nothing, ..} []
      foundUserType SearchContactsCfg {types = Just [], ..} []
      foundUserType SearchContactsCfg {types = Just ["app"], ..} []
      foundUserType SearchContactsCfg {types = Just ["app", "regular"], ..} []
      foundUserType SearchContactsCfg {types = Just ["regular"], ..} []

testRemoveServicesAccessRole :: (HasCallStack) => App ()
testRemoveServicesAccessRole = do
  domain <- make OwnDomain

  -- Create team A with owner, member, and app
  (ownerA, tidA, [memberA]) <- createTeam domain 2
  let newApp = def {name = "test-app"} :: NewApp
  app <- bindResponse (createApp ownerA tidA newApp) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "user"

  -- Create MLS clients
  [memberAClient, appClient] <- traverse (createMLSClient def) [memberA, app]
  traverse_ (uploadNewKeyPackage def) [memberAClient, appClient]

  -- Create an MLS team conversation and add app
  conv <- postConversation memberA defMLS {team = Just tidA, protocol = "mls"} >>= getJSON 201
  convId <- objConvId conv
  createGroup def memberAClient convId
  void $ createAddCommit memberAClient convId [app] >>= sendAndConsumeCommitBundle

  -- Verify all members are in the conversation
  bindResponse (getConversation memberA conv) $ \resp -> do
    resp.status `shouldMatchInt` 200
    members <- resp.json %. "members.others" >>= asList
    memberIds <- mapM (\m -> m %. "qualified_id.id" >>= asString) members
    appId <- app %. "qualified_id.id" & asString
    memberIds `shouldContain` [appId]

  -- Remove "services" from access roles -> app should be removed
  -- First verify we can get the conversation with the creator
  bindResponse (getConversation memberA conv) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "protocol" `shouldMatch` "mls"

  let noServices =
        [ "access" .= ["invite", "link"],
          "access_role" .= (["team_member", "non_team_member"] :: [String])
        ]
  -- Use the conversation creator for the update
  bindResponse (updateAccess memberA conv noServices) $ \resp -> do
    resp.status `shouldMatchInt` 200
  eventually $ do
    bindResponse (getConversation memberA conv) $ \resp -> do
      resp.status `shouldMatchInt` 200
      members <- resp.json %. "members.others" >>= asList
      memberIds <- mapM (\m -> m %. "qualified_id.id" >>= asString) members
      appId <- app %. "qualified_id.id" & asString
      memberIds `shouldNotContain` [appId]

testAppReceivesMemberJoinNotification :: (HasCallStack) => App ()
testAppReceivesMemberJoinNotification = do
  (owner, tid, []) <- createTeam OwnDomain 1

  -- Create an app in the team
  app <- bindResponse (createApp owner tid def) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "user"

  -- With websockets open for both owner and app, add a new regular member.
  -- Both should receive the team.member-join notification.
  withWebSockets [owner, app] $ \[wsOwner, wsApp] -> do
    newMember <- addUserToTeam owner

    memberJoinOwner <- awaitMatch isTeamMemberJoinNotif wsOwner
    memberJoinOwner %. "payload.0.team" `shouldMatch` tid
    memberJoinOwner %. "payload.0.data.user" `shouldMatch` objId newMember

    memberJoinApp <- awaitMatch isTeamMemberJoinNotif wsApp
    memberJoinApp %. "payload.0.team" `shouldMatch` tid
    memberJoinApp %. "payload.0.data.user" `shouldMatch` objId newMember

testTeamSizeWithApps :: (HasCallStack) => TaggedBool "test internal api" -> App ()
testTeamSizeWithApps (TaggedBool testInternalApi) = do
  domain <- make OwnDomain
  numRegulars <- liftIO $ randomRIO (1 :: Int, 3)
  numApps <- liftIO $ randomRIO (1 :: Int, 3)

  (owner, tid, extraMembers) <- createTeam domain (numRegulars + 1)

  apps <- replicateM numApps $ bindResponse (createApp owner tid def) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "user"

  let checkSize :: (HasCallStack) => Int -> Int -> App ()
      checkSize wantRegulars wantApps =
        (if testInternalApi then BrigI.getTeamSize else Brig.getTeamSize) owner tid `bindResponse` \resp -> do
          resp.status `shouldMatchInt` 200
          resp.json %. "teamSize" `shouldMatchInt` (1 + wantRegulars + wantApps)
          resp.json %. "teamSizeRegulars" `shouldMatchInt` (1 + wantRegulars)
          resp.json %. "teamSizeApps" `shouldMatchInt` wantApps

  BrigI.refreshIndex domain
  eventually $ do
    checkSize numRegulars numApps

  deleteTeamMember tid owner (head apps) >>= assertSuccess
  deleteTeamMember tid owner (head extraMembers) >>= assertSuccess

  BrigI.refreshIndex domain
  eventually $ do
    checkSize (numRegulars - 1) (numApps - 1)

testReAddExternalAppToGroupConversation :: (HasCallStack) => App ()
testReAddExternalAppToGroupConversation = do
  (owner1, tid1, []) <- createTeam OwnDomain 1
  (owner2, tid2, [member2]) <- createTeam OwnDomain 2

  let newApp = def {name = "external-app"} :: NewApp
  app <- bindResponse (createApp owner1 tid1 newApp) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "user"
  appId <- app %. "id" & asString

  let appPermissions = ["create_team_conversation", "implicit_connection"]
  addTeamCollaborator owner2 tid2 app appPermissions >>= assertSuccess

  -- Create MLS clients
  [member2Client, appClient] <- traverse (createMLSClient def) [member2, app]
  traverse_ (uploadNewKeyPackage def) [member2Client, appClient]

  -- Create an MLS team conversation and add app
  conv <- postConversation member2 defMLS {team = Just tid2, protocol = "mls"} >>= getJSON 201
  convId <- objConvId conv
  createGroup def member2Client convId
  void $ createAddCommit member2Client convId [app] >>= sendAndConsumeCommitBundle

  bindResponse (getConversation member2 conv) $ \resp -> do
    resp.status `shouldMatchInt` 200
    mems <- resp.json %. "members.others" >>= asList
    mIds <- mapM (\m -> m %. "qualified_id.id" >>= asString) mems
    mIds `shouldContain` [appId]

  -- removeTeamCollaborator triggers a backend-side removal: the backend
  -- removes the app from the conversation and sends an external Remove
  -- proposal. The remaining client must consume it and commit.
  removeTeamCollaborator owner2 tid2 app >>= assertSuccess

  -- Fetch the external Remove proposal from the notification queue and
  -- consume it on the client side.
  eventually $ do
    notifs <- getNotifications member2 def {client = Just member2Client.client} >>= getJSON 200
    allNotifs <- notifs %. "notifications" & asList
    mlsNotifs <- filterM (\n -> isNewMLSMessageNotif n) allNotifs
    mlsNotif <- assertOne mlsNotifs
    msgData <- mlsNotif %. "payload.0.data" & asByteString
    void $ mlsCliConsume convId def member2Client msgData

  -- Update test MLS state to reflect the removal
  modifyMLSState $ \mls ->
    mls
      { convs =
          Map.adjust
            ( \c ->
                c
                  { members = Set.filter (\m -> m.user /= appId) c.members,
                    memberUsers = Set.filter (/= appClient.qualifiedUserId) c.memberUsers
                  }
            )
            convId
            mls.convs
      }

  void $ createPendingProposalCommit convId member2Client >>= sendCommitBundle

  bindResponse (getConversation member2 conv) $ \resp -> do
    resp.status `shouldMatchInt` 200
    mems <- resp.json %. "members.others" >>= asList
    mIds <- mapM (\m -> m %. "qualified_id.id" >>= asString) mems
    mIds `shouldNotContain` [appId]

  addTeamCollaborator owner2 tid2 app appPermissions >>= assertSuccess

  bindResponse (getConversation member2 conv) $ \resp -> do
    resp.status `shouldMatchInt` 200
    mems <- resp.json %. "members.others" >>= asList
    mIds <- mapM (\m -> m %. "qualified_id.id" >>= asString) mems
    mIds `shouldNotContain` [appId]

  do
    void $ uploadNewKeyPackage def appClient
    mp <- createAddCommit member2Client convId [app]
    void $ sendCommitBundle mp
    mlsConv <- getMLSConv convId
    traverse_ (fromWelcome convId mlsConv.ciphersuite appClient) mp.welcome

  bindResponse (getConversation member2 conv) $ \resp -> do
    resp.status `shouldMatchInt` 200
    mems <- resp.json %. "members.others" >>= asList
    mIds <- mapM (\m -> m %. "qualified_id.id" >>= asString) mems
    mIds `shouldContain` [appId]

-- | The ways in which an MLS 1:1 conversation can end.
data EndOfRelationship
  = -- | A full member is removed from the team, account is deleted.
    TeamMemberRemoved
  | -- | An app collaborating with the team gets removed.
    AppCollaboratorRemoved
  | -- | An app keeps its collaborator record, but loses every
    -- permission.  (Nothing about the conversation changes.)
    AppCollaboratorPermissionsDropped
  | -- | The app's own team deletes the app, so its account goes away.
    AppDeleted
  | -- | Like 'AppCollaboratorRemoved', but with a regular user as the
    -- collaborator.  Unlike an app, a regular user *could* be connected, so
    -- the conversation may only be dissolved after establishing that they are
    -- not (see 'ConnectedUserCollaboratorRemoved').
    UserCollaboratorRemoved
  | -- | A regular user who is both connected to the team member and a
    -- collaborator of their team loses the collaborator record.  The
    -- connection still justifies the conversation, so nothing about it may
    -- change at all.
    --
    -- NOTE: collaborators of type "user" are not a thing in our
    -- product. Its existence is an artefact of how external apps have
    -- been implement, and nobody in the product department knows or
    -- cares about this.  We still keep these two cases to document
    -- that backend does intentionally work that way.
    ConnectedUserCollaboratorRemoved
  deriving (Generic, Show, Eq)

-- | An established MLS 1:1 conversation survives the end of the relationship
-- that justified it.
--
-- The conversation is never deleted: the remaining party keeps it
-- with all its metadata, and the only change is that the other party
-- is no longer a member.
--
-- Removing the other party from the conversation and removing them from the
-- MLS group go together: whenever the member is gone, the backend must also
-- have signed an external remove proposal for their leaf, and whenever the
-- member stays, there must be no such proposal.  (That a remaining client can
-- consume the proposal and commit the removal is covered by
-- 'testReAddExternalAppToGroupConversation', and is not repeated here.)
testMLSOne2OneAfterEndOfRelationship :: (HasCallStack) => EndOfRelationship -> App ()
testMLSOne2OneAfterEndOfRelationship scenario = do
  (owner1, tid1, []) <- createTeam OwnDomain 1
  (owner2, tid2, [member2, teamMate2]) <- createTeam OwnDomain 3

  let collaboratorPermissions = ["create_team_conversation", "implicit_connection"]

      mkApp :: (HasCallStack) => App Value
      mkApp = do
        app <- bindResponse (createApp owner1 tid1 (def {name = "external-app-o2o"} :: NewApp)) $ \resp -> do
          resp.status `shouldMatchInt` 200
          resp.json %. "user"
        addTeamCollaborator owner2 tid2 app collaboratorPermissions >>= assertSuccess
        pure app

  -- The other party of the 1:1, the action that ends the relationship, and
  -- whether that action should remove the other party from the conversation
  -- (and, equivalently, from the MLS group).
  (other, endRelationship, expectOtherRemoved) <- case scenario of
    TeamMemberRemoved ->
      pure
        ( teamMate2,
          deleteTeamMember tid2 owner2 teamMate2 >>= assertSuccess,
          True
        )
    AppCollaboratorRemoved -> do
      app <- mkApp
      pure
        ( app,
          removeTeamCollaborator owner2 tid2 app >>= assertSuccess,
          True
        )
    AppCollaboratorPermissionsDropped -> do
      app <- mkApp
      pure
        ( app,
          updateTeamCollaborator owner2 tid2 app [] >>= assertSuccess,
          False
        )
    AppDeleted -> do
      app <- mkApp
      pure
        ( app,
          deleteTeamMember tid1 owner1 app >>= assertSuccess,
          True
        )
    UserCollaboratorRemoved -> do
      user <- randomUser OwnDomain def
      addTeamCollaborator owner2 tid2 user collaboratorPermissions >>= assertSuccess
      pure
        ( user,
          removeTeamCollaborator owner2 tid2 user >>= assertSuccess,
          True
        )
    ConnectedUserCollaboratorRemoved -> do
      user <- randomUser OwnDomain def
      connectTwoUsers member2 user
      addTeamCollaborator owner2 tid2 user collaboratorPermissions >>= assertSuccess
      pure
        ( user,
          removeTeamCollaborator owner2 tid2 user >>= assertSuccess,
          False
        )

  otherId <- other %. "qualified_id.id" & asString

  -- The team member establishes the MLS 1:1 conversation with the other party.
  one2OneConv <- getMLSOne2OneConversation member2 other >>= getJSON 200
  convQid <- one2OneConv %. "conversation.qualified_id"
  convId <- objConvId $ one2OneConv %. "conversation"
  [member2Client, otherClient] <- traverse (createMLSClient def) [member2, other]
  void $ uploadNewKeyPackage def otherClient
  resetOne2OneGroup def member2Client one2OneConv
  void $ createAddCommit member2Client convId [other] >>= sendAndConsumeCommitBundle

  convBefore <- getConversation member2 convQid >>= getJSON 200
  do
    mems <- convBefore %. "members.others" >>= asList
    mIds <- traverse (\m -> m %. "qualified_id.id" & asString) mems
    mIds `shouldMatch` [otherId]

  -- How the conversation should look afterwards: the other party is gone (or
  -- not, in the control case), everything else is unchanged.
  convAfterExpected <-
    if expectOtherRemoved
      then convBefore & setField "members.others" ([] :: [Value])
      else pure convBefore

  endRelationship

  convAfter <- eventually $ do
    conv <- getConversation member2 convQid >>= getJSON 200
    conv `shouldMatch` convAfterExpected
    pure conv

  -- The remaining client is told about the removal in the MLS group as well.
  -- Not consumed here: it is enough that the backend sends the proposal, see
  -- the haddock of this test.
  eventually $ do
    -- The other party is the second member of the group, so its leaf is at index
    -- 1, and a removal initiated by the backend is signed by external sender 0.
    let isBackendRemoveProposal :: (HasCallStack) => Value -> App Bool
        isBackendRemoveProposal n =
          isNewMLSMessageNotif n
            &&~ isNotifConvId convId n
            &&~ do
              msg <- n %. "payload.0.data" & asByteString >>= showMessage def member2Client
              fieldEquals msg "message.content.body.Proposal.Remove.removed" (1 :: Int)
                &&~ fieldEquals msg "message.content.sender.External" (0 :: Int)
    notifs <- getNotifications member2 def {client = Just member2Client.client} >>= getJSON 200
    proposals <- notifs %. "notifications" & asList >>= filterM isBackendRemoveProposal
    length proposals `shouldMatchInt` (if expectOtherRemoved then 1 else 0)

  -- The remaining member can still use the conversation, and (made
  -- explicit here to catch regression) using it does not change it.
  do
    mp <- createApplicationMessage convId member2Client "hello"
    postMLSMessage mp.sender mp.message >>= assertStatus 201
  getConversation member2 convQid >>= getJSON 200 >>= (`shouldMatch` convAfter)
