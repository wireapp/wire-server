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

import API.Brig
import qualified API.BrigInternal as BrigI
import API.Common
import API.Galley
import Control.Lens hiding ((.=))
import Data.Aeson.QQ.Simple
import MLS.Util
import SetupHelpers
import Testlib.Prelude

testCreateApp :: (HasCallStack) => App ()
testCreateApp = do
  -- FUTUREWORK: what about federation?
  domain <- make OwnDomain
  (owner, tid, [regularMember]) <- createTeam domain 2
  let new =
        def
          { name = "chappie",
            description = "some description of this app",
            category = "ai"
          } ::
          NewApp

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
  let appIdObject = object ["domain" .= domain, "id" .= appId]
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

  void $ bindResponse (renewToken domain cookie) $ \resp -> do
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
  outsideUser <- randomUser domain def
  bindResponse (getApp outsideUser tid appId) $ \resp -> do
    -- this may change soon, see
    -- https://wearezeta.atlassian.net/browse/WPB-23995,
    -- https://wearezeta.atlassian.net/browse/WPB-23840
    resp.status `shouldMatchInt` 200

  (owner2, tid2, [regularMember2]) <- createTeam domain 2
  bindResponse (getApp owner2 tid appId) $ \resp -> resp.status `shouldMatchInt` 200
  bindResponse (getApp owner2 tid2 appId) $ \resp -> resp.status `shouldMatchInt` 200
  bindResponse (getApp regularMember2 tid appId) $ \resp -> resp.status `shouldMatchInt` 200

  -- Category can be any text; sanitization must happen by clients.
  void $ bindResponse (createApp owner tid new {category = "notinenum"}) $ \resp -> do
    resp.status `shouldMatchInt` 200
    deleteTeamMember tid owner (resp.json %. "user") >>= assertSuccess

  let foundUserType :: (HasCallStack) => Value -> String -> [String] -> App ()
      foundUserType searcher exactMatchTerm aTypes =
        searchContacts searcher exactMatchTerm OwnDomain `bindResponse` \resp -> do
          resp.status `shouldMatchInt` 200
          foundDocs :: [Value] <- resp.json %. "documents" >>= asList
          docsInTeam :: [Value] <- do
            -- make sure that matches from previous test runs don't get in the way.
            -- related: https://wearezeta.atlassian.net/browse/WPB-23995
            catMaybes
              <$> forM
                foundDocs
                ( \doc -> do
                    tidActual <- doc %. "team" & asString
                    pure $ if tidActual == tid then Just doc else Nothing
                )

          (%. "type") `mapM` docsInTeam `shouldMatch` aTypes

  -- App's user is findable from /search/contacts
  BrigI.refreshIndex domain
  foundUserType owner new.name ["app"]
  foundUserType regularMember new.name ["app"]

  -- App's user is *not* findable from other team.
  BrigI.refreshIndex domain
  foundUserType owner2 new.name []

  -- Regular members still have the type "regular"
  memberName <- regularMember %. "name" & asString
  foundUserType owner memberName ["regular"]

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
  appId <- bindResponse (createApp owner tid new) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "user.id" & asString

  let Object appMetadata =
        [aesonQQ|
        {
          "accent_id": 2147483647,
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

  withWebSockets [owner, regularMember] \[wsOwner, wsRegularMember] -> do
    bindResponse (putAppMetadata tid owner appId (Object appMetadata)) $ \resp -> do
      resp.status `shouldMatchInt` 200
    void $ assertNoEvent 5 wsOwner
    void $ assertNoEvent 5 wsRegularMember
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

  -- Create app A2 (member of team B)
  let newAppA2 = def {name = "app-a2"} :: NewApp
  appA2 <- bindResponse (createApp ownerB tidB newAppA2) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "user"

  -- M1 tries to connect to app A2 from team B => should fail
  -- Apps cannot create connections accross teams
  bindResponse (postConnection m1 appA2) $ \resp -> do
    resp.status `shouldMatchInt` 400
    resp.json %. "label" `shouldMatch` "invalid-user"

  -- Create app A1 (member of team A)
  let newAppA1 = def {name = "app-a1"} :: NewApp
  appA1 <- bindResponse (createApp ownerA tidA newAppA1) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "user"

  -- Create MLS clients for M1 and A1 (both on domainA)
  [m1c, appA1c] <- traverse (createMLSClient def) [m1, appA1]
  traverse_ (uploadNewKeyPackage def) [m1c, appA1c]

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

testRemoveServicesAccessRole :: (HasCallStack) => App ()
testRemoveServicesAccessRole = do
  domain <- make OwnDomain

  -- Create team A with owner, member, and app
  (ownerA, tidA, [memberA]) <- createTeam domain 2
  let newApp = def {name = "test-app"} :: NewApp
  app <- bindResponse (createApp ownerA tidA newApp) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "user"

  -- Create team B with a member
  (ownerB, _, []) <- createTeam domain 1

  -- Create a teamless user (guest)
  guest <- randomUser domain def

  -- Create MLS clients
  [memberAClient, appClient] <- traverse (createMLSClient def) [memberA, app]
  traverse_ (uploadNewKeyPackage def) [memberAClient, appClient]

  -- Create an MLS team conversation and add app
  conv <- postConversation memberA defMLS {team = Just tidA, protocol = "mls"} >>= getJSON 201
  convId <- objConvId conv
  createGroup def memberAClient convId
  void $ createAddCommit memberAClient convId [app] >>= sendAndConsumeCommitBundle

  -- Connect and add member from team B
  postConnection memberA ownerB >>= assertSuccess
  putConnection ownerB memberA "accepted" >>= assertSuccess
  ownerBClient <- createMLSClient def ownerB
  void $ uploadNewKeyPackage def ownerBClient
  void $ createAddCommit memberAClient convId [ownerB] >>= sendAndConsumeCommitBundle

  -- Connect and add teamless guest
  postConnection memberA guest >>= assertSuccess
  putConnection guest memberA "accepted" >>= assertSuccess
  guestClient <- createMLSClient def guest
  void $ uploadNewKeyPackage def guestClient
  void $ createAddCommit memberAClient convId [guest] >>= sendAndConsumeCommitBundle

  -- Verify all members are in the conversation
  bindResponse (getConversation memberA conv) $ \resp -> do
    resp.status `shouldMatchInt` 200
    members <- resp.json %. "members.others" >>= asList
    memberIds <- mapM (\m -> m %. "qualified_id.id" >>= asString) members
    appId <- app %. "qualified_id.id" & asString
    ownerBId <- ownerB %. "qualified_id.id" & asString
    guestId <- guest %. "qualified_id.id" & asString
    memberIds `shouldContain` [appId]
    memberIds `shouldContain` [ownerBId]
    memberIds `shouldContain` [guestId]

  -- Test 1: Remove "services" from access roles -> app should be removed
  -- First verify we can get the conversation with the creator
  bindResponse (getConversation memberA conv) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "protocol" `shouldMatch` "mls"

  let noServices =
        [ "access" .= ["invite", "link"],
          "access_role" .= (["team_member", "non_team_member", "guest"] :: [String])
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
      ownerBId <- ownerB %. "qualified_id.id" & asString
      guestId <- guest %. "qualified_id.id" & asString
      memberIds `shouldNotContain` [appId]
      memberIds `shouldContain` [ownerBId]
      memberIds `shouldContain` [guestId]

  -- Test 2: Remove "guest" from access roles -> teamless user and cross-team member should be removed
  let teamMemberOnly =
        [ "access" .= ["invite", "link"],
          "access_role" .= (["team_member"] :: [String])
        ]
  bindResponse (updateAccess memberA conv teamMemberOnly) $ \resp -> do
    resp.status `shouldMatchInt` 200
  eventually $ do
    bindResponse (getConversation memberA conv) $ \resp -> do
      resp.status `shouldMatchInt` 200
      members <- resp.json %. "members.others" >>= asList
      memberIds <- mapM (\m -> m %. "qualified_id.id" >>= asString) members
      ownerBId <- ownerB %. "qualified_id.id" & asString
      guestId <- guest %. "qualified_id.id" & asString
      memberIds `shouldNotContain` [ownerBId]
      memberIds `shouldNotContain` [guestId]
