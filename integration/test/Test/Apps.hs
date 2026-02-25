{-# OPTIONS -Wno-ambiguous-fields #-}

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
import API.Galley
import Data.Aeson.QQ.Simple
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

  -- Owner can create an app
  (appId, cookie) <- bindResponse (createApp owner tid new) $ \resp -> do
    resp.status `shouldMatchInt` 200
    appId <- resp.json %. "user.id" & asString
    cookie <- resp.json %. "cookie" & asString
    pure (appId, cookie)

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
    (sort <$> ((%. "name") `mapM` apps)) `shouldMatch` ["chappie", "fmappie"]

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
    (resp.json %. "description") `shouldMatch` "some description of this app"
    (resp.json %. "category") `shouldMatch` "ai"

  -- A teamless user can't get the app
  outsideUser <- randomUser domain def
  bindResponse (getApp outsideUser tid appId) $ \resp -> do
    resp.status `shouldMatchInt` 403
    resp.json %. "label" `shouldMatch` "app-no-permission"

  -- Another team's owner nor member can't get the app
  (owner2, tid2, [regularMember2]) <- createTeam domain 2
  bindResponse (getApp owner2 tid appId) $ \resp -> resp.status `shouldMatchInt` 403
  bindResponse (getApp owner2 tid2 appId) $ \resp -> resp.status `shouldMatchInt` 404
  bindResponse (getApp regularMember2 tid appId) $ \resp -> resp.status `shouldMatchInt` 403

  -- Category must be any of the values for the Category enum
  void $ bindResponse (createApp owner tid new {category = "notinenum"}) $ \resp -> do
    resp.status `shouldMatchInt` 400

  let foundUserType :: (HasCallStack) => Value -> String -> [String] -> App ()
      foundUserType searcher exactMatchTerm aTypes =
        searchContacts searcher exactMatchTerm OwnDomain `bindResponse` \resp -> do
          resp.status `shouldMatchInt` 200
          foundDocs :: [Value] <- resp.json %. "documents" >>= asList
          docsInTeam :: [Value] <- do
            -- make sure that matches from previous test runs don't get in the way.
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

  (appId, cookie) <- bindResponse (createApp alice tid new) $ \resp -> do
    resp.status `shouldMatchInt` 200
    appId <- resp.json %. "user.id" & asString
    cookie <- resp.json %. "cookie" & asString
    pure (appId, cookie)

  bindResponse (refreshAppCookie bob tid appId) $ \resp -> do
    resp.status `shouldMatchInt` 403
    resp.json %. "label" `shouldMatch` "app-no-permission"

  bindResponse (refreshAppCookie charlie tid appId) $ \resp -> do
    resp.status `shouldMatchInt` 403
    resp.json %. "label" `shouldMatch` "app-no-permission"

  cookie' <- bindResponse (refreshAppCookie alice tid appId) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "cookie" & asString

  for_ [cookie, cookie'] $ \c ->
    void $ bindResponse (renewToken OwnDomain c) $ \resp -> do
      resp.status `shouldMatchInt` 200
      resp.json %. "user" `shouldMatch` appId
      resp.json %. "token_type" `shouldMatch` "Bearer"
      resp.json %. "access_token" & asString

testDeleteAppFromTeam :: (HasCallStack) => App ()
testDeleteAppFromTeam = do
  domain <- make OwnDomain
  (owner, tid, _) <- createTeam domain 1
  let new = def {name = "chappie"} :: NewApp
  appId <- bindResponse (createApp owner tid new) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "user.id" & asString

  let appIdObject = object ["domain" .= domain, "id" .= appId]

  bindResponse (deleteTeamMember tid owner appIdObject) $ \resp -> do
    resp.status `shouldMatchInt` 202

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
  (owner, tid, _) <- createTeam domain 1
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

  bindResponse (putAppMetadata tid owner appId (Object appMetadata)) $ \resp -> do
    resp.status `shouldMatchInt` 200

  bindResponse (getApp owner tid appId) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json
      `shouldMatchShape` SObject
        [ ("accent_id", SNumber),
          ("assets", SArray (SObject [("key", SString), ("size", SString), ("type", SString)])),
          ("name", SString),
          ("category", SString),
          ("description", SString),
          ("metadata", SObject []),
          ("picture", SArray SAny)
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
            ("locale", SString),
            ("managed_by", SString),
            ("name", SString),
            ("picture", SArray SAny),
            ("qualified_id", SObject [("domain", SString), ("id", SString)]),
            ("searchable", SBool),
            ("status", SString),
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
          [ ("accent_id", SNumber),
            ("assets", SArray SAny),
            ("category", SString),
            ("description", SString),
            ("metadata", SObject []),
            ("name", SString),
            ("picture", SArray SAny)
            -- TODO: ("id", SString)
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

  domain <- make OwnDomain
  (owner, tid, [regular]) <- createTeam domain 2

  -- [`POST /teams/:tid/apps`](https://staging-nginz-https.zinfra.io/v15/api/swagger-ui/#/default/create-app) (route id: "create-app")
  let new = def {name = "chippie"} :: NewApp
  appCreated <- bindResponse (createApp owner tid new) $ \resp -> do
    resp.status `shouldMatchInt` 200
    pure resp.json
  appCreated
    `shouldMatchShape` SObject
      [ ("cookie", SString),
        ("user", userShape)
        -- TODO: , ("app", appShape)
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
    resp.json `shouldMatchShape` memberShape

  -- [`GET /teams/:tid/apps`](https://staging-nginz-https.zinfra.io/v15/api/swagger-ui/#/default/get-apps) (route id: "get-apps")
  getApps owner tid `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 200
    [appHave] <- resp.json & maybe (error "this shouldn't happen") asList
    -- TODO: SObject [("user", userShape), ("app", appShape)]
    appHave `shouldMatchShape` appShape

  -- [`GET /teams/:tid/apps/:uid`](https://staging-nginz-https.zinfra.io/v15/api/swagger-ui/#/default/get-app) (route id: "get-app")
  getApp owner tid appId `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json `shouldMatchShape` appShape

  -- [`POST /list-users`](https://staging-nginz-https.zinfra.io/v15/api/swagger-ui/#/default/list-users-by-ids-or-handles) (route id: "list-users-by-ids-or-handles")
  listUsers owner [appCreated %. "user"] `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json
      %. "found.0"
      `shouldMatchShape` SObject
        [ ("accent_id", SNumber),
          ("assets", SArray SAny),
          ("id", SString),
          ("legalhold_status", SString),
          ("name", SString),
          ("picture", SArray SAny),
          ("qualified_id", SObject [("domain", SString), ("id", SString)]),
          ("searchable", SBool),
          ("supported_protocols", SArray SString),
          ("team", SString),
          ("type", SString)
          -- TODO: [("user", ...), ("app", ...)] ?
        ]

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
      (`shouldMatchShape` searchResultShape) `mapM_` hits
