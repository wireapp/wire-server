{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Test.UserGroup where

import API.Brig
import API.Galley
import Notifications (isUserGroupCreatedNotif)
import SetupHelpers
import Testlib.Prelude

testUserGroupSmoke :: (HasCallStack) => App ()
testUserGroupSmoke = do
  (owner, team, [mem1, mem2, mem3, admin2]) <- createTeam OwnDomain 5
  updateTeamMember team owner admin2 Admin >>= assertSuccess
  mem1id <- asString $ mem1 %. "id"
  mem2id <- asString $ mem2 %. "id"
  mem3id <- asString $ mem3 %. "id"

  let badGid = "225c4d54-1ae7-11f0-8e9c-cbb31865d602"

  gid <- withWebSockets [owner, admin2] $ \wss -> do
    gid <- bindResponse (createUserGroup owner (object ["name" .= "none", "members" .= ([mem1id, mem2id])])) $ \resp -> do
      resp.status `shouldMatchInt` 200
      resp.json %. "name" `shouldMatch` "none"
      resp.json %. "members" `shouldMatch` [mem1id, mem2id]
      asString $ (resp.json %. "id")
    for_ wss $ \ws -> do
      notif <- awaitMatch isUserGroupCreatedNotif ws
      notif %. "payload.0.user_group.id" `shouldMatch` gid
    pure gid

  bindResponse (getUserGroup owner badGid) $ \resp -> do
    resp.status `shouldMatchInt` 404

  bindResponse (getUserGroup mem3 badGid) $ \resp -> do
    resp.status `shouldMatchInt` 404

  bindResponse (getUserGroup owner gid) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "members" `shouldMatch` [mem1id, mem2id]

  bindResponse (updateUserGroup owner badGid (object ["name" .= ""])) $ \resp -> do
    resp.status `shouldMatchInt` 400
    resp.json %. "label" `shouldMatch` "bad-request"

  bindResponse (updateUserGroup owner badGid (object ["name" .= "good name"])) $ \resp -> do
    resp.status `shouldMatchInt` 404

  bindResponse (updateUserGroup owner gid (object ["name" .= "also good"])) $ \resp -> do
    resp.status `shouldMatchInt` 200

  bindResponse (addUserToGroup owner gid mem3id) $ \resp -> do
    resp.status `shouldMatchInt` 204

  bindResponse (removeUserFromGroup owner gid mem1id) $ \resp -> do
    resp.status `shouldMatchInt` 204

  bindResponse (getUserGroup owner gid) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "name" `shouldMatch` "also good"
    resp.json %. "members" `shouldMatch` [mem2id, mem3id]

  bindResponse (getUserGroups owner def) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "page.0.name" `shouldMatch` "also good"

  bindResponse (deleteUserGroup owner badGid) $ \resp -> do
    resp.status `shouldMatchInt` 404

  bindResponse (deleteUserGroup owner gid) $ \resp -> do
    resp.status `shouldMatchInt` 204

  bindResponse (updateUserGroup owner gid (object ["name" .= "also good"])) $ \resp -> do
    resp.status `shouldMatchInt` 404

  bindResponse (addUserToGroup owner gid mem1id) $ \resp -> do
    resp.status `shouldMatchInt` 404

  bindResponse (removeUserFromGroup owner gid mem1id) $ \resp -> do
    resp.status `shouldMatchInt` 404

testUserGroupGetGroups :: (HasCallStack) => App ()
testUserGroupGetGroups = do
  (owner, _team, []) <- createTeam OwnDomain 1

  let groupNames = ["First group", "CC", "CCC"] <> ((: []) <$> ['A' .. 'G'])
  forM_ groupNames $ \gname -> do
    let newGroup = object ["name" .= gname, "members" .= ([] :: [()])]
    bindResponse (createUserGroup owner newGroup) $ \resp -> do
      resp.status `shouldMatchInt` 200
      resp.json %. "name" `shouldMatch` gname
      resp.json %. "members" `shouldMatch` ([] :: [()])

  let runSearch :: (HasCallStack) => GetUserGroupsArgs -> [String] -> App Value
      runSearch args expected =
        bindResponse (getUserGroups owner args) $ \resp -> do
          resp.status `shouldMatchInt` 200
          found <- ((%. "name") `mapM`) =<< asList =<< resp.json %. "page"
          found `shouldMatch` expected
          resp.json %. "state"

  -- filter & sort
  _ <- runSearch def {q = Just "C"} ["C", "CCC", "CC"]
  _ <- runSearch def {q = Just "CC", sortByKeys = Just "name"} ["CC", "CCC"]
  _ <-
    runSearch
      def {sortByKeys = Just "name"}
      [ "A",
        "B",
        "C",
        "CC",
        "CCC",
        "D",
        "E",
        "F",
        "First group",
        "G"
      ]
  _ <-
    runSearch
      def {sortByKeys = Just "created_at", sortOrder = Just "asc"}
      [ "First group",
        "CC",
        "CCC",
        "A",
        "B",
        "C",
        "D",
        "E",
        "F",
        "G"
      ]

  -- paginate
  pState1 <-
    runSearch
      def {sortByKeys = Just "name", sortOrder = Just "desc", pSize = Just 3}
      [ "G",
        "First group",
        "F"
      ]
  pState2 <-
    runSearch
      def {pState = Just pState1}
      [ "E",
        "D",
        "CCC"
      ]
  pState3 <-
    runSearch
      def {pState = Just pState2}
      [ "CC",
        "C",
        "B"
      ]

  void
    $ runSearch
      def {pState = Just pState3}
      [ "A"
      ]
