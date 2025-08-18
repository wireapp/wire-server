{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Test.UserGroup where

import API.Brig
import API.Galley
import Control.Error (lastMay)
import Notifications (isUserGroupCreatedNotif)
import SetupHelpers
import Testlib.Prelude

testUserGroupSmoke :: (HasCallStack) => App ()
testUserGroupSmoke = do
  (owner, team, [mem1, mem2, mem3, mem4, mem5, mem6, admin2]) <- createTeam OwnDomain 8
  updateTeamMember team owner admin2 Admin >>= assertSuccess
  mem1id <- asString $ mem1 %. "id"
  mem2id <- asString $ mem2 %. "id"
  mem3id <- asString $ mem3 %. "id"
  mem4id <- asString $ mem4 %. "id"
  mem5id <- asString $ mem5 %. "id"
  mem6id <- asString $ mem6 %. "id"

  let badGid = "225c4d54-1ae7-11f0-8e9c-cbb31865d602"
      badMemid = "7bf23c0b-0be6-4432-bc5d-ab301bf75a99"

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

  bindResponse (addUsersToGroup owner gid [mem3id, mem4id, mem5id]) $ \resp -> do
    resp.status `shouldMatchInt` 204

  bindResponse (addUsersToGroup mem3id gid [mem6id]) $ \resp -> do
    resp.status `shouldMatchInt` 401

  bindResponse (addUsersToGroup owner gid [badMemid, mem6id]) $ \resp -> do
    resp.status `shouldMatchInt` 400

  bindResponse (removeUserFromGroup owner gid mem1id) $ \resp -> do
    resp.status `shouldMatchInt` 204

  bindResponse (getUserGroup owner gid) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "name" `shouldMatch` "also good"
    resp.json %. "members" `shouldMatch` [mem2id, mem3id, mem4id, mem5id]

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

  bindResponse (addUsersToGroup owner gid [mem1id, mem5id]) $ \resp -> do
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

  -- Default sort by is createdAt, and sortOrder is DESC
  _ <- runSearch owner def {q = Just "C"} ["C", "CCC", "CC"]

  -- Default sortOrder is DESC, regardless of sortBy
  _ <- runSearch owner def {q = Just "CC", sortByKeys = Just "name"} ["CCC", "CC"]

  -- Test combinations of sortBy and sortOrder:
  _ <-
    runSearch
      owner
      def {sortByKeys = Just "name", sortOrder = Just "asc"}
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
      owner
      def {sortByKeys = Just "name", sortOrder = Just "desc"}
      ( reverse
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
      )
  _ <-
    runSearch
      owner
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
  _ <-
    runSearch
      owner
      def {sortByKeys = Just "created_at", sortOrder = Just "desc"}
      ( reverse
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
      )

  -- Test sorting and filtering works across pages
  let firstPageParams = def {sortByKeys = Just "name", sortOrder = Just "desc", pSize = Just 3}
  Just (name1, createdAt1, id1) <-
    runSearch
      owner
      firstPageParams
      [ "G",
        "First group",
        "F"
      ]
  Just (name2, createdAt2, id2) <-
    runSearch
      owner
      firstPageParams {lastName = Just name1, lastCreatedAt = Just createdAt1, lastId = Just id1}
      [ "E",
        "D",
        "CCC"
      ]
  Just (name3, createdAt3, id3) <-
    runSearch
      owner
      firstPageParams {lastName = Just name2, lastCreatedAt = Just createdAt2, lastId = Just id2}
      [ "CC",
        "C",
        "B"
      ]

  void
    $ runSearch
      owner
      firstPageParams {lastName = Just name3, lastCreatedAt = Just createdAt3, lastId = Just id3}
      ["A"]

runSearch :: (HasCallStack, MakesValue owner) => owner -> GetUserGroupsArgs -> [String] -> App (Maybe (String, String, String))
runSearch owner args expected =
  bindResponse (getUserGroups owner args) $ \resp -> do
    resp.status `shouldMatchInt` 200
    found <- ((%. "name") `mapM`) =<< asList =<< resp.json %. "page"
    found `shouldMatch` expected
    results <- asList $ resp.json %. "page"
    for (lastMay results) $ \lastGroup ->
      (,,)
        <$> asString (lastGroup %. "name")
        <*> asString (lastGroup %. "createdAt")
        <*> asString (lastGroup %. "id")

testUserGroupGetGroupsAllInputs :: (HasCallStack) => App ()
testUserGroupGetGroupsAllInputs = do
  (owner, _team, []) <- createTeam OwnDomain 1
  for_ ((: []) <$> ['A' .. 'Z']) $ \gname -> do
    let newGroup = object ["name" .= gname, "members" .= ([] :: [()])]
    createUserGroup owner newGroup >>= assertSuccess

  Just (ln, ltz, lid) <- runSearch owner def {pSize = Just 3} ["Z", "Y", "X"]
  let getUserGroupArgs = getUserGroupArgsCombinations ln ltz lid
  for_ getUserGroupArgs $ \args -> do
    bindResponse (getUserGroups owner args) $ \resp -> do
      -- most important check is that all combinations return 200
      resp.status `shouldMatchInt` 200
      -- additionally we can check a few invariants
      groups <- resp.json %. "page" >>= asList
      case (args.q, args.lastName, args.lastCreatedAt, args.lastId) of
        (Nothing, Nothing, Nothing, Nothing) -> length groups `shouldMatchInt` (fromMaybe 15 args.pSize)
        (Just _, Nothing, Nothing, Nothing) -> length groups `shouldMatchInt` 1
        _ -> pure ()
  where
    getUserGroupArgsCombinations :: String -> String -> String -> [GetUserGroupsArgs]
    getUserGroupArgsCombinations ln ltz lid =
      [ GetUserGroupsArgs
          { q = q',
            sortByKeys = sortBy',
            sortOrder = sortOrder',
            pSize = pSize',
            lastName = lastName',
            lastCreatedAt = lastCreatedAt',
            lastId = lastId',
            includeMemberCount = includeMemberCount'
          }
        | q' <- qs,
          sortBy' <- sortByKeysList,
          sortOrder' <- sortOrders,
          pSize' <- pSizes,
          lastName' <- lastNames,
          lastCreatedAt' <- lastCreatedAts,
          lastId' <- lastIds,
          includeMemberCount' <- [False, True]
      ]
      where
        qs = [Nothing, Just "A"]
        sortByKeysList = [Nothing, Just "name", Just "created_at"]
        sortOrders = [Nothing, Just "asc", Just "desc"]
        pSizes = [Nothing, Just 3]
        lastNames = [Nothing, Just ln]
        lastCreatedAts = [Nothing, Just ltz]
        lastIds = [Nothing, Just lid]

testUserGroupMembersCount :: (HasCallStack) => App ()
testUserGroupMembersCount = do
  (owner, _team, [mem1, mem2]) <- createTeam OwnDomain 3
  mem1id <- asString $ mem1 %. "id"
  mem2id <- asString $ mem2 %. "id"

  bindResponse (createUserGroup owner (object ["name" .= "none", "members" .= ([mem1id, mem2id])])) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "name" `shouldMatch` "none"
    resp.json %. "members" `shouldMatch` [mem1id, mem2id]

  bindResponse (getUserGroups owner (def {includeMemberCount = True})) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "page.0.membersCount" `shouldMatchInt` 2
