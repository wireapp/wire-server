{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Test.UserGroup where

import API.Brig
import API.Galley
import API.GalleyInternal (setTeamFeatureLockStatus)
import Control.Error (lastMay)
import Notifications (isMemberJoinNotif, isUserGroupCreatedNotif, isUserGroupUpdatedNotif)
import SetupHelpers
import Testlib.Prelude

testUserGroupSmoke :: (HasCallStack) => App ()
testUserGroupSmoke = do
  (owner, team, [mem1, mem2, mem3, mem4, mem5, mem6, admin2, mem8, mem9]) <- createTeam OwnDomain 10
  updateTeamMember team owner admin2 Admin >>= assertSuccess
  mem1id <- asString $ mem1 %. "id"
  mem2id <- asString $ mem2 %. "id"
  mem3id <- asString $ mem3 %. "id"
  mem4id <- asString $ mem4 %. "id"
  mem5id <- asString $ mem5 %. "id"
  mem6id <- asString $ mem6 %. "id"
  mem8id <- asString $ mem8 %. "id"
  mem9id <- asString $ mem9 %. "id"

  let badGid = "225c4d54-1ae7-11f0-8e9c-cbb31865d602"
      badMemid = "7bf23c0b-0be6-4432-bc5d-ab301bf75a99"

  gid <- withWebSockets [owner, admin2] $ \wss -> do
    gid <- bindResponse (createUserGroup owner (object ["name" .= "none", "members" .= [mem1id, mem2id]])) $ \resp -> do
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

  withWebSockets [owner, admin2] $ \wssAdmins -> do
    bindResponse (addUsersToGroup owner gid [mem3id, mem4id, mem5id]) $ \resp -> do
      resp.status `shouldMatchInt` 204
    for_ wssAdmins $ \ws -> do
      notif <- awaitMatch isUserGroupUpdatedNotif ws
      notif %. "payload.0.user_group.id" `shouldMatch` gid

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
    resp.json %. "total" `shouldMatchInt` 1

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

  withWebSockets [owner, admin2] $ \wssAdmins -> do
    ug2Id <- bindResponse (createUserGroup owner (object ["name" .= "ug 2", "members" .= [mem1id]])) $ \resp -> do
      resp.status `shouldMatchInt` 200
      ug2Id <- asString $ (resp.json %. "id")

      for_ wssAdmins $ \ws -> do
        notif <- awaitMatch isUserGroupCreatedNotif ws
        notif %. "payload.0.user_group.id" `shouldMatch` ug2Id

      pure ug2Id

    bindResponse (getUserGroup owner ug2Id) $ \resp -> do
      resp.status `shouldMatchInt` 200
      resp.json %. "members" `shouldMatch` [mem1id]

    bindResponse (updateUserGroupUsers owner ug2Id [mem8id, mem9id]) $ \resp -> do
      resp.status `shouldMatchInt` 200

      for_ wssAdmins $ \ws -> do
        notif <- awaitMatch isUserGroupUpdatedNotif ws
        notif %. "payload.0.user_group.id" `shouldMatch` ug2Id

    bindResponse (getUserGroup owner ug2Id) $ \resp -> do
      resp.status `shouldMatchInt` 200
      resp.json %. "members" `shouldMatch` [mem8id, mem9id]

    bindResponse (updateUserGroupUsers owner ug2Id []) $ \resp -> do
      resp.status `shouldMatchInt` 200

      for_ wssAdmins $ \ws -> do
        notif <- awaitMatch isUserGroupUpdatedNotif ws
        notif %. "payload.0.user_group.id" `shouldMatch` ug2Id

    bindResponse (getUserGroup owner ug2Id) $ \resp -> do
      resp.status `shouldMatchInt` 200
      resp.json %. "members" `shouldMatch` ([] :: [()])

testUserGroupAddGroupDenied :: (HasCallStack) => App ()
testUserGroupAddGroupDenied = do
  let noMember = [] :: [()]

  (owner0, _team0, []) <- createTeam OwnDomain 1
  gid0 <- bindResponse (createUserGroup owner0 (object ["name" .= "none", "members" .= noMember])) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "name" `shouldMatch` "none"
    resp.json %. "members" `shouldMatch` noMember
    asString $ (resp.json %. "id")

  (owner1, _team1, [mem10]) <- createTeam OwnDomain 2
  _gid1 <- bindResponse (createUserGroup owner1 (object ["name" .= "none", "members" .= noMember])) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "name" `shouldMatch` "none"
    resp.json %. "members" `shouldMatch` noMember
    asString $ (resp.json %. "id")
  mem10id <- asString $ mem10 %. "id"

  bindResponse (addUsersToGroup owner1 gid0 [mem10id]) $ \resp -> do
    resp.status `shouldMatchInt` 404

  bindResponse (getUserGroup owner0 gid0) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "name" `shouldMatch` "none"
    resp.json %. "members" `shouldMatch` noMember

testUserGroupGetGroups :: (HasCallStack) => App ()
testUserGroupGetGroups = do
  (owner, _team, []) <- createTeam OwnDomain 1

  let groupNames = ["First group", "CC", "CCC"] <> ((: []) <$> ['A' .. 'G'])
      totalCount = length groupNames
  forM_ groupNames $ \gname -> do
    let newGroup = object ["name" .= gname, "members" .= ([] :: [()])]
    bindResponse (createUserGroup owner newGroup) $ \resp -> do
      resp.status `shouldMatchInt` 200
      resp.json %. "name" `shouldMatch` gname
      resp.json %. "members" `shouldMatch` ([] :: [()])

  -- Default sort by is createdAt, and sortOrder is DESC
  _ <- runSearch owner def {q = Just "C"} ["C", "CCC", "CC"] 3

  -- Default sortOrder is DESC, regardless of sortBy
  _ <- runSearch owner def {q = Just "CC", sortByKeys = Just "name"} ["CCC", "CC"] 2

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
      totalCount
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
      totalCount
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
      totalCount
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
      totalCount

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
      totalCount
  Just (name2, createdAt2, id2) <-
    runSearch
      owner
      firstPageParams {lastName = Just name1, lastCreatedAt = Just createdAt1, lastId = Just id1}
      [ "E",
        "D",
        "CCC"
      ]
      totalCount
  Just (name3, createdAt3, id3) <-
    runSearch
      owner
      firstPageParams {lastName = Just name2, lastCreatedAt = Just createdAt2, lastId = Just id2}
      [ "CC",
        "C",
        "B"
      ]
      totalCount

  void
    $ runSearch
      owner
      firstPageParams {lastName = Just name3, lastCreatedAt = Just createdAt3, lastId = Just id3}
      ["A"]
      totalCount

runSearch :: (HasCallStack, MakesValue owner) => owner -> GetUserGroupsArgs -> [String] -> Int -> App (Maybe (String, String, String))
runSearch owner args expected expectedCount =
  bindResponse (getUserGroups owner args) $ \resp -> do
    resp.status `shouldMatchInt` 200
    found <- ((%. "name") `mapM`) =<< asList =<< resp.json %. "page"
    found `shouldMatch` expected
    results <- asList $ resp.json %. "page"
    resp.json %. "total" `shouldMatchInt` expectedCount
    for (lastMay results) $ \lastGroup ->
      (,,)
        <$> asString (lastGroup %. "name")
        <*> asString (lastGroup %. "createdAt")
        <*> asString (lastGroup %. "id")

testUserGroupGetGroupsAllInputs :: (HasCallStack) => App ()
testUserGroupGetGroupsAllInputs = do
  (owner, _team, []) <- createTeam OwnDomain 1
  let gnames = ['A' .. 'Z']
  for_ gnames $ \gname -> do
    let newGroup = object ["name" .= gname, "members" .= ([] :: [()])]
    createUserGroup owner newGroup >>= assertSuccess

  Just (ln, ltz, lid) <- runSearch owner def {pSize = Just 3} ["Z", "Y", "X"] 26
  let getUserGroupArgs = getUserGroupArgsCombinations ln ltz lid
  for_ getUserGroupArgs $ \args -> do
    bindResponse (getUserGroups owner args) $ \resp -> do
      -- most important check is that all combinations return 200
      resp.status `shouldMatchInt` 200
      -- additionally we can check a few invariants
      groups <- resp.json %. "page" >>= asList
      case (args.q, args.lastName, args.lastCreatedAt, args.lastId) of
        (Nothing, Nothing, Nothing, Nothing) -> do
          length groups `shouldMatchInt` (fromMaybe 15 args.pSize)
          resp.json %. "total" `shouldMatchInt` (length gnames)
        (Just _, Nothing, Nothing, Nothing) -> do
          length groups `shouldMatchInt` 1
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
            includeMemberCount = includeMemberCount',
            includeChannels = includeChannels'
          }
        | q' <- qs,
          sortBy' <- sortByKeysList,
          sortOrder' <- sortOrders,
          pSize' <- pSizes,
          lastName' <- lastNames,
          lastCreatedAt' <- lastCreatedAts,
          lastId' <- lastIds,
          includeMemberCount' <- [False, True],
          includeChannels' <- [False, True]
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
    resp.json %. "total" `shouldMatchInt` 1

testUserGroupRemovalOnDelete :: (HasCallStack) => App ()
testUserGroupRemovalOnDelete = do
  (alice, tid, [bob, charlie]) <- createTeam OwnDomain 3

  bobId <- bob %. "id" & asString
  charlieId <- charlie %. "id" & asString

  ug <-
    createUserGroup alice (object ["name" .= "none", "members" .= [bobId, charlieId]])
      >>= getJSON 200
  gid <- ug %. "id" & asString
  void $ deleteTeamMember tid alice bob >>= getBody 202

  bindResponse (getUserGroup alice gid) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "members" `shouldMatch` [charlieId]

testUserGroupUpdateChannelsSucceeds :: (HasCallStack) => App ()
testUserGroupUpdateChannelsSucceeds = do
  (alice, tid, _) <- createTeam OwnDomain 1
  setTeamFeatureLockStatus alice tid "channels" "unlocked"
  setTeamFeatureConfig alice tid "channels" channelsConfig >>= assertSuccess

  ug <- createUserGroup alice (object ["name" .= "none", "members" .= (mempty :: [String])]) >>= getJSON 200
  gid <- ug %. "id" & asString

  convs <- replicateM 5 $ postConversation alice (defMLS {team = Just tid, groupConvType = Just "channel"}) >>= getJSON 201 >>= objConvId

  withWebSocket alice $ \wsAlice -> do
    updateUserGroupChannels alice gid ((.id_) <$> take 2 convs) >>= assertSuccess

    notif <- awaitMatch isUserGroupUpdatedNotif wsAlice
    notif %. "payload.0.user_group.id" `shouldMatch` gid

  bindResponse (getUserGroupWithChannels alice gid) $ \resp -> do
    resp.status `shouldMatchInt` 200
    actual <- resp.json %. "channels" >>= asList >>= traverse objQid
    actual `shouldMatchSet` for (take 2 convs) objQid

  bindResponse (getUserGroups alice (def {includeChannels = True})) $ \resp -> do
    resp.status `shouldMatchInt` 200
    (resp.json %. "page.0.channels" >>= asList >>= traverse objQid) `shouldMatchSet` for (take 2 convs) objQid

  updateUserGroupChannels alice gid ((.id_) <$> tail convs) >>= assertSuccess

  bindResponse (getUserGroupWithChannels alice gid) $ \resp -> do
    resp.status `shouldMatchInt` 200
    (resp.json %. "channels" >>= asList >>= traverse objQid) `shouldMatchSet` for (tail convs) objQid

  bindResponse (getUserGroups alice (def {includeChannels = True})) $ \resp -> do
    resp.status `shouldMatchInt` 200
    (resp.json %. "page.0.channels" >>= asList >>= traverse objQid) `shouldMatchSet` for (tail convs) objQid

  updateUserGroupChannels alice gid [] >>= assertSuccess

  bindResponse (getUserGroupWithChannels alice gid) $ \resp -> do
    resp.status `shouldMatchInt` 200
    (resp.json %. "channels" >>= fmap length . asList) `shouldMatchInt` 0

testUserGroupUpdateChannelsNonAdmin :: (HasCallStack) => App ()
testUserGroupUpdateChannelsNonAdmin = do
  (alice, tid, [bob]) <- createTeam OwnDomain 2

  ug <-
    createUserGroup alice (object ["name" .= "none", "members" .= (mempty :: [String])])
      >>= getJSON 200
  gid <- ug %. "id" & asString

  convId <-
    postConversation alice (defProteus {team = Just tid})
      >>= getJSON 201
      >>= objConvId
  updateUserGroupChannels bob gid [convId.id_] >>= assertLabel 404 "user-group-not-found"

testUserGroupUpdateChannelsNonExisting :: (HasCallStack) => App ()
testUserGroupUpdateChannelsNonExisting = do
  (alice, tid, _) <- createTeam OwnDomain 1
  (bob, _, _) <- createTeam OwnDomain 1

  ug <-
    createUserGroup alice (object ["name" .= "none", "members" .= (mempty :: [String])])
      >>= getJSON 200
  gid <- ug %. "id" & asString

  convId <-
    postConversation alice (defProteus {team = Just tid})
      >>= getJSON 201
      >>= objConvId
  updateUserGroupChannels bob gid [convId.id_] >>= assertLabel 404 "user-group-not-found"

testUserGroupUpdateChannelsNonChannel :: (HasCallStack) => App ()
testUserGroupUpdateChannelsNonChannel = do
  (alice, tid, [_bob]) <- createTeam OwnDomain 2

  ug <-
    createUserGroup alice (object ["name" .= "none", "members" .= (mempty :: [String])])
      >>= getJSON 200
  gid <- ug %. "id" & asString

  convId <-
    postConversation alice (defProteus {team = Just tid})
      >>= getJSON 201
      >>= objConvId
  updateUserGroupChannels alice gid [convId.id_] >>= assertLabel 404 "user-group-channel-not-found"

testUserGroupAddUsersToGroupWithChannels :: (HasCallStack) => App ()
testUserGroupAddUsersToGroupWithChannels = do
  (alice, tid, mems@[bob, charlie, dave, eve, franzi]) <- createTeam OwnDomain 6
  setTeamFeatureLockStatus alice tid "channels" "unlocked"
  setTeamFeatureConfig alice tid "channels" channelsConfig >>= assertSuccess

  [bobId, charlieId, daveId, eveId, franziId] <- for mems $ asString . (%. "id")

  -- Create user group with bob as initial member
  ug <- createUserGroup alice (object ["name" .= "test group", "members" .= [bobId]]) >>= getJSON 200
  gid <- ug %. "id" & asString

  -- Create two conversations (channels) for the team
  [convId1, convId2] <- replicateM 2 $ postConversation alice (defMLS {team = Just tid, groupConvType = Just "channel"}) >>= getJSON 201 >>= objConvId

  -- Associate both channels with the user group
  withWebSocket bob $ \bobWs -> do
    updateUserGroupChannels alice gid [convId1.id_, convId2.id_] >>= assertSuccess
    replicateM_ 2 $ awaitMatch isMemberJoinNotif bobWs

  for_ [convId1, convId2] $ \convId -> do
    bindResponse (getConversation alice convId) $ \resp -> do
      resp.status `shouldMatchInt` 200
      members <- resp.json %. "members" %. "others" >>= asList
      memberIds <- mapM ((%. "qualified_id") >=> (%. "id") >=> asString) members
      memberIds `shouldMatchSet` [bobId]

  -- Add charlie, dave, and eve to the group using addUsersToGroup
  withWebSockets [charlie, dave, eve] $ \wss -> do
    addUsersToGroup alice gid [charlieId, daveId, eveId] >>= assertSuccess
    for_ wss $ replicateM_ 2 . awaitMatch isMemberJoinNotif

  -- Verify all three users are now in the first channel
  for_ [convId1, convId2] $ \convId -> do
    bindResponse (getConversation alice convId) $ \resp -> do
      resp.status `shouldMatchInt` 200
      members <- resp.json %. "members" %. "others" >>= asList
      memberIds <- mapM ((%. "qualified_id") >=> (%. "id") >=> asString) members
      memberIds `shouldMatchSet` [bobId, charlieId, daveId, eveId]

    -- now we make charlie and dave admins in the conversation
    updateConversationMember alice convId charlie "wire_admin" >>= assertSuccess
    updateConversationMember alice convId dave "wire_admin" >>= assertSuccess

    bindResponse (getConversation alice convId) $ \resp -> do
      resp.status `shouldMatchInt` 200
      members <- resp.json %. "members" %. "others" >>= asList
      actual <- for members toIdRolePair
      let expected = [(bobId, "wire_member"), (charlieId, "wire_admin"), (daveId, "wire_admin"), (eveId, "wire_member")]
      actual `shouldMatchSet` expected

  -- when we now add another user, we expect roles not be overwritten
  withWebSockets [franzi] $ \wss -> do
    addUsersToGroup alice gid [franziId] >>= assertSuccess
    for_ wss $ replicateM_ 2 . awaitMatch isMemberJoinNotif

  for_ [convId1] $ \convId -> do
    bindResponse (getConversation alice convId) $ \resp -> do
      resp.status `shouldMatchInt` 200
      members <- resp.json %. "members" %. "others" >>= asList
      actual <- for members toIdRolePair
      let expected = [(bobId, "wire_member"), (charlieId, "wire_admin"), (daveId, "wire_admin"), (eveId, "wire_member"), (franziId, "wire_member")]
      actual `shouldMatchSet` expected
  where
    toIdRolePair :: Value -> App (String, String)
    toIdRolePair mem = (,) <$> (mem %. "qualified_id.id" & asString) <*> (mem %. "conversation_role" & asString)

channelsConfig :: Value
channelsConfig =
  object
    [ "status" .= "enabled",
      "config"
        .= object
          [ "allowed_to_create_channels" .= "team-members",
            "allowed_to_open_channels" .= "team-members"
          ]
    ]
