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
  _mem3id <- asString $ mem3 %. "id"

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

  bindResponse (getUserGroup owner gid) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "members" `shouldMatch` [mem1id, mem2id]
