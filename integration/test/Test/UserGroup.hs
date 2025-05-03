{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Test.UserGroup where

import API.Brig
import SetupHelpers
import Testlib.Prelude

testUserGroupSmoke :: (HasCallStack) => App ()
testUserGroupSmoke = do
  (owner, _team, [mem1, mem2, mem3]) <- createTeam OwnDomain 4
  mem1id <- asString $ mem1 %. "id"
  mem2id <- asString $ mem2 %. "id"
  _mem3id <- asString $ mem3 %. "id"

  let badGid = "225c4d54-1ae7-11f0-8e9c-cbb31865d602"

  gid <- bindResponse (createUserGroup owner (object ["name" .= "none", "members" .= ([mem1id, mem2id])])) $ \resp -> do
    resp.status `shouldMatchInt` 201
    resp.json %. "name" `shouldMatch` "none"
    resp.json %. "members" `shouldMatch` [mem1id, mem2id]
    asString $ (resp.json %. "id")

  bindResponse (getUserGroup owner badGid) $ \resp -> do
    resp.status `shouldMatchInt` 404

  bindResponse (getUserGroup owner gid) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "members" `shouldMatch` [mem1id, mem2id]

{- TODO: make this not a comment.

  bindResponse (getUserGroups owner (Just 1) Nothing) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "members" `shouldMatch` [mem1id]

  bindResponse (getUserGroups owner (Just 3) (Just mem1id)) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "page" `shouldMatch` [mem2id]
    resp.json %. "hasMore" `shouldMatch` False

  bindResponse (updateUserGroup owner badGid (object ["name" .= ""])) $ \resp -> do
    resp.status `shouldMatchInt` 404

  bindResponse (updateUserGroup owner gid (object ["name" .= "still none"])) $ \resp -> do
    resp.status `shouldMatchInt` 200

  bindResponse (deleteUserGroup owner badGid) $ \resp -> do
    resp.status `shouldMatchInt` 204

  bindResponse (deleteUserGroup owner gid) $ \resp -> do
    resp.status `shouldMatchInt` 204

  bindResponse (addUserToGroup owner gid mem3id) $ \resp -> do
    resp.status `shouldMatchInt` 200

  bindResponse (removeUserFromGroup owner gid mem1id) $ \resp -> do
    resp.status `shouldMatchInt` 200

  bindResponse (getUserGroup owner gid) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "members" `shouldMatch` [mem2id, mem3id]
-}

-- TODO: check 403 when mem1 is calling
-- TODO: events!
