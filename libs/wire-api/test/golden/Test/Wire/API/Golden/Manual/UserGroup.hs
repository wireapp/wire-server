{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Test.Wire.API.Golden.Manual.UserGroup where

import Data.Id
import Data.Json.Util
import Data.UUID qualified as UUID (fromString)
import Data.Vector
import Imports
import Wire.API.User
import Wire.API.UserGroup

userGroupId1, userGroupId2 :: UserGroupId
userGroupId1 = Id (fromJust (UUID.fromString "1ea397ae-1adc-11f0-93ea-ff4ed7af095b"))
userGroupId2 = Id (fromJust (UUID.fromString "19bdd268-1adc-11f0-9a71-d351719dd165"))

userId1, userId2 :: UserId
userId1 = Id (fromJust (UUID.fromString "1ea397ae-1adc-11f0-93ea-ff4ed7af095b"))
userId2 = Id (fromJust (UUID.fromString "19bdd268-1adc-11f0-9a71-d351719dd165"))

someUTCTime :: UTCTimeMillis
Just someUTCTime = readUTCTimeMillis "2025-04-16T16:22:21.703Z"

unsafeToUserGroupName :: Text -> UserGroupName
unsafeToUserGroupName = either (error . show) id . userGroupNameFromText

testObject_NewUserGroup_1 :: NewUserGroup
testObject_NewUserGroup_1 = NewUserGroup (unsafeToUserGroupName "some name @@~~ii") mempty

testObject_NewUserGroup_2 :: NewUserGroup
testObject_NewUserGroup_2 = NewUserGroup (unsafeToUserGroupName "some other name @@~~ii") (fromList [userId1, userId2])

testObject_UserGroupUpdate_1 :: UserGroupUpdate
testObject_UserGroupUpdate_1 = UserGroupUpdate (unsafeToUserGroupName " ")

testObject_UserGroupUpdate_2 :: UserGroupUpdate
testObject_UserGroupUpdate_2 = UserGroupUpdate (unsafeToUserGroupName "some name @@~~ii")

testObject_UserGroup_1 :: UserGroup
testObject_UserGroup_1 = UserGroup userGroupId1 (unsafeToUserGroupName "name") mempty ManagedByWire someUTCTime

testObject_UserGroup_2 :: UserGroup
testObject_UserGroup_2 = UserGroup userGroupId2 (unsafeToUserGroupName "yet another one") (fromList [userId1, userId2]) ManagedByScim someUTCTime

testObject_UserGroupPage_1 :: UserGroupPage
testObject_UserGroupPage_1 = UserGroupPage [] False

testObject_UserGroupPage_2 :: UserGroupPage
testObject_UserGroupPage_2 = UserGroupPage [testObject_UserGroup_1, testObject_UserGroup_2] True
