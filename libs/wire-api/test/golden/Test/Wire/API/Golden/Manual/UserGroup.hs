module Test.Wire.API.Golden.Manual.UserGroup where

import Data.Id
import Data.Json.Util
import Data.UUID qualified as UUID (fromString)
import Data.Vector
import Imports
import Wire.API.User
import Wire.API.UserGroup

-- TODO: do we want to enforce a minimum group name length?

userGroupId1, userGroupId2 :: UserGroupId
userGroupId1 = Id (fromJust (UUID.fromString "1ea397ae-1adc-11f0-93ea-ff4ed7af095b"))
userGroupId2 = Id (fromJust (UUID.fromString "19bdd268-1adc-11f0-9a71-d351719dd165"))

userId1, userId2 :: UserId
userId1 = Id (fromJust (UUID.fromString "1ea397ae-1adc-11f0-93ea-ff4ed7af095b"))
userId2 = Id (fromJust (UUID.fromString "19bdd268-1adc-11f0-9a71-d351719dd165"))

_someUTCTime :: UTCTimeMillis
Just _someUTCTime = readUTCTimeMillis "2025-04-16T16:22:21.703Z"

testObject_NewUserGroup_1 :: NewUserGroup
testObject_NewUserGroup_1 = NewUserGroup "some name @@~~ii" mempty

testObject_NewUserGroup_2 :: NewUserGroup
testObject_NewUserGroup_2 = NewUserGroup "some other name @@~~ii" (fromList [userId1, userId2])

testObject_UserGroupUpdate_1 :: UserGroupUpdate
testObject_UserGroupUpdate_1 = UserGroupUpdate ""

testObject_UserGroupUpdate_2 :: UserGroupUpdate
testObject_UserGroupUpdate_2 = UserGroupUpdate "some name @@~~ii"

testObject_UserGroup_1 :: UserGroup
testObject_UserGroup_1 = UserGroup userGroupId1 "name" mempty ManagedByWire -- someUTCTime

testObject_UserGroup_2 :: UserGroup
testObject_UserGroup_2 = UserGroup userGroupId2 "yet another one" (fromList [userId1, userId2]) ManagedByScim -- someUTCTime

testObject_UserGroupPage_1 :: UserGroupPage
testObject_UserGroupPage_1 = UserGroupPage [] False

testObject_UserGroupPage_2 :: UserGroupPage
testObject_UserGroupPage_2 = UserGroupPage [testObject_UserGroup_1, testObject_UserGroup_2] True
