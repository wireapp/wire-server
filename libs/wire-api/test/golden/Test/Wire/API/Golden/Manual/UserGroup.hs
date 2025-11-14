{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

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

module Test.Wire.API.Golden.Manual.UserGroup where

import Data.Domain
import Data.Id
import Data.Json.Util
import Data.Qualified (Qualified (Qualified))
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

testObject_UserGroup_1 :: UserGroupMeta
testObject_UserGroup_1 =
  UserGroup_
    { id_ = userGroupId1,
      name = unsafeToUserGroupName "name",
      members = Const (),
      channels = Nothing,
      membersCount = Nothing,
      channelsCount = Just 0,
      managedBy = ManagedByWire,
      createdAt = someUTCTime
    }

testObject_UserGroup_2 :: UserGroup
testObject_UserGroup_2 =
  UserGroup_
    { id_ = userGroupId2,
      name = unsafeToUserGroupName "yet another one",
      members = Identity $ fromList [userId1, userId2],
      channels =
        Just . fromList $
          [ Qualified (Id (fromJust (UUID.fromString "445c08d2-a16b-49ea-a274-4208bb2efe8f"))) (Domain "example.com")
          ],
      membersCount = Nothing,
      channelsCount = Just 1,
      managedBy = ManagedByScim,
      createdAt = someUTCTime
    }
