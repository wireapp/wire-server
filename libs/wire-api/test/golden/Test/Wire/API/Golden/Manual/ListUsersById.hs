{-# LANGUAGE OverloadedLists #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
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

module Test.Wire.API.Golden.Manual.QualifiedUserClientPrekeyMap where

import Data.Domain
import GHC.Exts (IsList (fromList))
import Imports
import Test.Wire.API.Golden.Manual.UserClientPrekeyMap
import Wire.API.User.Client (QualifiedUserClientPrekeyMap, mkQualifiedUserClientPrekeyMap)

domain1, domain2 :: Domain
domain1 = Domain "example.com"
domain2 = Domain "test.net"

user1, user2 :: UserId
user1 = Id . fromJust $ UUID.fromString "4f201a43-935e-4e19-8fe0-0a878d3d6e74"
user2 = Id . fromJust $ UUID.fromString "eb48b095-d96f-4a94-b4ec-2a1d61447e13"

profile1, profile2 :: UserProfile
profile1 = UserProfile
  { userId = user1
  , profileQualifiedId = Qualified user1 domain1
  , userIdentity = Nothing
  , profileName = Name "user1"
  , userPict = Pict []
  , userAssets = []
  , userAccentId = ColourId 0
  , userDeleted = false
  , userLocale = "AU"
  , userService = Nothing
  , userHandle = Nothing
  , userExpire = Nothing
  , userTeam = Nothing
  , userManagedBy = ManagedByWire
  }
profile2 = UserProfile
  { userId = user2
  , profileQualifiedId = Qualified user2 domain2
  , userIdentity = Nothing
  , profileName = Name "user2"
  , userPict = Pict []
  , userAssets = []
  , userAccentId = ColourId 0
  , userDeleted = false
  , userLocale = "AU"
  , userService = Nothing
  , userHandle = Nothing
  , userExpire = Nothing
  , userTeam = Nothing
  , userManagedBy = ManagedByWire
  }

testObject_ListUsersById_1 :: ListUsersById
testObject_ListUsersById_1 = ListUsersById mempty Nothing

testObject_QualifiedUserClientPrekeyMap_2 :: ListUsersById
testObject_QualifiedUserClientPrekeyMap_2 = ListUsersById
  { listUsersByIdFound = [profile1, profile2]
  , listUsersByIdFailed = Just []
  }

testObject_QualifiedUserClientPrekeyMap_3 :: ListUsersById
testObject_QualifiedUserClientPrekeyMap_3 = ListUsersById
  { listUsersByIdFound = [profile1]
  , listUsersByIdFailed = pure $ [Qualified user2 domain2]
  }