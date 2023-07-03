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

module Test.Galley.Permissions where

import Galley.Types.Teams
import Imports
import Test.Tasty
import Test.Tasty.HUnit
import Wire.API.Team.Permission
import Wire.API.Team.Role

tests :: TestTree
tests =
  testGroup
    "Role, permsToInt"
    [ testCase "partner" $ assertEqual "" (permsToInt . _self $ rolePermissions RoleExternalPartner) 1025,
      testCase "member" $ assertEqual "" (permsToInt . _self $ rolePermissions RoleMember) 1587,
      testCase "admin" $ assertEqual "" (permsToInt . _self $ rolePermissions RoleAdmin) 5951,
      testCase "owner" $ assertEqual "" (permsToInt . _self $ rolePermissions RoleOwner) 8191
    ]
