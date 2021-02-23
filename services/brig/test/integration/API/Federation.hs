-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
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

module API.Federation where

import Bilge
import Bilge.Assert
import Brig.Types
import Data.ByteString.Conversion (toByteString')
import Imports
import Test.Tasty
import Util

tests :: Manager -> Brig -> IO TestTree
tests m brig = do
  return $
    testGroup "federation" $
      [ test m "GET /federation/users/by-handle : 200" (testGetUserByHandleSuccess brig),
        test m "GET /federation/users/by-handle : 404" (testGetUserByHandleNotFound brig)
      ]

testGetUserByHandleSuccess :: Brig -> Http ()
testGetUserByHandleSuccess brig = do
  user <- randomUser brig
  let uid = userId user
      quid = userQualifiedId user
  hdl <- randomHandle
  putHandle brig uid hdl
    !!! const 200 === statusCode
  get (brig . paths ["federation", "users", "by-handle"] . queryItem "handle" (toByteString' hdl)) !!! do
    const 200 === statusCode
    const (Just (UserHandleInfo quid)) === (responseJsonMaybe)

testGetUserByHandleNotFound :: Brig -> Http ()
testGetUserByHandleNotFound brig = do
  hdl <- randomHandle
  get (brig . paths ["federation", "users", "by-handle"] . queryItem "handle" (toByteString' hdl))
    !!! const 404 === statusCode
