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
import qualified Data.Aeson as Aeson
import Data.ByteString.Conversion (toByteString')
import Data.Id (Id (..), UserId)
import qualified Data.Set as Set
import qualified Data.UUID.V4 as UUIDv4
import Imports
import Test.Tasty
import Test.Tasty.HUnit (assertEqual)
import Util

tests :: Manager -> Brig -> IO TestTree
tests m brig = do
  return $
    testGroup "federation" $
      [ test m "GET /federation/users/by-handle : 200" (testGetUserByHandleSuccess brig),
        test m "GET /federation/users/by-handle : 404" (testGetUserByHandleNotFound brig),
        test m "GET /federation/users/get-by-id : 200 all found" (testGetUsersByIdsSuccess brig),
        test m "GET /federation/users/get-by-id : 200 partially found" (testGetUsersByIdsPartial brig),
        test m "GET /federation/users/get-by-id : 200 none found" (testGetUsersByIdsNoneFound brig)
      ]

testGetUserByHandleSuccess :: Brig -> Http ()
testGetUserByHandleSuccess brig = do
  user <- randomUser brig
  let uid = userId user
      quid = userQualifiedId user
  hdl <- randomHandle
  putHandle brig uid hdl
    !!! const 200 === statusCode
  profile <-
    responseJsonError
      =<< get
        ( brig
            . paths ["federation", "users", "by-handle"]
            . queryItem "handle" (toByteString' hdl)
            . expect2xx
        )
  liftIO $ do
    assertEqual "should return correct user Id" quid (profileQualifiedId profile)
    assertEqual "should not have email address" Nothing (profileEmail profile)

testGetUserByHandleNotFound :: Brig -> Http ()
testGetUserByHandleNotFound brig = do
  hdl <- randomHandle
  get (brig . paths ["federation", "users", "by-handle"] . queryItem "handle" (toByteString' hdl))
    !!! const 404 === statusCode

testGetUsersByIdsSuccess :: Brig -> Http ()
testGetUsersByIdsSuccess brig = do
  user1 <- randomUser brig
  user2 <- randomUser brig
  let uid1 = userId user1
      quid1 = userQualifiedId user1
      uid2 = userId user2
      quid2 = userQualifiedId user2
  profiles <-
    responseJsonError
      =<< post
        ( brig
            . paths ["federation", "users", "get-by-ids"]
            . body (RequestBodyLBS (Aeson.encode [uid1, uid2]))
            . contentJson
            . acceptJson
            . expect2xx
        )
  liftIO $ do
    assertEqual "should return correct user Id" (Set.fromList [quid1, quid2]) (Set.fromList $ profileQualifiedId <$> profiles)
    assertEqual "should not have email address" [Nothing, Nothing] (map profileEmail profiles)

testGetUsersByIdsPartial :: Brig -> Http ()
testGetUsersByIdsPartial brig = do
  presentUser <- randomUser brig
  absentUserId :: UserId <- Id <$> lift UUIDv4.nextRandom
  profiles <-
    responseJsonError
      =<< post
        ( brig
            . paths ["federation", "users", "get-by-ids"]
            . body (RequestBodyLBS (Aeson.encode [userId presentUser, absentUserId]))
            . contentJson
            . acceptJson
            . expect2xx
        )
  liftIO $
    assertEqual "should return the present user and skip the absent ones" [userQualifiedId presentUser] (profileQualifiedId <$> profiles)

testGetUsersByIdsNoneFound :: Brig -> Http ()
testGetUsersByIdsNoneFound brig = do
  absentUserId1 :: UserId <- Id <$> lift UUIDv4.nextRandom
  absentUserId2 :: UserId <- Id <$> lift UUIDv4.nextRandom
  profiles :: [UserProfile] <-
    responseJsonError
      =<< post
        ( brig
            . paths ["federation", "users", "get-by-ids"]
            . body (RequestBodyLBS (Aeson.encode [absentUserId1, absentUserId2]))
            . contentJson
            . acceptJson
            . expect2xx
        )
  liftIO $
    assertEqual "should return empty list" [] profiles
