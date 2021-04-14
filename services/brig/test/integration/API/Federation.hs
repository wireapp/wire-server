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

import Bilge hiding (head)
import Bilge.Assert
import Brig.Types
import Control.Arrow ((&&&))
import qualified Data.Aeson as Aeson
import Data.ByteString.Conversion (toByteString')
import Data.Id (Id (..), UserId)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.UUID.V4 as UUIDv4
import Imports
import Test.Tasty
import Test.Tasty.HUnit (assertEqual)
import Util
import Wire.API.Message (UserClientMap (..), UserClients (..))

tests :: Manager -> Brig -> IO TestTree
tests m brig =
  return $
    testGroup
      "federation"
      [ test m "GET /federation/users/by-handle : 200" (testGetUserByHandleSuccess brig),
        test m "GET /federation/users/by-handle : 404" (testGetUserByHandleNotFound brig),
        test m "GET /federation/users/get-by-id : 200 all found" (testGetUsersByIdsSuccess brig),
        test m "GET /federation/users/get-by-id : 200 partially found" (testGetUsersByIdsPartial brig),
        test m "GET /federation/users/get-by-id : 200 none found" (testGetUsersByIdsNoneFound brig),
        test m "GET /federation/users/prekey : 200" (testClaimPrekeySuccess brig),
        test m "GET /federation/users/prekey-bundle : 200" (testClaimPrekeyBundleSuccess brig),
        test m "POST /federation/users/multi-prekey-bundle : 200" (testClaimMultiPrekeyBundleSuccess brig)
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

testClaimPrekeySuccess :: Brig -> Http ()
testClaimPrekeySuccess brig = do
  user <- randomUser brig
  let uid = userId user
  let new = defNewClient PermanentClientType [head somePrekeys] (head someLastPrekeys)
  c <- responseJsonError =<< addClient brig uid new
  mkey <-
    responseJsonError
      =<< get
        ( brig
            . paths ["federation", "users", "prekey"]
            . queryItem "uid" (toByteString' uid)
            . queryItem "client" (toByteString' (clientId c))
            . expect2xx
        )
      <!! const 200 === statusCode
  liftIO $
    assertEqual
      "should return prekey 1"
      (Just (PrekeyId 1))
      (fmap (prekeyId . prekeyData) mkey)

generateClientPrekeys :: Brig -> [(Prekey, LastPrekey)] -> Http (UserId, [ClientPrekey])
generateClientPrekeys brig prekeys = do
  user <- randomUser brig
  let uid = userId user
      mkClient (pk, lpk) = defNewClient PermanentClientType [pk] lpk
      nclients = map mkClient prekeys
      mkClientPrekey (pk, _) c = ClientPrekey (clientId c) pk
  clients <- traverse (responseJsonError <=< addClient brig uid) nclients
  pure (uid, zipWith mkClientPrekey prekeys clients)

testClaimPrekeyBundleSuccess :: Brig -> Http ()
testClaimPrekeyBundleSuccess brig = do
  let prekeys = take 5 (zip somePrekeys someLastPrekeys)
  (uid, clients) <- generateClientPrekeys brig prekeys
  let sortClients = sortBy (compare `on` prekeyClient)
  get
    ( brig
        . paths ["federation", "users", "prekey-bundle"]
        . queryItem "uid" (toByteString' uid)
        . expect2xx
    )
    !!! do
      const 200 === statusCode
      const (Just (sortClients clients))
        === fmap (sortClients . prekeyClients) . responseJsonMaybe

testClaimMultiPrekeyBundleSuccess :: Brig -> Http ()
testClaimMultiPrekeyBundleSuccess brig = do
  let prekeys = zip somePrekeys someLastPrekeys
      (prekeys1, prekeys') = splitAt 5 prekeys
      prekeys2 = take 4 prekeys'
      mkClients = Set.fromList . map prekeyClient
      mkClientMap = Map.fromList . map (prekeyClient &&& prekeyData)
  c1 <- generateClientPrekeys brig prekeys1
  c2 <- generateClientPrekeys brig prekeys2
  let uc = UserClients (Map.fromList [mkClients <$> c1, mkClients <$> c2])
      ucm = UserClientMap (Map.fromList [mkClientMap <$> c1, mkClientMap <$> c2])
  post
    ( brig
        . paths ["federation", "users", "multi-prekey-bundle"]
        . body (RequestBodyLBS (Aeson.encode uc))
        . contentJson
        . acceptJson
        . expect2xx
    )
    !!! do
      const 200 === statusCode
      const (Just ucm) === responseJsonMaybe
