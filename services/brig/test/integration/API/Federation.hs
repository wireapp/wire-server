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
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module API.Federation where

import API.Search.Util (refreshIndex)
import API.User.Util
import Bilge hiding (head)
import Bilge.Assert
import qualified Brig.Options as Opt
import Brig.Types
import Control.Arrow (Arrow (first), (&&&))
import Data.Aeson (encode)
import Data.Handle (Handle (..))
import Data.Id
import qualified Data.Map as Map
import Data.Qualified
import Data.Range
import qualified Data.Set as Set
import Data.Timeout
import qualified Data.UUID.V4 as UUIDv4
import Federation.Util (generateClientPrekeys)
import Imports
import Test.QuickCheck (arbitrary)
import Test.QuickCheck.Gen (generate)
import Test.Tasty
import qualified Test.Tasty.Cannon as WS
import Test.Tasty.HUnit (assertEqual, assertFailure)
import Util
import Wire.API.Federation.API.Brig (GetUserClients (..), SearchRequest (SearchRequest), UserDeletedNotification (..))
import qualified Wire.API.Federation.API.Brig as FedBrig
import Wire.API.Message (UserClients (..))
import Wire.API.User.Client (mkUserClientPrekeyMap)
import Wire.API.UserMap (UserMap (UserMap))

-- Note: POST /federation/send-connection-action is implicitly tested in API.User.Connection
tests :: Manager -> Opt.Opts -> Brig -> Cannon -> FedBrigClient -> IO TestTree
tests m opts brig cannon fedBrigClient =
  return $
    testGroup
      "federation"
      [ test m "POST /federation/search-users : Found" (testSearchSuccess brig fedBrigClient),
        test m "POST /federation/search-users : NotFound" (testSearchNotFound fedBrigClient),
        test m "POST /federation/search-users : Empty Input - NotFound" (testSearchNotFoundEmpty fedBrigClient),
        test m "POST /federation/get-user-by-handle : Found" (testGetUserByHandleSuccess brig fedBrigClient),
        test m "POST /federation/get-user-by-handle : NotFound" (testGetUserByHandleNotFound fedBrigClient),
        test m "POST /federation/get-users-by-ids : 200 all found" (testGetUsersByIdsSuccess brig fedBrigClient),
        test m "POST /federation/get-users-by-ids : 200 partially found" (testGetUsersByIdsPartial brig fedBrigClient),
        test m "POST /federation/get-users-by-ids : 200 none found" (testGetUsersByIdsNoneFound fedBrigClient),
        test m "POST /federation/claim-prekey : 200" (testClaimPrekeySuccess brig fedBrigClient),
        test m "POST /federation/claim-prekey-bundle : 200" (testClaimPrekeyBundleSuccess brig fedBrigClient),
        test m "POST /federation/claim-multi-prekey-bundle : 200" (testClaimMultiPrekeyBundleSuccess brig fedBrigClient),
        test m "POST /federation/get-user-clients : 200" (testGetUserClients brig fedBrigClient),
        test m "POST /federation/get-user-clients : Not Found" (testGetUserClientsNotFound fedBrigClient),
        test m "POST /federation/on-user-deleted : 200" (testRemoteUserGetsDeleted opts brig cannon fedBrigClient)
      ]

testSearchSuccess :: Brig -> FedBrigClient -> Http ()
testSearchSuccess brig fedBrigClient = do
  (handle, user) <- createUserWithHandle brig
  let quid = userQualifiedId user

  -- create another user with a similar handle and the same display name
  -- That user should not be returned in search results.
  -- (as federated search should only search for exact handle matches)
  identityThief <- randomUser brig
  void $ putHandle brig (userId identityThief) (fromHandle handle <> "a")
  update'' :: UserUpdate <- liftIO $ generate arbitrary
  let update' = update'' {uupName = Just (Name (fromHandle handle))}
      update = RequestBodyLBS . encode $ update'
  put (brig . path "/self" . contentJson . zUser (userId identityThief) . zConn "c" . body update) !!! const 200 === statusCode
  refreshIndex brig

  searchResult <- FedBrig.searchUsers fedBrigClient (SearchRequest (fromHandle handle))
  liftIO $ do
    let contacts = contactQualifiedId <$> searchResult
    assertEqual "should return only the first user id but not the identityThief" [quid] contacts

testSearchNotFound :: FedBrigClient -> Http ()
testSearchNotFound fedBrigClient = do
  searchResult <- FedBrig.searchUsers fedBrigClient $ SearchRequest "this-handle-should-not-exist"
  liftIO $ assertEqual "should return empty array of users" [] searchResult

testSearchNotFoundEmpty :: FedBrigClient -> Http ()
testSearchNotFoundEmpty fedBrigClient = do
  searchResult <- FedBrig.searchUsers fedBrigClient $ SearchRequest ""
  liftIO $ assertEqual "should return empty array of users" [] searchResult

testGetUserByHandleSuccess :: Brig -> FedBrigClient -> Http ()
testGetUserByHandleSuccess brig fedBrigClient = do
  (handle, user) <- createUserWithHandle brig
  let quid = userQualifiedId user
  maybeProfile <- FedBrig.getUserByHandle fedBrigClient handle
  liftIO $ do
    case maybeProfile of
      Nothing -> assertFailure "Expected to find profile, found Nothing"
      Just profile -> do
        assertEqual "should return correct user Id" quid (profileQualifiedId profile)
        assertEqual "should not have email address" Nothing (profileEmail profile)

testGetUserByHandleNotFound :: FedBrigClient -> Http ()
testGetUserByHandleNotFound fedBrigClient = do
  hdl <- randomHandle
  maybeProfile <- FedBrig.getUserByHandle fedBrigClient (Handle hdl)
  liftIO $ assertEqual "should not return any UserProfile" Nothing maybeProfile

testGetUsersByIdsSuccess :: Brig -> FedBrigClient -> Http ()
testGetUsersByIdsSuccess brig fedBrigClient = do
  user1 <- randomUser brig
  user2 <- randomUser brig
  let uid1 = userId user1
      quid1 = userQualifiedId user1
      uid2 = userId user2
      quid2 = userQualifiedId user2
  profiles <- FedBrig.getUsersByIds fedBrigClient [uid1, uid2]
  liftIO $ do
    assertEqual "should return correct user Id" (Set.fromList [quid1, quid2]) (Set.fromList $ profileQualifiedId <$> profiles)
    assertEqual "should not have email address" [Nothing, Nothing] (map profileEmail profiles)

testGetUsersByIdsPartial :: Brig -> FedBrigClient -> Http ()
testGetUsersByIdsPartial brig fedBrigClient = do
  presentUser <- randomUser brig
  absentUserId :: UserId <- Id <$> lift UUIDv4.nextRandom
  profiles <- FedBrig.getUsersByIds fedBrigClient [userId presentUser, absentUserId]
  liftIO $
    assertEqual "should return the present user and skip the absent ones" [userQualifiedId presentUser] (profileQualifiedId <$> profiles)

testGetUsersByIdsNoneFound :: FedBrigClient -> Http ()
testGetUsersByIdsNoneFound fedBrigClient = do
  absentUserId1 :: UserId <- Id <$> lift UUIDv4.nextRandom
  absentUserId2 :: UserId <- Id <$> lift UUIDv4.nextRandom
  profiles <- FedBrig.getUsersByIds fedBrigClient [absentUserId1, absentUserId2]
  liftIO $
    assertEqual "should return empty list" [] profiles

testClaimPrekeySuccess :: Brig -> FedBrigClient -> Http ()
testClaimPrekeySuccess brig fedBrigClient = do
  user <- randomUser brig
  let uid = userId user
  let new = defNewClient PermanentClientType [head somePrekeys] (head someLastPrekeys)
  c <- responseJsonError =<< addClient brig uid new
  mkey <- FedBrig.claimPrekey fedBrigClient (uid, clientId c)
  liftIO $
    assertEqual
      "should return prekey 1"
      (Just (PrekeyId 1))
      (fmap (prekeyId . prekeyData) mkey)

testClaimPrekeyBundleSuccess :: Brig -> FedBrigClient -> Http ()
testClaimPrekeyBundleSuccess brig fedBrigClient = do
  let prekeys = take 5 (zip somePrekeys someLastPrekeys)
  (quid, clients) <- generateClientPrekeys brig prekeys
  let sortClients = sortBy (compare `on` prekeyClient)
  bundle <- FedBrig.claimPrekeyBundle fedBrigClient (qUnqualified quid)
  liftIO $
    assertEqual
      "bundle should contain the clients"
      (sortClients clients)
      (sortClients . prekeyClients $ bundle)

testClaimMultiPrekeyBundleSuccess :: Brig -> FedBrigClient -> Http ()
testClaimMultiPrekeyBundleSuccess brig fedBrigClient = do
  let prekeys = zip somePrekeys someLastPrekeys
      (prekeys1, prekeys') = splitAt 5 prekeys
      prekeys2 = take 4 prekeys'
      mkClients = Set.fromList . map prekeyClient
      mkClientMap = Map.fromList . map (prekeyClient &&& Just . prekeyData)
  c1 <- first qUnqualified <$> generateClientPrekeys brig prekeys1
  c2 <- first qUnqualified <$> generateClientPrekeys brig prekeys2
  let uc = UserClients (Map.fromList [mkClients <$> c1, mkClients <$> c2])
      ucm = mkUserClientPrekeyMap (Map.fromList [mkClientMap <$> c1, mkClientMap <$> c2])
  ucmResponse <- FedBrig.claimMultiPrekeyBundle fedBrigClient uc
  liftIO $
    assertEqual
      "should return the UserClientMap"
      ucm
      ucmResponse

addTestClients :: Brig -> UserId -> [Int] -> Http [Client]
addTestClients brig uid idxs =
  for idxs $ \idx -> do
    let (pk, lk) = (somePrekeys !! idx, someLastPrekeys !! idx)
    client :: Client <- responseJsonError =<< addClient brig uid (defNewClient PermanentClientType [pk] lk)
    pure client

testGetUserClients :: Brig -> FedBrigClient -> Http ()
testGetUserClients brig fedBrigClient = do
  uid1 <- userId <$> randomUser brig
  clients :: [Client] <- addTestClients brig uid1 [0, 1, 2]
  UserMap userClients <- FedBrig.getUserClients fedBrigClient (GetUserClients [uid1])
  liftIO $
    assertEqual
      "client set for user should match"
      (Just (Set.fromList (fmap clientId clients)))
      (fmap (Set.map pubClientId) . Map.lookup uid1 $ userClients)

testGetUserClientsNotFound :: FedBrigClient -> Http ()
testGetUserClientsNotFound fedBrigClient = do
  absentUserId <- randomId
  UserMap userClients <- FedBrig.getUserClients fedBrigClient (GetUserClients [absentUserId])
  liftIO $
    assertEqual
      "client set for user should match"
      (Just (Set.fromList []))
      (fmap (Set.map pubClientId) . Map.lookup absentUserId $ userClients)

testRemoteUserGetsDeleted :: Opt.Opts -> Brig -> Cannon -> FedBrigClient -> Http ()
testRemoteUserGetsDeleted opts brig cannon fedBrigClient = do
  connectedUser <- userId <$> randomUser brig
  pendingUser <- userId <$> randomUser brig
  blockedUser <- userId <$> randomUser brig
  unconnectedUser <- userId <$> randomUser brig
  remoteUser <- fakeRemoteUser

  sendConnectionAction brig opts connectedUser remoteUser (Just FedBrig.RemoteConnect) Accepted
  receiveConnectionAction brig fedBrigClient pendingUser remoteUser FedBrig.RemoteConnect Nothing Pending
  sendConnectionAction brig opts blockedUser remoteUser (Just FedBrig.RemoteConnect) Accepted
  putConnectionQualified brig blockedUser remoteUser Blocked !!! statusCode === const 200

  let localUsers = [connectedUser, pendingUser, blockedUser, unconnectedUser]
  void . WS.bracketRN cannon localUsers $ \[cc, pc, bc, uc] -> do
    _ <-
      FedBrig.onUserDeleted
        fedBrigClient
        (qDomain remoteUser)
        (UserDeletedNotification (qUnqualified remoteUser) (unsafeRange localUsers))

    WS.assertMatchN_ (5 # Second) [cc] $ matchDeleteUserNotification remoteUser
    WS.assertNoEvent (1 # Second) [pc, bc, uc]

  for_ localUsers $ \u ->
    getConnectionQualified brig u remoteUser !!! do
      const 404 === statusCode
