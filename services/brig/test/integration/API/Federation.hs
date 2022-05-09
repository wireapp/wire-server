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
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module API.Federation where

import API.MLS.Util
import API.Search.Util (refreshIndex)
import API.User.Util
import Bilge hiding (head)
import Bilge.Assert
import qualified Brig.Options as Opt
import Brig.Types
import Control.Arrow (Arrow (first), (&&&))
import Control.Lens ((?~))
import Data.Aeson
import Data.Domain (Domain (Domain))
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
import Test.QuickCheck hiding ((===))
import Test.Tasty
import qualified Test.Tasty.Cannon as WS
import Test.Tasty.HUnit
import UnliftIO.Temporary
import Util
import Web.HttpApiData
import Wire.API.Federation.API.Brig
import qualified Wire.API.Federation.API.Brig as FedBrig
import qualified Wire.API.Federation.API.Brig as S
import Wire.API.Federation.Component
import Wire.API.Federation.Version
import Wire.API.MLS.Credential
import Wire.API.MLS.KeyPackage
import Wire.API.Message (UserClients (..))
import Wire.API.User.Client (mkUserClientPrekeyMap)
import Wire.API.User.Search (FederatedUserSearchPolicy (..))
import Wire.API.UserMap (UserMap (UserMap))

-- Note: POST /federation/send-connection-action is implicitly tested in API.User.Connection
tests :: Manager -> Opt.Opts -> Brig -> Cannon -> FedClient 'Brig -> IO TestTree
tests m opts brig cannon fedBrigClient =
  return $
    testGroup
      "federation"
      [ test m "POST /federation/search-users : Found" (testSearchSuccess opts brig),
        test m "POST /federation/search-users : Found (fulltext)" (testFulltextSearchSuccess opts brig),
        test m "POST /federation/search-users : Found (multiple users)" (testFulltextSearchMultipleUsers opts brig),
        test m "POST /federation/search-users : NotFound" (testSearchNotFound opts),
        test m "POST /federation/search-users : Empty Input - NotFound" (testSearchNotFoundEmpty opts),
        test m "POST /federation/search-users : configured restrictions" (testSearchRestrictions opts brig),
        test m "POST /federation/get-user-by-handle : configured restrictions" (testGetUserByHandleRestrictions opts brig),
        test m "POST /federation/get-user-by-handle : Found" (testGetUserByHandleSuccess opts brig),
        test m "POST /federation/get-user-by-handle : NotFound" (testGetUserByHandleNotFound opts),
        test m "POST /federation/get-users-by-ids : 200 all found" (testGetUsersByIdsSuccess brig fedBrigClient),
        test m "POST /federation/get-users-by-ids : 200 partially found" (testGetUsersByIdsPartial brig fedBrigClient),
        test m "POST /federation/get-users-by-ids : 200 none found" (testGetUsersByIdsNoneFound fedBrigClient),
        test m "POST /federation/claim-prekey : 200" (testClaimPrekeySuccess brig fedBrigClient),
        test m "POST /federation/claim-prekey-bundle : 200" (testClaimPrekeyBundleSuccess brig fedBrigClient),
        test m "POST /federation/claim-multi-prekey-bundle : 200" (testClaimMultiPrekeyBundleSuccess brig fedBrigClient),
        test m "POST /federation/get-user-clients : 200" (testGetUserClients brig fedBrigClient),
        test m "POST /federation/get-user-clients : Not Found" (testGetUserClientsNotFound fedBrigClient),
        test m "POST /federation/on-user-deleted-connections : 200" (testRemoteUserGetsDeleted opts brig cannon fedBrigClient),
        test m "POST /federation/api-version : 200" (testAPIVersion brig fedBrigClient),
        test m "POST /federation/claim-key-packages : 200" (testClaimKeyPackages brig fedBrigClient)
      ]

allowFullSearch :: Domain -> Opt.Opts -> Opt.Opts
allowFullSearch domain opts =
  opts & Opt.optionSettings . Opt.federationDomainConfigs ?~ [Opt.FederationDomainConfig domain FullSearch]

testSearchSuccess :: Opt.Opts -> Brig -> Http ()
testSearchSuccess opts brig = do
  (handle, user) <- createUserWithHandle brig
  refreshIndex brig

  let quid = userQualifiedId user
  let domain = Domain "example.com"

  searchResponse <- withSettingsOverrides (allowFullSearch domain opts) $ do
    runWaiTestFedClient domain $
      createWaiTestFedClient @"search-users" @'Brig $
        SearchRequest (fromHandle handle)

  liftIO $ do
    let contacts = contactQualifiedId <$> S.contacts searchResponse
    assertEqual "should return the user id" [quid] contacts

testFulltextSearchSuccess :: Opt.Opts -> Brig -> Http ()
testFulltextSearchSuccess opts brig = do
  (_, user) <- createUserWithHandle brig
  refreshIndex brig

  let quid = userQualifiedId user
  let domain = Domain "example.com"

  searchResponse <- withSettingsOverrides (allowFullSearch domain opts) $ do
    runWaiTestFedClient domain $
      createWaiTestFedClient @"search-users" @'Brig $
        SearchRequest ((fromName . userDisplayName) user)

  liftIO $ do
    let contacts = contactQualifiedId <$> S.contacts searchResponse
    assertEqual "should return the user id" [quid] contacts

testFulltextSearchMultipleUsers :: Opt.Opts -> Brig -> Http ()
testFulltextSearchMultipleUsers opts brig = do
  (handle, user) <- createUserWithHandle brig

  let quid = userQualifiedId user

  -- Create another user with a display name matching the first user's handle.
  -- Both users should be returned in search results when freetext search is enabled.
  identityThief <- randomUser brig
  update'' :: UserUpdate <- liftIO $ generate arbitrary
  let update' = update'' {uupName = Just (Name (fromHandle handle))}
      update = RequestBodyLBS . encode $ update'
  put (brig . path "/self" . contentJson . zUser (userId identityThief) . zConn "c" . body update) !!! const 200 === statusCode

  refreshIndex brig

  let domain = Domain "example.com"

  searchResponse <- withSettingsOverrides (allowFullSearch domain opts) $ do
    runWaiTestFedClient domain $
      createWaiTestFedClient @"search-users" @'Brig $
        SearchRequest (fromHandle handle)

  liftIO $ do
    let contacts = contactQualifiedId <$> S.contacts searchResponse
    assertEqual "should find both users" (sort [quid, userQualifiedId identityThief]) (sort contacts)

testSearchNotFound :: Opt.Opts -> Http ()
testSearchNotFound opts = do
  let domain = Domain "example.com"

  searchResponse <- withSettingsOverrides (allowFullSearch domain opts) $ do
    runWaiTestFedClient domain $
      createWaiTestFedClient @"search-users" @'Brig $
        SearchRequest "this-handle-should-not-exist"

  liftIO $ assertEqual "should return empty array of users" [] (S.contacts searchResponse)

testSearchNotFoundEmpty :: Opt.Opts -> Http ()
testSearchNotFoundEmpty opts = do
  let domain = Domain "example.com"

  searchResponse <- withSettingsOverrides (allowFullSearch domain opts) $ do
    runWaiTestFedClient domain $
      createWaiTestFedClient @"search-users" @'Brig $
        SearchRequest "this-handle-should-not-exist"

  liftIO $ assertEqual "should return empty array of users" [] (S.contacts searchResponse)

testSearchRestrictions :: Opt.Opts -> Brig -> Http ()
testSearchRestrictions opts brig = do
  let domainNoSearch = Domain "no-search.example.com"
      domainExactHandle = Domain "exact-handle-only.example.com"
      domainFullSearch = Domain "full-search.example.com"
      domainUnconfigured = Domain "unconfigured.example.com"

  (handle, user) <- createUserWithHandle brig
  let quid = userQualifiedId user
  refreshIndex brig

  let opts' =
        opts & Opt.optionSettings . Opt.federationDomainConfigs
          ?~ [ Opt.FederationDomainConfig domainNoSearch NoSearch,
               Opt.FederationDomainConfig domainExactHandle ExactHandleSearch,
               Opt.FederationDomainConfig domainFullSearch FullSearch
             ]

  let expectSearch domain squery expectedUsers expectedSearchPolicy = do
        searchResponse <-
          runWaiTestFedClient domain $
            createWaiTestFedClient @"search-users" @'Brig (SearchRequest squery)
        liftIO $ assertEqual "Unexpected search result" expectedUsers (contactQualifiedId <$> S.contacts searchResponse)
        liftIO $ assertEqual "Unexpected search result" expectedSearchPolicy (S.searchPolicy searchResponse)

  withSettingsOverrides opts' $ do
    expectSearch domainNoSearch (fromHandle handle) [] NoSearch
    expectSearch domainExactHandle (fromHandle handle) [quid] ExactHandleSearch
    expectSearch domainExactHandle (fromName (userDisplayName user)) [] ExactHandleSearch
    expectSearch domainFullSearch (fromHandle handle) [quid] FullSearch
    expectSearch domainFullSearch (fromName (userDisplayName user)) [quid] FullSearch
    expectSearch domainUnconfigured (fromHandle handle) [] NoSearch
    expectSearch domainUnconfigured (fromName (userDisplayName user)) [] NoSearch

testGetUserByHandleRestrictions :: Opt.Opts -> Brig -> Http ()
testGetUserByHandleRestrictions opts brig = do
  let domainNoSearch = Domain "no-search.example.com"
      domainExactHandle = Domain "exact-handle-only.example.com"
      domainFullSearch = Domain "full-search.example.com"
      domainUnconfigured = Domain "unconfigured.example.com"

  (handle, user) <- createUserWithHandle brig
  let quid = userQualifiedId user
  refreshIndex brig

  let opts' =
        opts & Opt.optionSettings . Opt.federationDomainConfigs
          ?~ [ Opt.FederationDomainConfig domainNoSearch NoSearch,
               Opt.FederationDomainConfig domainExactHandle ExactHandleSearch,
               Opt.FederationDomainConfig domainFullSearch FullSearch
             ]

  let expectSearch domain expectedUser = do
        maybeUserProfile <-
          runWaiTestFedClient domain $
            createWaiTestFedClient @"get-user-by-handle" @'Brig handle
        liftIO $ assertEqual "Unexpected search result" expectedUser (profileQualifiedId <$> maybeUserProfile)

  withSettingsOverrides opts' $ do
    expectSearch domainNoSearch Nothing
    expectSearch domainExactHandle (Just quid)
    expectSearch domainFullSearch (Just quid)
    expectSearch domainUnconfigured Nothing

testGetUserByHandleSuccess :: Opt.Opts -> Brig -> Http ()
testGetUserByHandleSuccess opts brig = do
  (handle, user) <- createUserWithHandle brig
  let quid = userQualifiedId user
  let domain = Domain "example.com"

  maybeProfile <- withSettingsOverrides (allowFullSearch domain opts) $ do
    runWaiTestFedClient domain $
      createWaiTestFedClient @"get-user-by-handle" @'Brig $
        handle

  liftIO $ do
    case maybeProfile of
      Nothing -> assertFailure "Expected to find profile, found Nothing"
      Just profile -> do
        assertEqual "should return correct user Id" quid (profileQualifiedId profile)
        assertEqual "should not have email address" Nothing (profileEmail profile)

testGetUserByHandleNotFound :: Opt.Opts -> Http ()
testGetUserByHandleNotFound opts = do
  hdl <- randomHandle
  let domain = Domain "example.com"

  maybeProfile <- withSettingsOverrides (allowFullSearch domain opts) $ do
    runWaiTestFedClient domain $
      createWaiTestFedClient @"get-user-by-handle" @'Brig $
        Handle hdl

  liftIO $ assertEqual "should not return any UserProfile" Nothing maybeProfile

testGetUsersByIdsSuccess :: Brig -> FedClient 'Brig -> Http ()
testGetUsersByIdsSuccess brig fedBrigClient = do
  user1 <- randomUser brig
  user2 <- randomUser brig
  let uid1 = userId user1
      quid1 = userQualifiedId user1
      uid2 = userId user2
      quid2 = userQualifiedId user2
  profiles <- runFedClient @"get-users-by-ids" fedBrigClient (Domain "example.com") [uid1, uid2]
  liftIO $ do
    assertEqual "should return correct user Id" (Set.fromList [quid1, quid2]) (Set.fromList $ profileQualifiedId <$> profiles)
    assertEqual "should not have email address" [Nothing, Nothing] (map profileEmail profiles)

testGetUsersByIdsPartial :: Brig -> FedClient 'Brig -> Http ()
testGetUsersByIdsPartial brig fedBrigClient = do
  presentUser <- randomUser brig
  absentUserId :: UserId <- Id <$> lift UUIDv4.nextRandom
  profiles <-
    runFedClient @"get-users-by-ids" fedBrigClient (Domain "example.com") $
      [userId presentUser, absentUserId]
  liftIO $
    assertEqual "should return the present user and skip the absent ones" [userQualifiedId presentUser] (profileQualifiedId <$> profiles)

testGetUsersByIdsNoneFound :: FedClient 'Brig -> Http ()
testGetUsersByIdsNoneFound fedBrigClient = do
  absentUserId1 :: UserId <- Id <$> lift UUIDv4.nextRandom
  absentUserId2 :: UserId <- Id <$> lift UUIDv4.nextRandom
  profiles <- runFedClient @"get-users-by-ids" fedBrigClient (Domain "example.com") [absentUserId1, absentUserId2]
  liftIO $
    assertEqual "should return empty list" [] profiles

testClaimPrekeySuccess :: Brig -> FedClient 'Brig -> Http ()
testClaimPrekeySuccess brig fedBrigClient = do
  user <- randomUser brig
  let uid = userId user
  let new = defNewClient PermanentClientType [head somePrekeys] (head someLastPrekeys)
  c <- responseJsonError =<< addClient brig uid new
  mkey <- runFedClient @"claim-prekey" fedBrigClient (Domain "example.com") (uid, clientId c)
  liftIO $
    assertEqual
      "should return prekey 1"
      (Just (PrekeyId 1))
      (fmap (prekeyId . prekeyData) mkey)

testClaimPrekeyBundleSuccess :: Brig -> FedClient 'Brig -> Http ()
testClaimPrekeyBundleSuccess brig fedBrigClient = do
  let prekeys = take 5 (zip somePrekeys someLastPrekeys)
  (quid, clients) <- generateClientPrekeys brig prekeys
  let sortClients = sortBy (compare `on` prekeyClient)
  bundle <- runFedClient @"claim-prekey-bundle" fedBrigClient (Domain "example.com") (qUnqualified quid)
  liftIO $
    assertEqual
      "bundle should contain the clients"
      (sortClients clients)
      (sortClients . prekeyClients $ bundle)

testClaimMultiPrekeyBundleSuccess :: Brig -> FedClient 'Brig -> Http ()
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
  ucmResponse <- runFedClient @"claim-multi-prekey-bundle" fedBrigClient (Domain "example.com") uc
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

testGetUserClients :: Brig -> FedClient 'Brig -> Http ()
testGetUserClients brig fedBrigClient = do
  uid1 <- userId <$> randomUser brig
  clients :: [Client] <- addTestClients brig uid1 [0, 1, 2]
  UserMap userClients <- runFedClient @"get-user-clients" fedBrigClient (Domain "example.com") (GetUserClients [uid1])
  liftIO $
    assertEqual
      "client set for user should match"
      (Just (Set.fromList (fmap clientId clients)))
      (fmap (Set.map pubClientId) . Map.lookup uid1 $ userClients)

testGetUserClientsNotFound :: FedClient 'Brig -> Http ()
testGetUserClientsNotFound fedBrigClient = do
  absentUserId <- randomId
  UserMap userClients <- runFedClient @"get-user-clients" fedBrigClient (Domain "example.com") (GetUserClients [absentUserId])
  liftIO $
    assertEqual
      "client set for user should match"
      (Just (Set.fromList []))
      (fmap (Set.map pubClientId) . Map.lookup absentUserId $ userClients)

testRemoteUserGetsDeleted :: Opt.Opts -> Brig -> Cannon -> FedClient 'Brig -> Http ()
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
      runFedClient @"on-user-deleted-connections" fedBrigClient (qDomain remoteUser) $
        UserDeletedConnectionsNotification (qUnqualified remoteUser) (unsafeRange localUsers)

    WS.assertMatchN_ (5 # Second) [cc] $ matchDeleteUserNotification remoteUser
    WS.assertNoEvent (1 # Second) [pc, bc, uc]

  for_ localUsers $ \u ->
    getConnectionQualified brig u remoteUser !!! do
      const 404 === statusCode

testAPIVersion :: Brig -> FedClient 'Brig -> Http ()
testAPIVersion _brig fedBrigClient = do
  vinfo <- runFedClient @"api-version" fedBrigClient (Domain "far-away.example.com") ()
  liftIO $ vinfoSupported vinfo @?= toList supportedVersions

testClaimKeyPackages :: HasCallStack => Brig -> FedClient 'Brig -> Http ()
testClaimKeyPackages brig fedBrigClient = do
  alice <- fakeRemoteUser
  bob <- userQualifiedId <$> randomUser brig

  bobClients <- for (take 2 someLastPrekeys) $ \lpk -> do
    let new = defNewClient PermanentClientType [] lpk
    fmap clientId $ responseJsonError =<< addClient brig (qUnqualified bob) new

  withSystemTempFile "store.db" $ \store _ ->
    for_ bobClients $ \c ->
      uploadKeyPackages brig store SetKey bob c 2

  Just bundle <-
    runFedClient @"claim-key-packages" fedBrigClient (qDomain alice) $
      ClaimKeyPackageRequest (qUnqualified alice) (qUnqualified bob)

  liftIO $
    Set.map (\e -> (kpbeUser e, kpbeClient e)) (kpbEntries bundle)
      @?= Set.fromList [(bob, c) | c <- bobClients]

  -- check that we have one fewer key package now
  for_ bobClients $ \c -> do
    count <- getKeyPackageCount brig bob c
    liftIO $ count @?= 1

  -- check that the package refs are correctly mapped
  for_ (kpbEntries bundle) $ \e -> do
    cid <-
      responseJsonError
        =<< get (brig . paths ["i", "mls", "key-packages", toHeader (kpbeRef e)])
        <!! const 200 === statusCode
    liftIO $ do
      ciDomain cid @?= qDomain bob
      ciUser cid @?= qUnqualified bob
      ciClient cid @?= kpbeClient e
