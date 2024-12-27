{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- Disabling to stop warnings on HasCallStack
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

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

module API.Teams.LegalHold (tests) where

import API.Teams.LegalHold.Util
import API.Util
import Bilge hiding (accept, head, timeout, trace)
import Bilge.Assert
import Brig.Types.Test.Arbitrary ()
import Control.Concurrent.Chan
import Control.Lens hiding ((#))
import Data.Id
import Data.LegalHold
import Data.PEM
import Data.Range
import Data.Time.Clock qualified as Time
import Galley.Cassandra.LegalHold
import Galley.Env qualified as Galley
import Imports
import Network.HTTP.Types.Status (status200, status404)
import Network.Wai as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Test.QuickCheck.Instances ()
import Test.Tasty
import Test.Tasty.HUnit
import TestHelpers
import TestSetup
import Wire.API.Connection qualified as Conn
import Wire.API.Provider.Service
import Wire.API.Routes.Internal.Brig.Connection
import Wire.API.Team.LegalHold
import Wire.API.Team.Member
import Wire.API.Team.Permission
import Wire.API.Team.Role
import Wire.API.User.Client

tests :: IO TestSetup -> TestTree
tests s = testGroup "Legalhold" [testsPublic s, testsInternal s]

testsPublic :: IO TestSetup -> TestTree
testsPublic s =
  -- See also Client Tests in Brig; where behaviour around deleting/adding LH clients is tested
  -- These tests should all go to /integration/test/Test/LegalHold.hs (which should be cleaned up to tell a coherent story).
  testGroup
    "Teams LegalHold API (with flag whitelist-teams-and-implicit-consent)"
    [ -- legal hold settings
      testOnlyIfLhWhitelisted s "POST /teams/{tid}/legalhold/settings" testCreateLegalHoldTeamSettings,
      testOnlyIfLhWhitelisted s "Not implemented: DELETE /teams/{tid}/legalhold/settings" testRemoveLegalHoldFromTeam,
      -- behavior of existing end-points
      testOnlyIfLhWhitelisted s "POST /clients" testCannotCreateLegalHoldDeviceOldAPI,
      testOnlyIfLhWhitelisted s "POST /register - can add team members above fanout limit when whitelisting is enabled" testAddTeamUserTooLargeWithLegalholdWhitelisted,
      {- TODO:
          conversations/{cnv}/otr/messages - possibly show the legal hold device (if missing) as a different device type (or show that on device level, depending on how client teams prefer)
          GET /team/{tid}/members - show legal hold status of all members

      -}
      test s "settings.legalholdEnabledTeams teams liested bench hack" testBenchHack
    ]

testsInternal :: IO TestSetup -> TestTree
testsInternal s =
  testGroup
    "Legalhold Internal API"
    [testOnlyIfLhWhitelisted s "PUT, DELETE /i/legalhold/whitelisted-teams" testWhitelistingTeams]

testWhitelistingTeams :: TestM ()
testWhitelistingTeams = do
  let testTeamWhitelisted :: (HasCallStack) => TeamId -> TestM Bool
      testTeamWhitelisted tid = do
        res <- getLHWhitelistedTeam tid
        pure (Bilge.responseStatus res == status200)

  let expectWhitelisted :: (HasCallStack) => Bool -> TeamId -> TestM ()
      expectWhitelisted yes tid = do
        let msg = if yes then "team should be whitelisted" else "team should not be whitelisted"
        aFewTimesAssertBool msg (== yes) (testTeamWhitelisted tid)

  tid <- withTeam $ \_owner tid -> do
    expectWhitelisted False tid
    putLHWhitelistTeam tid !!! const 200 === statusCode
    expectWhitelisted True tid
    pure tid

  expectWhitelisted False tid

data IsWorking = Working | NotWorking
  deriving (Eq, Show)

testCreateLegalHoldTeamSettings :: TestM ()
testCreateLegalHoldTeamSettings = withTeam $ \owner tid -> do
  putLHWhitelistTeam tid !!! const 200 === statusCode
  member <- randomUser
  addTeamMemberInternal tid member (rolePermissions RoleMember) Nothing
  -- Random port, hopefully nothing is runing here!
  brokenService <- newLegalHoldService 4242
  -- not allowed to create if team is not whitelisted
  postSettings owner tid brokenService !!! testResponse 412 (Just "legalhold-unavailable")

  putLHWhitelistTeam tid !!! const 200 === statusCode

  -- not allowed for users with corresp. permission bit missing
  postSettings member tid brokenService !!! testResponse 403 (Just "operation-denied")
  -- rejected if service is not available
  postSettings owner tid brokenService !!! testResponse 412 (Just "legalhold-unavailable")
  -- checks /status of legal hold service (boolean argument says whether the service is
  -- behaving or not)
  let lhapp :: (HasCallStack) => IsWorking -> Chan Void -> Application
      lhapp NotWorking _ _ cont = cont respondBad
      lhapp Working _ req cont = do
        if
          | pathInfo req /= ["legalhold", "status"] -> cont respondBad
          | requestMethod req /= "GET" -> cont respondBad
          | otherwise -> cont respondOk
      respondOk :: Wai.Response
      respondOk = responseLBS status200 mempty mempty
      respondBad :: Wai.Response
      respondBad = responseLBS status404 mempty mempty
      lhtest :: (HasCallStack) => IsWorking -> Warp.Port -> Chan Void -> TestM ()
      lhtest NotWorking _ _ = do
        postSettings owner tid brokenService !!! testResponse 412 (Just "legalhold-unavailable")
      lhtest Working lhPort _ = do
        let Right [k] = pemParseBS "-----BEGIN PUBLIC KEY-----\n\n-----END PUBLIC KEY-----"
        newService <- newLegalHoldService lhPort
        let badServiceBadKey = newService {newLegalHoldServiceKey = ServiceKeyPEM k}
        postSettings owner tid badServiceBadKey !!! testResponse 400 (Just "legalhold-invalid-key")
        postSettings owner tid newService !!! testResponse 201 Nothing
        postSettings owner tid newService !!! testResponse 201 Nothing -- it's idempotent
        ViewLegalHoldService service <- getSettingsTyped owner tid
        liftIO $ do
          Just (_, fpr) <- validateServiceKey (newLegalHoldServiceKey newService)
          assertEqual "viewLegalHoldTeam" tid (viewLegalHoldServiceTeam service)
          assertEqual "viewLegalHoldServiceUrl" (newLegalHoldServiceUrl newService) (viewLegalHoldServiceUrl service)
          assertEqual "viewLegalHoldServiceFingerprint" fpr (viewLegalHoldServiceFingerprint service)
        -- The pubkey is different... if a connection would be reused
        -- this request would actually return a 201
        let badServiceValidKey = newService {newLegalHoldServiceKey = ServiceKeyPEM publicKeyNotMatchingService}
        postSettings owner tid badServiceValidKey !!! testResponse 412 (Just "legalhold-unavailable")
  -- We do not use the higher level withDummyTestServiceForTeam here because we want to make
  -- legalhold service misbehave on purpose in certain cases
  -- if no valid service response can be obtained, responds with 400
  withTestService (lhapp NotWorking) (lhtest NotWorking)
  -- if valid service response can be obtained, writes a pending entry to cassandra
  -- synchronously and respond with 201
  withTestService (lhapp Working) (lhtest Working)

testRemoveLegalHoldFromTeam :: TestM ()
testRemoveLegalHoldFromTeam = do
  (owner, tid) <- createBindingTeam
  member <- randomUser
  addTeamMemberInternal tid member noPermissions Nothing
  -- fails if LH for team is disabled
  deleteSettings (Just defPassword) owner tid !!! testResponse 403 (Just "legalhold-disable-unimplemented")

testAddTeamUserTooLargeWithLegalholdWhitelisted :: (HasCallStack) => TestM ()
testAddTeamUserTooLargeWithLegalholdWhitelisted = withTeam $ \owner tid -> do
  o <- view tsGConf
  let fanoutLimit = fromIntegral @_ @Integer . fromRange $ Galley.currentFanoutLimit o
  forM_ [2 .. (fanoutLimit + 5)] $ \_n -> do
    addUserToTeam' owner tid !!! do
      const 201 === statusCode

testCannotCreateLegalHoldDeviceOldAPI :: TestM ()
testCannotCreateLegalHoldDeviceOldAPI = do
  member <- randomUser
  (owner, tid) <- createBindingTeam
  -- user without team can't add LH device
  tryout member
  -- team member can't add LH device
  addTeamMemberInternal tid member (rolePermissions RoleMember) Nothing
  tryout member
  -- team owner can't add LH device
  tryout owner
  where
    tryout :: UserId -> TestM ()
    tryout uid = do
      brg <- viewBrig
      let newClientBody =
            (newClient LegalHoldClientType (head someLastPrekeys))
              { newClientPassword = Just defPassword
              }
          req =
            brg
              . path "clients"
              . json newClientBody
              . zUser uid
              . zConn "conn"
      post req !!! const 400 === statusCode
      assertZeroLegalHoldDevices uid

data GroupConvInvCase = InviteOnlyConsenters | InviteAlsoNonConsenters
  deriving (Show, Eq, Ord, Bounded, Enum)

testBenchHack :: (HasCallStack) => TestM ()
testBenchHack = do
  {- representative sample run on an old laptop:

     (10,0.186728036s)
     (30,0.283852693s)
     (100,0.712145446s)
     (300,1.72513614s)
     (600,3.47943481s)

     the test itself is running for ages, but most of the time is spent in setting up the
     connections.

     before running this test, you also need to change {galley,brig}.integration.yaml:

     ```
       diff --git a/services/brig/brig.integration.yaml b/services/brig/brig.integration.yaml
       -  setUserMaxConnections: 16
       -  setMaxTeamSize: 32
       -  setMaxConvSize: 16
       +  setUserMaxConnections: 999
       +  setMaxTeamSize: 999
       +  setMaxConvSize: 999
       diff --git a/services/galley/galley.integration.yaml b/services/galley/galley.integration.yaml
       -  maxTeamSize: 32
       -  maxFanoutSize: 18
       -  maxConvSize: 16
       +  maxTeamSize: 999
       +  maxFanoutSize: 999
       +  maxConvSize: 999
     ```

     (you can probably get away with changing fewer values here, but this patch has been
     tested and works.)
  -}

  when False $ do
    print =<< testBenchHack' 10
    print =<< testBenchHack' 30
    print =<< testBenchHack' 100
    print =<< testBenchHack' 300
    print =<< testBenchHack' 600

testBenchHack' :: (HasCallStack) => Int -> TestM (Int, Time.NominalDiffTime)
testBenchHack' numPeers = do
  (legalholder :: UserId, tid) <- createBindingTeam
  peers :: [UserId] <- replicateM numPeers randomUser
  galley <- viewGalley

  let doEnableLH :: (HasCallStack) => TestM ()
      doEnableLH = do
        withLHWhitelist tid (requestLegalHoldDevice' galley legalholder legalholder tid) !!! testResponse 201 Nothing
        withLHWhitelist tid (approveLegalHoldDevice' galley (Just defPassword) legalholder legalholder tid) !!! testResponse 200 Nothing
        UserLegalHoldStatusResponse userStatus _ _ <- withLHWhitelist tid (getUserStatusTyped' galley legalholder tid)
        liftIO $ assertEqual "approving should change status" UserLegalHoldEnabled userStatus

  withDummyTestServiceForTeam legalholder tid $ \_chan -> do
    for_ peers $ \peer -> do
      postConnection legalholder peer !!! const 201 === statusCode
      void $ putConnection peer legalholder Conn.Accepted <!! const 200 === statusCode

    startAt <- liftIO $ Time.getCurrentTime
    doEnableLH
    endAt <- liftIO $ Time.getCurrentTime

    assertConnections
      legalholder
      ((\peer -> ConnectionStatus legalholder peer Conn.MissingLegalholdConsent) <$> peers)
    -- FUTUREWORK: 'assertConnections' only returns 100 connections per page
    -- by default, 500 max.  you need to paginate through all results
    -- somehow to get 600 of them.  but this this is besides the point of
    -- the benchmark anyway.
    for_ peers $ \peer ->
      assertConnections
        peer
        [ConnectionStatus peer legalholder Conn.MissingLegalholdConsent]

    pure (numPeers, Time.diffUTCTime endAt startAt)
