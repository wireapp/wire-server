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

module API.Teams.LegalHold
  ( tests,
  )
where

import API.Teams.LegalHold.Util
import API.Util
import Bilge hiding (accept, head, timeout, trace)
import Bilge.Assert
import Brig.Types.Intra (UserSet (..))
import Brig.Types.Test.Arbitrary ()
import Brig.Types.User.Event qualified as Ev
import Cassandra.Exec qualified as Cql
import Control.Category ((>>>))
import Control.Concurrent.Chan
import Control.Lens hiding ((#))
import Data.Id
import Data.LegalHold
import Data.List.NonEmpty (NonEmpty (..))
import Data.List1 qualified as List1
import Data.Map.Strict qualified as Map
import Data.PEM
import Data.Qualified (Qualified (..))
import Data.Range
import Data.Set qualified as Set
import Data.Time.Clock qualified as Time
import Data.Timeout
import Galley.Cassandra.Client (lookupClients)
import Galley.Cassandra.LegalHold
import Galley.Cassandra.LegalHold qualified as LegalHoldData
import Galley.Env qualified as Galley
import Galley.Options (featureFlags, settings)
import Galley.Types.Clients qualified as Clients
import Galley.Types.Teams
import Imports
import Network.HTTP.Types.Status (status200, status404)
import Network.Wai as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Utilities.Error qualified as Error
import System.IO (hPutStrLn)
import Test.QuickCheck.Instances ()
import Test.Tasty
import Test.Tasty.Cannon qualified as WS
import Test.Tasty.HUnit
import TestHelpers
import TestSetup
import Wire.API.Connection (UserConnection)
import Wire.API.Connection qualified as Conn
import Wire.API.Conversation.Role (roleNameWireAdmin, roleNameWireMember)
import Wire.API.Message qualified as Msg
import Wire.API.Provider.Service
import Wire.API.Routes.Internal.Brig.Connection
import Wire.API.Team.Feature qualified as Public
import Wire.API.Team.LegalHold
import Wire.API.Team.LegalHold.External
import Wire.API.Team.Member
import Wire.API.Team.Member qualified as Team
import Wire.API.Team.Permission
import Wire.API.Team.Role
import Wire.API.User.Client
import Wire.API.User.Client qualified as Client

onlyIfLhWhitelisted :: TestM () -> TestM ()
onlyIfLhWhitelisted action = do
  featureLegalHold <- view (tsGConf . settings . featureFlags . flagLegalHold)
  case featureLegalHold of
    FeatureLegalHoldDisabledPermanently ->
      liftIO $ hPutStrLn stderr errmsg
    FeatureLegalHoldDisabledByDefault ->
      liftIO $ hPutStrLn stderr errmsg
    FeatureLegalHoldWhitelistTeamsAndImplicitConsent -> action
  where
    errmsg =
      "*** skipping test. This test only works if you manually adjust the server config files\
      \(the 'withLHWhitelist' trick does not work because it does not allow \
      \brig to talk to the dynamically spawned galley)."

tests :: IO TestSetup -> TestTree
tests s = testGroup "Legalhold" [testsPublic s, testsInternal s]

testsPublic :: IO TestSetup -> TestTree
testsPublic s =
  -- See also Client Tests in Brig; where behaviour around deleting/adding LH clients is tested
  testGroup
    "Teams LegalHold API (with flag whitelist-teams-and-implicit-consent)"
    [ -- device handling (CRUD)
      testOnlyIfLhWhitelisted s "POST /teams/{tid}/legalhold/{uid}" testRequestLegalHoldDevice,
      testOnlyIfLhWhitelisted s "PUT /teams/{tid}/legalhold/approve" testApproveLegalHoldDevice,
      test s "(user denies approval: nothing needs to be done in backend)" (pure ()),
      testOnlyIfLhWhitelisted s "GET /teams/{tid}/legalhold/{uid}" testGetLegalHoldDeviceStatus,
      testOnlyIfLhWhitelisted s "DELETE /teams/{tid}/legalhold/{uid}" testDisableLegalHoldForUser,
      -- legal hold settings
      testOnlyIfLhWhitelisted s "POST /teams/{tid}/legalhold/settings" testCreateLegalHoldTeamSettings,
      testOnlyIfLhWhitelisted s "GET /teams/{tid}/legalhold/settings" testGetLegalHoldTeamSettings,
      testOnlyIfLhWhitelisted s "Not implemented: DELETE /teams/{tid}/legalhold/settings" testRemoveLegalHoldFromTeam,
      testOnlyIfLhWhitelisted s "GET [/i]?/teams/{tid}/legalhold" testEnablePerTeam,
      -- behavior of existing end-points
      testOnlyIfLhWhitelisted s "POST /clients" testCannotCreateLegalHoldDeviceOldAPI,
      testOnlyIfLhWhitelisted s "GET /teams/{tid}/members" testGetTeamMembersIncludesLHStatus,
      testOnlyIfLhWhitelisted s "POST /register - can add team members above fanout limit when whitelisting is enabled" testAddTeamUserTooLargeWithLegalholdWhitelisted,
      testOnlyIfLhWhitelisted s "GET legalhold status in user profile" testGetLegalholdStatus,
      {- TODO:
          conversations/{cnv}/otr/messages - possibly show the legal hold device (if missing) as a different device type (or show that on device level, depending on how client teams prefer)
          GET /team/{tid}/members - show legal hold status of all members

      -}
      testGroup
        "settings.legalholdEnabledTeams" -- FUTUREWORK: ungroup this level
        [ testGroup -- FUTUREWORK: ungroup this level
            "teams listed"
            [ test s "happy flow" testInWhitelist,
              test s "handshake between LH device and user with old clients is blocked" testOldClientsBlockDeviceHandshake,
              testGroup "no-consent" $ do
                connectFirst <- ("connectFirst",) <$> [False, True]
                teamPeer <- ("teamPeer",) <$> [False, True]
                approveLH <- ("approveLH",) <$> [False, True]
                testPendingConnection <- ("testPendingConnection",) <$> [False, True]
                let name = intercalate ", " $ map (\(n, b) -> n <> "=" <> show b) [connectFirst, teamPeer, approveLH, testPendingConnection]
                pure . test s name $ testNoConsentBlockOne2OneConv (snd connectFirst) (snd teamPeer) (snd approveLH) (snd testPendingConnection),
              testGroup
                "Legalhold is activated for user A in a group conversation"
                [ testOnlyIfLhWhitelisted s "All admins are consenting: all non-consenters get removed from conversation" (testNoConsentRemoveFromGroupConv LegalholderIsAdmin),
                  testOnlyIfLhWhitelisted s "Some admins are consenting: all non-consenters get removed from conversation" (testNoConsentRemoveFromGroupConv BothAreAdmins),
                  testOnlyIfLhWhitelisted s "No admins are consenting: all LH activated/pending users get removed from conversation" (testNoConsentRemoveFromGroupConv PeerIsAdmin)
                ],
              testGroup
                "Users are invited to a group conversation."
                [ testGroup
                    "At least one invited user has activated legalhold. At least one admin of the group has given consent."
                    [ test
                        s
                        "If all all users in the invite have given consent then the invite succeeds and all non-consenters from the group get removed"
                        (onlyIfLhWhitelisted (testGroupConvInvitationHandlesLHConflicts InviteOnlyConsenters)),
                      test
                        s
                        "If any user in the invite has not given consent then the invite fails"
                        (onlyIfLhWhitelisted (testGroupConvInvitationHandlesLHConflicts InviteAlsoNonConsenters))
                    ],
                  testGroup
                    "The group conversation contains legalhold activated users."
                    [ testOnlyIfLhWhitelisted s "If any user in the invite has not given consent then the invite fails" testNoConsentCannotBeInvited
                    ]
                ],
              testOnlyIfLhWhitelisted s "Cannot create conversation with both LH activated and non-consenting users" testCannotCreateGroupWithUsersInConflict,
              test s "bench hack" testBenchHack,
              test s "User cannot fetch prekeys of LH users if consent is missing" (testClaimKeys TCKConsentMissing),
              test s "User cannot fetch prekeys of LH users: if user has old client" (testClaimKeys TCKOldClient),
              test s "User can fetch prekeys of LH users if consent is given and user has only new clients" (testClaimKeys TCKConsentAndNewClients)
            ]
        ]
    ]

testsInternal :: IO TestSetup -> TestTree
testsInternal s =
  testGroup
    "Legalhold Internal API"
    [testOnlyIfLhWhitelisted s "PUT, DELETE /i/legalhold/whitelisted-teams" testWhitelistingTeams]

testWhitelistingTeams :: TestM ()
testWhitelistingTeams = do
  let testTeamWhitelisted :: HasCallStack => TeamId -> TestM Bool
      testTeamWhitelisted tid = do
        res <- getLHWhitelistedTeam tid
        pure (Bilge.responseStatus res == status200)

  let expectWhitelisted :: HasCallStack => Bool -> TeamId -> TestM ()
      expectWhitelisted yes tid = do
        let msg = if yes then "team should be whitelisted" else "team should not be whitelisted"
        aFewTimesAssertBool msg (== yes) (testTeamWhitelisted tid)

  tid <- withTeam $ \_owner tid -> do
    expectWhitelisted False tid
    putLHWhitelistTeam tid !!! const 200 === statusCode
    expectWhitelisted True tid
    pure tid

  expectWhitelisted False tid

testRequestLegalHoldDevice :: TestM ()
testRequestLegalHoldDevice = withTeam $ \owner tid -> do
  member <- randomUser
  addTeamMemberInternal tid member (rolePermissions RoleMember) Nothing
  -- Can't request a device if team feature flag is disabled
  requestLegalHoldDevice owner member tid !!! testResponse 403 (Just "legalhold-not-enabled")
  cannon <- view tsCannon
  -- Assert that the appropriate LegalHold Request notification is sent to the user's
  -- clients
  WS.bracketR2 cannon member member $ \(ws, ws') -> withDummyTestServiceForTeamNoService $ \lhPort _chan -> do
    do
      -- test device creation without consent
      requestLegalHoldDevice member member tid !!! testResponse 403 (Just "legalhold-not-enabled")
      UserLegalHoldStatusResponse userStatus _ _ <- getUserStatusTyped member tid
      liftIO $
        assertEqual
          "User with insufficient permissions should be unable to start flow"
          UserLegalHoldNoConsent
          userStatus

    do
      requestLegalHoldDevice owner member tid !!! testResponse 403 (Just "legalhold-not-enabled")
      UserLegalHoldStatusResponse userStatus _ _ <- getUserStatusTyped member tid
      liftIO $
        assertEqual
          "User with insufficient permissions should be unable to start flow"
          UserLegalHoldNoConsent
          userStatus

    putLHWhitelistTeam tid !!! const 200 === statusCode
    newService <- newLegalHoldService lhPort
    postSettings owner tid newService !!! testResponse 201 Nothing

    do
      requestLegalHoldDevice member member tid !!! testResponse 403 (Just "operation-denied")
      UserLegalHoldStatusResponse userStatus _ _ <- getUserStatusTyped member tid
      liftIO $
        assertEqual
          "User with insufficient permissions should be unable to start flow"
          UserLegalHoldDisabled
          userStatus

    do
      requestLegalHoldDevice owner member tid !!! testResponse 201 Nothing
      UserLegalHoldStatusResponse userStatus _ _ <- getUserStatusTyped member tid
      liftIO $
        assertEqual
          "requestLegalHoldDevice should set user status to Pending"
          UserLegalHoldPending
          userStatus
    do
      requestLegalHoldDevice owner member tid !!! testResponse 204 Nothing
      UserLegalHoldStatusResponse userStatus _ _ <- getUserStatusTyped member tid
      liftIO $
        assertEqual
          "requestLegalHoldDevice when already pending should leave status as Pending"
          UserLegalHoldPending
          userStatus

    cassState <- view tsCass
    liftIO $ do
      storedPrekeys <- Cql.runClient cassState (LegalHoldData.selectPendingPrekeys member)
      assertBool "user should have pending prekeys stored" (not . null $ storedPrekeys)
    let pluck = \case
          (Ev.LegalHoldClientRequested rdata) -> do
            Ev.lhcTargetUser rdata @?= member
            Ev.lhcLastPrekey rdata @?= head someLastPrekeys
            Ev.lhcClientId rdata @?= someClientId
          _ -> assertBool "Unexpected event" False
    assertNotification ws pluck
    -- all devices get notified.
    assertNotification ws' pluck

testApproveLegalHoldDevice :: TestM ()
testApproveLegalHoldDevice = do
  (owner, tid) <- createBindingTeam
  member <- do
    usr <- randomUser
    addTeamMemberInternal tid usr (rolePermissions RoleMember) Nothing
    pure usr
  member2 <- do
    usr <- randomUser
    addTeamMemberInternal tid usr (rolePermissions RoleMember) Nothing
    pure usr
  outsideContact <- do
    usr <- randomUser
    connectUsers member (List1.singleton usr)
    pure usr
  stranger <- randomUser
  putLHWhitelistTeam tid !!! const 200 === statusCode
  approveLegalHoldDevice (Just defPassword) owner member tid
    !!! testResponse 403 (Just "access-denied")
  cannon <- view tsCannon
  WS.bracketRN cannon [owner, member, member, member2, outsideContact, stranger] $
    \[ows, mws, mws', member2Ws, outsideContactWs, strangerWs] -> withDummyTestServiceForTeam owner tid $ \chan -> do
      requestLegalHoldDevice owner member tid !!! testResponse 201 Nothing
      liftIO . assertMatchJSON chan $ \(RequestNewLegalHoldClient userId' teamId') -> do
        assertEqual "userId == member" userId' member
        assertEqual "teamId == tid" teamId' tid
      -- Only the user themself can approve adding a LH device
      approveLegalHoldDevice (Just defPassword) owner member tid !!! testResponse 403 (Just "access-denied")
      -- Requires password
      approveLegalHoldDevice Nothing member member tid !!! const 403 === statusCode
      approveLegalHoldDevice (Just defPassword) member member tid !!! testResponse 200 Nothing
      -- checks if the cookie we give to the legalhold service is actually valid
      assertMatchJSON chan $ \(LegalHoldServiceConfirm _clientId _uid _tid authToken) ->
        renewToken authToken
      cassState <- view tsCass
      liftIO $ do
        clients' <- Cql.runClient cassState $ lookupClients [member]
        assertBool "Expect clientId to be saved on the user" $
          Clients.contains member someClientId clients'
      UserLegalHoldStatusResponse userStatus _ _ <- getUserStatusTyped member tid
      liftIO $
        assertEqual
          "After approval user legalhold status should be Enabled"
          UserLegalHoldEnabled
          userStatus
      let pluck = \case
            Ev.ClientAdded _ eClient -> do
              clientId eClient @?= someClientId
              clientType eClient @?= LegalHoldClientType
              clientClass eClient @?= Just LegalHoldClient
            _ -> assertBool "Unexpected event" False
      assertNotification mws pluck
      assertNotification mws' pluck
      -- Other team users should get a user.legalhold-enable event
      let pluck' = \case
            Ev.UserLegalHoldEnabled eUser -> eUser @?= member
            _ -> assertBool "Unexpected event" False
      assertNotification ows pluck'
      -- We send to all members of a team. which includes the team-settings
      assertNotification member2Ws pluck'
      when False $ do
        -- this doesn't work any more since consent (personal users cannot grant consent).
        assertNotification outsideContactWs pluck'
      assertNoNotification strangerWs

testGetLegalHoldDeviceStatus :: TestM ()
testGetLegalHoldDeviceStatus = do
  (owner, tid) <- createBindingTeam
  member <- randomUser
  addTeamMemberInternal tid member (rolePermissions RoleMember) Nothing
  forM_ [owner, member] $ \uid -> do
    status <- getUserStatusTyped uid tid
    liftIO $
      assertEqual
        "unexpected status"
        (UserLegalHoldStatusResponse UserLegalHoldNoConsent Nothing Nothing)
        status

  putLHWhitelistTeam tid !!! const 200 === statusCode
  withDummyTestServiceForTeamNoService $ \lhPort _chan -> do
    do
      UserLegalHoldStatusResponse userStatus lastPrekey' clientId' <- getUserStatusTyped member tid
      liftIO $
        do
          assertEqual "User legal hold status should start as disabled" UserLegalHoldDisabled userStatus
          assertEqual "last_prekey should be Nothing when LH is disabled" Nothing lastPrekey'
          assertEqual "client.id should be Nothing when LH is disabled" Nothing clientId'

    do
      newService <- newLegalHoldService lhPort
      postSettings owner tid newService !!! testResponse 201 Nothing
      requestLegalHoldDevice owner member tid !!! testResponse 201 Nothing
      assertZeroLegalHoldDevices member
      UserLegalHoldStatusResponse userStatus lastPrekey' clientId' <- getUserStatusTyped member tid
      liftIO $
        do
          assertEqual "requestLegalHoldDevice should set user status to Pending" UserLegalHoldPending userStatus
          assertEqual "last_prekey should be set when LH is pending" (Just (head someLastPrekeys)) lastPrekey'
          assertEqual "client.id should be set when LH is pending" (Just someClientId) clientId'
    do
      requestLegalHoldDevice owner member tid !!! testResponse 204 Nothing
      UserLegalHoldStatusResponse userStatus _ _ <- getUserStatusTyped member tid
      liftIO $
        assertEqual
          "requestLegalHoldDevice when already pending should leave status as Pending"
          UserLegalHoldPending
          userStatus
    do
      approveLegalHoldDevice (Just defPassword) member member tid !!! testResponse 200 Nothing
      UserLegalHoldStatusResponse userStatus lastPrekey' clientId' <- getUserStatusTyped member tid
      liftIO $
        do
          assertEqual "approving should change status to Enabled" UserLegalHoldEnabled userStatus
          assertEqual "last_prekey should be set when LH is pending" (Just (head someLastPrekeys)) lastPrekey'
          assertEqual "client.id should be set when LH is pending" (Just someClientId) clientId'
    assertExactlyOneLegalHoldDevice member
    requestLegalHoldDevice owner member tid !!! testResponse 409 (Just "legalhold-already-enabled")

testDisableLegalHoldForUser :: TestM ()
testDisableLegalHoldForUser = withTeam $ \owner tid -> do
  member <- randomUser
  addTeamMemberInternal tid member (rolePermissions RoleMember) Nothing
  cannon <- view tsCannon
  putLHWhitelistTeam tid !!! const 200 === statusCode
  WS.bracketR2 cannon owner member $ \(ows, mws) -> withDummyTestServiceForTeam owner tid $ \chan -> do
    requestLegalHoldDevice owner member tid !!! testResponse 201 Nothing
    approveLegalHoldDevice (Just defPassword) member member tid !!! testResponse 200 Nothing
    assertNotification mws $ \case
      Ev.ClientAdded _ client -> do
        clientId client @?= someClientId
        clientType client @?= LegalHoldClientType
        clientClass client @?= Just LegalHoldClient
      _ -> assertBool "Unexpected event" False
    -- Only the admin can disable legal hold
    disableLegalHoldForUser (Just defPassword) tid member member !!! testResponse 403 (Just "operation-denied")
    assertExactlyOneLegalHoldDevice member
    -- Require password to disable for usern
    disableLegalHoldForUser Nothing tid owner member !!! const 403 === statusCode
    assertExactlyOneLegalHoldDevice member
    disableLegalHoldForUser (Just defPassword) tid owner member !!! testResponse 200 Nothing
    liftIO . assertMatchChan chan $ \(req, _) -> do
      assertEqual "method" "POST" (requestMethod req)
      assertEqual "path" (pathInfo req) ["legalhold", "remove"]
    assertNotification mws $ \case
      Ev.ClientEvent (Ev.ClientRemoved _ clientId') -> clientId' @?= someClientId
      _ -> assertBool "Unexpected event" False
    assertNotification mws $ \case
      Ev.UserEvent (Ev.UserLegalHoldDisabled uid) -> uid @?= member
      _ -> assertBool "Unexpected event" False
    -- Other users should also get the event
    assertNotification ows $ \case
      Ev.UserLegalHoldDisabled uid -> uid @?= member
      _ -> assertBool "Unexpected event" False
    assertZeroLegalHoldDevices member

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
  let lhapp :: HasCallStack => IsWorking -> Chan Void -> Application
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
      lhtest :: HasCallStack => IsWorking -> Warp.Port -> Chan Void -> TestM ()
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

-- NOTE: we do not expect event TeamEvent'TEAM_UPDATE as a reaction to this POST.

testGetLegalHoldTeamSettings :: TestM ()
testGetLegalHoldTeamSettings = do
  (owner, tid) <- createBindingTeam
  stranger <- randomUser
  member <- randomUser
  addTeamMemberInternal tid member (rolePermissions RoleMember) Nothing
  let lhapp :: Chan () -> Application
      lhapp _ch _req res = res $ responseLBS status200 mempty mempty
  withTestService lhapp $ \lhPort _ -> do
    newService <- newLegalHoldService lhPort
    -- returns 403 if user is not in team.
    getSettings stranger tid !!! testResponse 403 (Just "no-team-member")
    -- returns 200 with corresp. status if legalhold for team is disabled
    do
      let respOk :: ResponseLBS -> TestM ()
          respOk resp = liftIO $ do
            assertEqual "bad status code" 200 (statusCode resp)
            assertEqual "bad body" ViewLegalHoldServiceDisabled (responseJsonUnsafe resp)
      getSettings owner tid >>= respOk
      getSettings member tid >>= respOk

    putLHWhitelistTeam tid !!! const 200 === statusCode

    -- returns 200 with corresp. status if legalhold for team is enabled, but not configured
    do
      let respOk :: ResponseLBS -> TestM ()
          respOk resp = liftIO $ do
            assertEqual "bad status code" 200 (statusCode resp)
            assertEqual "bad body" ViewLegalHoldServiceNotConfigured (responseJsonUnsafe resp)
      getSettings owner tid >>= respOk
      getSettings member tid >>= respOk
    postSettings owner tid newService !!! testResponse 201 Nothing
    -- returns legal hold service info if team is under legal hold and user is in team (even
    -- no permissions).
    ViewLegalHoldService service <- getSettingsTyped member tid
    liftIO $ do
      let sKey = newLegalHoldServiceKey newService
      Just (_, fpr) <- validateServiceKey sKey
      assertEqual "viewLegalHoldServiceTeam" tid (viewLegalHoldServiceTeam service)
      assertEqual "viewLegalHoldServiceUrl" (newLegalHoldServiceUrl newService) (viewLegalHoldServiceUrl service)
      assertEqual "viewLegalHoldServiceFingerprint" fpr (viewLegalHoldServiceFingerprint service)
      assertEqual "viewLegalHoldServiceKey" sKey (viewLegalHoldServiceKey service)
      assertEqual "viewLegalHoldServiceAuthToken" (newLegalHoldServiceToken newService) (viewLegalHoldServiceAuthToken service)

testRemoveLegalHoldFromTeam :: TestM ()
testRemoveLegalHoldFromTeam = do
  (owner, tid) <- createBindingTeam
  member <- randomUser
  addTeamMemberInternal tid member noPermissions Nothing
  -- fails if LH for team is disabled
  deleteSettings (Just defPassword) owner tid !!! testResponse 403 (Just "legalhold-disable-unimplemented")

testEnablePerTeam :: TestM ()
testEnablePerTeam = withTeam $ \owner tid -> do
  member <- randomUser
  addTeamMemberInternal tid member (rolePermissions RoleMember) Nothing
  do
    status :: Public.WithStatusNoLock Public.LegalholdConfig <- responseJsonUnsafe <$> (getEnabled tid <!! testResponse 200 Nothing)
    let statusValue = Public.wssStatus status
    liftIO $ assertEqual "Teams should start with LegalHold disabled" statusValue Public.FeatureStatusDisabled

  putLHWhitelistTeam tid !!! const 200 === statusCode

  do
    status :: Public.WithStatusNoLock Public.LegalholdConfig <- responseJsonUnsafe <$> (getEnabled tid <!! testResponse 200 Nothing)
    let statusValue = Public.wssStatus status
    liftIO $ assertEqual "Calling 'putEnabled True' should enable LegalHold" statusValue Public.FeatureStatusEnabled
  withDummyTestServiceForTeam owner tid $ \_chan -> do
    putLHWhitelistTeam tid !!! const 200 === statusCode
    requestLegalHoldDevice owner member tid !!! const 201 === statusCode
    approveLegalHoldDevice (Just defPassword) member member tid !!! testResponse 200 Nothing
    do
      UserLegalHoldStatusResponse status _ _ <- getUserStatusTyped member tid
      liftIO $ assertEqual "User legal hold status should be enabled" UserLegalHoldEnabled status
    do
      putEnabled' id tid Public.FeatureStatusDisabled !!! testResponse 403 (Just "legalhold-whitelisted-only")
      status :: Public.WithStatusNoLock Public.LegalholdConfig <- responseJsonUnsafe <$> (getEnabled tid <!! testResponse 200 Nothing)
      let statusValue = Public.wssStatus status
      liftIO $ assertEqual "Calling 'putEnabled False' should have no effect." statusValue Public.FeatureStatusEnabled

testAddTeamUserTooLargeWithLegalholdWhitelisted :: HasCallStack => TestM ()
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

testGetTeamMembersIncludesLHStatus :: TestM ()
testGetTeamMembersIncludesLHStatus = do
  (owner, tid) <- createBindingTeam
  member <- randomUser
  addTeamMemberInternal tid member (rolePermissions RoleMember) Nothing

  let findMemberStatus :: [TeamMember] -> Maybe UserLegalHoldStatus
      findMemberStatus ms =
        ms ^? traversed . filtered (has $ Team.userId . only member) . legalHoldStatus

  let check :: HasCallStack => UserLegalHoldStatus -> String -> TestM ()
      check status msg = do
        members' <- view teamMembers <$> getTeamMembers owner tid
        liftIO $
          assertEqual
            ("legal hold status should be " <> msg)
            (Just status)
            (findMemberStatus members')

  check UserLegalHoldNoConsent "disabled when it is disabled for the team"
  withDummyTestServiceForTeamNoService $ \lhPort _chan -> do
    check UserLegalHoldNoConsent "no_consent on new team members"

    putLHWhitelistTeam tid !!! const 200 === statusCode
    newService <- newLegalHoldService lhPort
    postSettings owner tid newService !!! testResponse 201 Nothing

    check UserLegalHoldDisabled "disabled on team members that have granted consent"
    requestLegalHoldDevice owner member tid !!! testResponse 201 Nothing
    check UserLegalHoldPending "pending after requesting device"
    approveLegalHoldDevice (Just defPassword) member member tid !!! testResponse 200 Nothing
    check UserLegalHoldEnabled "enabled after confirming device"

testInWhitelist :: TestM ()
testInWhitelist = do
  g <- viewGalley
  (owner, tid) <- createBindingTeam
  member <- randomUser
  addTeamMemberInternal tid member (rolePermissions RoleMember) Nothing
  cannon <- view tsCannon

  putLHWhitelistTeam tid !!! const 200 === statusCode

  WS.bracketR2 cannon member member $ \(_ws, _ws') -> withDummyTestServiceForTeam owner tid $ \_chan -> do
    do
      -- members have granted consent (implicitly)...
      lhs <- view legalHoldStatus <$> withLHWhitelist tid (getTeamMember' g member tid member)
      liftIO $ assertEqual "" lhs UserLegalHoldDisabled

      -- ... and can do so again (idempotency).
      _ <- withLHWhitelist tid (void $ putLHWhitelistTeam' g tid)
      lhs' <- withLHWhitelist tid $ view legalHoldStatus <$> getTeamMember' g member tid member
      liftIO $ assertEqual "" lhs' UserLegalHoldDisabled

    do
      -- members can't request LH devices
      withLHWhitelist tid (requestLegalHoldDevice' g member member tid) !!! testResponse 403 (Just "operation-denied")
      UserLegalHoldStatusResponse userStatus _ _ <- withLHWhitelist tid (getUserStatusTyped' g member tid)
      liftIO $
        assertEqual
          "User with insufficient permissions should be unable to start flow"
          UserLegalHoldDisabled
          userStatus
    do
      -- owners can
      withLHWhitelist tid (requestLegalHoldDevice' g owner member tid) !!! testResponse 201 Nothing
      UserLegalHoldStatusResponse userStatus _ _ <- withLHWhitelist tid (getUserStatusTyped' g member tid)
      liftIO $
        assertEqual
          "requestLegalHoldDevice should set user status to Pending"
          UserLegalHoldPending
          userStatus
    do
      -- request device is idempotent
      withLHWhitelist tid (requestLegalHoldDevice' g owner member tid) !!! testResponse 204 Nothing
      UserLegalHoldStatusResponse userStatus _ _ <- withLHWhitelist tid (getUserStatusTyped' g member tid)
      liftIO $
        assertEqual
          "requestLegalHoldDevice when already pending should leave status as Pending"
          UserLegalHoldPending
          userStatus
    do
      -- owner cannot approve legalhold device
      withLHWhitelist tid (approveLegalHoldDevice' g (Just defPassword) owner member tid) !!! testResponse 403 (Just "access-denied")
    do
      -- approve works
      withLHWhitelist tid (approveLegalHoldDevice' g (Just defPassword) member member tid) !!! testResponse 200 Nothing
      UserLegalHoldStatusResponse userStatus lastPrekey' clientId' <- withLHWhitelist tid (getUserStatusTyped' g member tid)
      liftIO $
        do
          assertEqual "approving should change status to Enabled" UserLegalHoldEnabled userStatus
          assertEqual "last_prekey should be set when LH is pending" (Just (head someLastPrekeys)) lastPrekey'
          assertEqual "client.id should be set when LH is pending" (Just someClientId) clientId'

testOldClientsBlockDeviceHandshake :: TestM ()
testOldClientsBlockDeviceHandshake = do
  -- "handshake between LH device and user with old devices is blocked"
  --
  -- this specifically checks the place that handles otr messages and responds with status
  -- 412 and a list of missing clients.
  --
  -- if any of those clients are LH, this test provodes a "missing-legalhold-consent" error
  -- instead, without any information about the LH clients.  the condition is actually "has
  -- old device or has not granted consent", but the latter part is blocked earlier in 1:1 and
  -- group conversations, and hard to test at the device level.)
  --
  -- tracked here: https://wearezeta.atlassian.net/browse/SQSERVICES-454

  (legalholder, tid) <- createBindingTeam
  legalholder2 <- view Team.userId <$> addUserToTeam legalholder tid
  (peer, tid2) <-
    -- has to be a team member, granting LH consent for personal users is not supported.
    createBindingTeam

  let doEnableLH :: HasCallStack => UserId -> UserId -> TestM ClientId
      doEnableLH owner uid = do
        requestLegalHoldDevice owner uid tid !!! testResponse 201 Nothing
        approveLegalHoldDevice (Just defPassword) uid uid tid !!! testResponse 200 Nothing
        UserLegalHoldStatusResponse userStatus _ _ <- getUserStatusTyped uid tid
        liftIO $ assertEqual "approving should change status" UserLegalHoldEnabled userStatus
        getInternalClientsFull (UserSet $ Set.singleton uid)
          <&> do
            userClientsFull
              >>> Map.elems
              >>> Set.unions
              >>> Set.toList
              >>> head
              >>> clientId

  putLHWhitelistTeam tid !!! const 200 === statusCode

  withDummyTestServiceForTeam legalholder tid $ \_chan -> do
    legalholderLHDevice <- doEnableLH legalholder legalholder
    _legalholder2LHDevice <- doEnableLH legalholder legalholder2

    let caps = Set.singleton Client.ClientSupportsLegalholdImplicitConsent
    legalholderClient <- do
      clnt <- randomClientWithCaps legalholder (someLastPrekeys !! 1) (Just caps)
      ensureClientCaps legalholder clnt (Client.ClientCapabilityList caps)
      pure clnt
    legalholder2Client <- do
      clnt <- randomClient legalholder2 (someLastPrekeys !! 3)
      -- this another way to do it (instead of providing caps during client creation).
      ensureClientCaps legalholder2 clnt (Client.ClientCapabilityList mempty)
      upgradeClientToLH legalholder2 clnt
      ensureClientCaps legalholder2 clnt (Client.ClientCapabilityList caps)
      pure clnt
    putLHWhitelistTeam tid2 !!! const 200 === statusCode
    connectUsers peer (List1.list1 legalholder [legalholder2])

    convId <-
      decodeConvId
        <$> ( postConv peer [legalholder, legalholder2] (Just "gossip") [] Nothing Nothing
                <!! const 201 === statusCode
            )

    let runit :: HasCallStack => UserId -> ClientId -> TestM ResponseLBS
        runit sender senderClient = do
          postOtrMessage id sender senderClient convId rcps
          where
            rcps =
              [ (legalholder, legalholderClient, "ciphered"),
                (legalholder, legalholderLHDevice, "ciphered"),
                (legalholder2, legalholder2Client, "ciphered")
                -- legalholder2 LH device missing
              ]

    -- LH devices are treated as clients that have the ClientSupportsLegalholdImplicitConsent
    -- capability (so LH doesn't break for users who have LH devices; it sounds silly, but
    -- it's good to test this, since it did require adding a few lines of production code in
    -- 'addClient' about client capabilities).
    runit legalholder legalholderClient >>= errWith 412 (\(_ :: Msg.ClientMismatch) -> True)

    -- If user has a client without the ClientSupportsLegalholdImplicitConsent
    -- capability then message sending is prevented to legalhold devices.
    peerClient <- randomClient peer (someLastPrekeys !! 2)
    runit peer peerClient >>= errWith 403 (\err -> Error.label err == "missing-legalhold-consent")
    upgradeClientToLH peer peerClient
    runit peer peerClient >>= errWith 412 (\(_ :: Msg.ClientMismatch) -> True)

-- If LH is activated for other user in 1:1 conv, 1:1 conv is blocked
testNoConsentBlockOne2OneConv :: HasCallStack => Bool -> Bool -> Bool -> Bool -> TestM ()
testNoConsentBlockOne2OneConv connectFirst teamPeer approveLH testPendingConnection = do
  -- FUTUREWORK: maybe regular user for legalholder?
  (legalholder :: UserId, tid) <- createBindingTeam
  regularClient <- randomClient legalholder (head someLastPrekeys)

  peer :: UserId <- if teamPeer then fst <$> createBindingTeam else randomUser
  galley <- viewGalley

  putLHWhitelistTeam tid !!! const 200 === statusCode

  let doEnableLH :: HasCallStack => TestM (Maybe ClientId)
      doEnableLH = do
        -- register & (possibly) approve LH device for legalholder
        withLHWhitelist tid (requestLegalHoldDevice' galley legalholder legalholder tid) !!! testResponse 201 Nothing
        when approveLH $
          withLHWhitelist tid (approveLegalHoldDevice' galley (Just defPassword) legalholder legalholder tid) !!! testResponse 200 Nothing
        UserLegalHoldStatusResponse userStatus _ _ <- withLHWhitelist tid (getUserStatusTyped' galley legalholder tid)
        liftIO $ assertEqual "approving should change status" (if approveLH then UserLegalHoldEnabled else UserLegalHoldPending) userStatus
        if approveLH
          then
            getInternalClientsFull (UserSet $ Set.singleton legalholder)
              <&> do
                userClientsFull
                  >>> Map.elems
                  >>> Set.unions
                  >>> Set.toList
                  >>> listToMaybe
                  >>> fmap clientId
          else pure Nothing

      doDisableLH :: HasCallStack => TestM ()
      doDisableLH = do
        -- remove (only) LH device again
        withLHWhitelist tid (disableLegalHoldForUser' galley (Just defPassword) tid legalholder legalholder)
          !!! testResponse 200 Nothing

  cannon <- view tsCannon

  WS.bracketR2 cannon legalholder peer $ \(legalholderWs, peerWs) -> withDummyTestServiceForTeam legalholder tid $ \_chan -> do
    if not connectFirst
      then do
        void doEnableLH
        postConnection legalholder peer !!! do testResponse 403 (Just "missing-legalhold-consent")
        postConnection peer legalholder !!! do testResponse 403 (Just "missing-legalhold-consent")
      else do
        postConnection legalholder peer !!! const 201 === statusCode

        mbConn :: Maybe UserConnection <-
          if testPendingConnection
            then pure Nothing
            else do
              res <- putConnection peer legalholder Conn.Accepted <!! const 200 === statusCode
              pure $ Just $ responseJsonUnsafe res

        mbLegalholderLHDevice <- doEnableLH

        assertConnections legalholder [ConnectionStatus legalholder peer Conn.MissingLegalholdConsent]
        assertConnections peer [ConnectionStatus peer legalholder Conn.MissingLegalholdConsent]

        forM_ [legalholderWs, peerWs] $ \ws -> do
          assertNotification ws $
            \case
              (Ev.ConnectionEvent (Ev.ConnectionUpdated (Conn.ucStatus -> rel) _prev _name)) -> do
                rel @?= Conn.MissingLegalholdConsent
              _ -> assertBool "wrong event type" False

        forM_ [(legalholder, peer), (peer, legalholder)] $ \(one, two) -> do
          putConnection one two Conn.Accepted
            !!! testResponse 403 (Just "bad-conn-update")

        assertConnections legalholder [ConnectionStatus legalholder peer Conn.MissingLegalholdConsent]
        assertConnections peer [ConnectionStatus peer legalholder Conn.MissingLegalholdConsent]

        -- peer can't send message to legalhodler. the conversation appears gone.
        peerClient <- randomClient peer (someLastPrekeys !! 2)
        for_ ((,) <$> (mbConn >>= Conn.ucConvId) <*> mbLegalholderLHDevice) $ \(convId, legalholderLHDevice) -> do
          postOtrMessage
            id
            peer
            peerClient
            (qUnqualified convId)
            [ (legalholder, legalholderLHDevice, "cipher"),
              (legalholder, regularClient, "cipher")
            ]
            !!! do
              const 404 === statusCode
              const (Right "no-conversation") === fmap Error.label . responseJsonEither

        do
          doDisableLH

          when approveLH $ do
            legalholderLHDevice <- assertJust mbLegalholderLHDevice
            WS.assertMatch_ (5 # Second) legalholderWs $
              wsAssertClientRemoved legalholderLHDevice

          assertConnections
            legalholder
            [ ConnectionStatus legalholder peer $
                if testPendingConnection then Conn.Sent else Conn.Accepted
            ]
          assertConnections
            peer
            [ ConnectionStatus peer legalholder $
                if testPendingConnection then Conn.Pending else Conn.Accepted
            ]

        forM_ [legalholderWs, peerWs] $ \ws -> do
          assertNotification ws $
            \case
              (Ev.ConnectionEvent (Ev.ConnectionUpdated (Conn.ucStatus -> rel) _prev _name)) -> do
                assertBool "" (rel `elem` [Conn.Sent, Conn.Pending, Conn.Accepted])
              _ -> assertBool "wrong event type" False

        -- conversation reappears. peer can send message to legalholder again
        for_ ((,) <$> (mbConn >>= Conn.ucConvId) <*> mbLegalholderLHDevice) $ \(convId, legalholderLHDevice) -> do
          postOtrMessage
            id
            peer
            peerClient
            (qUnqualified convId)
            [ (legalholder, legalholderLHDevice, "cipher"),
              (legalholder, regularClient, "cipher")
            ]
            !!! do
              const 201 === statusCode
              assertMismatchWithMessage
                (Just "legalholderLHDevice is deleted")
                []
                []
                [(legalholder, Set.singleton legalholderLHDevice)]

data GroupConvAdmin
  = LegalholderIsAdmin
  | PeerIsAdmin
  | BothAreAdmins
  deriving (Show, Eq, Ord, Bounded, Enum)

testNoConsentRemoveFromGroupConv :: GroupConvAdmin -> HasCallStack => TestM ()
testNoConsentRemoveFromGroupConv whoIsAdmin = do
  (legalholder :: UserId, tid) <- createBindingTeam
  qLegalHolder <- Qualified legalholder <$> viewFederationDomain
  (peer :: UserId, teamPeer) <- createBindingTeam
  qPeer <- Qualified peer <$> viewFederationDomain
  galley <- viewGalley

  let enableLHForLegalholder :: HasCallStack => TestM ()
      enableLHForLegalholder = do
        requestLegalHoldDevice legalholder legalholder tid !!! testResponse 201 Nothing
        approveLegalHoldDevice (Just defPassword) legalholder legalholder tid !!! testResponse 200 Nothing
        UserLegalHoldStatusResponse userStatus _ _ <- getUserStatusTyped' galley legalholder tid
        liftIO $ assertEqual "approving should change status" UserLegalHoldEnabled userStatus

  cannon <- view tsCannon

  putLHWhitelistTeam tid !!! const 200 === statusCode
  WS.bracketR2 cannon legalholder peer $ \(legalholderWs, peerWs) -> withDummyTestServiceForTeam legalholder tid $ \_chan -> do
    postConnection legalholder peer !!! const 201 === statusCode
    void $ putConnection peer legalholder Conn.Accepted <!! const 200 === statusCode

    convId <- do
      let (inviter, tidInviter, invitee, inviteeRole) =
            case whoIsAdmin of
              LegalholderIsAdmin -> (qLegalHolder, tid, qPeer, roleNameWireMember)
              PeerIsAdmin -> (qPeer, teamPeer, qLegalHolder, roleNameWireMember)
              BothAreAdmins -> (qLegalHolder, tid, qPeer, roleNameWireAdmin)

      convId <- createTeamConvWithRole (qUnqualified inviter) tidInviter [qUnqualified invitee] (Just "group chat with external peer") Nothing Nothing inviteeRole
      mapM_ (assertConvMemberWithRole roleNameWireAdmin convId) ([inviter] <> [invitee | whoIsAdmin == BothAreAdmins])
      mapM_ (assertConvMemberWithRole roleNameWireMember convId) [invitee | whoIsAdmin /= BothAreAdmins]
      pure convId
    qconvId <- Qualified convId <$> viewFederationDomain

    checkConvCreateEvent convId legalholderWs
    checkConvCreateEvent convId peerWs

    assertConvMember qLegalHolder convId
    assertConvMember qPeer convId

    void enableLHForLegalholder

    case whoIsAdmin of
      LegalholderIsAdmin -> do
        assertConvMember qLegalHolder convId
        assertNotConvMember peer convId
        checkConvMemberLeaveEvent qconvId qPeer legalholderWs
        checkConvMemberLeaveEvent qconvId qPeer peerWs
      PeerIsAdmin -> do
        assertConvMember qPeer convId
        assertNotConvMember legalholder convId
        checkConvMemberLeaveEvent qconvId qLegalHolder legalholderWs
        checkConvMemberLeaveEvent qconvId qLegalHolder peerWs
      BothAreAdmins -> do
        assertConvMember qLegalHolder convId
        assertNotConvMember peer convId
        checkConvMemberLeaveEvent qconvId qPeer legalholderWs
        checkConvMemberLeaveEvent qconvId qPeer peerWs

data GroupConvInvCase = InviteOnlyConsenters | InviteAlsoNonConsenters
  deriving (Show, Eq, Ord, Bounded, Enum)

testGroupConvInvitationHandlesLHConflicts :: HasCallStack => GroupConvInvCase -> TestM ()
testGroupConvInvitationHandlesLHConflicts inviteCase = do
  localDomain <- viewFederationDomain
  -- team that is legalhold whitelisted
  (legalholder :: UserId, tid) <- createBindingTeam
  let qLegalHolder = Qualified legalholder localDomain
  userWithConsent <- (^. Team.userId) <$> addUserToTeam legalholder tid
  userWithConsent2 <- do
    uid <- (^. Team.userId) <$> addUserToTeam legalholder tid
    pure $ Qualified uid localDomain
  putLHWhitelistTeam tid !!! const 200 === statusCode

  -- team without legalhold
  (peer :: UserId, teamPeer) <- createBindingTeam
  peer2 <- (^. Team.userId) <$> addUserToTeam peer teamPeer
  let qpeer2 = Qualified peer2 localDomain

  do
    postConnection userWithConsent peer !!! const 201 === statusCode
    void $ putConnection peer userWithConsent Conn.Accepted <!! const 200 === statusCode

    postConnection userWithConsent peer2 !!! const 201 === statusCode
    void $ putConnection peer2 userWithConsent Conn.Accepted <!! const 200 === statusCode

  withDummyTestServiceForTeam legalholder tid $ \_chan -> do
    -- conversation with 1) userWithConsent and 2) peer
    convId <- createTeamConvWithRole userWithConsent tid [peer] (Just "corp + us") Nothing Nothing roleNameWireAdmin
    let qconvId = Qualified convId localDomain

    -- activate legalhold for legalholder
    do
      galley <- viewGalley
      requestLegalHoldDevice legalholder legalholder tid !!! testResponse 201 Nothing
      approveLegalHoldDevice (Just defPassword) legalholder legalholder tid !!! testResponse 200 Nothing
      UserLegalHoldStatusResponse userStatus _ _ <- getUserStatusTyped' galley legalholder tid
      liftIO $ assertEqual "approving should change status" UserLegalHoldEnabled userStatus

    case inviteCase of
      InviteOnlyConsenters -> do
        API.Util.postMembers userWithConsent (qLegalHolder :| [userWithConsent2]) qconvId
          !!! const 200 === statusCode

        assertConvMember qLegalHolder convId
        assertConvMember userWithConsent2 convId
        assertNotConvMember peer convId
      InviteAlsoNonConsenters -> do
        API.Util.postMembers userWithConsent (qLegalHolder :| [qpeer2]) qconvId
          >>= errWith 403 (\err -> Error.label err == "missing-legalhold-consent")

testNoConsentCannotBeInvited :: HasCallStack => TestM ()
testNoConsentCannotBeInvited = do
  localDomain <- viewFederationDomain
  -- team that is legalhold whitelisted
  (legalholder :: UserId, tid) <- createBindingTeam
  userLHNotActivated <- (^. Team.userId) <$> addUserToTeam legalholder tid
  putLHWhitelistTeam tid !!! const 200 === statusCode

  -- team without legalhold
  (peer :: UserId, teamPeer) <- createBindingTeam
  let qpeer = Qualified peer localDomain
  peer2 <- (^. Team.userId) <$> addUserToTeam peer teamPeer
  let qpeer2 = Qualified peer2 localDomain

  do
    postConnection userLHNotActivated peer !!! const 201 === statusCode
    void $ putConnection peer userLHNotActivated Conn.Accepted <!! const 200 === statusCode

    postConnection userLHNotActivated peer2 !!! const 201 === statusCode
    void $ putConnection peer2 userLHNotActivated Conn.Accepted <!! const 200 === statusCode

  withDummyTestServiceForTeam legalholder tid $ \_chan -> do
    convId <- createTeamConvWithRole userLHNotActivated tid [legalholder] (Just "corp + us") Nothing Nothing roleNameWireAdmin
    let qconvId = Qualified convId localDomain

    API.Util.postMembers userLHNotActivated (pure qpeer) qconvId
      !!! const 200 === statusCode

    -- activate legalhold for legalholder
    do
      galley <- viewGalley
      requestLegalHoldDevice legalholder legalholder tid !!! testResponse 201 Nothing
      approveLegalHoldDevice (Just defPassword) legalholder legalholder tid !!! testResponse 200 Nothing
      UserLegalHoldStatusResponse userStatus _ _ <- getUserStatusTyped' galley legalholder tid
      liftIO $ assertEqual "approving should change status" UserLegalHoldEnabled userStatus

    API.Util.postMembers userLHNotActivated (pure qpeer2) qconvId
      >>= errWith 403 (\err -> Error.label err == "missing-legalhold-consent")

    localdomain <- viewFederationDomain
    API.Util.postQualifiedMembers userLHNotActivated (Qualified peer2 localdomain :| []) qconvId
      >>= errWith 403 (\err -> Error.label err == "missing-legalhold-consent")

testCannotCreateGroupWithUsersInConflict :: HasCallStack => TestM ()
testCannotCreateGroupWithUsersInConflict = do
  -- team that is legalhold whitelisted
  (legalholder :: UserId, tid) <- createBindingTeam
  userLHNotActivated <- (^. Team.userId) <$> addUserToTeam legalholder tid
  putLHWhitelistTeam tid !!! const 200 === statusCode

  -- team without legalhold
  (peer :: UserId, teamPeer) <- createBindingTeam
  peer2 <- (^. Team.userId) <$> addUserToTeam peer teamPeer

  do
    postConnection userLHNotActivated peer !!! const 201 === statusCode
    void $ putConnection peer userLHNotActivated Conn.Accepted <!! const 200 === statusCode

    postConnection userLHNotActivated peer2 !!! const 201 === statusCode
    void $ putConnection peer2 userLHNotActivated Conn.Accepted <!! const 200 === statusCode

  withDummyTestServiceForTeam legalholder tid $ \_chan -> do
    createTeamConvAccessRaw userLHNotActivated tid [peer, legalholder] (Just "corp + us") Nothing Nothing Nothing (Just roleNameWireMember)
      !!! const 201 === statusCode

    -- activate legalhold for legalholder
    do
      galley <- viewGalley
      requestLegalHoldDevice legalholder legalholder tid !!! testResponse 201 Nothing
      approveLegalHoldDevice (Just defPassword) legalholder legalholder tid !!! testResponse 200 Nothing
      UserLegalHoldStatusResponse userStatus _ _ <- getUserStatusTyped' galley legalholder tid
      liftIO $ assertEqual "approving should change status" UserLegalHoldEnabled userStatus

    createTeamConvAccessRaw userLHNotActivated tid [peer2, legalholder] (Just "corp + us") Nothing Nothing Nothing (Just roleNameWireMember)
      >>= errWith 403 (\err -> Error.label err == "missing-legalhold-consent")

data TestClaimKeys
  = TCKConsentMissing
  | TCKOldClient
  | TCKConsentAndNewClients

testClaimKeys :: TestClaimKeys -> TestM ()
testClaimKeys testcase = do
  -- "cannot fetch prekeys of LH users if requester did not give consent or has old clients"
  (legalholder, tid) <- createBindingTeam
  (peer, teamPeer) <- createBindingTeam

  let doEnableLH :: HasCallStack => TeamId -> UserId -> UserId -> TestM ClientId
      doEnableLH team owner uid = do
        requestLegalHoldDevice owner uid team !!! testResponse 201 Nothing
        approveLegalHoldDevice (Just defPassword) uid uid team !!! testResponse 200 Nothing
        UserLegalHoldStatusResponse userStatus _ _ <- getUserStatusTyped uid team
        liftIO $ assertEqual "approving should change status" UserLegalHoldEnabled userStatus
        getInternalClientsFull (UserSet $ Set.singleton uid)
          <&> do
            userClientsFull
              >>> Map.elems
              >>> Set.unions
              >>> Set.toList
              >>> head
              >>> clientId

  let makePeerClient :: TestM ()
      makePeerClient = case testcase of
        TCKConsentMissing -> do
          peerClient <- randomClient peer (someLastPrekeys !! 2)
          upgradeClientToLH peer peerClient
        TCKOldClient -> do
          void $ randomClient peer (someLastPrekeys !! 2)
          putLHWhitelistTeam teamPeer !!! const 200 === statusCode
        TCKConsentAndNewClients -> do
          peerClient <- randomClient peer (someLastPrekeys !! 2)
          upgradeClientToLH peer peerClient
          putLHWhitelistTeam teamPeer !!! const 200 === statusCode

  let assertResponse' :: Assertions ()
      assertResponse' = case testcase of
        TCKConsentMissing -> bad
        TCKOldClient -> bad
        TCKConsentAndNewClients -> good
        where
          good = testResponse 200 Nothing
          bad = testResponse 403 (Just "missing-legalhold-consent")

  let fetchKeys :: ClientId -> TestM ()
      fetchKeys legalholderLHDevice = do
        getUsersPrekeysClientUnqualified peer legalholder legalholderLHDevice !!! assertResponse'
        getUsersPrekeyBundleUnqualified peer legalholder !!! assertResponse'
        let userClients = UserClients (Map.fromList [(legalholder, Set.fromList [legalholderLHDevice])])
        getMultiUserPrekeyBundleUnqualified peer userClients !!! assertResponse'

  putLHWhitelistTeam tid !!! const 200 === statusCode

  withDummyTestServiceForTeam legalholder tid $ \_chan -> do
    legalholderLHDevice <- doEnableLH tid legalholder legalholder

    makePeerClient
    fetchKeys legalholderLHDevice

testBenchHack :: HasCallStack => TestM ()
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

testBenchHack' :: HasCallStack => Int -> TestM (Int, Time.NominalDiffTime)
testBenchHack' numPeers = do
  (legalholder :: UserId, tid) <- createBindingTeam
  peers :: [UserId] <- replicateM numPeers randomUser
  galley <- viewGalley

  let doEnableLH :: HasCallStack => TestM ()
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
