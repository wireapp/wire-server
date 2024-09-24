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

module API.Teams.LegalHold.DisabledByDefault
  ( tests,
  )
where

import API.Teams.LegalHold.Util
import API.Util
import Bilge hiding (accept, head, timeout, trace)
import Bilge.Assert
import Brig.Types.Intra (UserSet (..))
import Brig.Types.Test.Arbitrary ()
import Cassandra.Exec qualified as Cql
import Control.Category ((>>>))
import Control.Concurrent.Chan
import Control.Lens
import Data.Id
import Data.LegalHold
import Data.List1 qualified as List1
import Data.Map.Strict qualified as Map
import Data.PEM
import Data.Range
import Data.Set qualified as Set
import Galley.Cassandra.Client
import Galley.Cassandra.LegalHold
import Galley.Cassandra.LegalHold qualified as LegalHoldData
import Galley.Env qualified as Galley
import Galley.Types.Clients qualified as Clients
import Imports
import Network.HTTP.Types.Status (status200, status404)
import Network.Wai as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Utilities.Error qualified as Error
import Test.QuickCheck.Instances ()
import Test.Tasty
import Test.Tasty.Cannon qualified as WS
import Test.Tasty.HUnit
import TestHelpers
import TestSetup
import Wire.API.Message qualified as Msg
import Wire.API.Provider.Service
import Wire.API.Team.Feature qualified as Public
import Wire.API.Team.LegalHold
import Wire.API.Team.LegalHold.External
import Wire.API.Team.Member
import Wire.API.Team.Member qualified as Team
import Wire.API.Team.Permission
import Wire.API.Team.Role
import Wire.API.User.Client
import Wire.API.User.Client qualified as Client
import Wire.API.UserEvent qualified as Ev

tests :: IO TestSetup -> TestTree
tests s =
  -- See also Client Tests in Brig; where behaviour around deleting/adding LH clients is tested
  testGroup
    "Teams LegalHold API (with flag disabled-by-default)"
    [ -- device handling (CRUD)
      testOnlyIfLhEnabled s "POST /teams/{tid}/legalhold/{uid}" testRequestLegalHoldDevice,
      testOnlyIfLhEnabled s "PUT /teams/{tid}/legalhold/approve" testApproveLegalHoldDevice,
      test s "(user denies approval: nothing needs to be done in backend)" (pure ()),
      testOnlyIfLhEnabled s "GET /teams/{tid}/legalhold/{uid}" testGetLegalHoldDeviceStatus,
      testOnlyIfLhEnabled s "DELETE /teams/{tid}/legalhold/{uid}" testDisableLegalHoldForUser,
      -- legal hold settings
      testOnlyIfLhEnabled s "POST /teams/{tid}/legalhold/settings" testCreateLegalHoldTeamSettings,
      testOnlyIfLhEnabled s "GET /teams/{tid}/legalhold/settings" testGetLegalHoldTeamSettings,
      testOnlyIfLhEnabled s "DELETE /teams/{tid}/legalhold/settings" testRemoveLegalHoldFromTeam,
      testOnlyIfLhEnabled s "GET, PUT [/i]?/teams/{tid}/legalhold" testEnablePerTeam,
      testOnlyIfLhEnabled s "GET, PUT [/i]?/teams/{tid}/legalhold - too large" testEnablePerTeamTooLarge,
      -- behavior of existing end-points
      testOnlyIfLhEnabled s "POST /clients" testCannotCreateLegalHoldDeviceOldAPI,
      testOnlyIfLhEnabled s "GET /teams/{tid}/members" testGetTeamMembersIncludesLHStatus,
      testOnlyIfLhEnabled s "POST /register - cannot add team members above fanout limit" testAddTeamUserTooLargeWithLegalhold,
      {- TODO:
          conversations/{cnv}/otr/messages - possibly show the legal hold device (if missing) as a different device type (or show that on device level, depending on how client teams prefer)
          GET /team/{tid}/members - show legal hold status of all members

      -}
      testOnlyIfLhEnabled s "handshake between LH device and user with old clients is blocked" testOldClientsBlockDeviceHandshake,
      testOnlyIfLhEnabled s "User cannot fetch prekeys of LH users if consent is missing" (testClaimKeys TCKConsentMissing),
      testOnlyIfLhEnabled s "User cannot fetch prekeys of LH users: if user has old client" (testClaimKeys TCKOldClient),
      testOnlyIfLhEnabled s "User can fetch prekeys of LH users if consent is given and user has only new clients" (testClaimKeys TCKConsentAndNewClients)
    ]

testRequestLegalHoldDevice :: TestM ()
testRequestLegalHoldDevice = do
  (owner, tid) <- createBindingTeam
  member <- randomUser
  addTeamMemberInternal tid member (rolePermissions RoleMember) Nothing
  -- Can't request a device if team feature flag is disabled
  requestLegalHoldDevice owner member tid !!! testResponse 403 (Just "legalhold-not-enabled")
  cannon <- view tsCannon
  -- Assert that the appropriate LegalHold Request notification is sent to the user's
  -- clients
  WS.bracketR2 cannon member member $ \(ws, ws') -> withDummyTestServiceForTeam' owner tid $ \_ _chan -> do
    do
      -- test device creation without consent
      requestLegalHoldDevice member member tid !!! testResponse 403 (Just "operation-denied")
      UserLegalHoldStatusResponse userStatus _ _ <- getUserStatusTyped member tid
      liftIO $
        assertEqual
          "User with insufficient permissions should be unable to start flow"
          UserLegalHoldNoConsent
          userStatus

    do
      requestLegalHoldDevice owner member tid !!! testResponse 409 (Just "legalhold-no-consent")
      UserLegalHoldStatusResponse userStatus _ _ <- getUserStatusTyped member tid
      liftIO $
        assertEqual
          "User with insufficient permissions should be unable to start flow"
          UserLegalHoldNoConsent
          userStatus

    do
      -- test granting consent
      lhs <- view legalHoldStatus <$> getTeamMember member tid member
      liftIO $ assertEqual "" lhs UserLegalHoldNoConsent

      grantConsent tid member
      lhs' <- view legalHoldStatus <$> getTeamMember member tid member
      liftIO $ assertEqual "" lhs' UserLegalHoldDisabled

      grantConsent tid member
      lhs'' <- view legalHoldStatus <$> getTeamMember member tid member
      liftIO $ assertEqual "" lhs'' UserLegalHoldDisabled

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
  grantConsent tid owner
  grantConsent tid member
  grantConsent tid member2
  -- not allowed to approve if team setting is disabled
  approveLegalHoldDevice (Just defPassword) owner member tid
    !!! testResponse 403 (Just "legalhold-not-enabled")
  cannon <- view tsCannon
  WS.bracketRN cannon [owner, member, member, member2, outsideContact, stranger] $
    \[ows, mws, mws', member2Ws, outsideContactWs, strangerWs] -> withDummyTestServiceForTeam' owner tid $ \_ chan -> do
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
            Ev.ClientAdded eClient -> do
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
  withDummyTestServiceForTeam' owner tid $ \_ _chan -> do
    grantConsent tid member
    do
      UserLegalHoldStatusResponse userStatus lastPrekey' clientId' <- getUserStatusTyped member tid
      liftIO $
        do
          assertEqual "User legal hold status should start as disabled" UserLegalHoldDisabled userStatus
          assertEqual "last_prekey should be Nothing when LH is disabled" Nothing lastPrekey'
          assertEqual "client.id should be Nothing when LH is disabled" Nothing clientId'
    do
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
testDisableLegalHoldForUser = do
  (owner, tid) <- createBindingTeam
  member <- randomUser
  addTeamMemberInternal tid member (rolePermissions RoleMember) Nothing
  cannon <- view tsCannon
  WS.bracketR2 cannon owner member $ \(ows, mws) -> withDummyTestServiceForTeam' owner tid $ \_ chan -> do
    grantConsent tid member
    requestLegalHoldDevice owner member tid !!! testResponse 201 Nothing
    approveLegalHoldDevice (Just defPassword) member member tid !!! testResponse 200 Nothing
    assertNotification mws $ \case
      Ev.ClientAdded client -> do
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
      Ev.ClientEvent (Ev.ClientRemoved clientId') -> clientId' @?= someClientId
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
testCreateLegalHoldTeamSettings = do
  (owner, tid) <- createBindingTeam
  member <- randomUser
  addTeamMemberInternal tid member (rolePermissions RoleMember) Nothing
  -- Random port, hopefully nothing is runing here!
  brokenService <- newLegalHoldService 4242
  -- not allowed to create if team setting is disabled
  postSettings owner tid brokenService !!! testResponse 403 (Just "legalhold-not-enabled")
  putEnabled tid Public.FeatureStatusEnabled -- enable it for this team

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
  -- We do not use the higher level withDummyTestServiceForTeam' here because we want to make
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
    -- returns 403 if user is not in team.
    newService <- newLegalHoldService lhPort
    getSettings stranger tid !!! testResponse 403 (Just "no-team-member")
    -- returns 200 with corresp. status if legalhold for team is disabled
    do
      let respOk :: ResponseLBS -> TestM ()
          respOk resp = liftIO $ do
            assertEqual "bad status code" 200 (statusCode resp)
            assertEqual "bad body" ViewLegalHoldServiceDisabled (responseJsonUnsafe resp)
      getSettings owner tid >>= respOk
      getSettings member tid >>= respOk
    putEnabled tid Public.FeatureStatusEnabled -- enable it for this team

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
  stranger <- randomUser
  member <- randomUser
  addTeamMemberInternal tid member noPermissions Nothing
  -- fails if LH for team is disabled
  deleteSettings (Just defPassword) owner tid !!! testResponse 403 (Just "legalhold-not-enabled")
  withDummyTestServiceForTeam' owner tid $ \lhPort chan -> do
    newService <- newLegalHoldService lhPort
    postSettings owner tid newService !!! testResponse 201 Nothing
    -- enable legalhold for member
    do
      grantConsent tid member
      requestLegalHoldDevice owner member tid !!! testResponse 201 Nothing
      approveLegalHoldDevice (Just defPassword) member member tid !!! testResponse 200 Nothing
      UserLegalHoldStatusResponse userStatus _ _ <- getUserStatusTyped member tid
      liftIO $
        assertEqual
          "After approval user legalhold status should be Enabled"
          UserLegalHoldEnabled
          userStatus
    -- returns 403 if user is not in team or has unsufficient permissions.
    deleteSettings (Just defPassword) stranger tid !!! testResponse 403 (Just "no-team-member")
    deleteSettings (Just defPassword) member tid !!! testResponse 403 (Just "operation-denied")
    -- Fails without password
    deleteSettings Nothing owner tid !!! testResponse 403 (Just "access-denied")
    let delete'' expectRemoteLHCall = do
          deleteSettings (Just defPassword) owner tid !!! testResponse 204 Nothing
          when expectRemoteLHCall . liftIO . assertMatchChan chan $ \(req, _) -> do
            print (pathInfo req, pathInfo req == ["legalhold", "remove"])
            print (requestMethod req, requestMethod req == "POST")
            assertEqual "path" ["legalhold", "remove"] (pathInfo req)
            assertEqual "method" "POST" (requestMethod req)
          resp <- getSettings owner tid
          liftIO $ assertEqual "bad body" ViewLegalHoldServiceNotConfigured (responseJsonUnsafe resp)
    -- returns 204 if legal hold is successfully removed from team
    -- is idempotent (deleting twice in a row works) from BE's PoV
    -- NOTE: Only if LH is active will there be a remote call to the LH service
    delete'' True
    delete'' False
    -- deletion of settings should disable for all team members and remove their clients
    do
      UserLegalHoldStatusResponse userStatus _ _ <- getUserStatusTyped member tid
      liftIO $
        assertEqual
          "After approval user legalhold status should be Disabled"
          UserLegalHoldDisabled
          userStatus
      assertZeroLegalHoldDevices member

testEnablePerTeam :: TestM ()
testEnablePerTeam = do
  (owner, tid) <- createBindingTeam
  member <- randomUser
  addTeamMemberInternal tid member (rolePermissions RoleMember) Nothing
  do
    feat :: Public.Feature Public.LegalholdConfig <- responseJsonUnsafe <$> (getEnabled tid <!! testResponse 200 Nothing)
    liftIO $ assertEqual "Teams should start with LegalHold disabled" feat.status Public.FeatureStatusDisabled
  putEnabled tid Public.FeatureStatusEnabled -- enable it for this team
  do
    feat :: Public.Feature Public.LegalholdConfig <- responseJsonUnsafe <$> (getEnabled tid <!! testResponse 200 Nothing)
    liftIO $ assertEqual "Calling 'putEnabled True' should enable LegalHold" feat.status Public.FeatureStatusEnabled
  withDummyTestServiceForTeam' owner tid $ \_ _chan -> do
    grantConsent tid member
    requestLegalHoldDevice owner member tid !!! const 201 === statusCode
    approveLegalHoldDevice (Just defPassword) member member tid !!! testResponse 200 Nothing
    do
      UserLegalHoldStatusResponse status _ _ <- getUserStatusTyped member tid
      liftIO $ assertEqual "User legal hold status should be enabled" UserLegalHoldEnabled status
    do
      putEnabled tid Public.FeatureStatusDisabled -- disable again
      feat :: Public.Feature Public.LegalholdConfig <- responseJsonUnsafe <$> (getEnabled tid <!! testResponse 200 Nothing)
      liftIO $ assertEqual "Calling 'putEnabled False' should disable LegalHold" feat.status Public.FeatureStatusDisabled
    do
      UserLegalHoldStatusResponse status _ _ <- getUserStatusTyped member tid
      liftIO $ assertEqual "User legal hold status should be disabled after disabling for team" UserLegalHoldDisabled status
    viewLHS <- getSettingsTyped owner tid
    liftIO $
      assertEqual
        "LH Service settings should be disabled"
        ViewLegalHoldServiceDisabled
        viewLHS

testEnablePerTeamTooLarge :: TestM ()
testEnablePerTeamTooLarge = do
  o <- view tsGConf
  let fanoutLimit = fromIntegral . fromRange $ Galley.currentFanoutLimit o
  -- TODO: it is impossible in this test to create teams bigger than the fanout limit.
  -- Change the +1 to anything else and look at the logs
  (tid, _owner, _others) <- createBindingTeamWithMembers (fanoutLimit + 5)

  feat :: Public.Feature Public.LegalholdConfig <- responseJsonUnsafe <$> (getEnabled tid <!! testResponse 200 Nothing)
  liftIO $ assertEqual "Teams should start with LegalHold disabled" feat.status Public.FeatureStatusDisabled
  -- You cannot enable legal hold on a team that is too large
  putEnabled' id tid Public.FeatureStatusEnabled !!! do
    const 403 === statusCode
    const (Just "too-large-team-for-legalhold") === fmap Error.label . responseJsonMaybe

testAddTeamUserTooLargeWithLegalhold :: TestM ()
testAddTeamUserTooLargeWithLegalhold = do
  o <- view tsGConf
  let fanoutLimit = fromIntegral . fromRange $ Galley.currentFanoutLimit o
  (tid, owner, _others) <- createBindingTeamWithMembers fanoutLimit
  feat :: Public.Feature Public.LegalholdConfig <- responseJsonUnsafe <$> (getEnabled tid <!! testResponse 200 Nothing)
  liftIO $ assertEqual "Teams should start with LegalHold disabled" feat.status Public.FeatureStatusDisabled
  -- You can still enable for this team
  putEnabled tid Public.FeatureStatusEnabled
  -- But now Adding a user should now fail since the team is too large
  addUserToTeam' owner tid !!! do
    const 403 === statusCode
    const (Just "too-many-members-for-legalhold") === fmap Error.label . responseJsonMaybe

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

  let check :: (HasCallStack) => UserLegalHoldStatus -> String -> TestM ()
      check status msg = do
        members' <- view teamMembers <$> getTeamMembers owner tid
        liftIO $
          assertEqual
            ("legal hold status should be " <> msg)
            (Just status)
            (findMemberStatus members')

  check UserLegalHoldNoConsent "disabled when it is disabled for the team"
  withDummyTestServiceForTeam' owner tid $ \_ _chan -> do
    check UserLegalHoldNoConsent "no_consent on new team members"
    grantConsent tid member
    check UserLegalHoldDisabled "disabled on team members that have granted consent"
    requestLegalHoldDevice owner member tid !!! testResponse 201 Nothing
    check UserLegalHoldPending "pending after requesting device"
    approveLegalHoldDevice (Just defPassword) member member tid !!! testResponse 200 Nothing
    check UserLegalHoldEnabled "enabled after confirming device"

testOldClientsBlockDeviceHandshake :: TestM ()
testOldClientsBlockDeviceHandshake = do
  -- "handshake between LH device and user with old devices is blocked"
  --
  -- this specifically checks the place that handles otr messages and responds with status
  -- 412 and a list of missing clients.
  --
  -- if any of those clients are LH, this test provides a "missing-legalhold-consent-old-clients" error
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

  let doEnableLH :: (HasCallStack) => UserId -> UserId -> TestM ClientId
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

  withDummyTestServiceForTeam' legalholder tid $ \_ _chan -> do
    grantConsent tid legalholder
    grantConsent tid legalholder2

    legalholderLHDevice <- doEnableLH legalholder legalholder
    _legalholder2LHDevice <- doEnableLH legalholder legalholder2

    let caps = Client.ClientCapabilityList $ Set.singleton Client.ClientSupportsLegalholdImplicitConsent
    legalholderClient <- do
      clnt <- randomClientWithCaps legalholder (someLastPrekeys !! 1) (Just caps)
      ensureClientCaps legalholder clnt caps
      pure clnt
    legalholder2Client <- do
      clnt <- randomClient legalholder2 (someLastPrekeys !! 3)
      -- this another way to do it (instead of providing caps during client creation).
      ensureClientCaps legalholder2 clnt (Client.ClientCapabilityList mempty)
      upgradeClientToLH legalholder2 clnt
      ensureClientCaps legalholder2 clnt caps
      pure clnt
    grantConsent tid2 peer
    connectUsers peer (List1.list1 legalholder [legalholder2])

    convId <-
      decodeConvId
        <$> ( postConv peer [legalholder, legalholder2] (Just "gossip") [] Nothing Nothing
                <!! const 201 === statusCode
            )

    let runit :: (HasCallStack) => UserId -> ClientId -> TestM ResponseLBS
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
    runit peer peerClient >>= errWith 403 (\err -> Error.label err == "missing-legalhold-consent-old-clients")
    upgradeClientToLH peer peerClient
    runit peer peerClient >>= errWith 412 (\(_ :: Msg.ClientMismatch) -> True)

data TestClaimKeys
  = TCKConsentMissing
  | TCKOldClient
  | TCKConsentAndNewClients

testClaimKeys :: TestClaimKeys -> TestM ()
testClaimKeys testcase = do
  -- "cannot fetch prekeys of LH users if requester did not give consent or has old clients"
  (legalholder, tid) <- createBindingTeam
  (peer, teamPeer) <- createBindingTeam

  let doEnableLH :: (HasCallStack) => TeamId -> UserId -> UserId -> TestM ClientId
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
          grantConsent teamPeer peer
        TCKConsentAndNewClients -> do
          peerClient <- randomClient peer (someLastPrekeys !! 2)
          upgradeClientToLH peer peerClient
          grantConsent teamPeer peer

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

  withDummyTestServiceForTeam' legalholder tid $ \_ _chan -> do
    grantConsent tid legalholder
    legalholderLHDevice <- doEnableLH tid legalholder legalholder

    makePeerClient
    fetchKeys legalholderLHDevice

--------------------------------------------------------------------
-- setup helpers

withDummyTestServiceForTeam' :: (HasCallStack) => UserId -> TeamId -> (Warp.Port -> Chan (Wai.Request, LByteString) -> TestM a) -> TestM a
withDummyTestServiceForTeam' owner tid go = do
  withDummyTestServiceForTeamNoService $ \lhPort chan -> do
    newService <- newLegalHoldService lhPort
    putEnabled tid Public.FeatureStatusEnabled
    postSettings owner tid newService !!! testResponse 201 Nothing
    go lhPort chan
