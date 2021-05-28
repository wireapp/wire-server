{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

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

module API.Teams.LegalHold
  ( tests,
  )
where

import API.SQS
import API.Util
import Bilge hiding (accept, head, timeout, trace)
import Bilge.Assert
import Brig.Types.Client
import Brig.Types.Intra (ConnectionStatus (ConnectionStatus), UserSet (..))
import Brig.Types.Provider
import Brig.Types.Team.LegalHold hiding (userId)
import Brig.Types.Test.Arbitrary ()
import qualified Brig.Types.User.Event as Ev
import qualified Cassandra.Exec as Cql
import qualified Control.Concurrent.Async as Async
import Control.Concurrent.Chan
import Control.Concurrent.Timeout hiding (threadDelay)
import Control.Exception (asyncExceptionFromException)
import Control.Lens
import Control.Monad.Catch
import Control.Retry (RetryPolicy, RetryStatus, exponentialBackoff, limitRetries, retrying)
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (FromJSON, withObject, (.:))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Conversion
import Data.Id
import Data.Json.Util (toUTCTimeMillis)
import Data.LegalHold
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.List1 as List1
import qualified Data.Map.Strict as Map
import Data.Misc (PlainTextPassword)
import Data.PEM
import Data.Proxy (Proxy (Proxy))
import Data.Range
import qualified Data.Set as Set
import Data.String.Conversions (LBS, cs)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Time.Clock as Time
import qualified Galley.App as Galley
import qualified Galley.Data as Data
import qualified Galley.Data.LegalHold as LegalHoldData
import Galley.External.LegalHoldService (validateServiceKey)
import Galley.Options (optSettings, setFeatureFlags, setLegalHoldTeamsWhitelist)
import qualified Galley.Types.Clients as Clients
import Galley.Types.Teams
import Gundeck.Types.Notification (ntfPayload)
import Imports
import Network.HTTP.Types.Status (status200, status400, status404)
import Network.Wai
import Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.Warp.Internal as Warp
import qualified Network.Wai.Handler.WarpTLS as Warp
import qualified Network.Wai.Test as WaiTest
import qualified Network.Wai.Utilities.Error as Error
import qualified Network.Wai.Utilities.Response as Wai
import Servant.Swagger (validateEveryToJSON)
import System.Environment (withArgs)
import System.IO (hPutStrLn)
import Test.Hspec (hspec)
import Test.QuickCheck.Instances ()
import Test.Tasty
import qualified Test.Tasty.Cannon as WS
import Test.Tasty.HUnit
import TestHelpers
import TestSetup
import Wire.API.Connection (UserConnection)
import qualified Wire.API.Connection as Conn
import qualified Wire.API.Message as Msg
import qualified Wire.API.Routes.Public.LegalHold as LegalHoldAPI
import qualified Wire.API.Team.Feature as Public
import Wire.API.User (UserProfile (..))
import Wire.API.User.Client (UserClients (..), UserClientsFull (userClientsFull))
import qualified Wire.API.User.Client as Client

onlyIfLhEnabled :: TestM () -> TestM ()
onlyIfLhEnabled action = do
  featureLegalHold <- view (tsGConf . optSettings . setFeatureFlags . flagLegalHold)
  case featureLegalHold of
    FeatureLegalHoldDisabledPermanently ->
      liftIO $ hPutStrLn stderr "*** legalhold feature flag disabled, not running this test case"
    FeatureLegalHoldDisabledByDefault ->
      action
    FeatureLegalHoldWhitelistTeamsAndImplicitConsent ->
      error
        "`FeatureLegalHoldWhitelistTeamsAndImplicitConsent` requires setting up a mock galley \
        \with `withLHWhitelist`; don't call `onlyIfLhEnabled` if you do that, that's redundant."

tests :: IO TestSetup -> TestTree
tests s =
  -- See also Client Tests in Brig; where behaviour around deleting/adding LH clients is tested
  testGroup
    "Teams LegalHold API"
    [ test s "swagger / json consistency" (onlyIfLhEnabled testSwaggerJsonConsistency),
      -- device handling (CRUD)
      test s "POST /teams/{tid}/legalhold/{uid}" (onlyIfLhEnabled testRequestLegalHoldDevice),
      test s "PUT /teams/{tid}/legalhold/approve" (onlyIfLhEnabled testApproveLegalHoldDevice),
      test s "(user denies approval: nothing needs to be done in backend)" (pure ()),
      test s "GET /teams/{tid}/legalhold/{uid}" (onlyIfLhEnabled testGetLegalHoldDeviceStatus),
      test s "DELETE /teams/{tid}/legalhold/{uid}" (onlyIfLhEnabled testDisableLegalHoldForUser),
      -- legal hold settings
      test s "POST /teams/{tid}/legalhold/settings" (onlyIfLhEnabled testCreateLegalHoldTeamSettings),
      test s "GET /teams/{tid}/legalhold/settings" (onlyIfLhEnabled testGetLegalHoldTeamSettings),
      test s "DELETE /teams/{tid}/legalhold/settings" (onlyIfLhEnabled testRemoveLegalHoldFromTeam),
      test s "GET, PUT [/i]?/teams/{tid}/legalhold" (onlyIfLhEnabled testEnablePerTeam),
      test s "GET, PUT [/i]?/teams/{tid}/legalhold - too large" (onlyIfLhEnabled testEnablePerTeamTooLarge),
      -- behavior of existing end-points
      test s "POST /clients" (onlyIfLhEnabled testCannotCreateLegalHoldDeviceOldAPI),
      test s "GET /teams/{tid}/members" (onlyIfLhEnabled testGetTeamMembersIncludesLHStatus),
      test s "POST /register - cannot add team members with LH - too large" (onlyIfLhEnabled testAddTeamUserTooLargeWithLegalhold),
      test s "GET legalhold status in user profile" testGetLegalholdStatus,
      {- TODO:
          conversations/{cnv}/otr/messages - possibly show the legal hold device (if missing) as a different device type (or show that on device level, depending on how client teams prefer)
          GET /team/{tid}/members - show legal hold status of all members

      -}
      testGroup
        "settings.legalholdEnabledTeams"
        [ testGroup
            "teams not listed"
            [ test s "happy flow" testNotInWhitelist
            ],
          testGroup
            "teams listed"
            [ test s "happy flow" testInWhitelist,
              test s "handshake between LH device and user with old clients is blocked" testOldClientsBlockDeviceHandshake,
              test
                s
                "If LH is activated for other user in 1:1 conv, 1:1 conv is blocked (connect after, personal peer)"
                (testNoConsentBlockOne2OneConv False False False False),
              test
                s
                "If LH is activated for other user in 1:1 conv, 1:1 conv is blocked (connect after, team peer)"
                (testNoConsentBlockOne2OneConv False True False False),
              test
                s
                "If LH is activated for other user in 1:1 conv, 1:1 conv is blocked (connect after, team peer, approve LH device)"
                (testNoConsentBlockOne2OneConv False True True False),
              test
                s
                "If LH is activated for other user in 1:1 conv, 1:1 conv is blocked (connect after, team peer, leave conn pending)"
                (testNoConsentBlockOne2OneConv False True False True),
              test
                s
                "If LH is activated for other user in 1:1 conv, 1:1 conv is blocked (connect after, team peer, approve LH device, leave conn pending)"
                (testNoConsentBlockOne2OneConv False True True True),
              test
                s
                "If LH is activated for other user in 1:1 conv, 1:1 conv is blocked (connect before, personal peer)"
                (testNoConsentBlockOne2OneConv True False False False),
              test
                s
                "If LH is activated for other user in 1:1 conv, 1:1 conv is blocked (connect before, team peer)"
                (testNoConsentBlockOne2OneConv True True False False),
              test
                s
                "If LH is activated for other user in 1:1 conv, 1:1 conv is blocked (connect before, team peer, approve LH device)"
                (testNoConsentBlockOne2OneConv True True True False),
              test
                s
                "If LH is activated for other user in group conv, this user gets removed with helpful message"
                testNoConsentBlockGroupConv,
              test s "bench hack" testBenchHack,
              test s "User cannot fetch prekeys of LH users if consent is missing" (testClaimKeys TCKConsentMissing),
              test s "User cannot fetch prekeys of LH users: if user has old client" (testClaimKeys TCKOldClient),
              test s "User can fetch prekeys of LH users if consent is given and user has only new clients" (testClaimKeys TCKConsentAndNewClients)
            ]
        ]
    ]

-- | Make sure the ToSchema and ToJSON instances are in sync for all of the swagger docs.
-- (this is more of a unit test, but galley doesn't have any, and it seems not worth it to
-- start another test suite just for this one line.)
--
-- UPDATE(fisx): galley does have unit tests now!  (and of course the "not worth it" was
-- deeply misguided from me.)
testSwaggerJsonConsistency :: TestM ()
testSwaggerJsonConsistency = do
  liftIO . withArgs [] . hspec $ validateEveryToJSON (Proxy @LegalHoldAPI.ServantAPI)

testRequestLegalHoldDevice :: TestM ()
testRequestLegalHoldDevice = do
  (owner, tid) <- createBindingTeam
  member <- randomUser
  addTeamMemberInternal tid member (rolePermissions RoleMember) Nothing
  ensureQueueEmpty
  -- Can't request a device if team feature flag is disabled
  requestLegalHoldDevice owner member tid !!! testResponse 403 (Just "legalhold-not-enabled")
  cannon <- view tsCannon
  -- Assert that the appropriate LegalHold Request notification is sent to the user's
  -- clients
  WS.bracketR2 cannon member member $ \(ws, ws') -> withDummyTestServiceForTeam owner tid $ \_chan -> do
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
  ensureQueueEmpty
  -- not allowed to approve if team setting is disabled
  approveLegalHoldDevice (Just defPassword) owner member tid
    !!! testResponse 403 (Just "legalhold-not-enabled")
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
        clients' <- Cql.runClient cassState $ Data.lookupClients [member]
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
  ensureQueueEmpty
  forM_ [owner, member] $ \uid -> do
    status <- getUserStatusTyped uid tid
    liftIO $
      assertEqual
        "unexpected status"
        (UserLegalHoldStatusResponse UserLegalHoldNoConsent Nothing Nothing)
        status
  withDummyTestServiceForTeam owner tid $ \_chan -> do
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
  ensureQueueEmpty
  cannon <- view tsCannon
  WS.bracketR2 cannon owner member $ \(ows, mws) -> withDummyTestServiceForTeam owner tid $ \chan -> do
    grantConsent tid member
    requestLegalHoldDevice owner member tid !!! testResponse 201 Nothing
    approveLegalHoldDevice (Just defPassword) member member tid !!! testResponse 200 Nothing
    assertNotification mws $ \case
      Ev.ClientAdded _ client -> do
        clientId client @?= someClientId
        clientType client @?= LegalHoldClientType
        clientClass client @?= (Just LegalHoldClient)
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
      Ev.ClientEvent (Ev.ClientRemoved _ clientId') -> clientId clientId' @?= someClientId
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
  ensureQueueEmpty
  newService <- newLegalHoldService
  -- not allowed to create if team setting is disabled
  postSettings owner tid newService !!! testResponse 403 (Just "legalhold-not-enabled")
  putEnabled tid Public.TeamFeatureEnabled -- enable it for this team

  -- not allowed for users with corresp. permission bit missing
  postSettings member tid newService !!! testResponse 403 (Just "operation-denied")
  -- rejected if service is not available
  postSettings owner tid newService !!! testResponse 412 (Just "legalhold-unavailable")
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
      lhtest :: HasCallStack => IsWorking -> Chan Void -> TestM ()
      lhtest NotWorking _ = do
        postSettings owner tid newService !!! testResponse 412 (Just "legalhold-unavailable")
      lhtest Working _ = do
        let Right [k] = pemParseBS "-----BEGIN PUBLIC KEY-----\n\n-----END PUBLIC KEY-----"
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
  ensureQueueEmpty
  newService <- newLegalHoldService
  let lhapp :: Chan () -> Application
      lhapp _ch _req res = res $ responseLBS status200 mempty mempty
  withTestService lhapp $ \_ -> do
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
    putEnabled tid Public.TeamFeatureEnabled -- enable it for this team

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
  ensureQueueEmpty
  -- fails if LH for team is disabled
  deleteSettings (Just defPassword) owner tid !!! testResponse 403 (Just "legalhold-not-enabled")
  withDummyTestServiceForTeam owner tid $ \chan -> do
    newService <- newLegalHoldService
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
            putStrLn (show (pathInfo req, pathInfo req == ["legalhold", "remove"]))
            putStrLn (show (requestMethod req, requestMethod req == "POST"))
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
  ensureQueueEmpty
  do
    status :: Public.TeamFeatureStatus 'Public.TeamFeatureLegalHold <- responseJsonUnsafe <$> (getEnabled tid <!! testResponse 200 Nothing)
    let statusValue = Public.tfwoStatus status
    liftIO $ assertEqual "Teams should start with LegalHold disabled" statusValue Public.TeamFeatureDisabled
  putEnabled tid Public.TeamFeatureEnabled -- enable it for this team
  do
    status :: Public.TeamFeatureStatus 'Public.TeamFeatureLegalHold <- responseJsonUnsafe <$> (getEnabled tid <!! testResponse 200 Nothing)
    let statusValue = Public.tfwoStatus status
    liftIO $ assertEqual "Calling 'putEnabled True' should enable LegalHold" statusValue Public.TeamFeatureEnabled
  withDummyTestServiceForTeam owner tid $ \_chan -> do
    grantConsent tid member
    requestLegalHoldDevice owner member tid !!! const 201 === statusCode
    approveLegalHoldDevice (Just defPassword) member member tid !!! testResponse 200 Nothing
    do
      UserLegalHoldStatusResponse status _ _ <- getUserStatusTyped member tid
      liftIO $ assertEqual "User legal hold status should be enabled" UserLegalHoldEnabled status
    do
      putEnabled tid Public.TeamFeatureDisabled -- disable again
      status :: Public.TeamFeatureStatus 'Public.TeamFeatureLegalHold <- responseJsonUnsafe <$> (getEnabled tid <!! testResponse 200 Nothing)
      let statusValue = Public.tfwoStatus status
      liftIO $ assertEqual "Calling 'putEnabled False' should disable LegalHold" statusValue Public.TeamFeatureDisabled
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
  (tid, _owner, _others) <- createBindingTeamWithMembers (fanoutLimit + 1)

  status :: Public.TeamFeatureStatus 'Public.TeamFeatureLegalHold <- responseJsonUnsafe <$> (getEnabled tid <!! testResponse 200 Nothing)
  let statusValue = Public.tfwoStatus status
  liftIO $ assertEqual "Teams should start with LegalHold disabled" statusValue Public.TeamFeatureDisabled
  -- You cannot enable legal hold on a team that is too large
  putEnabled' id tid Public.TeamFeatureEnabled !!! do
    const 403 === statusCode
    const (Just "too-large-team-for-legalhold") === fmap Error.label . responseJsonMaybe

testAddTeamUserTooLargeWithLegalhold :: TestM ()
testAddTeamUserTooLargeWithLegalhold = do
  o <- view tsGConf
  let fanoutLimit = fromIntegral . fromRange $ Galley.currentFanoutLimit o
  (tid, owner, _others) <- createBindingTeamWithMembers fanoutLimit
  status :: Public.TeamFeatureStatus 'Public.TeamFeatureLegalHold <- responseJsonUnsafe <$> (getEnabled tid <!! testResponse 200 Nothing)
  let statusValue = Public.tfwoStatus status
  liftIO $ assertEqual "Teams should start with LegalHold disabled" statusValue Public.TeamFeatureDisabled
  -- You can still enable for this team
  putEnabled tid Public.TeamFeatureEnabled
  -- But now Adding a user should now fail since the team is too large
  addUserToTeam' owner tid !!! do
    const 403 === statusCode
    const (Just "too-many-members-for-legalhold") === fmap Error.label . responseJsonMaybe

testCannotCreateLegalHoldDeviceOldAPI :: TestM ()
testCannotCreateLegalHoldDeviceOldAPI = do
  member <- randomUser
  (owner, tid) <- createBindingTeam
  ensureQueueEmpty
  -- user without team can't add LH device
  tryout member
  -- team member can't add LH device
  addTeamMemberInternal tid member (rolePermissions RoleMember) Nothing
  ensureQueueEmpty
  tryout member
  -- team owner can't add LH device
  tryout owner
  where
    tryout :: UserId -> TestM ()
    tryout uid = do
      brg <- view tsBrig
      let newClientBody =
            (newClient LegalHoldClientType (someLastPrekeys !! 0))
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
  ensureQueueEmpty

  let findMemberStatus :: [TeamMember] -> Maybe UserLegalHoldStatus
      findMemberStatus ms =
        ms ^? traversed . filtered (has $ userId . only member) . legalHoldStatus

  let check :: HasCallStack => UserLegalHoldStatus -> String -> TestM ()
      check status msg = do
        members' <- view teamMembers <$> getTeamMembers owner tid
        liftIO $
          assertEqual
            ("legal hold status should be " <> msg)
            (Just status)
            (findMemberStatus members')

  check UserLegalHoldNoConsent "disabled when it is disabled for the team"
  withDummyTestServiceForTeam owner tid $ \_chan -> do
    check UserLegalHoldNoConsent "no_consent on new team members"
    grantConsent tid member
    check UserLegalHoldDisabled "disabled on team members that have granted consent"
    requestLegalHoldDevice owner member tid !!! testResponse 201 Nothing
    check UserLegalHoldPending "pending after requesting device"
    approveLegalHoldDevice (Just defPassword) member member tid !!! testResponse 200 Nothing
    check UserLegalHoldEnabled "enabled after confirming device"

testNotInWhitelist :: TestM ()
testNotInWhitelist = do
  g <- view tsGalley
  (owner, tid) <- createBindingTeam
  member <- randomUser
  addTeamMemberInternal tid member (rolePermissions RoleMember) Nothing
  ensureQueueEmpty
  (_, tid') <- createBindingTeam -- we enable for this team, and then see how the above team behaves.
  cannon <- view tsCannon
  WS.bracketR2 cannon member member $ \(_ws, _ws') -> withDummyTestServiceForTeam owner tid $ \_chan -> do
    do
      -- enable NOT-whitelisted team should fail
      withLHWhitelist tid' (putEnabledM' g id tid Public.TeamFeatureEnabled)
        !!! testResponse 403 (Just "legalhold-whitelisted-only")

      -- disable whitelisted team should fail
      withLHWhitelist tid (putEnabledM' g id tid Public.TeamFeatureDisabled)
        !!! testResponse 403 (Just "legalhold-whitelisted-only")

      -- enable whitelisted team should fail (no need for this to work)
      withLHWhitelist tid (putEnabledM' g id tid Public.TeamFeatureEnabled)
        !!! testResponse 403 (Just "legalhold-whitelisted-only")

      -- disable NOT-whitelisted team should fail (no need for this to work)
      withLHWhitelist tid' (putEnabledM' g id tid Public.TeamFeatureDisabled)
        !!! testResponse 403 (Just "legalhold-whitelisted-only")

    do
      -- members have not granted consent implicitly...
      lhs <- view legalHoldStatus <$> withLHWhitelist tid' (getTeamMember' g member tid member)
      liftIO $ assertEqual "" lhs UserLegalHoldNoConsent

      -- ... but can do so explicitly.
      withLHWhitelist tid' (grantConsent' g tid member)
      lhs' <- withLHWhitelist tid' $ view legalHoldStatus <$> getTeamMember' g member tid member
      liftIO $ assertEqual "" lhs' UserLegalHoldDisabled

    do
      -- nobody can request LH device.
      withLHWhitelist tid' (requestLegalHoldDevice' g owner member tid) !!! testResponse 403 (Just "legalhold-not-enabled")
      UserLegalHoldStatusResponse userStatus _ _ <- withLHWhitelist tid' (getUserStatusTyped' g member tid)
      liftIO $
        assertEqual
          "requestLegalHoldDevice should leave user status in Disabled"
          UserLegalHoldDisabled
          userStatus

testInWhitelist :: TestM ()
testInWhitelist = do
  g <- view tsGalley
  (owner, tid) <- createBindingTeam
  member <- randomUser
  addTeamMemberInternal tid member (rolePermissions RoleMember) Nothing
  ensureQueueEmpty
  cannon <- view tsCannon
  WS.bracketR2 cannon member member $ \(_ws, _ws') -> withDummyTestServiceForTeam owner tid $ \_chan -> do
    do
      -- members have granted consent (implicitly)...
      lhs <- view legalHoldStatus <$> withLHWhitelist tid (getTeamMember' g member tid member)
      liftIO $ assertEqual "" lhs UserLegalHoldDisabled

      -- ... and can do so again (idempotency).
      _ <- withLHWhitelist tid (grantConsent' g tid member)
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
  legalholder2 <- view userId <$> addUserToTeam legalholder tid
  ensureQueueEmpty
  (peer, tid2) <-
    -- has to be a team member, granting LH consent for personal users is not supported.
    createBindingTeam
  ensureQueueEmpty

  let doEnableLH :: HasCallStack => UserId -> UserId -> TestM ClientId
      doEnableLH owner uid = do
        requestLegalHoldDevice owner uid tid !!! testResponse 201 Nothing
        approveLegalHoldDevice (Just defPassword) uid uid tid !!! testResponse 200 Nothing
        UserLegalHoldStatusResponse userStatus _ _ <- getUserStatusTyped uid tid
        liftIO $ assertEqual "approving should change status" UserLegalHoldEnabled userStatus
        getInternalClientsFull (UserSet $ Set.fromList [uid])
          <&> userClientsFull
          <&> Map.elems
          <&> Set.unions
          <&> Set.toList
          <&> (\[x] -> x)
          <&> clientId

  withDummyTestServiceForTeam legalholder tid $ \_chan -> do
    grantConsent tid legalholder
    grantConsent tid legalholder2

    legalholderLHDevice <- doEnableLH legalholder legalholder
    _legalholder2LHDevice <- doEnableLH legalholder legalholder2

    legalholderClient <- randomClient legalholder (someLastPrekeys !! 1)
    upgradeClientToLH legalholder legalholderClient
    legalholder2Client <- randomClient legalholder2 (someLastPrekeys !! 3)
    upgradeClientToLH legalholder2 legalholder2Client

    grantConsent tid2 peer
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

        errWith :: (HasCallStack, Typeable a, FromJSON a) => Int -> (a -> Bool) -> ResponseLBS -> TestM ()
        errWith wantStatus wantBody rsp = liftIO $ do
          assertEqual "" wantStatus (statusCode rsp)
          assertBool
            (show $ responseBody rsp)
            ( case responseJsonMaybe rsp of
                Nothing -> False
                Just bdy -> wantBody bdy
            )

    -- LH devices are treated as clients that have the ClientSupportsLegalholdImplicitConsent
    -- capability (so LH doesn't break for users who have LH devices; it sounds silly, but
    -- it's good to test this, since it did require adding a few lines of production code in
    -- 'addClient' about client capabilities).
    runit legalholder legalholderClient >>= errWith 412 (\(_ :: Msg.ClientMismatch) -> True)

    -- If user has a client without the ClientSupportsLegalholdImplicitConsent
    -- capability then message sending is prevented to legalhold devices.
    peerClient <- randomClient peer (someLastPrekeys !! 2)
    runit peer peerClient >>= errWith 412 (\err -> Error.label err == "missing-legalhold-consent")
    upgradeClientToLH peer peerClient
    runit peer peerClient >>= errWith 412 (\(_ :: Msg.ClientMismatch) -> True)

-- If LH is activated for other user in 1:1 conv, 1:1 conv is blocked
testNoConsentBlockOne2OneConv :: HasCallStack => Bool -> Bool -> Bool -> Bool -> TestM ()
testNoConsentBlockOne2OneConv connectFirst teamPeer approveLH testPendingConnection = do
  -- FUTUREWORK: maybe regular user for legalholder?
  (legalholder :: UserId, tid) <- createBindingTeam
  peer :: UserId <- if teamPeer then fst <$> createBindingTeam else randomUser
  galley <- view tsGalley

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
            getInternalClientsFull (UserSet $ Set.fromList [legalholder])
              <&> userClientsFull
              <&> Map.elems
              <&> Set.unions
              <&> Set.toList
              <&> (\[x] -> x)
              <&> clientId
              <&> Just
          else pure Nothing

      doDisableLH :: HasCallStack => TestM ()
      doDisableLH = do
        -- remove (only) LH device again
        withLHWhitelist tid (disableLegalHoldForUser' galley (Just defPassword) tid legalholder legalholder)
          !!! testResponse 200 Nothing

  cannon <- view tsCannon

  WS.bracketR2 cannon legalholder peer $ \(legalholderWs, peerWs) -> withDummyTestServiceForTeam legalholder tid $ \_chan -> do
    if connectFirst
      then do
        postConnection legalholder peer !!! const 201 === statusCode

        mbConn :: Maybe UserConnection <-
          if testPendingConnection
            then pure Nothing
            else do
              res <- putConnection peer legalholder Conn.Accepted <!! const 200 === statusCode
              pure $ Just $ responseJsonUnsafe res

        ensureQueueEmpty

        mbLegalholderLHDevice <- doEnableLH

        assertConnections legalholder [ConnectionStatus legalholder peer Conn.MissingLegalholdConsent]
        assertConnections peer [ConnectionStatus peer legalholder Conn.MissingLegalholdConsent]

        forM_ [legalholderWs, peerWs] $ \ws -> do
          -- (if this fails, it may be because there are other messages in the queue, but i
          -- think we implemented this in a way that doens't trip over wrong orderings.)
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
            convId
            [ (legalholder, legalholderLHDevice, "cipher")
            ]
            !!! do
              const 404 === statusCode
              const (Just "no-conversation") === fmap Error.label . responseJsonMaybe

        do
          doDisableLH
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

        -- conversation reappears. peer can send message to legalholder again
        for_ ((,) <$> (mbConn >>= Conn.ucConvId) <*> mbLegalholderLHDevice) $ \(convId, legalholderLHDevice) -> do
          postOtrMessage
            id
            peer
            peerClient
            convId
            [ (legalholder, legalholderLHDevice, "cipher")
            ]
            !!! do
              const 201 === statusCode
      else do
        void doEnableLH
        postConnection legalholder peer !!! do testResponse 412 (Just "missing-legalhold-consent")
        postConnection peer legalholder !!! do testResponse 412 (Just "missing-legalhold-consent")

testNoConsentBlockGroupConv :: TestM ()
testNoConsentBlockGroupConv = do
  -- "If LH is activated for other user in group conv, this user gets removed with helpful message"
  -- tracked here: https://wearezeta.atlassian.net/browse/SQSERVICES-428
  pure ()

data TestClaimKeys
  = TCKConsentMissing
  | TCKOldClient
  | TCKConsentAndNewClients

testClaimKeys :: TestClaimKeys -> TestM ()
testClaimKeys testcase = do
  -- "cannot fetch prekeys of LH users if requester did not give consent or has old clients"
  (legalholder, tid) <- createBindingTeam
  ensureQueueEmpty
  (peer, teamPeer) <- createBindingTeam
  ensureQueueEmpty

  let doEnableLH :: HasCallStack => TeamId -> UserId -> UserId -> TestM ClientId
      doEnableLH team owner uid = do
        requestLegalHoldDevice owner uid team !!! testResponse 201 Nothing
        approveLegalHoldDevice (Just defPassword) uid uid team !!! testResponse 200 Nothing
        UserLegalHoldStatusResponse userStatus _ _ <- getUserStatusTyped uid team
        liftIO $ assertEqual "approving should change status" UserLegalHoldEnabled userStatus
        getInternalClientsFull (UserSet $ Set.fromList [uid])
          <&> userClientsFull
          <&> Map.elems
          <&> Set.unions
          <&> Set.toList
          <&> (\[x] -> x)
          <&> clientId

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

  let assertResponse :: Assertions ()
      assertResponse = case testcase of
        TCKConsentMissing -> bad
        TCKOldClient -> bad
        TCKConsentAndNewClients -> good
        where
          good = testResponse 200 Nothing
          bad = testResponse 412 (Just "missing-legalhold-consent")

  let fetchKeys :: ClientId -> TestM ()
      fetchKeys legalholderLHDevice = do
        getUsersPrekeysClientUnqualified peer legalholder legalholderLHDevice !!! assertResponse
        getUsersPrekeyBundleUnqualified peer legalholder !!! assertResponse
        let userClients = UserClients (Map.fromList [(legalholder, Set.fromList [legalholderLHDevice])])
        getMultiUserPrekeyBundleUnqualified peer userClients !!! assertResponse

  withDummyTestServiceForTeam legalholder tid $ \_chan -> do
    grantConsent tid legalholder
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
  galley <- view tsGalley

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
      ensureQueueEmpty

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

----------------------------------------------------------------------
-- API helpers

getEnabled :: HasCallStack => TeamId -> TestM ResponseLBS
getEnabled tid = do
  g <- view tsGalley
  get $
    g
      . paths ["i", "teams", toByteString' tid, "features", "legalhold"]

renewToken :: HasCallStack => Text -> TestM ()
renewToken tok = do
  b <- view tsBrig
  void . post $
    b
      . paths ["access"]
      . cookieRaw "zuid" (toByteString' tok)
      . expect2xx

putEnabled :: HasCallStack => TeamId -> Public.TeamFeatureStatusValue -> TestM ()
putEnabled tid enabled = do
  g <- view tsGalley
  putEnabledM g tid enabled

putEnabledM :: (HasCallStack, MonadHttp m, MonadIO m) => GalleyR -> TeamId -> Public.TeamFeatureStatusValue -> m ()
putEnabledM g tid enabled = void $ putEnabledM' g expect2xx tid enabled

putEnabled' :: HasCallStack => (Bilge.Request -> Bilge.Request) -> TeamId -> Public.TeamFeatureStatusValue -> TestM ResponseLBS
putEnabled' extra tid enabled = do
  g <- view tsGalley
  putEnabledM' g extra tid enabled

putEnabledM' :: (HasCallStack, MonadHttp m, MonadIO m) => GalleyR -> (Bilge.Request -> Bilge.Request) -> TeamId -> Public.TeamFeatureStatusValue -> m ResponseLBS
putEnabledM' g extra tid enabled = do
  put $
    g
      . paths ["i", "teams", toByteString' tid, "features", "legalhold"]
      . json (Public.TeamFeatureStatusNoConfig enabled)
      . extra

postSettings :: HasCallStack => UserId -> TeamId -> NewLegalHoldService -> TestM ResponseLBS
postSettings uid tid new =
  -- Retry calls to this endpoint, on k8s it sometimes takes a while to establish a working
  -- connection.
  retrying policy only412 $ \_ -> do
    g <- view tsGalley
    post $
      g
        . paths ["teams", toByteString' tid, "legalhold", "settings"]
        . zUser uid
        . zConn "conn"
        . zType "access"
        . json new
  where
    policy :: RetryPolicy
    policy = exponentialBackoff 50 <> limitRetries 5
    only412 :: RetryStatus -> ResponseLBS -> TestM Bool
    only412 _ resp = pure $ statusCode resp == 412

getSettingsTyped :: HasCallStack => UserId -> TeamId -> TestM ViewLegalHoldService
getSettingsTyped uid tid = responseJsonUnsafe <$> (getSettings uid tid <!! testResponse 200 Nothing)

getSettings :: HasCallStack => UserId -> TeamId -> TestM ResponseLBS
getSettings uid tid = do
  g <- view tsGalley
  get $
    g
      . paths ["teams", toByteString' tid, "legalhold", "settings"]
      . zUser uid
      . zConn "conn"
      . zType "access"

deleteSettings :: HasCallStack => Maybe PlainTextPassword -> UserId -> TeamId -> TestM ResponseLBS
deleteSettings mPassword uid tid = do
  g <- view tsGalley
  delete $
    g
      . paths ["teams", toByteString' tid, "legalhold", "settings"]
      . zUser uid
      . zConn "conn"
      . zType "access"
      . json (RemoveLegalHoldSettingsRequest mPassword)

getUserStatusTyped :: HasCallStack => UserId -> TeamId -> TestM UserLegalHoldStatusResponse
getUserStatusTyped uid tid = do
  g <- view tsGalley
  getUserStatusTyped' g uid tid

getUserStatusTyped' :: (HasCallStack, MonadHttp m, MonadIO m, MonadCatch m) => GalleyR -> UserId -> TeamId -> m UserLegalHoldStatusResponse
getUserStatusTyped' g uid tid = do
  resp <- getUserStatus' g uid tid <!! testResponse 200 Nothing
  return $ responseJsonUnsafe resp

getUserStatus' :: (HasCallStack, MonadHttp m, MonadIO m) => GalleyR -> UserId -> TeamId -> m ResponseLBS
getUserStatus' g uid tid = do
  get $
    g
      . paths ["teams", toByteString' tid, "legalhold", toByteString' uid]
      . zUser uid
      . zConn "conn"
      . zType "access"

approveLegalHoldDevice :: HasCallStack => Maybe PlainTextPassword -> UserId -> UserId -> TeamId -> TestM ResponseLBS
approveLegalHoldDevice mPassword zusr uid tid = do
  g <- view tsGalley
  approveLegalHoldDevice' g mPassword zusr uid tid

approveLegalHoldDevice' ::
  (HasCallStack, MonadHttp m, MonadIO m) =>
  GalleyR ->
  Maybe PlainTextPassword ->
  UserId ->
  UserId ->
  TeamId ->
  m ResponseLBS
approveLegalHoldDevice' g mPassword zusr uid tid = do
  put $
    g
      . paths ["teams", toByteString' tid, "legalhold", toByteString' uid, "approve"]
      . zUser zusr
      . zConn "conn"
      . zType "access"
      . json (ApproveLegalHoldForUserRequest mPassword)

disableLegalHoldForUser ::
  HasCallStack =>
  Maybe PlainTextPassword ->
  TeamId ->
  UserId ->
  UserId ->
  TestM ResponseLBS
disableLegalHoldForUser mPassword tid zusr uid = do
  g <- view tsGalley
  disableLegalHoldForUser' g mPassword tid zusr uid

disableLegalHoldForUser' ::
  (HasCallStack, MonadHttp m, MonadIO m) =>
  GalleyR ->
  Maybe PlainTextPassword ->
  TeamId ->
  UserId ->
  UserId ->
  m ResponseLBS
disableLegalHoldForUser' g mPassword tid zusr uid = do
  delete $
    g
      . paths ["teams", toByteString' tid, "legalhold", toByteString' uid]
      . zUser zusr
      . zType "access"
      . json (DisableLegalHoldForUserRequest mPassword)

_postOtrMessageJson :: UserId -> ConvId -> TestM ResponseLBS
_postOtrMessageJson zusr cnvid = do
  g <- view tsGalley
  otrmsg :: Msg.NewOtrMessage <- pure (error "TODO")
  post $
    g
      . paths ["conversations", toByteString' cnvid, "otr", "messages"]
      . zUser zusr
      . zConn "conn"
      . zType "access"
      -- TODO: @.&. def Public.OtrReportAllMissing filterMissing@
      . json otrmsg

_postOtrMessageProto :: UserId -> ConvId -> TestM ResponseLBS
_postOtrMessageProto = (error "TODO")

assertExactlyOneLegalHoldDevice :: HasCallStack => UserId -> TestM ()
assertExactlyOneLegalHoldDevice uid = do
  clients :: [Client] <-
    getClients uid >>= responseJsonError
  liftIO $ do
    let numdevs = length $ clientType <$> clients
    assertEqual ("expected exactly one legal hold device for user: " <> show uid) numdevs 1

assertZeroLegalHoldDevices :: HasCallStack => UserId -> TestM ()
assertZeroLegalHoldDevices uid = do
  clients :: [Client] <-
    getClients uid >>= responseJsonError
  liftIO $ do
    let numdevs = length $ clientType <$> clients
    assertBool
      ( "a legal hold device was found when none was expected for user"
          <> show uid
      )
      (numdevs == 0)

---------------------------------------------------------------------
--- Device helpers

grantConsent :: HasCallStack => TeamId -> UserId -> TestM ()
grantConsent tid zusr = do
  g <- view tsGalley
  grantConsent' g tid zusr

grantConsent' :: (HasCallStack, MonadHttp m, MonadIO m) => GalleyR -> TeamId -> UserId -> m ()
grantConsent' = grantConsent'' expect2xx

grantConsent'' :: (HasCallStack, MonadHttp m, MonadIO m) => (Bilge.Request -> Bilge.Request) -> GalleyR -> TeamId -> UserId -> m ()
grantConsent'' expectation g tid zusr = do
  void . post $
    g
      . paths ["teams", toByteString' tid, "legalhold", "consent"]
      . zUser zusr
      . zConn "conn"
      . zType "access"
      . expectation

requestLegalHoldDevice :: HasCallStack => UserId -> UserId -> TeamId -> TestM ResponseLBS
requestLegalHoldDevice zusr uid tid = do
  g <- view tsGalley
  requestLegalHoldDevice' g zusr uid tid

requestLegalHoldDevice' :: (HasCallStack, MonadHttp m, MonadIO m) => GalleyR -> UserId -> UserId -> TeamId -> m ResponseLBS
requestLegalHoldDevice' g zusr uid tid = do
  post $
    g
      . paths ["teams", toByteString' tid, "legalhold", toByteString' uid]
      . zUser zusr
      . zConn "conn"
      . zType "access"

--------------------------------------------------------------------
-- setup helpers

-- | Create a new legal hold service creation request with the URL from the integration test
-- config.
newLegalHoldService :: HasCallStack => TestM NewLegalHoldService
newLegalHoldService = do
  config <- view (tsIConf . to provider)
  key' <- liftIO $ readServiceKey (publicKey config)
  let Just url =
        fromByteString $
          encodeUtf8 (botHost config) <> ":" <> cs (show (botPort config)) <> "/legalhold"
  return
    NewLegalHoldService
      { newLegalHoldServiceUrl = url,
        newLegalHoldServiceKey = key',
        newLegalHoldServiceToken = ServiceToken "tok"
      }

-- | FUTUREWORK: reduce duplication (copied from brig/Provider.hs)
readServiceKey :: (HasCallStack, MonadIO m) => FilePath -> m ServiceKeyPEM
readServiceKey fp = liftIO $ do
  bs <- BS.readFile fp
  let Right [k] = pemParseBS bs
  return (ServiceKeyPEM k)

-- FUTUREWORK: run this test suite against an actual LH service (by changing URL and key in
-- the config file), and see if it works as well as with our mock service.
withDummyTestServiceForTeam ::
  forall a.
  HasCallStack =>
  UserId ->
  TeamId ->
  -- | the test
  (Chan (Wai.Request, LBS) -> TestM a) ->
  TestM a
withDummyTestServiceForTeam owner tid go = do
  withTestService dummyService runTest
  where
    runTest :: Chan (Wai.Request, LBS) -> TestM a
    runTest chan = do
      newService <- newLegalHoldService
      putEnabled tid Public.TeamFeatureEnabled -- enable it for this team
      postSettings owner tid newService !!! testResponse 201 Nothing
      go chan

    dummyService :: Chan (Wai.Request, LBS) -> Wai.Application
    dummyService ch req cont = do
      reqBody <- Wai.strictRequestBody req
      writeChan ch (req, reqBody)
      case (pathInfo req, requestMethod req, getRequestHeader "Authorization" req) of
        (["legalhold", "status"], "GET", _) -> cont respondOk
        (_, _, Nothing) -> cont missingAuth
        (["legalhold", "initiate"], "POST", Just _) -> cont initiateResp
        (["legalhold", "confirm"], "POST", Just _) ->
          cont respondOk
        (["legalhold", "remove"], "POST", Just _) -> cont respondOk
        _ -> cont respondBad

    initiateResp :: Wai.Response
    initiateResp =
      Wai.json $
        -- FUTUREWORK: use another key to prevent collisions with keys used by tests
        NewLegalHoldClient somePrekeys (head $ someLastPrekeys)

    respondOk :: Wai.Response
    respondOk = responseLBS status200 mempty mempty

    respondBad :: Wai.Response
    respondBad = responseLBS status404 mempty mempty

    missingAuth :: Wai.Response
    missingAuth = responseLBS status400 mempty "no authorization header"

    getRequestHeader :: String -> Wai.Request -> Maybe ByteString
    getRequestHeader name req = lookup (fromString name) $ requestHeaders req

withLHWhitelist :: forall a. HasCallStack => TeamId -> WaiTest.Session a -> TestM a
withLHWhitelist tid action = do
  opts <- view tsGConf
  let opts' =
        opts & optSettings . setLegalHoldTeamsWhitelist .~ Just [tid]
          & optSettings . setFeatureFlags . flagLegalHold .~ FeatureLegalHoldWhitelistTeamsAndImplicitConsent
  withSettingsOverrides opts' action

-- | Run a test with an mock legal hold service application.  The mock service is also binding
-- to a TCP socket for the backend to connect to.  The mock service can expose internal
-- details to the test (for both read and write) via a 'Chan'.
--
-- WARNINGS: (1) This is not concurrency-proof!  (2) tests need to be written in a way that
-- they can be run several times if they fail the first time.  this is the allow for the ssl
-- service to have some time to propagate through the test system (needed on k8s).
withTestService ::
  HasCallStack =>
  -- | the mock service
  (Chan e -> Application) ->
  -- | the test
  (Chan e -> TestM a) ->
  TestM a
withTestService mkApp go = do
  config <- view (tsIConf . to provider)
  let tlss = Warp.tlsSettings (cert config) (privateKey config)
  let defs = Warp.defaultSettings {Warp.settingsPort = botPort config}
  buf <- liftIO newChan
  srv <-
    liftIO . Async.async $
      Warp.runTLS tlss defs $
        mkApp buf
  go buf `finally` liftIO (Async.cancel srv)

publicKeyNotMatchingService :: PEM
publicKeyNotMatchingService =
  let Right [k] =
        pemParseBS . BS.unlines $
          [ "-----BEGIN PUBLIC KEY-----",
            "MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAu+Kg/PHHU3atXrUbKnw0",
            "G06FliXcNt3lMwl2os5twEDcPPFw/feGiAKymxp+7JqZDrseS5D9THGrW+OQRIPH",
            "WvUBdiLfGrZqJO223DB6D8K2Su/odmnjZJ2z23rhXoEArTplu+Dg9K+c2LVeXTKV",
            "VPOaOzgtAB21XKRiQ4ermqgi3/njr03rXyq/qNkuNd6tNcg+HAfGxfGvvCSYBfiS",
            "bUKr/BeArYRcjzr/h5m1In6fG/if9GEI6m8dxHT9JbY53wiksowy6ajCuqskIFg8",
            "7X883H+LA/d6X5CTiPv1VMxXdBUiGPuC9IT/6CNQ1/LFt0P37ax58+LGYlaFo7la",
            "nQIDAQAZ",
            "-----END PUBLIC KEY-----"
          ]
   in k

testGetLegalholdStatus :: TestM ()
testGetLegalholdStatus = do
  (owner1, tid1) <- createBindingTeam
  member1 <- view userId <$> addUserToTeam owner1 tid1
  ensureQueueEmpty

  (owner2, tid2) <- createBindingTeam
  member2 <- view userId <$> addUserToTeam owner2 tid2
  ensureQueueEmpty

  personal <- randomUser

  let check :: HasCallStack => UserId -> UserId -> Maybe TeamId -> UserLegalHoldStatus -> TestM ()
      check getter targetUser targetTeam stat = do
        profile <- getUserProfile getter targetUser
        when (profileLegalholdStatus profile /= stat) $ do
          meminfo <- getUserStatusTyped targetUser `mapM` targetTeam

          liftIO . forM_ meminfo $ \mem -> do
            assertEqual "member LH status" stat (ulhsrStatus mem)
            assertEqual "team id in brig user record" targetTeam (profileTeam profile)

          liftIO $ assertEqual "user profile status info" stat (profileLegalholdStatus profile)

      requestDev :: HasCallStack => UserId -> UserId -> TeamId -> TestM ()
      requestDev requestor target tid = do
        requestLegalHoldDevice requestor target tid !!! testResponse 201 Nothing

      approveDev :: HasCallStack => UserId -> TeamId -> TestM ()
      approveDev target tid = do
        approveLegalHoldDevice (Just defPassword) target target tid !!! testResponse 200 Nothing

  check owner1 member1 (Just tid1) UserLegalHoldNoConsent
  check member1 member1 (Just tid1) UserLegalHoldNoConsent
  check owner2 member1 (Just tid1) UserLegalHoldNoConsent
  check member2 member1 (Just tid1) UserLegalHoldNoConsent
  check personal member1 (Just tid1) UserLegalHoldNoConsent
  check owner1 personal Nothing UserLegalHoldNoConsent
  check member1 personal Nothing UserLegalHoldNoConsent
  check owner2 personal Nothing UserLegalHoldNoConsent
  check member2 personal Nothing UserLegalHoldNoConsent
  check personal personal Nothing UserLegalHoldNoConsent

  onlyIfLhEnabled $
    withDummyTestServiceForTeam owner1 tid1 $ \_chan -> do
      grantConsent tid1 member1
      check owner1 member1 (Just tid1) UserLegalHoldDisabled
      check member2 member1 (Just tid1) UserLegalHoldDisabled
      check personal member1 (Just tid1) UserLegalHoldDisabled

      requestDev owner1 member1 tid1
      check personal member1 (Just tid1) UserLegalHoldPending

      approveDev member1 tid1
      check personal member1 (Just tid1) UserLegalHoldEnabled

----------------------------------------------------------------------
-- test helpers

deriving instance Show Ev.Event

deriving instance Show Ev.UserEvent

deriving instance Show Ev.ClientEvent

deriving instance Show Ev.PropertyEvent

deriving instance Show Ev.ConnectionEvent

-- (partial implementation, just good enough to make the tests work)
instance FromJSON Ev.Event where
  parseJSON ev = flip (withObject "Ev.Event") ev $ \o -> do
    typ :: Text <- o .: "type"
    if
        | typ `elem` ["user.legalhold-request", "user.legalhold-enable", "user.legalhold-disable"] -> Ev.UserEvent <$> Aeson.parseJSON ev
        | typ `elem` ["user.client-add", "user.client-remove"] -> Ev.ClientEvent <$> Aeson.parseJSON ev
        | typ `elem` ["user.connection"] -> Ev.ConnectionEvent <$> Aeson.parseJSON ev
        | otherwise -> fail $ "Ev.Event: unsupported event type: " <> show typ

-- (partial implementation, just good enough to make the tests work)
instance FromJSON Ev.UserEvent where
  parseJSON = withObject "Ev.UserEvent" $ \o -> do
    tag :: Text <- o .: "type"
    case tag of
      "user.legalhold-enable" -> Ev.UserLegalHoldEnabled <$> o .: "id"
      "user.legalhold-disable" -> Ev.UserLegalHoldDisabled <$> o .: "id"
      "user.legalhold-request" ->
        Ev.LegalHoldClientRequested
          <$> ( Ev.LegalHoldClientRequestedData
                  <$> o .: "id" -- this is the target user
                  <*> o .: "last_prekey"
                  <*> (o .: "client" >>= withObject "id" (.: "id"))
              )
      x -> fail $ "Ev.UserEvent: unsupported event type: " ++ show x

-- (partial implementation, just good enough to make the tests work)
instance FromJSON Ev.ClientEvent where
  parseJSON = withObject "Ev.ClientEvent" $ \o -> do
    tag :: Text <- o .: "type"
    case tag of
      "user.client-add" -> Ev.ClientAdded fakeuid <$> o .: "client"
      "user.client-remove" -> Ev.ClientRemoved fakeuid <$> (makeFakeClient <$> (o .: "client" >>= withObject "id" (.: "id")))
      x -> fail $ "Ev.ClientEvent: unsupported event type: " ++ show x
    where
      fakeuid = read @UserId "6980fb5e-ba64-11eb-a339-0b3625bf01be"
      makeFakeClient cid =
        Client
          cid
          PermanentClientType
          (toUTCTimeMillis $ read "2021-05-23 09:39:15.937523809 UTC")
          Nothing
          Nothing
          Nothing
          Nothing
          Nothing
          (Client.ClientCapabilityList mempty)

instance FromJSON Ev.ConnectionEvent where
  parseJSON = Aeson.withObject "ConnectionEvent" $ \o -> do
    tag :: Text <- o .: "type"
    case tag of
      "user.connection" ->
        Ev.ConnectionUpdated
          <$> o .: "connection"
          <*> pure Nothing
          <*> pure Nothing
      x -> fail $ "unspported event type: " ++ show x

assertNotification :: (HasCallStack, FromJSON a, MonadIO m) => WS.WebSocket -> (a -> Assertion) -> m ()
assertNotification ws predicate =
  void . liftIO . WS.assertMatch (5 WS.# WS.Second) ws $ \notif -> do
    unless ((NonEmpty.length . List1.toNonEmpty $ ntfPayload $ notif) == 1) $
      error $ "not suppored by test helper: event with more than one object in the payload: " <> cs (Aeson.encode notif)
    let j = Aeson.Object $ List1.head (ntfPayload notif)
    case Aeson.fromJSON j of
      Aeson.Success x -> predicate x
      Aeson.Error s -> error $ s ++ " in " ++ cs (Aeson.encode j)

assertNoNotification :: (HasCallStack, MonadIO m) => WS.WebSocket -> m ()
assertNoNotification ws = void . liftIO $ WS.assertNoEvent (5 WS.# WS.Second) [ws]

assertMatchJSON :: (HasCallStack, FromJSON a, MonadThrow m, MonadCatch m, MonadIO m) => Chan (Wai.Request, LBS) -> (a -> m ()) -> m ()
assertMatchJSON c match = do
  assertMatchChan c $ \(_, reqBody) -> do
    case Aeson.eitherDecode reqBody of
      Right x -> match x
      Left s -> error $ s ++ " in " ++ cs reqBody

assertMatchChan :: (HasCallStack, MonadThrow m, MonadCatch m, MonadIO m) => Chan a -> (a -> m ()) -> m ()
assertMatchChan c match = go []
  where
    refill = mapM_ (liftIO <$> writeChan c)
    go buf = do
      m <- liftIO . timeout (5 WS.# WS.Second) . readChan $ c
      case m of
        Just n ->
          do
            match n
            refill buf
            `catchAll` \e -> case asyncExceptionFromException e of
              Just x -> error $ show (x :: SomeAsyncException)
              Nothing -> go (n : buf)
        Nothing -> do
          refill buf
          error "Timeout"
