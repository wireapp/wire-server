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
import qualified API.SQS as SQS
import API.Util
import Bilge hiding (accept, head, timeout, trace)
import Bilge.Assert
import qualified Bilge.TestSession as BilgeTest
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
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.List1 as List1
import qualified Data.Map.Strict as Map
import Data.Misc (PlainTextPassword)
import Data.PEM
import Data.Qualified (Qualified (Qualified))
import Data.Range
import qualified Data.Set as Set
import Data.String.Conversions (LBS, cs)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Time.Clock as Time
import qualified Galley.App as Galley
import qualified Galley.Data as Data
import qualified Galley.Data.LegalHold as LegalHoldData
import Galley.External.LegalHoldService (validateServiceKey)
import Galley.Options (optSettings, setFeatureFlags)
import qualified Galley.Types.Clients as Clients
import Galley.Types.Teams
import Gundeck.Types.Notification (ntfPayload)
import Imports
import Network.HTTP.Types.Status (status200, status400, status404)
import Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.Warp.Internal as Warp
import qualified Network.Wai.Handler.WarpTLS as Warp
import qualified Network.Wai.Utilities.Error as Error
import qualified Network.Wai.Utilities.Response as Wai
import System.IO (hPutStrLn)
import Test.QuickCheck.Instances ()
import Test.Tasty
import qualified Test.Tasty.Cannon as WS
import Test.Tasty.HUnit
import TestHelpers
import TestSetup
import Wire.API.Connection (UserConnection)
import qualified Wire.API.Connection as Conn
import Wire.API.Conversation.Role (roleNameWireAdmin, roleNameWireMember)
import qualified Wire.API.Message as Msg
import qualified Wire.API.Team.Feature as Public
import Wire.API.User (UserProfile (..))
import Wire.API.User.Client (UserClients (..), UserClientsFull (userClientsFull))
import qualified Wire.API.User.Client as Client

onlyIfLhWhitelisted :: TestM () -> TestM ()
onlyIfLhWhitelisted action = do
  featureLegalHold <- view (tsGConf . optSettings . setFeatureFlags . flagLegalHold)
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
      test s "POST /teams/{tid}/legalhold/{uid}" (onlyIfLhWhitelisted testRequestLegalHoldDevice),
      test s "PUT /teams/{tid}/legalhold/approve" (onlyIfLhWhitelisted testApproveLegalHoldDevice),
      test s "(user denies approval: nothing needs to be done in backend)" (pure ()),
      test s "GET /teams/{tid}/legalhold/{uid}" (onlyIfLhWhitelisted testGetLegalHoldDeviceStatus),
      test s "DELETE /teams/{tid}/legalhold/{uid}" (onlyIfLhWhitelisted testDisableLegalHoldForUser),
      -- legal hold settings
      test s "POST /teams/{tid}/legalhold/settings" (onlyIfLhWhitelisted testCreateLegalHoldTeamSettings),
      test s "GET /teams/{tid}/legalhold/settings" (onlyIfLhWhitelisted testGetLegalHoldTeamSettings),
      test s "Not implemented: DELETE /teams/{tid}/legalhold/settings" (onlyIfLhWhitelisted testRemoveLegalHoldFromTeam),
      test s "GET [/i]?/teams/{tid}/legalhold" (onlyIfLhWhitelisted testEnablePerTeam),
      -- behavior of existing end-points
      test s "POST /clients" (onlyIfLhWhitelisted testCannotCreateLegalHoldDeviceOldAPI),
      test s "GET /teams/{tid}/members" (onlyIfLhWhitelisted testGetTeamMembersIncludesLHStatus),
      test s "POST /register - can add team members above fanout limit when whitelisting is enabled" (onlyIfLhWhitelisted testAddTeamUserTooLargeWithLegalholdWhitelisted),
      test s "GET legalhold status in user profile" (onlyIfLhWhitelisted testGetLegalholdStatus),
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
              testGroup "no-consent" $
                flip fmap [(a, b, c, d) | a <- [minBound ..], b <- [minBound ..], c <- [minBound ..], d <- [minBound ..]] $
                  \args@(a, b, c, d) ->
                    test s (show args) $ testNoConsentBlockOne2OneConv a b c d,
              testGroup
                "Legalhold is activated for user A in a group conversation"
                [ test s "All admins are consenting: all non-consenters get removed from conversation" (onlyIfLhWhitelisted (testNoConsentRemoveFromGroupConv LegalholderIsAdmin)),
                  test s "Some admins are consenting: all non-consenters get removed from conversation" (onlyIfLhWhitelisted (testNoConsentRemoveFromGroupConv BothAreAdmins)),
                  test s "No admins are consenting: all LH activated/pending users get removed from conversation" (onlyIfLhWhitelisted (testNoConsentRemoveFromGroupConv PeerIsAdmin))
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
                    [ test s "If any user in the invite has not given consent then the invite fails" (onlyIfLhWhitelisted testNoConsentCannotBeInvited)
                    ]
                ],
              test s "Cannot create conversation with both LH activated and non-consenting users" (onlyIfLhWhitelisted testCannotCreateGroupWithUsersInConflict),
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
    [test s "PUT, DELETE /i/legalhold/whitelisted-teams" (onlyIfLhWhitelisted testWhitelistingTeams)]

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
  ensureQueueEmpty

testRequestLegalHoldDevice :: TestM ()
testRequestLegalHoldDevice = withTeam $ \owner tid -> do
  member <- randomUser
  addTeamMemberInternal tid member (rolePermissions RoleMember) Nothing
  ensureQueueEmpty
  -- Can't request a device if team feature flag is disabled
  requestLegalHoldDevice owner member tid !!! testResponse 403 (Just "legalhold-not-enabled")
  cannon <- view tsCannon
  -- Assert that the appropriate LegalHold Request notification is sent to the user's
  -- clients
  WS.bracketR2 cannon member member $ \(ws, ws') -> withDummyTestServiceForTeamNoService $ \_chan -> do
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
    newService <- newLegalHoldService
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
    ensureQueueEmpty

testApproveLegalHoldDevice :: TestM ()
testApproveLegalHoldDevice = do
  (owner, tid) <- createBindingTeam
  ensureQueueEmpty
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
  ensureQueueEmpty
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

  putLHWhitelistTeam tid !!! const 200 === statusCode
  withDummyTestServiceForTeamNoService $ \_chan -> do
    do
      UserLegalHoldStatusResponse userStatus lastPrekey' clientId' <- getUserStatusTyped member tid
      liftIO $
        do
          assertEqual "User legal hold status should start as disabled" UserLegalHoldDisabled userStatus
          assertEqual "last_prekey should be Nothing when LH is disabled" Nothing lastPrekey'
          assertEqual "client.id should be Nothing when LH is disabled" Nothing clientId'

    do
      newService <- newLegalHoldService
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
  ensureQueueEmpty
  cannon <- view tsCannon
  putLHWhitelistTeam tid !!! const 200 === statusCode
  WS.bracketR2 cannon owner member $ \(ows, mws) -> withDummyTestServiceForTeam owner tid $ \chan -> do
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
testCreateLegalHoldTeamSettings = withTeam $ \owner tid -> do
  putLHWhitelistTeam tid !!! const 200 === statusCode
  member <- randomUser
  addTeamMemberInternal tid member (rolePermissions RoleMember) Nothing
  ensureQueueEmpty
  newService <- newLegalHoldService
  -- not allowed to create if team is not whitelisted
  postSettings owner tid newService !!! testResponse 412 (Just "legalhold-unavailable")

  putLHWhitelistTeam tid !!! const 200 === statusCode

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
  ensureQueueEmpty
  -- fails if LH for team is disabled
  deleteSettings (Just defPassword) owner tid !!! testResponse 403 (Just "legalhold-disable-unimplemented")

testEnablePerTeam :: TestM ()
testEnablePerTeam = withTeam $ \owner tid -> do
  member <- randomUser
  addTeamMemberInternal tid member (rolePermissions RoleMember) Nothing
  ensureQueueEmpty
  do
    status :: Public.TeamFeatureStatus 'Public.TeamFeatureLegalHold <- responseJsonUnsafe <$> (getEnabled tid <!! testResponse 200 Nothing)
    let statusValue = Public.tfwoStatus status
    liftIO $ assertEqual "Teams should start with LegalHold disabled" statusValue Public.TeamFeatureDisabled

  putLHWhitelistTeam tid !!! const 200 === statusCode

  do
    status :: Public.TeamFeatureStatus 'Public.TeamFeatureLegalHold <- responseJsonUnsafe <$> (getEnabled tid <!! testResponse 200 Nothing)
    let statusValue = Public.tfwoStatus status
    liftIO $ assertEqual "Calling 'putEnabled True' should enable LegalHold" statusValue Public.TeamFeatureEnabled
  withDummyTestServiceForTeam owner tid $ \_chan -> do
    putLHWhitelistTeam tid !!! const 200 === statusCode
    requestLegalHoldDevice owner member tid !!! const 201 === statusCode
    approveLegalHoldDevice (Just defPassword) member member tid !!! testResponse 200 Nothing
    do
      UserLegalHoldStatusResponse status _ _ <- getUserStatusTyped member tid
      liftIO $ assertEqual "User legal hold status should be enabled" UserLegalHoldEnabled status
    do
      putEnabled' id tid Public.TeamFeatureDisabled !!! testResponse 403 (Just "legalhold-whitelisted-only")
      status :: Public.TeamFeatureStatus 'Public.TeamFeatureLegalHold <- responseJsonUnsafe <$> (getEnabled tid <!! testResponse 200 Nothing)
      let statusValue = Public.tfwoStatus status
      liftIO $ assertEqual "Calling 'putEnabled False' should have no effect." statusValue Public.TeamFeatureEnabled

testAddTeamUserTooLargeWithLegalholdWhitelisted :: HasCallStack => TestM ()
testAddTeamUserTooLargeWithLegalholdWhitelisted = withTeam $ \owner tid -> do
  o <- view tsGConf
  let fanoutLimit = fromIntegral @_ @Integer . fromRange $ Galley.currentFanoutLimit o
  forM_ [2 .. (fanoutLimit + 5)] $ \_n -> do
    addUserToTeam' owner tid !!! do
      const 201 === statusCode
  ensureQueueEmpty

testCannotCreateLegalHoldDeviceOldAPI :: TestM ()
testCannotCreateLegalHoldDeviceOldAPI = do
  member <- randomUser
  ensureQueueEmpty
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
  withDummyTestServiceForTeamNoService $ \_chan -> do
    check UserLegalHoldNoConsent "no_consent on new team members"

    putLHWhitelistTeam tid !!! const 200 === statusCode
    newService <- newLegalHoldService
    postSettings owner tid newService !!! testResponse 201 Nothing

    check UserLegalHoldDisabled "disabled on team members that have granted consent"
    requestLegalHoldDevice owner member tid !!! testResponse 201 Nothing
    check UserLegalHoldPending "pending after requesting device"
    approveLegalHoldDevice (Just defPassword) member member tid !!! testResponse 200 Nothing
    check UserLegalHoldEnabled "enabled after confirming device"

testInWhitelist :: TestM ()
testInWhitelist = do
  g <- view tsGalley
  (owner, tid) <- createBindingTeam
  member <- randomUser
  addTeamMemberInternal tid member (rolePermissions RoleMember) Nothing
  ensureQueueEmpty
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

  putLHWhitelistTeam tid !!! const 200 === statusCode

  withDummyTestServiceForTeam legalholder tid $ \_chan -> do
    putLHWhitelistTeam tid !!! const 200 === statusCode

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
  peer :: UserId <- if teamPeer then fst <$> createBindingTeam else randomUser
  galley <- view tsGalley

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
              const (Right "no-conversation") === fmap Error.label . responseJsonEither

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
            convId
            [ (legalholder, legalholderLHDevice, "cipher")
            ]
            !!! do
              const 201 === statusCode
      else do
        void doEnableLH
        postConnection legalholder peer !!! do testResponse 403 (Just "missing-legalhold-consent")
        postConnection peer legalholder !!! do testResponse 403 (Just "missing-legalhold-consent")

data GroupConvAdmin
  = LegalholderIsAdmin
  | PeerIsAdmin
  | BothAreAdmins
  deriving (Show, Eq, Ord, Bounded, Enum)

testNoConsentRemoveFromGroupConv :: GroupConvAdmin -> HasCallStack => TestM ()
testNoConsentRemoveFromGroupConv whoIsAdmin = do
  (legalholder :: UserId, tid) <- createBindingTeam
  (peer :: UserId, teamPeer) <- createBindingTeam
  galley <- view tsGalley

  let enableLHForLegalholder :: HasCallStack => TestM ()
      enableLHForLegalholder = do
        requestLegalHoldDevice legalholder legalholder tid !!! testResponse 201 Nothing
        approveLegalHoldDevice (Just defPassword) legalholder legalholder tid !!! testResponse 200 Nothing
        UserLegalHoldStatusResponse userStatus _ _ <- getUserStatusTyped' galley legalholder tid
        liftIO $ assertEqual "approving should change status" UserLegalHoldEnabled userStatus

  cannon <- view tsCannon

  putLHWhitelistTeam tid !!! const 200 === statusCode
  WS.bracketR2 cannon legalholder peer $ \(legalholderWs, peerWs) -> withDummyTestServiceForTeam legalholder tid $ \_chan -> do
    ensureQueueEmpty

    postConnection legalholder peer !!! const 201 === statusCode
    void $ putConnection peer legalholder Conn.Accepted <!! const 200 === statusCode

    convId <- do
      let (inviter, tidInviter, invitee, inviteeRole) =
            case whoIsAdmin of
              LegalholderIsAdmin -> (legalholder, tid, peer, roleNameWireMember)
              PeerIsAdmin -> (peer, teamPeer, legalholder, roleNameWireMember)
              BothAreAdmins -> (legalholder, tid, peer, roleNameWireAdmin)

      convId <- createTeamConvWithRole inviter tidInviter [invitee] (Just "group chat with external peer") Nothing Nothing inviteeRole
      mapM_ (assertConvMemberWithRole roleNameWireAdmin convId) ([inviter] <> [invitee | whoIsAdmin == BothAreAdmins])
      mapM_ (assertConvMemberWithRole roleNameWireMember convId) [invitee | whoIsAdmin /= BothAreAdmins]
      pure convId

    checkConvCreateEvent convId legalholderWs
    checkConvCreateEvent convId peerWs

    assertConvMember legalholder convId
    assertConvMember peer convId

    void enableLHForLegalholder

    localdomain <- viewFederationDomain

    case whoIsAdmin of
      LegalholderIsAdmin -> do
        assertConvMember legalholder convId
        assertNotConvMember peer convId
        checkConvMemberLeaveEvent (Qualified convId localdomain) (Qualified peer localdomain) legalholderWs
        checkConvMemberLeaveEvent (Qualified convId localdomain) (Qualified peer localdomain) peerWs
      PeerIsAdmin -> do
        assertConvMember peer convId
        assertNotConvMember legalholder convId
        checkConvMemberLeaveEvent (Qualified convId localdomain) (Qualified legalholder localdomain) legalholderWs
        checkConvMemberLeaveEvent (Qualified convId localdomain) (Qualified legalholder localdomain) peerWs
      BothAreAdmins -> do
        assertConvMember legalholder convId
        assertNotConvMember peer convId
        checkConvMemberLeaveEvent (Qualified convId localdomain) (Qualified peer localdomain) legalholderWs
        checkConvMemberLeaveEvent (Qualified convId localdomain) (Qualified peer localdomain) peerWs

data GroupConvInvCase = InviteOnlyConsenters | InviteAlsoNonConsenters
  deriving (Show, Eq, Ord, Bounded, Enum)

testGroupConvInvitationHandlesLHConflicts :: HasCallStack => GroupConvInvCase -> TestM ()
testGroupConvInvitationHandlesLHConflicts inviteCase = do
  -- team that is legalhold whitelisted
  (legalholder :: UserId, tid) <- createBindingTeam
  userWithConsent <- (^. userId) <$> addUserToTeam legalholder tid
  userWithConsent2 <- (^. userId) <$> addUserToTeam legalholder tid
  ensureQueueEmpty
  putLHWhitelistTeam tid !!! const 200 === statusCode

  -- team without legalhold
  (peer :: UserId, teamPeer) <- createBindingTeam
  peer2 <- (^. userId) <$> addUserToTeam peer teamPeer
  ensureQueueEmpty

  do
    postConnection userWithConsent peer !!! const 201 === statusCode
    void $ putConnection peer userWithConsent Conn.Accepted <!! const 200 === statusCode

    postConnection userWithConsent peer2 !!! const 201 === statusCode
    void $ putConnection peer2 userWithConsent Conn.Accepted <!! const 200 === statusCode

  withDummyTestServiceForTeam legalholder tid $ \_chan -> do
    -- conversation with 1) userWithConsent and 2) peer
    convId <- createTeamConvWithRole userWithConsent tid [peer] (Just "corp + us") Nothing Nothing roleNameWireAdmin

    -- activate legalhold for legalholder
    do
      galley <- view tsGalley
      requestLegalHoldDevice legalholder legalholder tid !!! testResponse 201 Nothing
      approveLegalHoldDevice (Just defPassword) legalholder legalholder tid !!! testResponse 200 Nothing
      UserLegalHoldStatusResponse userStatus _ _ <- getUserStatusTyped' galley legalholder tid
      liftIO $ assertEqual "approving should change status" UserLegalHoldEnabled userStatus

    case inviteCase of
      InviteOnlyConsenters -> do
        API.Util.postMembers userWithConsent (List1.list1 legalholder [userWithConsent2]) convId
          !!! const 200 === statusCode

        assertConvMember legalholder convId
        assertConvMember userWithConsent2 convId
        assertNotConvMember peer convId
      InviteAlsoNonConsenters -> do
        API.Util.postMembers userWithConsent (List1.list1 legalholder [peer2]) convId
          >>= errWith 403 (\err -> Error.label err == "missing-legalhold-consent")

testNoConsentCannotBeInvited :: HasCallStack => TestM ()
testNoConsentCannotBeInvited = do
  -- team that is legalhold whitelisted
  (legalholder :: UserId, tid) <- createBindingTeam
  userLHNotActivated <- (^. userId) <$> addUserToTeam legalholder tid
  ensureQueueEmpty
  putLHWhitelistTeam tid !!! const 200 === statusCode

  -- team without legalhold
  (peer :: UserId, teamPeer) <- createBindingTeam
  peer2 <- (^. userId) <$> addUserToTeam peer teamPeer
  ensureQueueEmpty

  do
    postConnection userLHNotActivated peer !!! const 201 === statusCode
    void $ putConnection peer userLHNotActivated Conn.Accepted <!! const 200 === statusCode

    postConnection userLHNotActivated peer2 !!! const 201 === statusCode
    void $ putConnection peer2 userLHNotActivated Conn.Accepted <!! const 200 === statusCode

  withDummyTestServiceForTeam legalholder tid $ \_chan -> do
    convId <- createTeamConvWithRole userLHNotActivated tid [legalholder] (Just "corp + us") Nothing Nothing roleNameWireAdmin

    API.Util.postMembers userLHNotActivated (List1.list1 peer []) convId
      !!! const 200 === statusCode

    -- activate legalhold for legalholder
    do
      galley <- view tsGalley
      requestLegalHoldDevice legalholder legalholder tid !!! testResponse 201 Nothing
      approveLegalHoldDevice (Just defPassword) legalholder legalholder tid !!! testResponse 200 Nothing
      UserLegalHoldStatusResponse userStatus _ _ <- getUserStatusTyped' galley legalholder tid
      liftIO $ assertEqual "approving should change status" UserLegalHoldEnabled userStatus

    API.Util.postMembers userLHNotActivated (List1.list1 peer2 []) convId
      >>= errWith 403 (\err -> Error.label err == "missing-legalhold-consent")

    localdomain <- viewFederationDomain
    API.Util.postQualifiedMembers userLHNotActivated ((Qualified peer2 localdomain) :| []) convId
      >>= errWith 403 (\err -> Error.label err == "missing-legalhold-consent")

testCannotCreateGroupWithUsersInConflict :: HasCallStack => TestM ()
testCannotCreateGroupWithUsersInConflict = do
  -- team that is legalhold whitelisted
  (legalholder :: UserId, tid) <- createBindingTeam
  userLHNotActivated <- (^. userId) <$> addUserToTeam legalholder tid
  ensureQueueEmpty
  putLHWhitelistTeam tid !!! const 200 === statusCode

  -- team without legalhold
  (peer :: UserId, teamPeer) <- createBindingTeam
  peer2 <- (^. userId) <$> addUserToTeam peer teamPeer
  ensureQueueEmpty

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
      galley <- view tsGalley
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

_putEnabled :: HasCallStack => TeamId -> Public.TeamFeatureStatusValue -> TestM ()
_putEnabled tid enabled = do
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

withDummyTestServiceForTeam ::
  forall a.
  HasCallStack =>
  UserId ->
  TeamId ->
  -- | the test
  (Chan (Wai.Request, LBS) -> TestM a) ->
  TestM a
withDummyTestServiceForTeam owner tid go =
  withDummyTestServiceForTeamNoService $ \chan -> do
    newService <- newLegalHoldService
    postSettings owner tid newService !!! testResponse 201 Nothing
    go chan

-- FUTUREWORK: run this test suite against an actual LH service (by changing URL and key in
-- the config file), and see if it works as well as with our mock service.
withDummyTestServiceForTeamNoService ::
  forall a.
  HasCallStack =>
  -- | the test
  (Chan (Wai.Request, LBS) -> TestM a) ->
  TestM a
withDummyTestServiceForTeamNoService go = do
  withTestService dummyService runTest
  where
    runTest :: Chan (Wai.Request, LBS) -> TestM a
    runTest chan = do
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

-- | FUTUREWORK: this function calls an internal end-point to whitelist a team.  It only
-- appears to bracket this state change and undo it in a finalizer.
--
-- We should probably not have this function, just do the call inline, and use the 'TestM'
-- actions again rather than the polymorphic ones that we have here.
--
-- it's here for historical reason because we did this in galley.yaml
-- at some point in the past rather than in an internal end-point, and that required spawning
-- another galley 'Application' with 'withSettingsOverrides'.
withLHWhitelist :: forall a. HasCallStack => TeamId -> BilgeTest.SessionT TestM a -> TestM a
withLHWhitelist tid action = do
  void $ putLHWhitelistTeam tid
  opts <- view tsGConf
  withSettingsOverrides opts action

-- | If you play with whitelists, you should use this one.  Every whitelisted team that does
-- not get fully deleted will blow up the whitelist that is cached in every warp handler.
withTeam :: forall a. HasCallStack => (HasCallStack => UserId -> TeamId -> TestM a) -> TestM a
withTeam action =
  bracket
    createBindingTeam
    (uncurry deleteTeam >=> const waitForDeleteEvent)
    (uncurry action)
  where
    waitForDeleteEvent :: TestM ()
    waitForDeleteEvent =
      tryAssertQueue 10 "waitForDeleteEvent" SQS.tDelete

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

  putLHWhitelistTeam tid1 !!! const 200 === statusCode

  withDummyTestServiceForTeam owner1 tid1 $ \_chan -> do
    check owner1 member1 (Just tid1) UserLegalHoldDisabled
    check member2 member1 (Just tid1) UserLegalHoldDisabled
    check personal member1 (Just tid1) UserLegalHoldDisabled

    requestDev owner1 member1 tid1
    check personal member1 (Just tid1) UserLegalHoldPending

    approveDev member1 tid1
    check personal member1 (Just tid1) UserLegalHoldEnabled

  ensureQueueEmpty

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

getLHWhitelistedTeam :: HasCallStack => TeamId -> TestM ResponseLBS
getLHWhitelistedTeam tid = do
  galley <- view tsGalley
  getLHWhitelistedTeam' galley tid

getLHWhitelistedTeam' :: (HasCallStack, MonadHttp m, MonadIO m) => GalleyR -> TeamId -> m ResponseLBS
getLHWhitelistedTeam' g tid = do
  get
    ( g
        . paths ["i", "legalhold", "whitelisted-teams", toByteString' tid]
    )

putLHWhitelistTeam :: HasCallStack => TeamId -> TestM ResponseLBS
putLHWhitelistTeam tid = do
  galley <- view tsGalley
  putLHWhitelistTeam' galley tid

putLHWhitelistTeam' :: (HasCallStack, MonadHttp m, MonadIO m) => GalleyR -> TeamId -> m ResponseLBS
putLHWhitelistTeam' g tid = do
  put
    ( g
        . paths ["i", "legalhold", "whitelisted-teams", toByteString' tid]
    )

_deleteLHWhitelistTeam :: HasCallStack => TeamId -> TestM ResponseLBS
_deleteLHWhitelistTeam tid = do
  galley <- view tsGalley
  deleteLHWhitelistTeam' galley tid

deleteLHWhitelistTeam' :: (HasCallStack, MonadHttp m, MonadIO m) => GalleyR -> TeamId -> m ResponseLBS
deleteLHWhitelistTeam' g tid = do
  delete
    ( g
        . paths ["i", "legalhold", "whitelisted-teams", toByteString' tid]
    )

errWith :: (HasCallStack, Typeable a, FromJSON a) => Int -> (a -> Bool) -> ResponseLBS -> TestM ()
errWith wantStatus wantBody rsp = liftIO $ do
  assertEqual "" wantStatus (statusCode rsp)
  assertBool
    (show $ responseBody rsp)
    ( case responseJsonMaybe rsp of
        Nothing -> False
        Just bdy -> wantBody bdy
    )
