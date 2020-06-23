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
import Brig.Types.Provider
import Brig.Types.Team.LegalHold hiding (userId)
import Brig.Types.Test.Arbitrary ()
import qualified Cassandra.Exec as Cql
import qualified Control.Concurrent.Async as Async
import Control.Concurrent.Chan
import Control.Concurrent.Timeout hiding (threadDelay)
import Control.Exception (SomeAsyncException, asyncExceptionFromException)
import Control.Lens
import Control.Monad.Catch
import Control.Retry (RetryPolicy, RetryStatus, exponentialBackoff, limitRetries, retrying)
import qualified Data.Aeson as Aeson
import Data.Aeson.Types ((.:), FromJSON)
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Conversion
import Data.Id
import Data.LegalHold
import qualified Data.List1 as List1
import Data.Misc (PlainTextPassword)
import Data.PEM
import Data.Proxy (Proxy (Proxy))
import Data.Range
import Data.String.Conversions (LBS, cs)
import Data.Text.Encoding (encodeUtf8)
import GHC.Generics hiding (to)
import GHC.TypeLits
import Galley.API.Swagger (GalleyRoutes)
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
import Network.Wai
import Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.Warp.Internal as Warp
import qualified Network.Wai.Handler.WarpTLS as Warp
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
import Test.Tasty.HUnit (assertBool)
import TestHelpers
import TestSetup
import qualified Wire.API.Team.Feature as Public

onlyIfLhEnabled :: TestM () -> TestM ()
onlyIfLhEnabled action = do
  featureLegalHold <- view (tsGConf . optSettings . setFeatureFlags . flagLegalHold)
  case featureLegalHold of
    FeatureLegalHoldDisabledPermanently ->
      liftIO $ hPutStrLn stderr "*** legalhold feature flag disabled, not running integration tests"
    FeatureLegalHoldDisabledByDefault ->
      action

tests :: IO TestSetup -> TestTree
tests s =
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
      test s "POST /register - cannot add team members with LH - too large" (onlyIfLhEnabled testAddTeamUserTooLargeWithLegalhold)
      -- See also Client Tests in Brig; where behaviour around deleting/adding LH clients is
      -- tested

      {- TODO:
          conversations/{cnv}/otr/messages - possibly show the legal hold device (if missing) as a different device type (or show that on device level, depending on how client teams prefer)
          GET /team/{tid}/members - show legal hold status of all members

      -}
    ]

-- | Make sure the ToSchema and ToJSON instances are in sync for all of the swagger docs.
-- (this is more of a unit test, but galley doesn't have any, and it seems not worth it to
-- start another test suite just for this one line.)
testSwaggerJsonConsistency :: TestM ()
testSwaggerJsonConsistency = do
  liftIO . withArgs [] . hspec $ validateEveryToJSON (Proxy @GalleyRoutes)

testRequestLegalHoldDevice :: TestM ()
testRequestLegalHoldDevice = do
  (owner, tid) <- createBindingTeam
  member <- randomUser
  addTeamMemberInternal tid $ newTeamMember member (rolePermissions RoleMember) Nothing
  ensureQueueEmpty
  -- Can't request a device if team feature flag is disabled
  requestLegalHoldDevice owner member tid !!! testResponse 403 (Just "legalhold-not-enabled")
  cannon <- view tsCannon
  -- Assert that the appropriate LegalHold Request notification is sent to the user's
  -- clients
  WS.bracketR2 cannon member member $ \(ws, ws') -> withDummyTestServiceForTeam owner tid $ \_chan -> do
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
          (LegalHoldClientRequested rdata) -> do
            lhcTargetUser rdata @?= member
            lhcLastPrekey rdata @?= head someLastPrekeys
            API.Teams.LegalHold.lhcClientId rdata @?= someClientId
          _ -> assertBool "Unexpected event" False
    assertNotification ws pluck
    -- all devices get notified.
    assertNotification ws' pluck

testApproveLegalHoldDevice :: TestM ()
testApproveLegalHoldDevice = do
  (owner, tid) <- createBindingTeam
  member <- do
    usr <- randomUser
    addTeamMemberInternal tid $ newTeamMember usr (rolePermissions RoleMember) Nothing
    pure usr
  member2 <- do
    usr <- randomUser
    addTeamMemberInternal tid $ newTeamMember usr (rolePermissions RoleMember) Nothing
    pure usr
  outsideContact <- do
    usr <- randomUser
    connectUsers member (List1.singleton usr)
    pure usr
  stranger <- randomUser
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
          Clients.contains (makeIdOpaque member) someClientId clients'
      UserLegalHoldStatusResponse userStatus _ _ <- getUserStatusTyped member tid
      liftIO $
        assertEqual
          "After approval user legalhold status should be Enabled"
          UserLegalHoldEnabled
          userStatus
      let pluck = \case
            ClientAdded eClient -> do
              clientId eClient @?= someClientId
              clientType eClient @?= LegalHoldClientType
              clientClass eClient @?= Just LegalHoldClient
            _ -> assertBool "Unexpected event" False
      assertNotification mws pluck
      assertNotification mws' pluck
      -- Other team users should get a user.legalhold-enable event
      let pluck' = \case
            UserLegalHoldEnabled' eUser -> eUser @?= member
            _ -> assertBool "Unexpected event" False
      assertNotification ows pluck'
      -- We send to all members of a team. which includes the team-settings
      assertNotification member2Ws pluck'
      assertNotification outsideContactWs pluck'
      assertNoNotification strangerWs

testGetLegalHoldDeviceStatus :: TestM ()
testGetLegalHoldDeviceStatus = do
  (owner, tid) <- createBindingTeam
  member <- randomUser
  addTeamMemberInternal tid $ newTeamMember member (rolePermissions RoleMember) Nothing
  ensureQueueEmpty
  forM_ [owner, member] $ \uid -> do
    status <- getUserStatusTyped uid tid
    liftIO $
      assertEqual
        "unexpected status"
        status
        (UserLegalHoldStatusResponse UserLegalHoldDisabled Nothing Nothing)
  withDummyTestServiceForTeam owner tid $ \_chan -> do
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
  addTeamMemberInternal tid $ newTeamMember member (rolePermissions RoleMember) Nothing
  ensureQueueEmpty
  cannon <- view tsCannon
  WS.bracketR2 cannon owner member $ \(ows, mws) -> withDummyTestServiceForTeam owner tid $ \chan -> do
    requestLegalHoldDevice owner member tid !!! testResponse 201 Nothing
    approveLegalHoldDevice (Just defPassword) member member tid !!! testResponse 200 Nothing
    assertNotification mws $ \case
      ClientAdded client -> do
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
      ClientRemoved clientId' -> clientId' @?= someClientId
      _ -> assertBool "Unexpected event" False
    assertNotification mws $ \case
      UserLegalHoldDisabled' uid -> uid @?= member
      _ -> assertBool "Unexpected event" False
    -- Other users should also get the event
    assertNotification ows $ \case
      UserLegalHoldDisabled' uid -> uid @?= member
      _ -> assertBool "Unexpected event" False
    assertZeroLegalHoldDevices member

data IsWorking = Working | NotWorking
  deriving (Eq, Show)

testCreateLegalHoldTeamSettings :: TestM ()
testCreateLegalHoldTeamSettings = do
  (owner, tid) <- createBindingTeam
  member <- randomUser
  addTeamMemberInternal tid $ newTeamMember member (rolePermissions RoleMember) Nothing
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
        if  | pathInfo req /= ["legalhold", "status"] -> cont respondBad
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
  addTeamMemberInternal tid $ newTeamMember member (rolePermissions RoleMember) Nothing
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
  addTeamMemberInternal tid $ newTeamMember member noPermissions Nothing
  ensureQueueEmpty
  -- fails if LH for team is disabled
  deleteSettings (Just defPassword) owner tid !!! testResponse 403 (Just "legalhold-not-enabled")
  withDummyTestServiceForTeam owner tid $ \chan -> do
    newService <- newLegalHoldService
    postSettings owner tid newService !!! testResponse 201 Nothing
    -- enable legalhold for member
    do
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
  addTeamMemberInternal tid $ newTeamMember member (rolePermissions RoleMember) Nothing
  ensureQueueEmpty
  do
    Public.TeamFeatureStatus status <- responseJsonUnsafe <$> (getEnabled tid <!! testResponse 200 Nothing)
    liftIO $ assertEqual "Teams should start with LegalHold disabled" status Public.TeamFeatureDisabled
  putEnabled tid Public.TeamFeatureEnabled -- enable it for this team
  do
    Public.TeamFeatureStatus status <- responseJsonUnsafe <$> (getEnabled tid <!! testResponse 200 Nothing)
    liftIO $ assertEqual "Calling 'putEnabled True' should enable LegalHold" status Public.TeamFeatureEnabled
  withDummyTestServiceForTeam owner tid $ \_chan -> do
    requestLegalHoldDevice owner member tid !!! const 201 === statusCode
    approveLegalHoldDevice (Just defPassword) member member tid !!! testResponse 200 Nothing
    do
      UserLegalHoldStatusResponse status _ _ <- getUserStatusTyped member tid
      liftIO $ assertEqual "User legal hold status should be enabled" UserLegalHoldEnabled status
    do
      putEnabled tid Public.TeamFeatureDisabled -- disable again
      Public.TeamFeatureStatus status <- responseJsonUnsafe <$> (getEnabled tid <!! testResponse 200 Nothing)
      liftIO $ assertEqual "Calling 'putEnabled False' should disable LegalHold" status Public.TeamFeatureDisabled
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

  Public.TeamFeatureStatus status <- responseJsonUnsafe <$> (getEnabled tid <!! testResponse 200 Nothing)
  liftIO $ assertEqual "Teams should start with LegalHold disabled" status Public.TeamFeatureDisabled
  -- You cannot enable legal hold on a team that is too large
  putEnabled' id tid Public.TeamFeatureEnabled !!! do
    const 403 === statusCode
    const (Just "too-large-team-for-legalhold") === fmap Error.label . responseJsonMaybe

testAddTeamUserTooLargeWithLegalhold :: TestM ()
testAddTeamUserTooLargeWithLegalhold = do
  o <- view tsGConf
  let fanoutLimit = fromIntegral . fromRange $ Galley.currentFanoutLimit o
  (tid, owner, _others) <- createBindingTeamWithMembers fanoutLimit
  Public.TeamFeatureStatus status <- responseJsonUnsafe <$> (getEnabled tid <!! testResponse 200 Nothing)
  liftIO $ assertEqual "Teams should start with LegalHold disabled" status Public.TeamFeatureDisabled
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
  addTeamMemberInternal tid $ newTeamMember member (rolePermissions RoleMember) Nothing
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
  addTeamMemberInternal tid $ newTeamMember member (rolePermissions RoleMember) Nothing
  ensureQueueEmpty
  let findMemberStatus :: [TeamMember] -> Maybe UserLegalHoldStatus
      findMemberStatus ms =
        ms ^? traversed . filtered (has $ userId . only member) . legalHoldStatus
  let check status msg = do
        members' <- view teamMembers <$> getTeamMembers owner tid
        liftIO $
          assertEqual
            ("legal hold status should be " <> msg)
            (Just status)
            (findMemberStatus members')
  check UserLegalHoldDisabled "disabled when it is disabled for the team"
  withDummyTestServiceForTeam owner tid $ \_chan -> do
    check UserLegalHoldDisabled "disabled on new team members"
    requestLegalHoldDevice owner member tid !!! testResponse 201 Nothing
    check UserLegalHoldPending "pending after requesting device"
    approveLegalHoldDevice (Just defPassword) member member tid !!! testResponse 200 Nothing
    check UserLegalHoldEnabled "enabled after confirming device"

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
putEnabled tid enabled = void $ putEnabled' expect2xx tid enabled

putEnabled' :: HasCallStack => (Bilge.Request -> Bilge.Request) -> TeamId -> Public.TeamFeatureStatusValue -> TestM ResponseLBS
putEnabled' extra tid enabled = do
  g <- view tsGalley
  put $
    g
      . paths ["i", "teams", toByteString' tid, "features", "legalhold"]
      . json (Public.TeamFeatureStatus enabled)
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
  resp <- getUserStatus uid tid <!! testResponse 200 Nothing
  return $ responseJsonUnsafe resp

getUserStatus :: HasCallStack => UserId -> TeamId -> TestM ResponseLBS
getUserStatus uid tid = do
  g <- view tsGalley
  get $
    g
      . paths ["teams", toByteString' tid, "legalhold", toByteString' uid]
      . zUser uid
      . zConn "conn"
      . zType "access"

approveLegalHoldDevice :: HasCallStack => Maybe PlainTextPassword -> UserId -> UserId -> TeamId -> TestM ResponseLBS
approveLegalHoldDevice mPassword zusr uid tid = do
  g <- view tsGalley
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
-- the config file), and see if it works as well as with out mock service.
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
    dummyService :: Chan (Wai.Request, LBS) -> Wai.Request -> (Wai.Response -> IO Wai.ResponseReceived) -> IO Wai.ResponseReceived
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
        NewLegalHoldClient somePrekeys (head $ someLastPrekeys)
    respondOk :: Wai.Response
    respondOk = responseLBS status200 mempty mempty
    respondBad :: Wai.Response
    respondBad = responseLBS status404 mempty mempty
    missingAuth :: Wai.Response
    missingAuth = responseLBS status400 mempty "no authorization header"
    getRequestHeader :: String -> Wai.Request -> Maybe ByteString
    getRequestHeader name req = lookup (fromString name) $ requestHeaders req

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
    liftIO . Async.async
      $ Warp.runTLS tlss defs
      $ mkApp buf
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

----------------------------------------------------------------------
-- test helpers

-- FUTUREWORK: Currently, the encoding of events is confusingly inside brig and not
-- brig-types. (Look for toPushFormat in the code) We should refactor. To make
-- our lives a bit easier we are going to  copy these datatypes from brig verbatim
data UserEvent
  = UserLegalHoldDisabled' !UserId
  | UserLegalHoldEnabled' !UserId
  | LegalHoldClientRequested LegalHoldClientRequestedData
  deriving (Generic)

data ClientEvent
  = ClientAdded !Client
  | ClientRemoved !ClientId
  deriving (Generic)

data LegalHoldClientRequestedData = LegalHoldClientRequestedData
  { lhcTargetUser :: !UserId,
    lhcLastPrekey :: !LastPrekey,
    lhcClientId :: !ClientId
  }
  deriving stock (Show)

instance FromJSON ClientEvent where
  parseJSON = withObject' $ \o -> do
    tag :: Text <- o .: "type"
    case tag of
      "user.client-add" -> ClientAdded <$> o .: "client"
      "user.client-remove" -> ClientRemoved <$> (o .: "client" >>= Aeson.withObject "id" (.: "id"))
      x -> fail $ "unspported event type: " ++ show x

instance FromJSON UserEvent where
  parseJSON = withObject' $ \o -> do
    tag :: Text <- o .: "type"
    case tag of
      "user.legalhold-enable" -> UserLegalHoldEnabled' <$> o .: "id"
      "user.legalhold-disable" -> UserLegalHoldDisabled' <$> o .: "id"
      "user.legalhold-request" ->
        LegalHoldClientRequested
          <$> ( LegalHoldClientRequestedData
                  <$> o .: "id" -- this is the target user
                  <*> o .: "last_prekey"
                  <*> (o .: "client" >>= Aeson.withObject "id" (.: "id"))
              )
      x -> fail $ "unspported event type: " ++ show x

-- these are useful in other parts of the codebase. maybe move out?
type family NameOf (a :: *) :: Symbol where
  NameOf a = NameOf' (Rep a a)

type family NameOf' r :: Symbol where
  NameOf' (M1 D ('MetaData name _module _package _newtype) _ _) = name

withObject' :: forall a. (KnownSymbol (NameOf a), Generic a) => (Aeson.Object -> Aeson.Parser a) -> Aeson.Value -> Aeson.Parser a
withObject' = Aeson.withObject (symbolVal @(NameOf a) Proxy)

assertNotification :: (HasCallStack, FromJSON a, MonadIO m) => WS.WebSocket -> (a -> Assertion) -> m ()
assertNotification ws predicate =
  void . liftIO . WS.assertMatch (5 WS.# WS.Second) ws $ \notif -> do
    let j = Aeson.Object $ List1.head (ntfPayload notif)
    case Aeson.fromJSON j of
      Aeson.Success x -> predicate x
      Aeson.Error s -> assertBool (s ++ " in " ++ show j) False

assertNoNotification :: (HasCallStack, MonadIO m) => WS.WebSocket -> m ()
assertNoNotification ws = void . liftIO $ WS.assertNoEvent (5 WS.# WS.Second) [ws]

assertMatchJSON :: (FromJSON a, HasCallStack, MonadThrow m, MonadCatch m, MonadIO m) => Chan (Wai.Request, LBS) -> (a -> m ()) -> m ()
assertMatchJSON c match = do
  assertMatchChan c $ \(_, reqBody) -> do
    case Aeson.eitherDecode reqBody of
      Right x -> match x
      Left s -> liftIO $ assertBool (s ++ " in " ++ show reqBody) False

assertMatchChan :: (HasCallStack, MonadThrow m, MonadCatch m, MonadIO m) => Chan a -> (a -> m ()) -> m ()
assertMatchChan c match = go []
  where
    refill = mapM_ (liftIO <$> writeChan c)
    go buf = do
      m <- liftIO . timeout (5 WS.# WS.Second) . readChan $ c
      case m of
        Just n -> do
          match n
          refill buf
          `catchAll` \e -> case asyncExceptionFromException e of
            Just x -> throwM (x :: SomeAsyncException)
            Nothing -> go (n : buf)
        Nothing -> do
          refill buf
          liftIO $ assertBool "Timeout" False
