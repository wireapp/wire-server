{-# OPTIONS_GHC -Wno-orphans #-}

module API.Teams.LegalHold (tests) where

import Imports

import API.SQS
import API.Util hiding (createTeam)
import Bilge.Assert
import Bilge hiding (trace, accept, timeout, head)
import Brig.Types.Client
import Brig.Types.Provider
import Brig.Types.Team.LegalHold hiding (userId)
import Brig.Types.Test.Arbitrary ()
import Control.Concurrent.Chan
import Control.Lens
import Control.Monad.Catch
import Control.Retry (recoverAll, exponentialBackoff, limitRetries)
import Data.Aeson.Lens
import Data.ByteString.Conversion
import Data.EitherR (fmapL)
import Data.Id
import Data.LegalHold
import Data.Misc (PlainTextPassword)
import Data.PEM
import Data.Proxy (Proxy(Proxy))
import Data.String.Conversions (LBS, ST, cs)
import Data.Text.Encoding (encodeUtf8)
import Galley.External.LegalHoldService (validateServiceKey)
import Galley.API.Swagger (GalleyRoutes)
import Galley.Types.Teams
import Gundeck.Types.Notification (ntfPayload)
import Network.HTTP.Types.Status (status200, status400, status404)
import Network.Wai
import Network.Wai as Wai
import Servant.Swagger (validateEveryToJSON)
import System.Environment (withArgs)
import TestHelpers
import Test.Hspec (hspec)
import Test.QuickCheck.Instances ()
import TestSetup
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.HUnit (assertBool)

import qualified API.Util                          as Util
import qualified Control.Concurrent.Async          as Async
import qualified Cassandra.Exec                    as Cql
import qualified Data.Aeson                        as Aeson
import qualified Data.ByteString                   as BS
import qualified Data.ByteString.Char8             as BS
import qualified Data.List1                        as List1
import qualified Network.Wai.Handler.Warp          as Warp
import qualified Network.Wai.Handler.Warp.Internal as Warp
import qualified Network.Wai.Handler.WarpTLS       as Warp
import qualified Network.Wai.Utilities.Response    as Wai
import qualified Galley.Data                       as Data
import qualified Galley.Data.LegalHold             as LegalHoldData
import qualified Galley.Types.Clients              as Clients
import qualified Test.Tasty.Cannon                 as WS


tests :: IO TestSetup -> TestTree
tests s = testGroup "Teams LegalHold API"
    [ test s "swagger / json consistency" testSwaggerJsonConsistency

      -- device handling (CRUD)
    , test s "POST /teams/{tid}/legalhold/{uid}" testRequestLegalHoldDevice
    , test s "POST /teams/{tid}/legalhold/{uid} - twice" testCreateTwoLegalHoldDevices
    , test s "PUT /teams/{tid}/legalhold/approve" testApproveLegalHoldDevice
    , test s "(user denies approval: nothing needs to be done in backend)" (pure ())
    , test s "GET /teams/{tid}/legalhold/{uid}" testGetLegalHoldDeviceStatus
    , test s "DELETE /teams/{tid}/legalhold/{uid}" testDisableLegalHoldForUser

      -- legal hold settings
    , test s "POST /teams/{tid}/legalhold/settings" testCreateLegalHoldTeamSettings
    , test s "GET /teams/{tid}/legalhold/settings" testGetLegalHoldTeamSettings
    , test s "DELETE /teams/{tid}/legalhold/settings" testRemoveLegalHoldFromTeam
    , test s "GET, PUT [/i]?/teams/{tid}/legalhold" testEnablePerTeam

      -- behavior of existing end-points
    , test s "POST /clients" testCreateLegalHoldDeviceOldAPI

    , test s "GET /teams/{tid}/members" testGetTeamMembersIncludesLHStatus

    -- See also Client Tests in Brig; where behaviour around deleting/adding LH clients is
    -- tested

{- TODO:
    zauth/libzauth level: Allow access to legal hold service tokens
        conversations/{cnv}/otr/messages
        /notifications
        /access
        (maybe others?)
    conversations/{cnv}/otr/messages - possibly show the legal hold device (if missing) as a different device type (or show that on device level, depending on how client teams prefer)
    GET /team/{tid}/members - show legal hold status of all members

  TODO: feature flag!  (not sure tests are needed for this.)

-}

    ]


-- TODO: delete this function when we're done fixing all the test cases.
ignore :: Monad m => m () -> m ()
ignore _ = trace "\n*** ignored test case!!\n" $ pure ()


-- | Make sure the ToSchema and ToJSON instances are in sync for all of the swagger docs.
-- (this is more of a unit test, but galley doesn't have any, and it seems not worth it to
-- start another test suite just for this one line.)
testSwaggerJsonConsistency :: TestM ()
testSwaggerJsonConsistency = do
    liftIO . withArgs [] . hspec $ validateEveryToJSON (Proxy @GalleyRoutes)


testRequestLegalHoldDevice :: TestM ()
testRequestLegalHoldDevice = do
    (owner, tid) <- createTeam
    member <- randomUser
    addTeamMemberInternal tid $ newTeamMember member (rolePermissions RoleMember) Nothing

    cannon <- view tsCannon

    -- Assert that the appropriate LegalHold Request notification is sent to the user's
    -- clients
    WS.bracketR cannon member $ \ws -> withDummyTestServiceForTeam owner tid $ \_chan -> do
        -- Can't request a device if team feature flag is disabled
        -- TODO: Add this back once the check is re-enabled
        ignore $ requestDevice owner member tid !!! testResponse 403 (Just "legalhold-not-enabled")
        putEnabled tid LegalHoldEnabled -- enable it for this team

        do requestDevice member member tid !!! testResponse 403 (Just "operation-denied")
           UserLegalHoldStatusResponse userStatus _ _ <- getUserStatusTyped member tid
           liftIO $ assertEqual "User with insufficient permissions should be unable to start flow"
                      UserLegalHoldDisabled userStatus

        do requestDevice owner member tid !!! testResponse 204 Nothing
           UserLegalHoldStatusResponse userStatus _ _ <- getUserStatusTyped member tid
           liftIO $ assertEqual "requestDevice should set user status to Pending"
                      UserLegalHoldPending userStatus

        do requestDevice owner member tid !!! testResponse 204 Nothing
           UserLegalHoldStatusResponse userStatus _ _ <- getUserStatusTyped member tid
           liftIO $ assertEqual "requestDevice when already pending should leave status as Pending"
                      UserLegalHoldPending userStatus

        cassState <- view tsCass
        liftIO $ do
            storedPrekeys <- Cql.runClient cassState (LegalHoldData.selectPendingPrekeys member)
            assertBool "user should have pending prekeys stored" (not . null $ storedPrekeys)

        -- This test is mirrored in brig tests: API.User.Client.testRequestLegalHoldClient
        -- And should probably continue to exist in both locations.
        liftIO $ do
            void . liftIO $ WS.assertMatch (5 WS.# WS.Second) ws $ \n -> do
                let j = Aeson.Object $ List1.head (ntfPayload n)
                let etype = j ^? key "type" . _String
                let eTargetUser = j ^? key "id" . _String
                let eLastPrekey = j ^? key "last_prekey" . _JSON
                let eClientId = j ^? key "client" . key "id" . _JSON
                etype @?= Just "user.legalhold-request"
                eTargetUser @?= Just (idToText member)
                eClientId @?= Just someClientId
                -- These need to match the values provided by the 'dummy service'
                Just (head someLastPrekeys) @?= eLastPrekey

        ensureQueueEmpty

    -- fail if legal hold service is disabled via feature flag

    -- all of user's clients receive an event
    -- requests approval from monitored user asynchronously; request contains pre-keys


testCreateTwoLegalHoldDevices :: TestM ()
testCreateTwoLegalHoldDevices = do
    pure ()

    -- creating a second valid a legal hold device is rejected.
    -- also when the legal hold status of a user is 'pending".


testApproveLegalHoldDevice :: TestM ()
testApproveLegalHoldDevice = do
    (owner, tid) <- createTeam
    member <- randomUser
    addTeamMemberInternal tid $ newTeamMember member (rolePermissions RoleMember) Nothing

    cannon <- view tsCannon

    WS.bracketR2 cannon owner member $ \(ows, mws) -> withDummyTestServiceForTeam owner tid $ \chan -> do
        -- not allowed to approve if team setting is disabled
        -- TODO: remove the following 'ignore' once 'disabled' is the default
        ignore $ approveLegalHoldDevice (Just defPassword) owner member tid !!! testResponse 403 (Just "legalhold-not-enabled")

        putEnabled tid LegalHoldEnabled
        requestDevice owner member tid !!! testResponse 204 Nothing

        liftIO $ do
          _statusCheck' <- readChan chan
          reqBody <- readChan chan
          let RequestNewLegalHoldClient userId' teamId' =  reqBody ^?! _JSON
          assertEqual "userId == member" userId' member
          assertEqual "teamId == tid" teamId' tid

        -- Only the user themself can approve adding a LH device
        approveLegalHoldDevice (Just defPassword) owner member tid !!! testResponse 403 (Just "access-denied")
        -- Requires password
        approveLegalHoldDevice Nothing member member tid !!! const 403 === statusCode
        approveLegalHoldDevice (Just defPassword) member member tid !!! testResponse 200 Nothing

        do
          reqBody <- liftIO $ readChan chan
          let LegalHoldServiceConfirm _clientId _uid _tid authToken = reqBody ^?! _JSON
          renewToken authToken

        cassState <- view tsCass
        liftIO $ do
            clients' <- Cql.runClient cassState $ Data.lookupClients [member]
            assertBool "Expect clientId to be saved on the user"
              $ Clients.contains member someClientId clients'

        UserLegalHoldStatusResponse userStatus _ _ <- getUserStatusTyped member tid
        liftIO $ assertEqual "After approval user legalhold status should be Enabled"
                    UserLegalHoldEnabled userStatus

        liftIO $ do
            void . liftIO $ WS.assertMatch (5 WS.# WS.Second) mws $ \n -> do
                let j = Aeson.Object $ List1.head (ntfPayload n)
                let etype = j ^? key "type" . _String
                let eClient = j ^? key "client" . _JSON
                etype @?= Just "user.client-add"
                clientId <$> eClient @?= Just someClientId
                clientType <$> eClient @?= Just LegalHoldClientType
                clientClass <$> eClient @?= Just (Just LegalHoldClient)

        -- Other team users should get a user.legalhold-enable event
        liftIO $ do
            void . liftIO $ WS.assertMatch (5 WS.# WS.Second) ows $ \n -> do
                let j = Aeson.Object $ List1.head (ntfPayload n)
                let etype = j ^? key "type" . _String
                let eUser = j ^? key "id" . _JSON
                etype @?= Just "user.legalhold-enable"
                eUser @?= Just member

    ensureQueueEmpty

        -- fail if GLOBAL legal Hold feature flag disabled
        -- sends an event to team settings (however that works; it's a client-independent event i think)
        -- all of user's communication peers receive an event

testGetLegalHoldDeviceStatus :: TestM ()
testGetLegalHoldDeviceStatus = do
    (owner, tid) <- createTeam
    member <- randomUser
    addTeamMemberInternal tid $ newTeamMember member (rolePermissions RoleMember) Nothing

    withDummyTestServiceForTeam owner tid $ \_chan -> do
        -- Initial status should be disabled
        do UserLegalHoldStatusResponse userStatus lastPrekey' clientId' <- getUserStatusTyped member tid
           liftIO $
             do assertEqual "User legal hold status should start as disabled" UserLegalHoldDisabled userStatus
                assertEqual "last_prekey should be Nothing when LH is disabled" Nothing lastPrekey'
                assertEqual "client.id should be Nothing when LH is disabled" Nothing clientId'

        do requestDevice owner member tid !!! testResponse 204 Nothing
           UserLegalHoldStatusResponse userStatus lastPrekey' clientId' <- getUserStatusTyped member tid
           liftIO $
             do assertEqual "requestDevice should set user status to Pending" UserLegalHoldPending userStatus
                assertEqual "last_prekey should be set when LH is pending" (Just (head someLastPrekeys)) lastPrekey'
                assertEqual "client.id should be set when LH is pending" (Just someClientId) clientId'

        do requestDevice owner member tid !!! testResponse 204 Nothing
           UserLegalHoldStatusResponse userStatus _ _ <- getUserStatusTyped member tid
           liftIO $ assertEqual "requestDevice when already pending should leave status as Pending"
                      UserLegalHoldPending userStatus

        do approveLegalHoldDevice (Just defPassword) member member tid !!! testResponse 200 Nothing
           UserLegalHoldStatusResponse userStatus lastPrekey' clientId' <- getUserStatusTyped member tid
           liftIO $
             do assertEqual "approving should change status to Enabled" UserLegalHoldEnabled userStatus
                assertEqual "last_prekey should be set when LH is pending" (Just (head someLastPrekeys)) lastPrekey'
                assertEqual "client.id should be set when LH is pending" (Just someClientId) clientId'

    ensureQueueEmpty

testDisableLegalHoldForUser :: TestM ()
testDisableLegalHoldForUser = do
    (owner, tid) <- createTeam
    member <- randomUser

    cannon <- view tsCannon
    WS.bracketR2 cannon owner member $ \(ows, mws) -> withDummyTestServiceForTeam owner tid $ \_chan -> do
        addTeamMemberInternal tid $ newTeamMember member (rolePermissions RoleMember) Nothing
        putEnabled tid LegalHoldEnabled
        requestDevice owner member tid !!! testResponse 204 Nothing
        assertZeroLegalHoldDevices member
        approveLegalHoldDevice (Just defPassword) member member tid !!! testResponse 200 Nothing
        assertExactlyOneLegalHoldDevice member
        -- Only the admin can disable legal hold
        disableLegalHoldForUser (Just defPassword) tid member member !!! testResponse 403 (Just "operation-denied")
        assertExactlyOneLegalHoldDevice member
        -- Require password to disable for usern
        disableLegalHoldForUser Nothing tid owner member !!! const 403 === statusCode
        assertExactlyOneLegalHoldDevice member
        disableLegalHoldForUser (Just defPassword) tid owner member !!! testResponse 200 Nothing
        assertZeroLegalHoldDevices member

        liftIO $ do
            void . WS.assertMatch (5 WS.# WS.Second) mws $ \n -> do
              let j = Aeson.Object $ List1.head (ntfPayload n)
              let etype = j ^? key "type" . _String
              let eClient = j ^? key "client" . _JSON
              etype @?= Just "user.client-add"
              clientId <$> eClient @?= Just someClientId
              clientType <$> eClient @?= Just LegalHoldClientType
              clientClass <$> eClient @?= Just (Just LegalHoldClient)
            void . WS.assertMatch (5 WS.# WS.Second) mws $ \n -> do
              let j = Aeson.Object $ List1.head (ntfPayload n)
              let eType = j ^? key "type" . _String
              let eClientId = j ^? key "client" . key "id" .  _JSON
              eType @?= Just "user.client-remove"
              eClientId @?= Just someClientId
            void . liftIO $ WS.assertMatch (5 WS.# WS.Second) mws $ \n -> do
              let j = Aeson.Object $ List1.head (ntfPayload n)
              let eType = j ^? key "type" . _String
              let euid = j ^?  key "id" .  _JSON
              eType @?= Just "user.legalhold-disable"
              euid @?= Just member
            -- Other users should also get the event
            void . liftIO $ WS.assertMatch (5 WS.# WS.Second) ows $ \n -> do
              let j = Aeson.Object $ List1.head (ntfPayload n)
              let eType = j ^? key "type" . _String
              let euid = j ^?  key "id" .  _JSON
              eType @?= Just "user.legalhold-disable"
              euid @?= Just member
    ensureQueueEmpty

testCreateLegalHoldTeamSettings :: TestM ()
testCreateLegalHoldTeamSettings = do
    (owner, tid) <- createTeam
    member <- randomUser
    addTeamMemberInternal tid $ newTeamMember member (rolePermissions RoleMember) Nothing
    newService <- newLegalHoldService
    -- TODO: not allowed if feature is disabled globally in galley config yaml

    -- not allowed for users with corresp. permission bit missing
    postSettings member tid newService !!! testResponse 403 (Just "operation-denied")

    -- not allowed to create if team setting is disabled
    -- TODO: remove the following 'ignore' once 'disabled' is the default
    ignore $ postSettings owner tid newService !!! testResponse 403 (Just "legalhold-not-enabled")

    putEnabled tid LegalHoldEnabled -- enable it for this team

    -- rejected if service is not available
    postSettings owner tid newService !!! testResponse 412 (Just "legalhold-unavailable")


    -- checks /status of legal hold service (boolean argument says whether the service is
    -- behaving or not)
    let lhapp :: HasCallStack => Bool -> Chan Void -> Application
        lhapp _isworking@False _ _   cont = cont respondBad
        lhapp _isworking@True  _ req cont = trace "APP" $ do
            if | pathInfo req /= ["legalhold", "status"] -> cont respondBad
               | requestMethod req /= "GET" -> cont respondBad
               | otherwise -> cont respondOk

        respondOk :: Wai.Response
        respondOk = responseLBS status200 mempty mempty

        respondBad :: Wai.Response
        respondBad = responseLBS status404 mempty mempty

        lhtest :: HasCallStack => Bool -> Chan Void -> TestM ()
        lhtest _isworking@False _ = do
            postSettings owner tid newService !!! testResponse 412 (Just "legalhold-unavailable")

        lhtest _isworking@True _ = do
            let Right [k] = pemParseBS "-----BEGIN PUBLIC KEY-----\n\n-----END PUBLIC KEY-----"
            let badServiceBadKey = newService { newLegalHoldServiceKey = ServiceKeyPEM k }
            postSettings owner tid badServiceBadKey !!! testResponse 400 (Just "legalhold-invalid-key")
            postSettings owner tid newService !!! testResponse 201 Nothing
            ViewLegalHoldService service <- getSettingsTyped owner tid
            liftIO $ do
                Just (_, fpr) <- validateServiceKey (newLegalHoldServiceKey newService)
                assertEqual "viewLegalHoldTeam" tid (viewLegalHoldServiceTeam service)
                assertEqual "viewLegalHoldServiceUrl" (newLegalHoldServiceUrl newService) (viewLegalHoldServiceUrl service)
                assertEqual "viewLegalHoldServiceFingerprint" fpr (viewLegalHoldServiceFingerprint service)

            -- The pubkey is different... if a connection would be reused
            -- this request would actually return a 201
            let badServiceValidKey = newService { newLegalHoldServiceKey = ServiceKeyPEM randomButValidPublicKey }
            ignore $ postSettings owner tid badServiceValidKey !!! testResponse 400 (Just "legalhold-invalid-key")
            -- TODO: request gets a 412 (unavialable) instead of 400.  it should work as the
            -- test says: the above line should work, and the line below should fail.
            postSettings owner tid badServiceValidKey !!! testResponse 412 (Just "legalhold-unavailable")

    -- if no valid service response can be obtained, responds with 400
    withTestService (lhapp False) (lhtest False)

    -- if valid service response can be obtained, writes a pending entry to cassandra
    -- synchronously and respond with 201
    withTestService (lhapp True) (lhtest True)

    -- TODO: expect event TeamEvent'TEAM_UPDATE as a reaction to this POST.
    -- TODO: should we expect any other events?

    ensureQueueEmpty  -- TODO: there are some pending events in there.  make sure it's the
                      -- right ones.  (i think this has to od with the plumbing that is the
                      -- same in all settings-related tests.)


testGetLegalHoldTeamSettings :: TestM ()
testGetLegalHoldTeamSettings = do
    (owner, tid) <- createTeam
    stranger <- randomUser
    member <- randomUser
    addTeamMemberInternal tid $ newTeamMember member (rolePermissions RoleMember) Nothing
    newService <- newLegalHoldService

    let lhapp :: Chan () -> Application
        lhapp _ch _req res = res $ responseLBS status200 mempty mempty

    withTestService lhapp $ \_ -> do
        -- TODO: not allowed if feature is disabled globally in galley config yaml

        -- TODO: not allowed if team has feature bit not set

        -- returns 403 if user is not in team.
        getSettings stranger tid !!! testResponse 403 (Just "no-team-member")

        -- returns 200 with corresp. status if legalhold for team is disabled
        do  let respOk :: ResponseLBS -> TestM ()
                respOk resp = liftIO $ do
                  assertEqual "bad status code" 200 (statusCode resp)
                  -- TODO: remove the following 'ignore' once 'disabled' is the default
                  ignore $ assertEqual "bad body" ViewLegalHoldServiceDisabled (jsonBody resp)
            getSettings owner  tid >>= respOk
            getSettings member tid >>= respOk

        putEnabled tid LegalHoldEnabled -- enable it for this team

        -- returns 200 with corresp. status if legalhold for team is enabled, but not configured
        do  let respOk :: ResponseLBS -> TestM ()
                respOk resp = liftIO $ do
                  assertEqual "bad status code" 200 (statusCode resp)
                  assertEqual "bad body" ViewLegalHoldServiceNotConfigured (jsonBody resp)
            getSettings owner  tid >>= respOk
            getSettings member tid >>= respOk

        postSettings owner tid newService !!! testResponse 201 Nothing

        -- returns legal hold service info if team is under legal hold and user is in team (even
        -- no permissions).
        ViewLegalHoldService service <- getSettingsTyped member tid
        liftIO $ do
            Just (_, fpr) <- validateServiceKey (newLegalHoldServiceKey newService)
            assertEqual "viewLegalHoldServiceTeam" tid (viewLegalHoldServiceTeam service)
            assertEqual "viewLegalHoldServiceUrl" (newLegalHoldServiceUrl newService) (viewLegalHoldServiceUrl service)
            assertEqual "viewLegalHoldServiceFingerprint" fpr (viewLegalHoldServiceFingerprint service)

    ensureQueueEmpty  -- TODO: there are some pending events in there.  make sure it's the right ones.


testRemoveLegalHoldFromTeam :: TestM ()
testRemoveLegalHoldFromTeam = do
    (owner, tid) <- createTeam
    stranger <- randomUser
    member <- randomUser
    addTeamMemberInternal tid $ newTeamMember member noPermissions Nothing

    -- is idempotent
    deleteSettings (Just defPassword) owner tid !!! testResponse 204 Nothing

    withDummyTestServiceForTeam owner tid $ \_chan -> do
        newService <- newLegalHoldService
        putEnabled tid LegalHoldEnabled -- enable it for this team
        postSettings owner tid newService !!! testResponse 201 Nothing

        -- enable legalhold for member
        do requestDevice owner member tid !!! testResponse 204 Nothing
           approveLegalHoldDevice (Just defPassword) member member tid !!! testResponse 200 Nothing
           UserLegalHoldStatusResponse userStatus _ _ <- getUserStatusTyped member tid
           liftIO $ assertEqual "After approval user legalhold status should be Enabled"
                        UserLegalHoldEnabled userStatus

        -- TODO: not allowed if feature is disabled globally in galley config yaml

        -- TODO: not allowed if team has feature bit not set

        -- returns 403 if user is not in team or has unsufficient permissions.
        deleteSettings (Just defPassword) stranger tid !!! testResponse 403 (Just "no-team-member")
        deleteSettings (Just defPassword) member tid !!! testResponse 403 (Just "operation-denied")

        -- Fails without password
        deleteSettings Nothing owner tid !!! testResponse 403 (Just "access-denied")
        -- returns 204 if legal hold is successfully removed from team
        deleteSettings (Just defPassword) owner tid !!! testResponse 204 Nothing

        -- deletion is successful (both witnessed on the API and in the backend)
        resp <- getSettings owner tid
        liftIO $ assertEqual "bad body" ViewLegalHoldServiceNotConfigured (jsonBody resp)

        -- deletion of settings should disable for all team members and remove their clients
        do UserLegalHoldStatusResponse userStatus _ _ <- getUserStatusTyped member tid
           liftIO $ assertEqual "After approval user legalhold status should be Disabled"
                        UserLegalHoldDisabled userStatus
           assertZeroLegalHoldDevices member

        -- TODO: do we also want to check the DB?

        -- TODO: do we really want any trace of the fact that this team has been under legal hold
        -- to go away?  or should a team that has been under legal hold in the past be observably
        -- different for the members from one that never has?

    ensureQueueEmpty  -- TODO: there are some pending events in there.  make sure it's the right ones.


testEnablePerTeam :: TestM ()
testEnablePerTeam = do
    (owner, tid) <- createTeam
    member <- randomUser
    addTeamMemberInternal tid $ newTeamMember member (rolePermissions RoleMember) Nothing

    -- TODO: Change this value once we change the default; note that disabling is tested further down
    ignore $ do
        LegalHoldTeamConfig isInitiallyEnabled <- jsonBody <$> (getEnabled tid <!! testResponse 200 Nothing)
        liftIO $ assertEqual "Teams should start with LegalHold disabled" isInitiallyEnabled LegalHoldDisabled

    -- TODO: Remove this test once we change the default value
    LegalHoldTeamConfig isInitiallyEnabled <- jsonBody <$> (getEnabled tid <!! testResponse 200 Nothing)
    liftIO $ assertEqual "Teams should start with LegalHold enabled" isInitiallyEnabled LegalHoldEnabled

    putEnabled tid LegalHoldEnabled -- enable it for this team
    LegalHoldTeamConfig isEnabledAfter <- jsonBody <$> (getEnabled tid <!! testResponse 200 Nothing)
    liftIO $ assertEqual "Calling 'putEnabled True' should enable LegalHold" isEnabledAfter LegalHoldEnabled

    withDummyTestServiceForTeam owner tid $ \_chan -> do
        requestDevice owner member tid !!! const 204 === statusCode
        approveLegalHoldDevice (Just defPassword) member member tid !!! testResponse 200 Nothing
        do UserLegalHoldStatusResponse status _ _ <- getUserStatusTyped member tid
           liftIO $ assertEqual "User legal hold status should be enabled" UserLegalHoldEnabled status

        putEnabled tid LegalHoldDisabled -- disable again
        LegalHoldTeamConfig isEnabledAfterUnset <- jsonBody <$> (getEnabled tid <!! testResponse 200 Nothing)
        liftIO $ assertEqual "Calling 'putEnabled False' should disable LegalHold" isEnabledAfterUnset LegalHoldDisabled

        -- TODO: This test needs to be re-thought, so I will keep a TODO here; what should really happen when disabling?
        ignore $ do
            UserLegalHoldStatusResponse status _ _ <- getUserStatusTyped member tid
            liftIO $ assertEqual "User legal hold status should be disabled after disabling for team" UserLegalHoldDisabled status

        viewLHS <- getSettingsTyped owner tid
        -- liftIO $ assertEqual "LH Service settings should be cleared"
        --            ViewLegalHoldServiceNotConfigured viewLHS
        -- TODO: NotConfigured only makes sense given the wrong default;
        --       I think we should change the default to clean up code!
        liftIO $ assertEqual "LH Service settings should be disabled"
                   ViewLegalHoldServiceDisabled viewLHS

    ensureQueueEmpty

testCreateLegalHoldDeviceOldAPI :: TestM ()
testCreateLegalHoldDeviceOldAPI = do
    -- regular users cannot create LegalHoldClients
    let lk = (someLastPrekeys !! 0)
    u <- randomUser

    -- TODO: requests to /clients with type=LegalHoldClientType should fail  (400 instead of 201)
    -- TODO: HOWEVER currently this endpoint is actually used internally to create the legal
    -- hold device, so we'll need to split it to another endpoint, or figure out some other
    -- form of auth on it.
    void $ randomClientWithType LegalHoldClientType 201 u lk

    -- team users cannot create LegalHoldClients
    (owner, _) <- createTeam

    -- TODO: requests to /clients with type=LegalHoldClientType should fail (400 instead of 201)
    void $ randomClientWithType LegalHoldClientType 201 owner lk
    assertExactlyOneLegalHoldDevice owner

    -- TODO: the remainder of this test can be removed once `POST /clients` does not work any
    -- more for legal hold devices.
    void $ randomClientWithType LegalHoldClientType 201 owner lk  -- overwrite
    assertExactlyOneLegalHoldDevice owner

testGetTeamMembersIncludesLHStatus :: TestM ()
testGetTeamMembersIncludesLHStatus = do
    (owner, tid) <- createTeam
    member <- randomUser
    addTeamMemberInternal tid $ newTeamMember member (rolePermissions RoleMember) Nothing

    let findMemberStatus :: [TeamMember] -> Maybe UserLegalHoldStatus
        findMemberStatus ms =
            ms ^? traversed . filtered (has $ userId . only member) . legalHoldStatus
    withDummyTestServiceForTeam owner tid $ \_chan -> do
        do members' <- view (teamMembers) <$> getTeamMembers owner tid
           liftIO $ assertEqual "legal hold status should be disabled on new team members"
                      (Just UserLegalHoldDisabled) (findMemberStatus members')

        putEnabled tid LegalHoldEnabled
        do requestDevice owner member tid !!! testResponse 204 Nothing
           members' <- view teamMembers <$> getTeamMembers owner tid
           liftIO $ assertEqual "legal hold status should pending after requesting device"
                      (Just UserLegalHoldPending) (findMemberStatus members')

        do approveLegalHoldDevice (Just defPassword) member member tid !!! testResponse 200 Nothing
           members' <- view teamMembers <$> getTeamMembers owner tid
           liftIO $ assertEqual "legal hold status should be enabled after confirming device"
                      (Just UserLegalHoldEnabled) (findMemberStatus members')
    ensureQueueEmpty


----------------------------------------------------------------------
-- API helpers

createTeam :: HasCallStack => TestM (UserId, TeamId)
createTeam = do
    ownerid <- Util.randomUser
    let tname :: Text = cs $ show ownerid  -- doesn't matter what, but needs to be unique!
    teamid <- Util.createTeamInternal tname ownerid
    assertQueue "create team" tActivate
    pure (ownerid, teamid)

getEnabled :: HasCallStack => TeamId -> TestM ResponseLBS
getEnabled tid = do
    g <- view tsGalley
    get $ g
         . paths ["i", "teams", toByteString' tid, "legalhold"]

renewToken :: HasCallStack => Text -> TestM ()
renewToken tok = do
  b <- view tsBrig
  void . post $ b
       . paths [ "access" ]
       . cookieRaw "zuid" (toByteString' tok)
       . expect2xx

putEnabled :: HasCallStack => TeamId -> LegalHoldStatus -> TestM ()
putEnabled tid enabled = do
    g <- view tsGalley
    void . put $ g
         . paths ["i", "teams", toByteString' tid, "legalhold"]
         . json (LegalHoldTeamConfig enabled)
         . expect2xx

postSettings :: HasCallStack => UserId -> TeamId -> NewLegalHoldService -> TestM ResponseLBS
postSettings uid tid new =
    -- Retry calls to this endpoint, on k8s it sometimes takes a while to establish a working
    -- connection.
    -- TODO: only retry on 412
    recoverAll (exponentialBackoff 50 <> limitRetries 5) $ \_ -> do
        g <- view tsGalley
        post $ g
            . paths ["teams", toByteString' tid, "legalhold", "settings"]
            . zUser uid . zConn "conn"
            . zType "access"
            . json new

getSettingsTyped :: HasCallStack => UserId -> TeamId -> TestM ViewLegalHoldService
getSettingsTyped uid tid = jsonBody <$> (getSettings uid tid <!! testResponse 200 Nothing)

getSettings :: HasCallStack => UserId -> TeamId -> TestM ResponseLBS
getSettings uid tid = do
    g <- view tsGalley
    get $ g
        . paths ["teams", toByteString' tid, "legalhold", "settings"]
        . zUser uid . zConn "conn"
        . zType "access"

deleteSettings :: HasCallStack => Maybe PlainTextPassword -> UserId -> TeamId -> TestM ResponseLBS
deleteSettings mPassword uid tid = do
    g <- view tsGalley
    delete $ g
           . paths ["teams", toByteString' tid, "legalhold", "settings"]
           . zUser uid . zConn "conn"
           . zType "access"
           . json (RemoveLegalHoldSettingsRequest mPassword)

getUserStatusTyped :: HasCallStack => UserId -> TeamId -> TestM UserLegalHoldStatusResponse
getUserStatusTyped uid tid = do
    resp <- getUserStatus uid tid <!! testResponse 200 Nothing
    return $ jsonBody resp

getUserStatus :: HasCallStack => UserId -> TeamId -> TestM ResponseLBS
getUserStatus uid tid = do
    g <- view tsGalley
    get $ g
           . paths ["teams", toByteString' tid, "legalhold", toByteString' uid]
           . zUser uid . zConn "conn"
           . zType "access"

approveLegalHoldDevice :: HasCallStack => Maybe PlainTextPassword -> UserId -> UserId -> TeamId -> TestM ResponseLBS
approveLegalHoldDevice mPassword zusr uid tid = do
    g <- view tsGalley
    put $ g
           . paths ["teams", toByteString' tid, "legalhold", toByteString' uid, "approve"]
           . zUser zusr . zConn "conn"
           . zType "access"
           . json (ApproveLegalHoldForUserRequest mPassword)

disableLegalHoldForUser
    :: HasCallStack
    => Maybe PlainTextPassword
    -> TeamId
    -> UserId
    -> UserId
    -> TestM ResponseLBS
disableLegalHoldForUser mPassword tid zusr uid = do
    g <- view tsGalley
    delete $ g
           . paths ["teams", toByteString' tid, "legalhold", toByteString' uid]
           . zUser zusr
           . zType "access"
           . json (DisableLegalHoldForUserRequest mPassword)

assertExactlyOneLegalHoldDevice :: HasCallStack => UserId -> TestM ()
assertExactlyOneLegalHoldDevice uid = do
    clients :: [Client]
        <- getClients uid >>= maybe (error $ "decodeBody: [Client]") pure . decodeBody
    liftIO $ do
        let numdevs = length $ clientType <$> clients
        assertEqual ("expected exactly one legal hold device for user: " <> show uid) numdevs  1

assertZeroLegalHoldDevices :: HasCallStack  => UserId -> TestM ()
assertZeroLegalHoldDevices uid = do
    clients :: [Client] <- getClients uid
        >>= maybe (error $ "decodeBody: [Client]") pure . decodeBody
    liftIO $ do
        let numdevs = length $ clientType <$> clients
        assertBool ("a legal hold device was found when none was expected for user"
                    <> show uid)
                   (numdevs == 0)

jsonBody :: (HasCallStack, Aeson.FromJSON v) => ResponseLBS -> v
jsonBody resp = either (error . show . (, bdy)) id . Aeson.eitherDecode $ bdy
  where
    bdy = fromJust $ responseBody resp


---------------------------------------------------------------------
--- Device helpers

-- data NewLegalHoldDevice = NewLegalHoldDevice
--     { newLegalHoldDeviceTeam  :: TeamId
--     , newLegalHoldDeviceUser  :: UserId
--     }
--   deriving (Eq, Show, Generic)

-- getDevice :: HasCallStack => TeamId -> UserId -> TestM ResponseLBS
-- getDevice = undefined

requestDevice :: HasCallStack => UserId -> UserId -> TeamId -> TestM ResponseLBS
requestDevice zusr uid tid = do
    g <- view tsGalley
    post $ g
           . paths ["teams", toByteString' tid, "legalhold", toByteString' uid]
           . zUser zusr . zConn "conn"
           . zType "access"


--------------------------------------------------------------------
-- setup helpers

-- | Create a new legal hold service creation request with the URL from the integration test
-- config.
newLegalHoldService :: HasCallStack => TestM NewLegalHoldService
newLegalHoldService = do
    config <- view (tsIConf . to provider)
    key' <- liftIO $ readServiceKey (publicKey config)
    let Just url = fromByteString $
            encodeUtf8 (botHost config) <> ":" <> cs (show (botPort config)) <> "/legalhold"
    return NewLegalHoldService
        { newLegalHoldServiceUrl     = url
        , newLegalHoldServiceKey     = key'
        , newLegalHoldServiceToken   = ServiceToken "tok"
        }

-- | FUTUREWORK: reduce duplication (copied from brig/Provider.hs)
readServiceKey :: (HasCallStack, MonadIO m) => FilePath -> m ServiceKeyPEM
readServiceKey fp = liftIO $ do
    bs <- BS.readFile fp
    let Right [k] = pemParseBS bs
    return (ServiceKeyPEM k)

withDummyTestServiceForTeam
    :: forall a. HasCallStack
    => UserId
    -> TeamId
    -> (Chan LBS -> TestM a)      -- ^ the test
    -> TestM a
withDummyTestServiceForTeam owner tid go = do
    withTestService dummyService runTest
  where
    runTest :: Chan LBS -> TestM a
    runTest chan = do
        newService <- newLegalHoldService
        putEnabled tid LegalHoldEnabled -- enable it for this team
        postSettings owner tid newService !!! testResponse 201 Nothing
        go chan
    dummyService :: Chan LBS -> Wai.Request -> (Wai.Response -> IO Wai.ResponseReceived) -> IO Wai.ResponseReceived
    dummyService ch req cont = do
        reqBody <- Wai.strictRequestBody req
        writeChan ch reqBody
        case (pathInfo req, requestMethod req, getRequestHeader "Authorization" req) of
            (["legalhold", "status"], "GET", _)         -> cont respondOk
            (_, _, Nothing)                             -> cont missingAuth
            (["legalhold", "initiate"], "POST", Just _) -> cont initiateResp
            (["legalhold", "confirm"], "POST", Just _)  ->
              cont respondOk
            (["legalhold", "remove"], "POST", Just _)   -> cont respondOk
            _ -> cont respondBad

    initiateResp :: Wai.Response
    initiateResp =
        Wai.json
        $ NewLegalHoldClient somePrekeys (head $ someLastPrekeys)

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
withTestService
    :: HasCallStack
    => (Chan e -> Application)  -- ^ the mock service
    -> (Chan e -> TestM a)      -- ^ the test
    -> TestM a
withTestService mkApp go = do
    config <- view (tsIConf . to provider)
    let tlss = Warp.tlsSettings (cert config) (privateKey config)
    let defs = Warp.defaultSettings { Warp.settingsPort = botPort config }
    buf <- liftIO newChan
    srv <- liftIO . Async.async $
        Warp.runTLS tlss defs $
            mkApp buf
    go buf `finally` liftIO (Async.cancel srv)

randomButValidPublicKey :: PEM
randomButValidPublicKey =
    let Right [k] = pemParseBS . BS.unlines $
              [ "-----BEGIN PUBLIC KEY-----"
              , "MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAu+Kg/PHHU3atXrUbKnw0"
              , "G06FliXcNt3lMwl2os5twEDcPPFw/feGiAKymxp+7JqZDrseS5D9THGrW+OQRIPH"
              , "WvUBdiLfGrZqJO223DB6D8K2Su/odmnjZJ2z23rhXoEArTplu+Dg9K+c2LVeXTKV"
              , "VPOaOzgtAB21XKRiQ4ermqgi3/njr03rXyq/qNkuNd6tNcg+HAfGxfGvvCSYBfiS"
              , "bUKr/BeArYRcjzr/h5m1In6fG/if9GEI6m8dxHT9JbY53wiksowy6ajCuqskIFg8"
              , "7X883H+LA/d6X5CTiPv1VMxXdBUiGPuC9IT/6CNQ1/LFt0P37ax58+LGYlaFo7la"
              , "nQIDAQAZ"
              , "-----END PUBLIC KEY-----"
              ]
    in k
-- TODO: adding two new legal hold settings on one team is not possible (409)
-- TODO: deleting or disabling lh settings deletes all lh devices
-- TODO: PATCH lh settings for updating URL or pubkey.


----------------------------------------------------------------------
-- test helpers

testResponse :: HasCallStack => Int -> Maybe TestErrorLabel -> Assertions ()
testResponse status mlabel = do
    const status === statusCode
    case mlabel of
        Just label -> responseJSON === const (Right label)
        Nothing    -> (isLeft <$> responseJSON @TestErrorLabel) === const True

newtype TestErrorLabel = TestErrorLabel { fromTestErrorLabel :: ST }
    deriving (Eq, Show)

instance IsString TestErrorLabel where
    fromString = TestErrorLabel . cs

instance Aeson.FromJSON TestErrorLabel where
    parseJSON = fmap TestErrorLabel . Aeson.withObject "TestErrorLabel" (Aeson..: "label")

-- TODO: move this to /lib/bilge?  (there is another copy of this in spar.)
responseJSON :: (HasCallStack, Aeson.FromJSON a) => ResponseLBS -> Either String a
responseJSON = fmapL show . Aeson.eitherDecode <=< maybe (Left "no body") pure . responseBody
