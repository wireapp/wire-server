{-# OPTIONS_GHC -Wno-orphans #-}

module API.Teams.LegalHold (tests) where

import Imports

import API.SQS
import API.Util hiding (createTeam)
import Bilge.Assert
import Bilge hiding (trace, accept, timeout, head)
import Brig.Types.Client
import Brig.Types.Provider
import Brig.Types.Team.LegalHold
import Brig.Types.Test.Arbitrary ()
import Control.Concurrent.Chan
import Control.Lens
import Control.Monad.Catch
import Control.Retry (recoverAll, exponentialBackoff, limitRetries)
import Data.Aeson.Lens
import Data.ByteString.Conversion
import Data.Id
import Data.PEM
import Data.Proxy (Proxy(Proxy))
import Data.String.Conversions (cs)
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
    , test s "POST /teams/{tid}/legalhold/{uid}" testCreateLegalHoldDevice
    , test s "POST /teams/{tid}/legalhold/{uid} - twice" testCreateTwoLegalHoldDevices
    , test s "PUT /teams/{tid}/legalhold/approve" testApproveLegalHoldDevice
    , test s "(user denies approval: nothing needs to be done in backend)" (pure ())
    , test s "GET /teams/{tid}/legalhold/{uid}" testGetLegalHoldDeviceStatus
    , test s "DELETE /teams/{tid}/legalhold/{uid}" testRemoveLegalHoldDevice

      -- legal hold settings
    , test s "POST /teams/{tid}/legalhold/settings" testCreateLegalHoldTeamSettings
    , test s "GET /teams/{tid}/legalhold/settings" testGetLegalHoldTeamSettings
    , test s "DELETE /teams/{tid}/legalhold/settings" testRemoveLegalHoldFromTeam
    , test s "GET, PUT /i/teams/{tid}/legalhold" testEnablePerTeam

      -- behavior of existing end-points
    , test s "POST /clients" testCreateLegalHoldDeviceOldAPI
    , test s "DELETE /clients/{cid}" testDeleteLegalHoldDeviceOldAPI

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


testCreateLegalHoldDevice :: TestM ()
testCreateLegalHoldDevice = do
    (owner, tid) <- createTeam
    member <- randomUser
    addTeamMemberInternal tid $ newTeamMember member (rolePermissions RoleMember) Nothing

    cannon <- view tsCannon

    -- Assert that the appropriate LegalHold Request notification is sent to the user's
    -- clients
    WS.bracketR cannon member $ \ws -> withDummyTestServiceForTeam owner tid $ do
        -- Can't request a device if team feature flag is disabled
        -- TODO: Add this back once the check is re-enabled
        -- requestDevice owner member tid !!! const 403 === statusCode
        -- putEnabled tid LegalHoldEnabled -- enable it for this team

        do userStatus <- getUserStatusTyped member tid
           liftIO $ assertEqual "User legal hold status should start as disabled" userStatus UserLegalHoldDisabled

        do requestDevice member member tid !!! const 403 === statusCode
           userStatus <- getUserStatusTyped member tid
           liftIO $ assertEqual "User with insufficient permissions should be unable to change user status"
                      userStatus UserLegalHoldDisabled

        do requestDevice owner member tid !!! const 204 === statusCode
           userStatus <- getUserStatusTyped member tid
           liftIO $ assertEqual "requestDevice should set user status to Pending"
                      userStatus UserLegalHoldPending

        do requestDevice owner member tid !!! const 204 === statusCode
           userStatus <- getUserStatusTyped member tid
           liftIO $ assertEqual "requestDevice when already pending should leave status as Pending"
                      userStatus UserLegalHoldPending

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
                let eRequester = j ^? key "requester" . _String
                let eTargetUser = j ^? key "target_user" . _String
                let eLastPrekey = j ^? key "last_prekey" . _JSON
                let ePrekeys = j ^? key "prekeys" . _JSON
                etype @?= Just "user.client-legal-hold-request"
                eRequester @?= Just (idToText owner)
                eTargetUser @?= Just (idToText member)
                -- These need to match the values provided by the 'dummy service'
                Just (head someLastPrekeys) @?= eLastPrekey
                Just somePrekeys @?= ePrekeys

        -- TODO: Not sure why I have to do this; which extra notification is stuck on the
        -- queue?
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

    WS.bracketR cannon member $ \ws -> withDummyTestServiceForTeam owner tid $ do
        -- not allowed to approve if team setting is disabled
        -- TODO: remove the following 'ignore' once 'disabled' is the default
        ignore $ approveLegalHoldDevice owner member tid !!! const 403 === statusCode

        putEnabled tid LegalHoldEnabled
        requestDevice owner member tid !!! const 204 === statusCode

        putEnabled tid LegalHoldDisabled
        -- Can't approve device when in disabled state
        -- TODO: remove the following 'ignore' once 'disabled' is the default
        ignore $ approveLegalHoldDevice member member tid !!! const 403 === statusCode
        putEnabled tid LegalHoldEnabled

        -- Only the user themself can approve adding a LH device
        approveLegalHoldDevice owner member tid !!! const 403 === statusCode
        approveLegalHoldDevice member member tid !!! const 200 === statusCode

        cassState <- view tsCass
        liftIO $ do
            clients' <- Cql.runClient cassState $ Data.lookupClients [member]
            assertBool "Expect clientId to be saved on the user"
              $ Clients.contains member someClientId clients'

        userStatus <- getUserStatusTyped member tid
        liftIO $ assertEqual "After approval user legalhold status should be Enabled"
                    UserLegalHoldEnabled userStatus

        liftIO $ do
            void . liftIO $ WS.assertMatch (5 WS.# WS.Second) ws $ \n -> do
                let j = Aeson.Object $ List1.head (ntfPayload n)
                let etype = j ^? key "type" . _String
                let eClient = j ^? key "client" . _JSON
                etype @?= Just "user.client-add"
                clientId <$> eClient @?= Just someClientId
                clientType <$> eClient @?= Just LegalHoldClientType
                clientClass <$> eClient @?= Just (Just LegalHoldClient)

    ensureQueueEmpty

        -- fail if GLOBAL legal Hold feature flag disabled
        -- sends an event to team settings (however that works; it's a client-independent event i think)
        -- all of user's communication peers receive an event


testGetLegalHoldDeviceStatus :: TestM ()
testGetLegalHoldDeviceStatus = do
    pure ()

    -- Show whether enabled, pending, disabled


testRemoveLegalHoldDevice :: TestM ()
testRemoveLegalHoldDevice = do
    pure ()

    -- Remove Legal hold device from user.
    -- send event to all of user's devices
    -- send event to all communication peers
    -- when still pending, notify clients they no longer need approval if deleted when still pending


testCreateLegalHoldTeamSettings :: TestM ()
testCreateLegalHoldTeamSettings = do
    (owner, tid) <- createTeam
    member <- randomUser
    addTeamMemberInternal tid $ newTeamMember member (rolePermissions RoleMember) Nothing
    newService <- newLegalHoldService
    let Right [k] = pemParseBS "-----BEGIN PUBLIC KEY-----\n\n-----END PUBLIC KEY-----"
        badService = newService { newLegalHoldServiceKey = ServiceKeyPEM k }
        -- TODO: make a bad service with a syntactically valid, but wrong certificate.

    -- TODO: not allowed if feature is disabled globally in galley config yaml

    -- not allowed for users with corresp. permission bit missing
    postSettings member tid newService !!! const 403 === statusCode  -- TODO: test err label

    -- not allowed to create if team setting is disabled
    -- TODO: remove the following 'ignore' once 'disabled' is the default
    ignore $ postSettings owner tid newService !!! const 403 === statusCode

    putEnabled tid LegalHoldEnabled -- enable it for this team

    -- rejected if service is not available
    postSettings owner tid newService !!! const 400 === statusCode  -- TODO: test err label


    -- checks /status of legal hold service (boolean argument says whether the service is
    -- behaving or not)
    let lhapp :: HasCallStack => Bool -> Chan Void -> Application
        lhapp _isworking@False _ _   cont = cont respondBad
        lhapp _isworking@True  _ req cont = trace "APP" $ do
            if | pathInfo req /= ["legalhold", "bots", "status"] -> cont respondBad
               | requestMethod req /= "GET" -> cont respondBad
               | otherwise -> trace "hit" $ cont respondOk

        respondOk :: Wai.Response
        respondOk = responseLBS status200 mempty mempty

        respondBad :: Wai.Response
        respondBad = responseLBS status404 mempty mempty

        lhtest :: HasCallStack => Bool -> Chan Void -> TestM ()
        lhtest _isworking@False _ = do
            postSettings owner tid newService !!! const 400 === statusCode  -- TODO: test err label

        lhtest _isworking@True _ = do
            postSettings owner tid badService !!! const 400 === statusCode  -- TODO: test err label
            postSettings owner tid newService !!! const 201 === statusCode
            ViewLegalHoldService service <- getSettingsTyped owner tid
            liftIO $ do
                Just (_, fpr) <- validateServiceKey (newLegalHoldServiceKey newService)
                assertEqual "viewLegalHoldTeam" tid (viewLegalHoldServiceTeam service)
                assertEqual "viewLegalHoldServiceUrl" (newLegalHoldServiceUrl newService) (viewLegalHoldServiceUrl service)
                assertEqual "viewLegalHoldServiceFingerprint" fpr (viewLegalHoldServiceFingerprint service)
            -- TODO: check cassandra as well?

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
        getSettings stranger tid !!! const 403 === statusCode

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

        postSettings owner tid newService !!! const 201 === statusCode

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
    deleteSettings owner tid !!! const 204 === statusCode

    let lhapp :: Chan () -> Application
        lhapp _ch _req res = res $ responseLBS status200 mempty mempty

    withTestService lhapp $ \_ -> do
        newService <- newLegalHoldService
        putEnabled tid LegalHoldEnabled -- enable it for this team
        postSettings owner tid newService !!! const 201 === statusCode

        -- TODO: not allowed if feature is disabled globally in galley config yaml

        -- TODO: not allowed if team has feature bit not set

        -- returns 403 if user is not in team or has unsufficient permissions.
        deleteSettings stranger tid !!! const 403 === statusCode
        deleteSettings member tid !!! const 403 === statusCode

        -- returns 204 if legal hold is successfully removed from team
        deleteSettings owner tid !!! const 204 === statusCode

        -- deletion is successful (both witnessed on the API and in the backend)
        resp <- getSettings owner tid
        liftIO $ assertEqual "bad body" ViewLegalHoldServiceNotConfigured (jsonBody resp)

        -- TODO: do we also want to check the DB?

        -- TODO: do we really want any trace of the fact that this team has been under legal hold
        -- to go away?  or should a team that has been under legal hold in the past be observably
        -- different for the members from one that never has?

        -- TODO: also remove all devices from users in this team!!

    ensureQueueEmpty  -- TODO: there are some pending events in there.  make sure it's the right ones.


testEnablePerTeam :: TestM ()
testEnablePerTeam = do
    (_, tid) <- createTeam
    LegalHoldTeamConfig isInitiallyEnabled <- jsonBody <$> (getEnabled tid <!! const 200 === statusCode)
    liftIO $ assertEqual "Teams should start with LegalHold disabled" isInitiallyEnabled LegalHoldDisabled

    putEnabled tid LegalHoldEnabled -- enable it for this team
    LegalHoldTeamConfig isEnabledAfter <- jsonBody <$> (getEnabled tid <!! const 200 === statusCode)
    liftIO $ assertEqual "Calling 'putEnabled True' should enable LegalHold" isEnabledAfter LegalHoldEnabled

    putEnabled tid LegalHoldDisabled -- disable again
    LegalHoldTeamConfig isEnabledAfterUnset <- jsonBody <$> (getEnabled tid <!! const 200 === statusCode)
    liftIO $ assertEqual "Calling 'putEnabled False' should disable LegalHold" isEnabledAfterUnset LegalHoldDisabled

    -- TODO: Check that disabling legalhold for a team removes the LH device from all team
    -- members

testCreateLegalHoldDeviceOldAPI :: TestM ()
testCreateLegalHoldDeviceOldAPI = do
    -- regular users cannot create LegalHoldClients
    let lk = (someLastPrekeys !! 0)
    u <- randomUser

    -- TODO: requests to /clients with type=LegalHoldClientType should fail  (400 instead of 201)
    void $ randomClientWithType LegalHoldClientType 201 u lk

    -- team users cannot create LegalHoldClients
    (owner, _) <- createTeam

    -- TODO: requests to /clients with type=LegalHoldClientType should fail (400 instead of 201)
    void $ randomClientWithType LegalHoldClientType 201 owner lk
    exactlyOneLegalHoldDevice owner

    -- TODO: the remainder of this test can be removed once `POST /clients` does not work any
    -- more for legal hold devices.
    void $ randomClientWithType LegalHoldClientType 201 owner lk  -- overwrite
    exactlyOneLegalHoldDevice owner


testDeleteLegalHoldDeviceOldAPI :: TestM ()
testDeleteLegalHoldDeviceOldAPI = do
    pure ()

    -- legal hold device cannot be deleted by anybody, ever.


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

putEnabled :: HasCallStack => TeamId -> LegalHoldStatus -> TestM ()
putEnabled tid enabled = do
    g <- view tsGalley
    void . put $ g
         . paths ["i", "teams", toByteString' tid, "legalhold"]
         . json (LegalHoldTeamConfig enabled)
         . expect2xx

postSettings :: HasCallStack => UserId -> TeamId -> NewLegalHoldService -> TestM ResponseLBS
postSettings uid tid new = do
    g <- view tsGalley
    post $ g
         . paths ["teams", toByteString' tid, "legalhold", "settings"]
         . zUser uid . zConn "conn"
         . zType "access"
         . json new

getSettingsTyped :: HasCallStack => UserId -> TeamId -> TestM ViewLegalHoldService
getSettingsTyped uid tid = jsonBody <$> (getSettings uid tid <!! const 200 === statusCode)

getSettings :: HasCallStack => UserId -> TeamId -> TestM ResponseLBS
getSettings uid tid = do
    g <- view tsGalley
    get $ g
        . paths ["teams", toByteString' tid, "legalhold", "settings"]
        . zUser uid . zConn "conn"
        . zType "access"

deleteSettings :: HasCallStack => UserId -> TeamId -> TestM ResponseLBS
deleteSettings uid tid = do
    g <- view tsGalley
    delete $ g
           . paths ["teams", toByteString' tid, "legalhold", "settings"]
           . zUser uid . zConn "conn"
           . zType "access"

getUserStatusTyped :: HasCallStack => UserId -> TeamId -> TestM UserLegalHoldStatus
getUserStatusTyped uid tid = jsonBody <$> (getUserStatus uid tid <!! const 200 === statusCode)

getUserStatus :: HasCallStack => UserId -> TeamId -> TestM ResponseLBS
getUserStatus uid tid = do
    g <- view tsGalley
    get $ g
           . paths ["teams", toByteString' tid, "legalhold", toByteString' uid]
           . zUser uid . zConn "conn"
           . zType "access"

approveLegalHoldDevice :: HasCallStack => UserId -> UserId -> TeamId -> TestM ResponseLBS
approveLegalHoldDevice zusr uid tid = do
    g <- view tsGalley
    put $ g
           . paths ["teams", toByteString' tid, "legalhold", toByteString' uid, "approve"]
           . zUser zusr . zConn "conn"
           . zType "access"

exactlyOneLegalHoldDevice :: HasCallStack => UserId -> TestM ()
exactlyOneLegalHoldDevice uid = do
    clients :: [Client]
        <- getClients uid >>= maybe (error $ "decodeBody: [Client]") pure . decodeBody
    liftIO $ do
        let numdevs = length $ clientType <$> clients
        assertBool ("no legal hold device for user " <> show uid) (numdevs > 0)
        assertBool ("more than one legal hold device for user " <> show uid) (numdevs < 2)

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

-- deleteDevice :: HasCallStack => TeamId -> UserId -> TestM ResponseLBS
-- deleteDevice = undefined

--------------------------------------------------------------------
-- setup helpers

-- | Create a new legal hold service creation request with the URL from the integration test
-- config.
newLegalHoldService :: HasCallStack => TestM NewLegalHoldService
newLegalHoldService = do
    config <- view (tsIConf . to provider)
    key' <- liftIO $ readServiceKey (publicKey config)
    let Just url = fromByteString $
            encodeUtf8 (botHost config) <> ":" <> cs (show (botPort config))
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
    :: HasCallStack
    => UserId
    -> TeamId
    -> TestM a      -- ^ the test
    -> TestM a
withDummyTestServiceForTeam owner tid go = do
    withTestService dummyService runTest
  where
    runTest _chan = do
        newService <- newLegalHoldService
        putEnabled tid LegalHoldEnabled -- enable it for this team
        postSettings owner tid newService !!! const 201 === statusCode
        go
    dummyService :: Chan () -> Application
    dummyService _ch req cont = do
        case (pathInfo req, requestMethod req, getRequestHeader "Authorization" req) of
            (["legalhold", "bots", "status"], "GET", _) -> cont respondOk
            (_, _, Nothing) -> cont missingAuth
            (["legalhold", "initiate"], "POST", Just _) -> cont initiateResp
            (["legalhold", "confirm"], "POST", Just _) -> cont respondOk
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
    recoverAll (exponentialBackoff 50 <> limitRetries 5) (\_ -> go buf)
        `finally` liftIO (Async.cancel srv)


-- TODO: adding two new legal hold settings on one team is not possible (409)
-- TODO: deleting or disabling lh settings deletes all lh devices
-- TODO: PATCH lh settings for updating URL or pubkey.
