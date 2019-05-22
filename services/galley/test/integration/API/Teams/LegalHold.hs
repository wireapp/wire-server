module API.Teams.LegalHold (tests) where

import Imports

import API.SQS
import API.Util hiding (createTeam)
import Bilge.Assert
import Bilge hiding (trace, accept, timeout, head)
import Brig.Types.Client
import Brig.Types.Provider
import Brig.Types.Team.LegalHold
import Control.Lens
import Data.ByteString.Conversion
import Data.Id
import Data.PEM
import Galley.API.LegalHold (validateServiceKey)
import Galley.Types.Teams
import TestSetup
import Test.Tasty
import Test.Tasty.HUnit (assertBool)

import qualified API.Util as Util
import qualified Data.ByteString                   as BS


-- import qualified Network.Wai.Utilities.Response as Wai
-- import Data.Void
import Network.Wai
import Data.String.Conversions (cs)
-- import TestSetup
-- import Bilge hiding (accept, timeout, head, trace)
-- import Bilge.Assert
-- import Brig.Types hiding (NewPasswordReset (..), CompletePasswordReset(..), EmailUpdate (..), PasswordReset (..), PasswordChange (..))
-- import Brig.Types.Provider
-- import Brig.Types.Provider.Tag
-- import Control.Arrow ((&&&))
import Control.Concurrent.Chan
-- import Control.Concurrent.Timeout (timeout, threadDelay)
-- import Control.Lens ((^.))
import Control.Monad.Catch
import qualified Data.Aeson as Aeson
-- import Data.ByteString.Conversion
-- import Data.Id hiding (client)
-- import Data.List1 (List1)
-- import Data.Misc (PlainTextPassword(..))
-- import Data.PEM
-- import Data.Range
import Data.Text.Encoding (encodeUtf8)
-- import Data.Time.Clock
-- import Data.Timeout (Timeout, TimeoutUnit (..), (#), TimedOut (..))
-- import Galley.Types (ConvMembers (..), OtherMember (..))
-- import Galley.Types (Event (..), EventType (..), EventData (..), OtrMessage (..))
-- import Galley.Types.Bot (ServiceRef, newServiceRef, serviceRefId, serviceRefProvider)
-- import Gundeck.Types.Notification
import Network.HTTP.Types.Status (status200, status404)
import Network.Wai as Wai
-- import OpenSSL.PEM (writePublicKey)
-- import OpenSSL.RSA (generateRSAKey')
-- import System.IO.Temp (withSystemTempFile)
-- import Test.Tasty hiding (Timeout)
import Test.Tasty.HUnit
-- import Web.Cookie (SetCookie (..), parseSetCookie)
-- import Util

-- import qualified API.Team.Util                     as Team
-- import qualified Brig.Code                         as Code
-- import qualified Brig.Types.Intra                  as Intra
-- import qualified Brig.Types.Provider.External      as Ext
-- import qualified Cassandra                         as DB
import qualified Control.Concurrent.Async          as Async
-- import qualified Data.ByteString                   as BS
-- import qualified Data.ByteString.Char8             as C8
-- import qualified Data.ByteString.Lazy.Char8        as LC8
-- import qualified Data.HashMap.Strict               as HashMap
-- import qualified Data.List1                        as List1
-- import qualified Data.Set                          as Set
-- import qualified Data.Text.Ascii                   as Ascii
-- import qualified Data.Text                         as Text
-- import qualified Data.Text.Encoding                as Text
-- import qualified Data.UUID                         as UUID
-- import qualified Data.ZAuth.Token                  as ZAuth
-- import qualified Galley.Types.Teams                as Team
import qualified Network.Wai.Handler.Warp          as Warp
import qualified Network.Wai.Handler.WarpTLS       as Warp
import qualified Network.Wai.Handler.Warp.Internal as Warp
-- import qualified Network.Wai.Route                 as Wai
-- import qualified Network.Wai.Utilities.Error       as Error
-- import qualified Test.Tasty.Cannon                 as WS
import TestHelpers




tests :: IO TestSetup -> TestTree
tests s = testGroup "Teams LegalHold API"
    [ -- device handling (CRUD)
      test s "POST /teams/{tid}/legalhold/{uid}" testCreateLegalHoldDevice
    , test s "POST /teams/{tid}/legalhold/{uid} - twice" testCreateTwoLegalHoldDevices
    , test s "PUT /teams/{tid}/legalhold/{uid}/approve" testApproveLegalHoldDevice
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


testCreateLegalHoldDevice :: TestM ()
testCreateLegalHoldDevice = ignore $ do
    pure ()

    -- team admin, owner can do it.  (implemented via new internal permission bit.)
    -- member can't do it.
    -- fail if legal hold service is disabled for team
    -- fail if legal hold service is disabled via feature flag
    -- all of user's clients receive an event
    -- contacts team's legal hold service and establishes a cryptobox
    -- responds with public key etc of legal hold device
    -- requests approval from monitored user asynchronously; request contains pre-keys


testCreateTwoLegalHoldDevices :: TestM ()
testCreateTwoLegalHoldDevices = do
    pure ()

    -- creating a second valid a legal hold device is rejected.
    -- also when the legal hold status of a user is 'pending".


testApproveLegalHoldDevice :: TestM ()
testApproveLegalHoldDevice = do
    pure ()

    -- only user themself can do it
    -- fail if no legal hold service registered
    -- fail if legal Hold feature flag disabled
    -- generates and stores legalhold tokens/cookies
    -- synchronously sends tokens/cookies to team's legal hold service
    -- expect return payload from service to contain device creation information (NewClient)
    -- adds device to user's list of devices
    -- sends an event to all user's clients
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
    postSettings owner tid newService !!! const 403 === statusCode
    putEnabled tid True !!! const 204 === statusCode -- enable it for this team

    -- rejected if service is not available
    postSettings owner tid newService !!! const 400 === statusCode  -- TODO: test err label


    -- checks /status of legal hold service (boolean argument says whether the service is
    -- behaving or not)
    let lhapp :: HasCallStack => Bool -> Chan Void -> Application
        lhapp _isworking@False _ _   cont = cont respondBad
        lhapp _isworking@True  _ req cont = do
            if | pathInfo req /= ["status"] -> cont respondBad
               | requestMethod req /= "GET" -> cont respondBad
               | otherwise -> cont respondOk

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
            service <- getSettingsTyped owner tid
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

        -- returns 403 if legalhold disabled for team
        getSettings owner tid !!! const 403 === statusCode
        putEnabled tid True !!! const 204 === statusCode -- enable for team

        -- returns 412 if team is not under legal hold
        getSettings owner tid !!! const 412 === statusCode
        getSettings member tid !!! const 412 === statusCode

        postSettings owner tid newService !!! const 201 === statusCode

        -- returns legal hold service info if team is under legal hold and user is in team (even
        -- no permissions).
        resp <- getSettingsTyped member tid
        liftIO $ do
            Just (_, fpr) <- validateServiceKey (newLegalHoldServiceKey newService)
            assertEqual "viewLegalHoldServiceTeam" tid (viewLegalHoldServiceTeam resp)
            assertEqual "viewLegalHoldServiceUrl" (newLegalHoldServiceUrl newService) (viewLegalHoldServiceUrl resp)
            assertEqual "viewLegalHoldServiceFingerprint" fpr (viewLegalHoldServiceFingerprint resp)

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
        putEnabled tid True !!! const 204 === statusCode -- enable for team
        postSettings owner tid newService !!! const 201 === statusCode

        -- TODO: not allowed if feature is disabled globally in galley config yaml

        -- TODO: not allowed if team has feature bit not set

        -- returns 403 if user is not in team or has unsufficient permissions.
        deleteSettings stranger tid !!! const 403 === statusCode
        deleteSettings member tid !!! const 403 === statusCode

        -- returns 204 if legal hold is successfully removed from team
        deleteSettings owner tid !!! const 204 === statusCode

        -- deletion is successful (both witnessed on the API and in the backend)
        getSettings owner tid !!! const 412 === statusCode

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
    liftIO $ assertBool "Teams should start with LegalHold disabled" (not isInitiallyEnabled)

    putEnabled tid True !!! const 204 === statusCode
    LegalHoldTeamConfig isEnabledAfter <- jsonBody <$> (getEnabled tid <!! const 200 === statusCode)
    liftIO $ assertBool "Calling 'putEnabled True' should enable LegalHold" isEnabledAfter

    putEnabled tid False !!! const 204 === statusCode
    LegalHoldTeamConfig isEnabledAfterUnset <- jsonBody <$> (getEnabled tid <!! const 200 === statusCode)
    liftIO $ assertBool "Calling 'putEnabled False' should disable LegalHold" (not isEnabledAfterUnset)
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

putEnabled :: HasCallStack => TeamId -> Bool -> TestM ResponseLBS
putEnabled tid enabled = do
    g <- view tsGalley
    put $ g
         . paths ["i", "teams", toByteString' tid, "legalhold"]
         . json (LegalHoldTeamConfig enabled)

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

exactlyOneLegalHoldDevice :: HasCallStack => UserId -> TestM ()
exactlyOneLegalHoldDevice uid = do
    clients :: [Client]
        <- getClients uid >>= maybe (error $ "decodeBody: [Client]") pure . decodeBody
    liftIO $ do
        let numdevs = length $ clientType <$> clients
        assertBool ("no legal hold device for user " <> show uid) (numdevs > 0)
        assertBool ("more than one legal hold device for user " <> show uid) (numdevs < 2)

jsonBody :: Aeson.FromJSON v => ResponseLBS -> v
jsonBody = either (error . show) id . Aeson.eitherDecode . fromJust . responseBody


--------------------------------------------------------------------
-- setup helpers

-- | Create a new legal hold service creation request with the URL from the integration test
-- config.
newLegalHoldService :: HasCallStack => TestM NewLegalHoldService
newLegalHoldService = do
    config <- view (tsIConf . to provider)
    key <- liftIO $ readServiceKey (publicKey config)
    let Just url = fromByteString $
            encodeUtf8 (botHost config) <> ":" <> cs (show (botPort config))
    return NewLegalHoldService
        { newLegalHoldServiceUrl     = url
        , newLegalHoldServiceKey     = key
        , newLegalHoldServiceToken   = ServiceToken "tok"
        }

-- | FUTUREWORK: reduce duplication (copied from brig/Provider.hs)
readServiceKey :: (HasCallStack, MonadIO m) => FilePath -> m ServiceKeyPEM
readServiceKey fp = liftIO $ do
    bs <- BS.readFile fp
    let Right [k] = pemParseBS bs
    return (ServiceKeyPEM k)

-- | Run a test with an mock legal hold service application.  The mock service is also binding
-- to a TCP socket for the backend to connect to.  The mock service can expose internal
-- details to the test (for both read and write) via a 'Chan'.  This is not concurrency-proof!
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


-- TODO: adding two new legal hold settings on one team is not possible (409)
-- TODO: deleting or disabling lh settings deletes all lh devices
-- TODO: PATCH lh settings for updating URL or pubkey.
