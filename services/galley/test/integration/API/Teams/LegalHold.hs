module API.Teams.LegalHold (tests) where

import Imports

import API.SQS
import API.Util hiding (createTeam)
import Bilge.Assert
import Bilge hiding (trace, accept, timeout, head)
import Brig.Types.Client
-- import Brig.Types hiding (NewPasswordReset (..), CompletePasswordReset(..), EmailUpdate (..), PasswordReset (..), PasswordChange (..))
import Brig.Types.Provider
-- import Brig.Types.Provider.Tag
-- import Control.Arrow ((&&&))
-- import Control.Concurrent.Chan
-- import Control.Lens ((^.))
-- import Control.Monad.Catch
import Data.Aeson
-- import Data.Aeson hiding (json)
import Data.ByteString.Conversion
import Data.Id
-- import Data.Id hiding (client)
-- import Data.List1 (List1)
-- import Data.Misc (PlainTextPassword(..))
import Data.PEM
-- import Data.Range
-- import Data.Text.Encoding (encodeUtf8)
-- import Data.Time.Clock
-- import Data.Timeout (Timeout, TimeoutUnit (..), (#), TimedOut (..))
-- import Galley.Types (Access (..), AccessRole (..), ConversationAccessUpdate (..), NewConv (..), NewConvUnmanaged (..), Conversation (..), Members (..))
-- import Galley.Types.Bot (ServiceRef, newServiceRef, serviceRefId, serviceRefProvider)
-- import Galley.Types (ConvMembers (..), OtherMember (..))
-- import Galley.Types (Event (..), EventType (..), EventData (..), OtrMessage (..))
-- import Gundeck.Types.Notification
-- import Network.HTTP.Types.Status (status200, status201, status400)
-- import Network.Wai (Application, responseLBS, strictRequestBody)
-- import OpenSSL.PEM (writePublicKey)
-- import OpenSSL.RSA (generateRSAKey')
import qualified API.Util as Util
-- import qualified Brig.Code                         as Code
-- import qualified Brig.Types.Intra                  as Intra
-- import qualified Brig.Types.Provider.External      as Ext
-- import qualified Cassandra                         as DB
-- import qualified Control.Concurrent.Async          as Async
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
-- import qualified Network.Wai.Handler.Warp          as Warp
-- import qualified Network.Wai.Handler.Warp.Internal as Warp
-- import qualified Network.Wai.Handler.WarpTLS       as Warp
-- import qualified Network.Wai.Route                 as Wai
-- import qualified Network.Wai.Utilities.Error       as Error
-- import qualified Test.Tasty.Cannon                 as WS
-- import System.IO.Temp (withSystemTempFile)
import TestSetup
import Test.Tasty
-- import Test.Tasty hiding (Timeout)
-- import Test.Tasty.HUnit
import Test.Tasty.HUnit (assertBool)
-- import Web.Cookie (SetCookie (..), parseSetCookie)


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
    , test s "GET, PUT /i/teams/{tid}/legalhold?enabled={true,false}" testEnablePerTeam

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



-- | This type is analogous to 'NewService' for bots.
data NewLegalHoldService = NewLegalHoldService
    { newLegalHoldServiceUrl     :: !HttpsUrl
    , newLegalHoldServiceKey     :: !ServiceKeyPEM
    , newLegalHoldServiceToken   :: !(Maybe ServiceToken)
    }

instance ToJSON NewLegalHoldService where
    toJSON = undefined

instance FromJSON NewLegalHoldService where
    parseJSON = undefined



testCreateLegalHoldTeamSettings :: TestM ()
testCreateLegalHoldTeamSettings = do
    -- bad key
    let Just newUrl = fromByteString "https://new.localhost/"
        Right [k] = pemParseBS "-----BEGIN PUBLIC KEY-----\n\n-----END PUBLIC KEY-----"
        newService = NewLegalHoldService newUrl (ServiceKeyPEM k) Nothing
    postSettings newService !!! const 400 === statusCode

    (_owner, _tid) <- createTeam


{-
    -- ...
    let Just newUrl = fromByteString "https://new.localhost/"
        newService = NewLegalHoldService newUrl _ Nothing
    postSettings newService !!! const 400 === statusCode
-}


--     POST /teams/{tid}/legalhold/settings

    pure ()

    -- only allowed for users with corresp. permission bit
    -- only allowed if feature is on globally
    -- only allowed if team has feature bit set
    -- checks /status of legal hold service
    -- /status returns 5xx if unavailable
    -- legal hold device identity is authentic: the public key in the create request payload needs to match a signature in the /status response.
    -- after this, corresp. entry in cassandra will exist in a table with index TeamId.  (the entry must contain the public key from the request.)




testGetLegalHoldTeamSettings :: TestM ()
testGetLegalHoldTeamSettings = do
    pure ()

    -- only allowed for users with corresp. permission bit
    -- only allowed if feature is on globally
    -- only allowed if team has feature bit set
    -- returns 404 if team is not under legal hold
    -- returns table entry if team is under legal hold


testRemoveLegalHoldFromTeam :: TestM ()
testRemoveLegalHoldFromTeam = do
    pure ()

    -- only allowed for users with corresp. permission bit
    -- only allowed if feature is on globally
    -- only allowed if team has feature bit set
    -- after this, corresp. entry in cassandra will *NOT* exist


testEnablePerTeam :: TestM ()
testEnablePerTeam = do
    pure ()

    -- TODO: ...


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
-- helpers

createTeam :: TestM (UserId, TeamId)
createTeam = do
    ownerid <- Util.randomUser
    teamid <- Util.createTeamInternal "foo" ownerid
    assertQueue "create team" tActivate
    pure (ownerid, teamid)

postSettings :: NewLegalHoldService -> TestM ResponseLBS
postSettings = undefined

exactlyOneLegalHoldDevice :: UserId -> TestM ()
exactlyOneLegalHoldDevice uid = do
    clients :: [Client]
        <- getClients uid >>= maybe (error $ "decodeBody: [Client]") pure . decodeBody
    liftIO $ do
        let numdevs = length $ clientType <$> clients
        assertBool ("no legal hold device for user " <> show uid) (numdevs > 0)
        assertBool ("more than one legal hold device for user " <> show uid) (numdevs < 2)
