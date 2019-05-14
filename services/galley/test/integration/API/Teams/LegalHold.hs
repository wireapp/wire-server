module API.Teams.LegalHold (tests) where

import Imports
import API.Util
-- import Bilge hiding (timeout)
-- import Bilge.Assert
-- import Control.Lens hiding ((#), (.=))
-- import Data.Aeson hiding (json)
-- import Data.Aeson.Lens
-- import Data.ByteString.Conversion
import Data.Id
-- import Data.List1
-- import Data.Misc (PlainTextPassword (..))
-- import Data.Range
-- import Galley.Types hiding (EventType (..), EventData (..), MemberUpdate (..))
-- import Galley.Types.Teams
-- import Galley.Types.Teams.Intra
-- import Gundeck.Types.Notification
import Test.Tasty
import Test.Tasty.HUnit (assertBool)
import TestSetup
import API.SQS
import Brig.Types.Client

import qualified API.Util as Util
-- import qualified Data.Currency as Currency
-- import qualified Data.List1 as List1
-- import qualified Data.Set as Set
-- import qualified Data.Text as T
-- import qualified Data.UUID as UUID
-- import qualified Galley.Types as Conv
-- import qualified Network.Wai.Utilities.Error as Error

tests :: IO TestSetup -> TestTree
tests s = testGroup "Teams LegalHold API"
    [ test s "create" testCreateLegalHoldDevice
    , test s "create twice" testCreateTwoLegalHoldDevices
    , test s "create via POST /clients" testCreateLegalHoldDeviceOldAPI
    ]


testCreateLegalHoldDevice :: TestM ()
testCreateLegalHoldDevice = do
    -- TODO: test valid creation of a legal hold device here.  make sure it works.
    pure ()


testCreateTwoLegalHoldDevices :: TestM ()
testCreateTwoLegalHoldDevices = do
    -- TODO: test that creating a second valid a legal hold device is rejected.
    pure ()

testCreateLegalHoldDeviceOldAPI :: TestM ()
testCreateLegalHoldDeviceOldAPI = do
    -- regular users cannot create LegalHoldClients
    let lk = (someLastPrekeys !! 0)
    u <- randomUser

    -- TODO: requests to /clients with type=LegalHoldClientType should fail  (400 instead of 201)
    void $ randomClientWithType LegalHoldClientType 201 u lk

    -- team users cannot create LegalHoldClients
    owner <- Util.randomUser
    _tid   <- Util.createTeamInternal "foo" owner
    assertQueue "create team" tActivate

    -- TODO: requests to /clients with type=LegalHoldClientType should fail (400 instead of 201)
    void $ randomClientWithType LegalHoldClientType 201 owner lk
    exactlyOneLegalHoldDevice owner

    -- TODO: the remainder of this test can be removed once `POST /clients` does not work any
    -- more for legal hold devices.
    void $ randomClientWithType LegalHoldClientType 201 owner lk  -- overwrite
    exactlyOneLegalHoldDevice owner



-- TODO: DELETE /client/:cid => bad.


----------------------------------------------------------------------
-- helpers

exactlyOneLegalHoldDevice :: UserId -> TestM ()
exactlyOneLegalHoldDevice uid = do
    clients :: [Client]
        <- getClients uid >>= maybe (error $ "decodeBody: [Client]") pure . decodeBody
    liftIO $ do
        let numdevs = length $ clientType <$> clients
        assertBool ("no legal hold device for user " <> show uid) (numdevs > 0)
        assertBool ("more than one legal hold device for user " <> show uid) (numdevs < 2)
    -- TODO: ...  and can we just use hspec?
