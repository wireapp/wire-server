module API.Teams.LegalHold (tests) where

import Imports
import API.Util
import Bilge hiding (timeout)
import Bilge.Assert
import Control.Lens hiding ((#), (.=))
import Data.Aeson hiding (json)
import Data.Aeson.Lens
import Data.ByteString.Conversion
import Data.Id
import Data.List1
import Data.Misc (PlainTextPassword (..))
import Data.Range
import Galley.Types hiding (EventType (..), EventData (..), MemberUpdate (..))
import Galley.Types.Teams
import Galley.Types.Teams.Intra
import Gundeck.Types.Notification
import Test.Tasty
import Test.Tasty.Cannon (TimeoutUnit (..), (#))
import Test.Tasty.HUnit
import TestSetup (test,  TestSetup, TestM, tsCannon, tsGalley)
import API.SQS
import UnliftIO (mapConcurrently, mapConcurrently_)
import Brig.Types.Client

import qualified API.Util as Util
import qualified Data.Currency as Currency
import qualified Data.List1 as List1
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.UUID as UUID
import qualified Galley.Types as Conv
import qualified Network.Wai.Utilities.Error as Error
import qualified Test.Tasty.Cannon as WS

tests :: IO TestSetup -> TestTree
tests s = testGroup "Teams LegalHold API"
    [ test s "create client" testDisallowLegalHoldDeviceCreation
    ]

timeout :: WS.Timeout
timeout = 3 # Second

testDisallowLegalHoldDeviceCreation :: TestM ()
testDisallowLegalHoldDeviceCreation = do
    -- regular users cannot create LegalHoldClients
    let lk = (someLastPrekeys !! 0)
    u <- randomUser

    -- TODO: requests to /clients with type=LegalHoldClientType should fail  (400 instead of 201)
    void $ randomClientWithType LegalHoldClientType 201 u lk

    -- team users cannot create LegalHoldClients
    owner <- Util.randomUser
    tid   <- Util.createTeamInternal "foo" owner
    assertQueue "create team" tActivate

    -- TODO: requests to /clients with type=LegalHoldClientType should fail (400 instead of 201)
    void $ randomClientWithType LegalHoldClientType 201 owner lk

    -- TODO: also check this with a team and a user both cleared for legalhold.
