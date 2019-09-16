module API.MessageTimer (tests) where

import Imports hiding (head)
import API.Util
import Bilge hiding (timeout)
import Bilge.Assert
import Data.List1
import Data.Misc
import Galley.Types
import Network.Wai.Utilities.Error
import Test.Tasty
import Test.Tasty.Cannon (TimeoutUnit (..), (#))
import TestHelpers
import TestSetup
import Control.Lens (view)

import qualified Galley.Types.Teams       as Teams
import qualified Test.Tasty.Cannon        as WS

tests :: IO TestSetup -> TestTree
tests s = testGroup "Per-conversation message timer"
    [ testGroup "timer can be set at creation time"
        [ test s "1 second" (messageTimerInit timer1sec)
        , test s "nothing"  (messageTimerInit Nothing) ]
    , test s "timer can be changed" messageTimerChange
    , test s "timer can't be set by guests" messageTimerChangeGuest
    , test s "timer can't be set in 1:1 conversations" messageTimerChangeO2O
    , test s "setting the timer generates an event" messageTimerEvent
    ]

messageTimerInit
    :: Maybe Milliseconds    -- ^ Timer value
    -> TestM ()
messageTimerInit mtimer = do
    -- Create a conversation with a timer
    [alice, bob, jane] <- randomUsers 3
    connectUsers alice (list1 bob [jane])
    rsp <- postConv alice [bob, jane] Nothing [] Nothing mtimer <!!
        const 201 === statusCode
    cid <- assertConv rsp RegularConv alice alice [bob, jane] Nothing mtimer
    -- Check that the timer is indeed what it should be
    getConv jane cid !!!
        const mtimer === (cnvMessageTimer <=< responseJsonUnsafe)

messageTimerChange :: TestM ()
messageTimerChange = do
    -- Create a conversation without a timer
    [alice, bob, jane] <- randomUsers 3
    connectUsers alice (list1 bob [jane])
    rsp <- postConv alice [bob, jane] Nothing [] Nothing Nothing <!!
        const 201 === statusCode
    cid <- assertConv rsp RegularConv alice alice [bob, jane] Nothing Nothing
    -- Set timer to null and observe 204
    putMessageTimerUpdate alice cid (ConversationMessageTimerUpdate Nothing) !!!
        const 204 === statusCode
    -- Set timer to 1 second
    putMessageTimerUpdate alice cid (ConversationMessageTimerUpdate timer1sec) !!!
        const 200 === statusCode
    getConv jane cid !!!
        const timer1sec === (cnvMessageTimer <=< responseJsonUnsafe)
    -- Set timer to null
    putMessageTimerUpdate bob cid (ConversationMessageTimerUpdate Nothing) !!!
        const 200 === statusCode
    getConv jane cid !!!
        const Nothing === (cnvMessageTimer <=< responseJsonUnsafe)
    -- Set timer to 1 year
    putMessageTimerUpdate bob cid (ConversationMessageTimerUpdate timer1year) !!!
        const 200 === statusCode
    getConv jane cid !!!
        const timer1year === (cnvMessageTimer <=< responseJsonUnsafe)

messageTimerChangeGuest :: TestM ()
messageTimerChangeGuest = do
    -- Create a team and a guest user
    [owner, member, guest] <- randomUsers 3
    connectUsers owner (list1 member [guest])
    tid <- createTeam "team" owner [Teams.newTeamMember member Teams.fullPermissions Nothing]
    -- Create a conversation
    cid <- createTeamConv owner tid [member, guest] Nothing Nothing Nothing
    -- Try to change the timer (as the guest user) and observe failure
    putMessageTimerUpdate guest cid (ConversationMessageTimerUpdate timer1sec) !!! do
        const 403 === statusCode
        const "access-denied" === (label . responseJsonUnsafeWithMsg "error label")
    getConv guest cid !!!
        const Nothing === (cnvMessageTimer <=< responseJsonUnsafe)
    -- Try to change the timer (as a team member) and observe success
    putMessageTimerUpdate member cid (ConversationMessageTimerUpdate timer1sec) !!!
        const 200 === statusCode
    getConv guest cid !!!
        const timer1sec === (cnvMessageTimer <=< responseJsonUnsafe)

messageTimerChangeO2O :: TestM ()
messageTimerChangeO2O = do
    -- Create a 1:1 conversation
    [alice, bob] <- randomUsers 2
    connectUsers alice (singleton bob)
    rsp <- postO2OConv alice bob Nothing <!!
        const 200 === statusCode
    cid <- assertConv rsp One2OneConv alice alice [bob] (Just "chat") Nothing
    -- Try to change the timer and observe failure
    putMessageTimerUpdate alice cid (ConversationMessageTimerUpdate timer1sec) !!! do
        const 403 === statusCode
        const "invalid-op" === (label . responseJsonUnsafeWithMsg "error label")
    getConv alice cid !!!
        const Nothing === (cnvMessageTimer <=< responseJsonMaybe)

messageTimerEvent :: TestM ()
messageTimerEvent = do
    ca <- view tsCannon
    -- Create a conversation
    [alice, bob] <- randomUsers 2
    connectUsers alice (singleton bob)
    rsp <- postConv alice [bob] Nothing [] Nothing Nothing <!!
        const 201 === statusCode
    cid <- assertConv rsp RegularConv alice alice [bob] Nothing Nothing
    -- Set timer to 1 second and check that all participants got the event
    WS.bracketR2 ca alice bob $ \(wsA, wsB) -> do
        let update = ConversationMessageTimerUpdate timer1sec
        putMessageTimerUpdate alice cid update !!!
            const 200 === statusCode
        void . liftIO $ WS.assertMatchN (5 # Second) [wsA, wsB] $
            wsAssertConvMessageTimerUpdate cid alice update

----------------------------------------------------------------------------
-- Utilities

timer1sec :: Maybe Milliseconds
timer1sec = Just 1000

timer1year :: Maybe Milliseconds
timer1year = Just (365*86400*1000)
