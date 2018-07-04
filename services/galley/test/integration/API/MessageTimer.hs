{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

module API.MessageTimer (tests) where

import API.Util
import Bilge hiding (timeout)
import Bilge.Assert
import Control.Monad hiding (mapM_)
import Control.Monad.IO.Class
import Data.List1
import Data.Misc
import Data.Maybe
import Galley.Types
import Network.Wai.Utilities.Error
import Prelude hiding (head, mapM_)
import Test.Tasty
import Test.Tasty.Cannon (Cannon, TimeoutUnit (..), (#))
import Test.Tasty.HUnit

import qualified Galley.Types.Teams       as Teams
import qualified Test.Tasty.Cannon        as WS

type TestSignature a = Galley -> Brig -> Cannon -> TestSetup -> Http a

test :: IO TestSetup -> TestName -> TestSignature a -> TestTree
test s n t = testCase n runTest
  where
    runTest = do
        setup <- s
        (void $ runHttpT (manager setup) (t (galley setup) (brig setup) (cannon setup) setup))

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
    -> Galley -> Brig -> Cannon -> TestSetup -> Http ()
messageTimerInit mtimer g b _ca _ = do
    -- Create a conversation with a timer
    [alice, bob, jane] <- randomUsers b 3
    connectUsers b alice (list1 bob [jane])
    rsp <- postConv g alice [bob, jane] Nothing [] Nothing mtimer <!!
        const 201 === statusCode
    cid <- assertConv rsp RegularConv alice alice [bob, jane] Nothing mtimer
    -- Check that the timer is indeed what it should be
    getConv g jane cid !!!
        const mtimer === (cnvMessageTimer <=< decodeBody)

messageTimerChange :: Galley -> Brig -> Cannon -> TestSetup -> Http ()
messageTimerChange g b _ca _ = do
    -- Create a conversation without a timer
    [alice, bob, jane] <- randomUsers b 3
    connectUsers b alice (list1 bob [jane])
    rsp <- postConv g alice [bob, jane] Nothing [] Nothing Nothing <!!
        const 201 === statusCode
    cid <- assertConv rsp RegularConv alice alice [bob, jane] Nothing Nothing
    -- Set timer to null and observe 204
    putMessageTimerUpdate g alice cid (ConversationMessageTimerUpdate Nothing) !!!
        const 204 === statusCode
    -- Set timer to 1 second
    putMessageTimerUpdate g alice cid (ConversationMessageTimerUpdate timer1sec) !!!
        const 200 === statusCode
    getConv g jane cid !!!
        const timer1sec === (cnvMessageTimer <=< decodeBody)
    -- Set timer to null
    putMessageTimerUpdate g bob cid (ConversationMessageTimerUpdate Nothing) !!!
        const 200 === statusCode
    getConv g jane cid !!!
        const Nothing === (cnvMessageTimer <=< decodeBody)
    -- Set timer to 1 year
    putMessageTimerUpdate g bob cid (ConversationMessageTimerUpdate timer1year) !!!
        const 200 === statusCode
    getConv g jane cid !!!
        const timer1year === (cnvMessageTimer <=< decodeBody)

messageTimerChangeGuest :: Galley -> Brig -> Cannon -> TestSetup -> Http ()
messageTimerChangeGuest g b _ca _ = do
    -- Create a team and a guest user
    [owner, member, guest] <- randomUsers b 3
    connectUsers b owner (list1 member [guest])
    tid <- createTeam g "team" owner [Teams.newTeamMember member Teams.fullPermissions]
    -- Create a conversation
    cid <- createTeamConv g owner tid [member, guest] Nothing Nothing Nothing
    -- Try to change the timer (as the guest user) and observe failure
    putMessageTimerUpdate g guest cid (ConversationMessageTimerUpdate timer1sec) !!! do
        const 403 === statusCode
        const "access-denied" === (label . decodeBody' "error label")
    getConv g guest cid !!!
        const Nothing === (cnvMessageTimer <=< decodeBody)
    -- Try to change the timer (as a team member) and observe success
    putMessageTimerUpdate g member cid (ConversationMessageTimerUpdate timer1sec) !!!
        const 200 === statusCode
    getConv g guest cid !!!
        const timer1sec === (cnvMessageTimer <=< decodeBody)

messageTimerChangeO2O :: Galley -> Brig -> Cannon -> TestSetup -> Http ()
messageTimerChangeO2O g b _ca _ = do
    -- Create a 1:1 conversation
    [alice, bob] <- randomUsers b 2
    connectUsers b alice (singleton bob)
    rsp <- postO2OConv g alice bob Nothing <!!
        const 200 === statusCode
    cid <- assertConv rsp One2OneConv alice alice [bob] (Just "chat") Nothing
    -- Try to change the timer and observe failure
    putMessageTimerUpdate g alice cid (ConversationMessageTimerUpdate timer1sec) !!! do
        const 403 === statusCode
        const "invalid-op" === (label . decodeBody' "error label")
    getConv g alice cid !!!
        const Nothing === (cnvMessageTimer <=< decodeBody)

messageTimerEvent :: Galley -> Brig -> Cannon -> TestSetup -> Http ()
messageTimerEvent g b ca _ = do
    -- Create a conversation
    [alice, bob] <- randomUsers b 2
    connectUsers b alice (singleton bob)
    rsp <- postConv g alice [bob] Nothing [] Nothing Nothing <!!
        const 201 === statusCode
    cid <- assertConv rsp RegularConv alice alice [bob] Nothing Nothing
    -- Set timer to 1 second and check that all participants got the event
    WS.bracketR2 ca alice bob $ \(wsA, wsB) -> do
        let update = ConversationMessageTimerUpdate timer1sec
        putMessageTimerUpdate g alice cid update !!!
            const 200 === statusCode
        void . liftIO $ WS.assertMatchN (5 # Second) [wsA, wsB] $
            wsAssertConvMessageTimerUpdate cid alice update

----------------------------------------------------------------------------
-- Utilities

timer1sec :: Maybe Milliseconds
timer1sec = Just 1000

timer1year :: Maybe Milliseconds
timer1year = Just (365*86400*1000)
