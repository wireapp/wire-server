{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

module API.MessageTimer (tests) where

import API.Util
import Bilge hiding (timeout)
import Bilge.Assert
import Brig.Types
import Control.Applicative hiding (empty)
import Control.Error
import Control.Lens ((^.))
import Control.Monad hiding (mapM_)
import Control.Monad.IO.Class
import Data.Aeson hiding (json)
import Data.ByteString.Conversion
import Data.Foldable (mapM_)
import Data.Id
import Data.Int
import Data.List ((\\), find)
import Data.List1
import Data.Misc
import Data.Maybe
import Data.Monoid
import Data.Range
import Galley.Types
import Gundeck.Types.Notification
import Network.Wai.Utilities.Error
import Prelude hiding (head, mapM_)
import Test.Tasty
import Test.Tasty.Cannon (Cannon, TimeoutUnit (..), (#))
import Test.Tasty.HUnit
import API.SQS

import qualified Data.Text.Ascii          as Ascii
import qualified Galley.Types.Teams       as Teams
import qualified API.Teams                as Teams
import qualified Control.Concurrent.Async as Async
import qualified Data.List1               as List1
import qualified Data.Map.Strict          as Map
import qualified Data.Set                 as Set
import qualified Data.Text                as T
import qualified Test.Tasty.Cannon        as WS
import qualified Data.Code                as Code

type TestSignature a = Galley -> Brig -> Cannon -> TestSetup -> Http a

test :: IO TestSetup -> TestName -> TestSignature a -> TestTree
test s n t = testCase n runTest
  where
    runTest = do
        setup <- s
        (void $ runHttpT (manager setup) (t (galley setup) (brig setup) (cannon setup) setup))

tests :: IO TestSetup -> TestTree
tests s = testGroup "Per-conversation message timer"
    [ test s "timer can be set at creation time" messageTimerInit
    , test s "timer can be changed" messageTimerChange
    , test s "timer can't be set by guests" messageTimerChangeGuest
    , test s "timer can't be set in 1:1 conversations" messageTimerChange1to1
    , test s "setting the timer generates an event" messageTimerEvent
    ]

messageTimerInit :: Galley -> Brig -> Cannon -> TestSetup -> Http ()
messageTimerInit g b ca _ = do
    pure ()
    -- create a conversation
    -- check that the timer is whatever
    -- create a conversation without timer
    -- check that the timer is nothing

messageTimerChange :: Galley -> Brig -> Cannon -> TestSetup -> Http ()
messageTimerChange g b ca _ = do
    pure ()
    -- create a conversation without timer
    -- set timer to something
    -- set timer to null
    -- set timer to something else

messageTimerChangeGuest :: Galley -> Brig -> Cannon -> TestSetup -> Http ()
messageTimerChangeGuest g b ca _ = do
    pure ()
    -- create a conversation with a guest user
    -- try to change the timer (as the guest user) and observe failure

messageTimerChange1to1 :: Galley -> Brig -> Cannon -> TestSetup -> Http ()
messageTimerChange1to1 g b ca _ = do
    pure ()
    -- create a 1:1 conversation
    -- try to change the timer and observe failure

messageTimerEvent :: Galley -> Brig -> Cannon -> TestSetup -> Http ()
messageTimerEvent g b ca _ = do
    pure ()
    -- create a conversation
    -- change the timer
    -- check that the returned event is correct
    -- check that other participants have also got the event
