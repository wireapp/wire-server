{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TupleSections       #-}

module API.User.Handles (tests) where

import Imports
import API.User.Util
import Bilge hiding (accept, timeout)
import Bilge.Assert
import Brig.Types
import Control.Lens ((^?), (^?!))
import Data.Aeson
import Data.Aeson.Lens
import Data.ByteString.Conversion
import Data.Id hiding (client)
import Gundeck.Types.Notification
import Test.Tasty hiding (Timeout)
import Test.Tasty.Cannon hiding (Cannon)
import Test.Tasty.HUnit
import Util
import UnliftIO (mapConcurrently)

import qualified API.Search.Util             as Search
import qualified Brig.Options                as Opt
import qualified Data.List1                  as List1
import qualified Data.UUID                   as UUID
import qualified Network.Wai.Utilities.Error as Error
import qualified Test.Tasty.Cannon           as WS

tests :: ConnectionLimit -> Opt.Timeout -> Maybe Opt.Opts -> Manager -> Brig -> Cannon -> Galley -> TestTree
tests _cl _at _conf p b c _g = testGroup "handles"
    [ test p "handles/update" $ testHandleUpdate b c
    , test p "handles/race"   $ testHandleRace b
    , test p "handles/query"  $ testHandleQuery b
    ]

testHandleUpdate :: Brig -> Cannon -> Http ()
testHandleUpdate brig cannon = do
    uid <- userId <$> randomUser brig

    -- Invalid handles are rejected
    let badHandles = ["ca$h", "w", "Capital", "wire"]
    forM_ badHandles $ \h -> do
        let upd = RequestBodyLBS . encode $ HandleUpdate h
        put (brig . path "/self/handle" . contentJson . zUser uid . zConn "c" . body upd) !!! do
            const 400 === statusCode
            const (Just "invalid-handle") === fmap Error.label . decodeBody

    -- Claim a valid handle & receive notification
    hdl <- randomHandle
    let update = RequestBodyLBS . encode $ HandleUpdate hdl
    WS.bracketR cannon uid $ \ws -> do
        put (brig . path "/self/handle" . contentJson . zUser uid . zConn "c" . body update) !!!
            const 200 === statusCode
        void . liftIO $ WS.assertMatch (5 # Second) ws $ \n -> do
            let j = Object $ List1.head (ntfPayload n)
            j ^? key "type" . _String @?= Just "user.update"
            let u = j ^?! key "user"
            u ^? key "id" . _String     @?= Just (UUID.toText (toUUID uid))
            u ^? key "handle" . _String @?= Just hdl

    -- The owner of the handle can always retry the update
    put (brig . path "/self/handle" . contentJson . zUser uid . zConn "c" . body update) !!!
        const 200 === statusCode
    Bilge.head (brig . paths ["users", "handles", toByteString' hdl] . zUser uid) !!!
        const 200 === statusCode

    -- For other users, the handle is unavailable
    uid2 <- userId <$> randomUser brig
    put (brig . path "/self/handle" . contentJson . zUser uid2 . zConn "c" . body update) !!! do
        const 409 === statusCode
        const (Just "handle-exists") === fmap Error.label . decodeBody

    -- The owner appears by that handle in search
    Search.refreshIndex brig
    Search.assertCanFind brig uid2 uid hdl

    -- Change the handle again, thus freeing the old handle
    hdl2 <- randomHandle
    let update2 = RequestBodyLBS . encode $ HandleUpdate hdl2
    put (brig . path "/self/handle" . contentJson . zUser uid . zConn "c" . body update2) !!!
        const 200 === statusCode
    Bilge.head (brig . paths ["users", "handles", toByteString' hdl] . zUser uid) !!!
        const 404 === statusCode

    -- The owner appears by the new handle in search
    Search.refreshIndex brig
    Search.assertCan'tFind brig uid2 uid hdl
    Search.assertCanFind   brig uid2 uid hdl2

    -- Other users cannot immediately claim the old handle since the previous claim
    -- is still active.
    put (brig . path "/self/handle" . contentJson . zUser uid2 . zConn "c" . body update) !!! do
        const 409 === statusCode
        const (Just "handle-exists") === fmap Error.label . decodeBody

    -- The old handle can be claimed again immediately by the user who previously
    -- owned it (since the claim is either still active but his own, or expired).
    put (brig . path "/self/handle" . contentJson . zUser uid . zConn "c" . body update) !!!
        const 200 === statusCode

testHandleRace :: Brig -> Http ()
testHandleRace brig = do
    us <- replicateM 10 (userId <$> randomUser brig)
    -- 10 races. In each race, 10 users try to claim the same handle.
    -- At most one of them should get the handle in each race
    -- (usually no-one due to the contention).
    void $ replicateM 10 $ do
        hdl <- randomHandle
        let update = RequestBodyLBS . encode $ HandleUpdate hdl
        void $ flip mapConcurrently us $ \u ->
            put (brig . path "/self/handle" . contentJson . zUser u . zConn "c" . body update)
        ps <- forM us $ \u -> decodeBody <$> get (brig . path "/self" . zUser u)
        let owners = catMaybes $ filter (maybe False ((== Just (Handle hdl)) . userHandle)) ps
        liftIO $ assertBool "More than one owner of a handle" (length owners <= 1)

testHandleQuery :: Brig -> Http ()
testHandleQuery brig = do
    uid <- userId <$> randomUser brig
    hdl <- randomHandle

    -- Query for the handle availability (must be free)
    Bilge.head (brig . paths ["users", "handles", toByteString' hdl] . zUser uid) !!!
        const 404 === statusCode

    -- Set handle
    let update = RequestBodyLBS . encode $ HandleUpdate hdl
    put (brig . path "/self/handle" . contentJson . zUser uid . zConn "c" . body update) !!!
        const 200 === statusCode

    -- Query the updated profile
    get (brig . path "/self" . zUser uid) !!! do
        const 200 === statusCode
        const (Just (Handle hdl)) === (>>= userHandle) . decodeBody

    -- Query for the handle availability (must be taken)
    Bilge.head (brig . paths ["users", "handles", toByteString' hdl] . zUser uid) !!!
        const 200 === statusCode

    -- Query user profiles by handles
    get (brig . path "/users" . queryItem "handles" (toByteString' hdl) . zUser uid) !!! do
        const 200 === statusCode
        const (Just (Handle hdl)) === (>>= (listToMaybe >=> userHandle)) . decodeBody

    -- Bulk availability check
    hdl2 <- randomHandle
    hdl3 <- randomHandle
    checkHandles brig uid [hdl, hdl2, "InVa£iD", hdl3] 1 !!! do
        const 200 === statusCode
        const (Just [hdl2]) === decodeBody
    checkHandles brig uid [hdl2, hdl, hdl3] 3 !!! do
        const 200 === statusCode
        const (Just [hdl2, hdl3]) === decodeBody
