{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE ViewPatterns               #-}

{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module API (TestSetup(..), tests) where

import Bilge
import Bilge.Assert
import Control.Arrow ((&&&))
import Control.Concurrent.Async       (Async, async, wait, forConcurrently_)
import Control.Lens                   ((.~), (^.), (^?), view, (<&>))
import Control.Retry                  (retrying, constantDelay, limitRetries)
import Data.Aeson              hiding (json)
import Data.Aeson.Lens
import Data.ByteString.Conversion
import Data.ByteString.Lazy           (fromStrict)
import Data.Id
import Data.List1                     (List1)
import Data.Range
import Data.UUID.V4
import Gundeck.Types
import Imports
import Network.URI                    (parseURI)
import Safe
import System.Random                  (randomIO)
import System.Timeout                 (timeout)
import Test.Tasty
import Test.Tasty.HUnit
import Types

import qualified Cassandra              as Cql
import qualified Data.Aeson.Types       as Aeson
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8  as C
import qualified Data.ByteString.Lazy   as BL
import qualified Data.HashMap.Strict    as HashMap
import qualified Data.List1             as List1
import qualified Data.Set               as Set
import qualified Data.Text.Encoding     as T
import qualified Data.UUID              as UUID
import qualified Gundeck.Client.Data    as Clients
import qualified Gundeck.Push.Data      as Push
import qualified Network.HTTP.Client    as Http
import qualified Network.WebSockets     as WS
import qualified Prelude

appName :: AppName
appName = AppName "test"

data TestSetup = TestSetup
  { manager :: Manager
  , gundeck :: Gundeck
  , cannon  :: Cannon
  , cannon2 :: Cannon
  , brig    :: Brig
  , cass    :: Cql.ClientState
  }

type TestSignature a = Gundeck -> Cannon -> Brig -> Cql.ClientState -> Http a
type TestSignature2 a = Gundeck -> Cannon -> Cannon -> Brig -> Cql.ClientState -> Http a

test :: IO TestSetup -> TestName -> (TestSignature a) -> TestTree
test setup n h = testCase n runTest
  where
    runTest = do
        s <- setup
        void $ runHttpT (manager s) (h (gundeck s) (cannon s) (brig s) (cass s))

test2 :: IO TestSetup -> TestName -> (TestSignature2 a) -> TestTree
test2 setup n h = testCase n runTest
  where
    runTest = do
        s <- setup
        void $ runHttpT (manager s) (h (gundeck s) (cannon s) (cannon2 s) (brig s) (cass s))

tests :: IO TestSetup -> TestTree
tests s = testGroup "Gundeck integration tests" [
    testGroup "Push"
        [ test s "Register a user"       $ addUser
        , test s "Delete a user"         $ removeUser
        , test s "Replace presence"      $ replacePresence
        , test s "Remove stale presence" $ removeStalePresence
        , test s "Single user push"      $ singleUserPush
        , test2 s "Push many to Cannon via bulkpush (via gundeck; group notif)" $ bulkPush False 50 8
        , test2 s "Push many to Cannon via bulkpush (via gundeck; e2e notif)" $ bulkPush True 50 8
        , test s "Send a push, ensure origin does not receive it" $ sendSingleUserNoPiggyback
        , test s "Targeted push by connection" $ targetConnectionPush
        , test s "Targeted push by client" $ targetClientPush
        ],
    testGroup "Notifications"
        [ test s "No notifications" $ testNoNotifs
        , test s "Fetch all notifications" $ testFetchAllNotifs
        , test s "Fetch new notifications" $ testFetchNewNotifs
        , test s "No new notifications" $ testNoNewNotifs
        , test s "Missing notifications" $ testMissingNotifs
        , test s "Fetch last notification" $ testFetchLastNotif
        , test s "No last notification" $ testNoLastNotif
        , test s "Bad 'since' parameter" $ testFetchNotifBadSince
        , test s "Fetch notification by ID" $ testFetchNotifById
        , test s "Filter notifications by client" $ testFilterNotifByClient
        , test s "Paging" $ testNotificationPaging
        ],
    testGroup "Clients"
        [ test s "(un)register a client" $ testRegisterClient
        ],
    testGroup "Tokens"
        [ test s "register a push token"     $ testRegisterPushToken
        , test s "unregister a push token"   $ testUnregisterPushToken
        ],
    -- TODO: The following tests require (at the moment), the usage real AWS
    --       services so they are kept in a separate group to simplify testing
    testGroup "RealAWS"
        [ test s "Send a push to online and offline users" $ sendMultipleUsers
        , test s "register too many push tokens" $ testRegisterTooManyTokens
        , test s "share push token"          $ testSharePushToken
        , test s "replace shared push token" $ testReplaceSharedPushToken
        , test s "fail on long push token"   $ testLongPushToken
        ]
    ]

-----------------------------------------------------------------------------
-- Push

addUser :: TestSignature (UserId, ConnId)
addUser gu ca _ _ = registerUser gu ca

removeUser :: TestSignature ()
removeUser g c _ s = do
    user <- fst <$> registerUser g c
    clt  <- randomClient g user
    tok  <- randomToken clt gcmToken
    _    <- registerPushToken user tok g
    _    <- sendPush g (buildPush user [(user, [])] (textPayload "data"))
    deleteUser g user
    ntfs <- listNotifications user Nothing g
    liftIO $ do
        keys   <- Cql.runClient s (Clients.select user clt)
        tokens <- Cql.runClient s (Push.lookup user Push.Quorum)
        isNothing keys @?= True
        null tokens    @?= True
        ntfs           @?= []

replacePresence :: TestSignature ()
replacePresence gu ca _ _ = do
    uid <- randomId
    con <- randomConnId
    let localhost8080 = URI . fromJust $ parseURI "http://localhost:8080"
    let localhost8081 = URI . fromJust $ parseURI "http://localhost:8081"
    let pres1 = Presence uid (ConnId "dummy_dev") localhost8080 Nothing 0 ""
    let pres2 = Presence uid (ConnId "dummy_dev") localhost8081 Nothing 0 ""
    void $ connectUser gu ca uid con
    setPresence gu pres1 !!! const 201 === statusCode
    sendPush gu (push uid [uid])
    getPresence gu (showUser uid) !!! do
        const 2 === length . decodePresence
        assertTrue "Cannon is not removed" $
            elem localhost8080 . map resource . decodePresence
    setPresence gu pres2 !!!
        const 201 === statusCode
    getPresence gu (showUser uid) !!! do
        const 2 === length . decodePresence
        assertTrue "New Cannon" $
            elem localhost8081 . map resource . decodePresence
        assertTrue "Old Cannon is removed" $
            notElem localhost8080 . map resource . decodePresence
  where
    pload     = List1.singleton $ HashMap.fromList [ "foo" .= (42 :: Int) ]
    push u us = newPush u (toRecipients us) pload & pushOriginConnection .~ Just (ConnId "dev")

removeStalePresence :: TestSignature ()
removeStalePresence gu ca _ _ = do
    uid <- randomId
    con <- randomConnId
    void $ connectUser gu ca uid con
    ensurePresent gu uid 1
    sendPush gu (push uid [uid])
    m <- liftIO newEmptyMVar
    w <- wsRun ca uid con (wsCloser m)
    wsAssertPresences gu uid 1
    liftIO $ void $ putMVar m () >> wait w
    sendPush gu (push uid [uid])
    ensurePresent gu uid 0
  where
    pload     = List1.singleton $ HashMap.fromList [ "foo" .= (42 :: Int) ]
    push u us = newPush u (toRecipients us) pload & pushOriginConnection .~ Just (ConnId "dev")

singleUserPush :: TestSignature ()
singleUserPush gu ca _ _ = do
    uid <- randomId
    ch  <- connectUser gu ca uid =<< randomConnId
    sendPush gu (push uid [uid])
    liftIO $ do
        msg <- waitForMessage ch
        assertBool  "No push message received" (isJust msg)
        assertEqual "Payload altered during transmission"
            (Just pload)
            (ntfPayload <$> (decode . fromStrict . fromJust) msg)
  where
    pload     = List1.singleton $ HashMap.fromList [ "foo" .= (42 :: Int) ]
    push u us = newPush u (toRecipients us) pload & pushOriginConnection .~ Just (ConnId "dev")

-- | Create a number of users with a number of connections each, and connect each user's connections
-- to one of two cannons at random.  Push either encrypted notifications (@isE2E == True@) or
-- notifications from server (@isE2E == False@) to all connections, and make sure they all arrive at
-- the destination devices.  This also works if you pass the same 'Cannon' twice, even if 'Cannon'
-- is a k8s load balancer that dispatches requests to different replicas.
bulkPush :: Bool -> Int -> Int -> TestSignature2 ()
bulkPush isE2E numUsers numConnsPerUser gu ca ca2 _ _ = do
    uids@(uid:_)        :: [UserId]   <- replicateM numUsers randomId
    (connids@((_:_):_)) :: [[ConnId]] <- replicateM numUsers $ replicateM numConnsPerUser randomConnId
    let ucs  :: [(UserId, [ConnId])]         = zip uids connids
        ucs' :: [(UserId, [(ConnId, Bool)])] = toggle (mconcat $ repeat [True, False]) ucs

    chs <- do
      let (ucs1,  ucs2)  = splitAt (fromIntegral (length ucs `div` 2)) ucs
          (ucs1', ucs2') = splitAt (fromIntegral (length ucs `div` 2)) ucs'
      chs1 <- injectucs ca  ucs1' . fmap snd <$> connectUsersAndDevices gu ca  ucs1
      chs2 <- injectucs ca2 ucs2' . fmap snd <$> connectUsersAndDevices gu ca2 ucs2
      pure $ chs1 ++ chs2

    let pushData = mconcat . replicate 3 $ (if isE2E then pushE2E else pushGroup) uid ucs'
    sendPushes gu pushData
    liftIO $ forConcurrently_ chs $ replicateM 3 . checkMsg
  where
    -- associate chans with userid, connid.
    injectucs :: Cannon -> [(UserId, [(ConnId, Bool)])] -> [[TChan ByteString]]
              -> [(Cannon, UserId, ((ConnId, Bool), TChan ByteString))]
    injectucs ca_ ucs chs = mconcat $ zipWith (\(uid, connids) chs_ -> (ca_, uid,) <$> zip connids chs_) ucs chs

    -- will a notification actually be sent?
    toggle :: [Bool] -> [(UserId, [ConnId])] -> [(UserId, [(ConnId, Bool)])]
    toggle = f1
      where
        f1 _ [] = []
        f1 shoulds ((uid, connids) : ucs') = (uid, zip connids shoulds) : f1 shoulds' ucs'
          where shoulds' = drop (length connids) shoulds

    ploadGroup :: List1 Aeson.Object
    ploadGroup = List1.singleton $ HashMap.fromList [ ("foo" :: Text) .= (42 :: Int)]

    pushGroup :: UserId -> [(UserId, [(ConnId, Bool)])] -> [Push]
    pushGroup u ucs = [newPush u (toRecipients $ fst <$> ucs) ploadGroup & pushConnections .~ Set.fromList conns]
      where
        conns = [ connid | (_, cns) <- ucs
                         , (connid, shouldSend) <- cns
                         , shouldSend ]

    ploadE2E :: ConnId -> List1 Aeson.Object
    ploadE2E connid = List1.singleton $ HashMap.fromList [ "connid" .= connid ]

    pushE2E :: UserId -> [(UserId, [(ConnId, Bool)])] -> [Push]
    pushE2E u ucs = targets <&> \(uid, connid) -> newPush u (toRecipients [uid]) (ploadE2E connid)
                                                  & pushConnections .~ Set.singleton connid
      where
        targets :: [(UserId, ConnId)]
        targets = [ (uid, connid) | (uid, cns) <- ucs
                                  , (connid, shouldSend) <- cns
                                  , shouldSend
                                  ]

    checkMsg :: (cannon, userId, ((ConnId, Bool), TChan ByteString)) -> IO ()
    checkMsg (_ca, _uid, ((connid, shouldReceive), ch)) = do
        let timeoutmusecs = 1000000 + 10000 * numUsers * numConnsPerUser  -- 10ms of extra timeout for every conn.
        msg <- waitForMessage' timeoutmusecs ch
        if shouldReceive
            then do
                assertBool  "No push message received" (isJust msg)
                assertEqual "Payload altered during transmission"
                    (Just $ if isE2E then ploadE2E connid else ploadGroup)
                    (ntfPayload <$> (decode . fromStrict . fromJust) msg)
            else do
                assertBool  "Unexpected push message received" (isNothing msg)

sendSingleUserNoPiggyback :: TestSignature ()
sendSingleUserNoPiggyback gu ca _ _ = do
    uid <- randomId
    did <- randomConnId
    ch  <- connectUser gu ca uid did
    sendPush gu (push uid [uid] did)
    liftIO $ do
        msg <- waitForMessage ch
        assertBool "Push message received" (isNothing msg)
  where
    pload       = List1.singleton $ HashMap.fromList [ "foo" .= (42 :: Int) ]
    push u us d = newPush u (toRecipients us) pload & pushOriginConnection .~ Just d

sendMultipleUsers :: TestSignature ()
sendMultipleUsers gu ca _ _ = do
    uid1 <- randomId -- offline and no native push
    uid2 <- randomId -- online
    uid3 <- randomId -- offline and native push
    clt  <- randomClient gu uid3
    tok  <- randomToken clt gcmToken
    _    <- registerPushToken uid3 tok gu

    ws <- connectUser gu ca uid2 =<< randomConnId
    sendPush gu (push uid1 [uid1, uid2, uid3])
    -- 'uid2' should get the push over the websocket
    liftIO $ do
        msg <- waitForMessage ws
        assertBool  "No push message received" (isJust msg)
        assertEqual "Payload altered during transmission"
            (Just pload)
            (ntfPayload <$> (decode . fromStrict . fromJust) msg)

    -- We should get a 'DeliveryFailure' and / or 'EndpointUpdated'
    -- via SQS and thus remove the token.
    liftIO $ putStrLn "Waiting for SQS feedback to remove the token (~60-90s) ..."
    void $ retryWhileN 90 (not . null) (listPushTokens uid3 gu)

    -- 'uid1' and 'uid2' should each have 1 notification
    ntfs1 <- listNotifications uid1 Nothing gu
    ntfs2 <- listNotifications uid2 Nothing gu
    liftIO $ forM_ [ntfs1, ntfs2] $ \ntfs -> do
        assertEqual "Not exactly 1 notification" 1 (length ntfs)
        let p = view queuedNotificationPayload (Prelude.head ntfs)
        assertEqual "Wrong events in notification" pload p

    -- 'uid3' should have two notifications, one for the message and one
    -- for the removed token.
    ntfs3 <- listNotifications uid3 Nothing gu
    liftIO $ do
        assertBool "Not at least 2 notifications" (length ntfs3 >= 2)
        let (n1,nx) = checkNotifications ntfs3
        -- The first notification must be the test payload
        let p1 = view queuedNotificationPayload n1
        assertEqual "Wrong events in 1st notification" pload p1
        -- Followed by at least one notification for the token removal
        forM_ nx $ \n ->
            let p2 = fromJSON (Object (List1.head (n^.queuedNotificationPayload)))
            in assertEqual "Wrong events in notification" (Success (PushRemove tok)) p2
  where
    checkNotifications []     = error "No notifications received!"
    checkNotifications (x:xs) = (x,xs)

    pload     = List1.singleton pevent
    pevent    = HashMap.fromList [ "foo" .= (42 :: Int) ]
    push u us = newPush u (toRecipients us) pload & pushOriginConnection .~ Just (ConnId "dev")

targetConnectionPush :: TestSignature ()
targetConnectionPush gu ca _ _ = do
    uid   <- randomId
    conn1 <- randomConnId
    c1 <- connectUser gu ca uid conn1
    c2 <- connectUser gu ca uid =<< randomConnId
    sendPush gu (push uid conn1)
    liftIO $ do
        e1 <- waitForMessage c1
        e2 <- waitForMessage c2
        assertBool "No push message received" (isJust e1)
        assertBool "Unexpected push message received" (isNothing e2)
  where
    pload    = List1.singleton $ HashMap.fromList [ "foo" .= (42 :: Int) ]
    push u t = newPush u (toRecipients [u]) pload & pushConnections .~ Set.singleton t

targetClientPush :: TestSignature ()
targetClientPush gu ca _ _ = do
    uid <- randomId
    cid1 <- randomClientId
    cid2 <- randomClientId
    let ca1 = Cannon (runCannon ca . queryItem "client" (toByteString' cid1))
    let ca2 = Cannon (runCannon ca . queryItem "client" (toByteString' cid2))
    c1  <- connectUser gu ca1 uid =<< randomConnId
    c2  <- connectUser gu ca2 uid =<< randomConnId
    -- Push only to the first client
    sendPush gu (push uid cid1)
    liftIO $ do
        e1 <- waitForMessage c1
        e2 <- waitForMessage c2
        assertBool "No push message received" (isJust e1)
        assertBool "Unexpected push message received" (isNothing e2)
    -- Push only to the second client
    sendPush gu (push uid cid2)
    liftIO $ do
        e1 <- waitForMessage c1
        e2 <- waitForMessage c2
        assertBool "Unexpected push message received" (isNothing e1)
        assertBool "No push message received" (isJust e2)
    -- Check the notification stream
    ns1 <- listNotifications uid (Just cid1) gu
    ns2 <- listNotifications uid (Just cid2) gu
    liftIO $ forM_ [(ns1, cid1), (ns2, cid2)] $ \(ns, c) -> do
        assertEqual "Not exactly 1 notification" 1 (length ns)
        let p = view queuedNotificationPayload (Prelude.head ns)
        assertEqual "Wrong events in notification" (pload c) p
  where
    pevent c = HashMap.fromList [ "foo" .= client c ]
    pload  c = List1.singleton (pevent c)
    rcpt u c = recipient u RouteAny & recipientClients .~ [c]
    push u c = newPush u (unsafeRange (Set.singleton (rcpt u c))) (pload c)

-----------------------------------------------------------------------------
-- Notifications

testNoNotifs :: TestSignature ()
testNoNotifs gu _ _ _ = do
    ally <- randomId
    ns <- listNotifications ally Nothing gu
    liftIO $ assertEqual "Unexpected notifications" 0 (length ns)

testFetchAllNotifs :: TestSignature ()
testFetchAllNotifs gu _ _ _ = do
    ally <- randomId
    let pload = textPayload "hello"
    replicateM_ 10 (sendPush gu (buildPush ally [(ally, [])] pload))
    ns <- listNotifications ally Nothing gu
    liftIO $ assertEqual "Unexpected notification count" 10 (length ns)
    liftIO $ assertEqual "Unexpected notification payloads"
                (replicate 10 pload)
                (map (view queuedNotificationPayload) ns)

testFetchNewNotifs :: TestSignature ()
testFetchNewNotifs gu _ _ _ = do
    ally <- randomId
    let pload = textPayload "hello"
    replicateM_ 4 (sendPush gu (buildPush ally [(ally, [])] pload))
    ns <- map (view queuedNotificationId) <$> listNotifications ally Nothing gu
    get ( runGundeck gu
        . zUser ally
        . path "notifications"
        . query [("since", Just (toByteString' (ns !! 1)))]
        ) !!! do
        const 200 === statusCode
        const (Just $ drop 2 ns) === parseNotificationIds

testNoNewNotifs :: TestSignature ()
testNoNewNotifs gu _ _ _ = do
    ally <- randomId
    sendPush gu (buildPush ally [(ally, [])] (textPayload "hello"))
    (n:_) <- map (view queuedNotificationId) <$> listNotifications ally Nothing gu
    get ( runGundeck gu
        . zUser ally
        . path "notifications"
        . query [("since", Just (toByteString' n))]
        ) !!! do
            const 200 === statusCode
            const (Just []) === parseNotificationIds

testMissingNotifs :: TestSignature ()
testMissingNotifs gu _ _ _ = do
    other   <- randomId
    sendPush gu (buildPush other [(other, [])] (textPayload "hello"))
    (old:_) <- map (view queuedNotificationId) <$> listNotifications other Nothing gu
    ally    <- randomId
    sendPush gu (buildPush ally [(ally, [])] (textPayload "hello"))
    ns <- listNotifications ally Nothing gu
    get ( runGundeck gu
        . zUser ally
        . path "notifications"
        . query [("since", Just (toByteString' old))]) !!! do
        const 404 === statusCode
        const (Just ns) === parseNotifications

testFetchLastNotif :: TestSignature ()
testFetchLastNotif gu _ _ _ = do
    ally <- randomId
    sendPush gu (buildPush ally [(ally, [])] (textPayload "first"))
    sendPush gu (buildPush ally [(ally, [])] (textPayload "last"))
    [_, n] <- listNotifications ally Nothing gu
    get (runGundeck gu . zUser ally . paths ["notifications", "last"]) !!! do
        const 200 === statusCode
        const (Just n) === parseNotification

testNoLastNotif :: TestSignature ()
testNoLastNotif gu _ _ _ = do
    ally <- randomId
    get (runGundeck gu . zUser ally . paths ["notifications", "last"]) !!! do
        const 404 === statusCode
        const (Just "not-found") =~= responseBody

testFetchNotifBadSince :: TestSignature ()
testFetchNotifBadSince gu _ _ _ = do
    ally <- randomId
    sendPush gu (buildPush ally [(ally, [])] (textPayload "first"))
    ns <- listNotifications ally Nothing gu
    get ( runGundeck gu
        . zUser ally
        . path "notifications"
        . query [("since", Just "jumberjack")] ) !!! do
        const 404 === statusCode
        const (Just ns) === parseNotifications

testFetchNotifById :: TestSignature ()
testFetchNotifById gu _ _ _ = do
    ally <- randomId
    c1 <- randomClientId
    c2 <- randomClientId
    sendPush gu (buildPush ally [(ally, [c1])] (textPayload "first"))
    sendPush gu (buildPush ally [(ally, [c2])] (textPayload "second"))
    [n1, n2] <- listNotifications ally Nothing gu
    forM_ [(n1, c1), (n2, c2)] $ \(n, c) ->
        let nid = toByteString' (view queuedNotificationId n)
            cid = toByteString' c
        in get ( runGundeck gu
               . zUser ally
               . paths ["notifications", nid]
               . queryItem "client" cid
               ) !!! do
            const 200 === statusCode
            const (Just n) === parseNotification

testFilterNotifByClient :: TestSignature ()
testFilterNotifByClient gu _ _ _ = do
    alice <- randomId
    clt1  <- randomClientId
    clt2  <- randomClientId
    clt3  <- randomClientId

    -- Add a notification for client 1
    sendPush gu (buildPush alice [(alice, [clt1])] (textPayload "first"))
    [n] <- listNotifications alice (Just clt1) gu

    -- get all for the first client
    getNotifications gu alice (Just clt1) !!! do
        const 200        === statusCode
        const (Just [n]) === parseNotifications
    -- get all for the second client
    getNotifications gu alice (Just clt2) !!! do
        const 200       === statusCode
        const (Just []) === parseNotifications
    -- get all for all clients
    getNotifications gu alice Nothing !!! do
        const 200        === statusCode
        const (Just [n]) === parseNotifications

    -- Add another notification for client 3
    sendPush gu (buildPush alice [(alice, [clt3])] (textPayload "last"))
    [n'] <- listNotifications alice (Just clt3) gu

    -- get last for the first client
    getLastNotification gu alice (Just clt1) !!! do
        const 200      === statusCode
        const (Just n) === parseNotification
    -- get last for a second client
    getLastNotification gu alice (Just clt2) !!! do
        const 404                === statusCode
        const (Just "not-found") =~= responseBody
    -- get last for a third client
    getLastNotification gu alice (Just clt3) !!! do
        const 200       === statusCode
        const (Just n') === parseNotification
    -- get last for any client
    getLastNotification gu alice Nothing !!! do
        const 200       === statusCode
        const (Just n') === parseNotification

    -- Add a lot of notifications for client 3
    replicateM_ 101 $ sendPush gu (buildPush alice [(alice, [clt3])]
                                  (textPayload "final"))
    ns <- listNotifications alice (Just clt3) gu
    liftIO $ assertBool "notification count" (length ns == 102)

    -- last for the first client still unchanged
    getLastNotification gu alice (Just clt1) !!! do
        const 200      === statusCode
        const (Just n) === parseNotification

    -- still no notification for the second client
    getLastNotification gu alice (Just clt2) !!! do
        const 404                === statusCode
        const (Just "not-found") =~= responseBody

    -- last for the third client updated
    getLastNotification gu alice (Just clt3) !!! do
        const 200              === statusCode
        const (Just (last ns)) === parseNotification

testNotificationPaging :: TestSignature ()
testNotificationPaging gu _ _ _ = do
    -- Without client ID
    u1 <- randomId
    replicateM_ 399 (insert u1 Nothing)
    paging u1 Nothing 399 399 [399, 0]
    paging u1 Nothing 399 100 [100, 100, 100, 99, 0]
    paging u1 Nothing 399 101 [101, 101, 101, 96, 0]

    -- With client ID
    u2 <- randomId
    clients@[c1, c2, c3] <- replicateM 3 randomClientId
    let numClients = length clients
    forM_ [0..999] (insert u2 . Just . (clients !!) . (`mod` numClients))
    -- View of client 1
    paging u2 (Just c1) 334 100 [100, 100, 100, 34, 0]
    paging u2 (Just c1) 334 334 [334, 0]
    -- View of client 2
    paging u2 (Just c2) 333 100 [100, 100, 100, 33, 0]
    paging u2 (Just c2) 333 333 [333, 0]
    -- View of client 3
    paging u2 (Just c3) 333 100 [100, 100, 100, 33, 0]
    paging u2 (Just c3) 333 333 [333, 0]

    -- With overlapped pages and excess elements on the last page
    u3 <- randomId
    replicateM_ 90 (insert u3 (Just c1))
    replicateM_ 20 (insert u3 (Just c2))
    replicateM_ 20 (insert u3 (Just c1))
    paging u3 (Just c1) 110 100 [100, 10, 0]
    paging u3 (Just c1) 110 110 [110, 0]
    paging u3 (Just c2)  20 100 [20, 0]
  where
    insert u c = sendPush gu (buildPush u [(u, maybeToList c)] (textPayload "data"))

    paging u c total step = foldM_ (next u c (total, step)) (0, Nothing)

    next :: UserId
         -> Maybe ClientId
         -> (Int, Int)
         -> (Int, Maybe NotificationId)
         -> Int
         -> Http (Int, Maybe NotificationId)
    next u c (total, step) (count, start) pageSize = do
        let range = maybe id (queryItem "client" . toByteString') c
                  . maybe id (queryItem "since" . toByteString') start
                  . queryItem "size" (toByteString' step)
        r <- get (runGundeck gu . path "/notifications" . zUser u . range) <!! const 200 === statusCode
        let rs = decode =<< responseBody r
        let (ns, more) = (fmap (view queuedNotifications) &&& fmap (view queuedHasMore)) rs
        let count' = count + step
        let start' = ns >>= fmap (view queuedNotificationId) . listToMaybe . reverse
        liftIO $ assertEqual "page size" (Just pageSize) (length <$> ns)
        liftIO $ assertEqual "has more" (Just (count' < total)) more
        return (count', start')

-----------------------------------------------------------------------------
-- Client registration

testRegisterClient :: TestSignature ClientId
testRegisterClient g _ _ _ = do
    uid <- randomId
    cid <- randomClientId
    sig <- randomSignalingKeys
    registerClient g uid cid sig
        !!! const 200 === statusCode
    unregisterClient g uid cid
        !!! const 200 === statusCode
    return cid

-----------------------------------------------------------------------------
-- Native push token registration

testRegisterPushToken :: TestSignature ()
testRegisterPushToken g _ b _ = do
    uid <- randomUser b

    -- Client 1 with 4 distinct tokens
    c1   <- randomClient g uid
    t11  <- randomToken c1 apnsToken
    t11' <- randomToken c1 apnsToken -- overlaps
    t12  <- randomToken c1 apnsToken{tName = (AppName "com.wire.ent")} -- different app
    t13  <- randomToken c1 gcmToken  -- different transport

    -- Client 2 with 1 token
    c2   <- randomClient g uid
    t21  <- randomToken c2 apnsToken
    t22  <- randomToken c2 gcmToken -- different transport
    t22' <- randomToken c2 gcmToken -- overlaps

    -- Register non-overlapping tokens
    _ <- registerPushToken uid t11 g
    _ <- registerPushToken uid t12 g
    _ <- registerPushToken uid t13 g
    _ <- registerPushToken uid t21 g
    _ <- registerPushToken uid t22 g

    -- Check tokens
    _tokens <- sortPushTokens <$> listPushTokens uid g
    let _expected = sortPushTokens [t11, t12, t13, t21, t22]
    liftIO $ assertEqual "unexpected tokens" _expected _tokens

    -- Register overlapping tokens. The previous overlapped
    -- tokens should be removed, but none of the others.
    _ <- registerPushToken uid t11' g
    _ <- registerPushToken uid t22' g

    -- Check tokens
    _tokens <- sortPushTokens <$> listPushTokens uid g
    let _expected = sortPushTokens [t11', t12, t13, t21, t22']
    liftIO $ assertEqual "unexpected tokens" _expected _tokens

    -- Native push tokens are deleted together with the client
    unregisterClient g uid c1 !!! const 200 === statusCode
    unregisterClient g uid c1 !!! const 404 === statusCode
    unregisterClient g uid c2 !!! const 200 === statusCode
    unregisterClient g uid c2 !!! const 404 === statusCode
    _tokens <- listPushTokens uid g
    liftIO $ assertEqual "unexpected tokens" [] _tokens

-- TODO: Try to make this test more performant, this test takes too long right now
testRegisterTooManyTokens :: TestSignature ()
testRegisterTooManyTokens g _ _ _ = do
    -- create tokens for reuse with multiple users
    gcmTok <- Token . T.decodeUtf8 . toByteString' <$> randomId
    uids   <- liftIO $ replicateM 55 randomId
    -- create 55 users with these tokens, which should succeed
    mapM_ (registerToken 201 gcmTok) uids
    -- should run out of space in endpoint metadata and fail with a 413 on number 56
    registerToken 413 gcmTok =<< randomId
  where
    registerToken status gcmTok uid = do
        con <- randomClientId
        let tkg = pushToken GCM "test" gcmTok con
        registerPushTokenRequest uid tkg g !!! const status === statusCode

testUnregisterPushToken :: TestSignature ()
testUnregisterPushToken g _ b _ = do
    uid <- randomUser b
    clt <- randomClient g uid
    tkn <- randomToken clt gcmToken
    void $ registerPushToken uid tkn g
    void $ retryWhileN 12 null (listPushTokens uid g)
    unregisterPushToken uid (tkn^.token) g !!! const 204 === statusCode
    void $ retryWhileN 12 (not . null) (listPushTokens uid g)
    unregisterPushToken uid (tkn^.token) g !!! const 404 === statusCode

testSharePushToken :: TestSignature ()
testSharePushToken g _ b _ = do
    gcmTok <- Token . T.decodeUtf8 . toByteString' <$> randomId
    apsTok <- Token . T.decodeUtf8 . B16.encode <$> randomBytes 32
    let tok1 = pushToken GCM "test" gcmTok
    let tok2 = pushToken APNSVoIP "com.wire.dev.ent" apsTok
    let tok3 = pushToken APNS "com.wire.int.ent" apsTok
    forM_ [tok1, tok2, tok3] $ \tk -> do
        u1 <- randomUser b
        u2 <- randomUser b
        c1 <- randomClient g u1
        c2 <- randomClient g u2
        let t1 = tk c1
        let t2 = tk c2
        t1' <- registerPushToken u1 t1 g
        t2' <- registerPushToken u2 t2 g -- share the token with u1
        -- ^ Unfortunately this fails locally :(
        -- "Duplicate endpoint token: 61d22005-af6e-4199-add9-899aae79c70a"
        -- Instead of getting something in the lines of
        -- "Invalid parameter: Token Reason: Endpoint <arn> " already exists with the same Token, but different attributes."
        liftIO $ assertEqual "token mismatch" (t1^.token) t1'
        liftIO $ assertEqual "token mismatch" (t2^.token) t2'
        liftIO $ assertEqual "token mismatch" t1' t2'
        ts1 <- retryWhile ((/= 1) . length) (listPushTokens u1 g)
        ts2 <- retryWhile ((/= 1) . length) (listPushTokens u2 g)
        liftIO $ assertEqual "token mismatch" [t1] ts1
        liftIO $ assertEqual "token mismatch" [t2] ts2
        unregisterPushToken u1 t1' g !!! const 204 === statusCode
        unregisterPushToken u2 t2' g !!! const 204 === statusCode

testReplaceSharedPushToken :: TestSignature ()
testReplaceSharedPushToken g _ b _ = do
    u1 <- randomUser b
    u2 <- randomUser b
    c1 <- randomClient g u1
    c2 <- randomClient g u2

    -- Set up a shared token
    t1 <- Token . T.decodeUtf8 . toByteString' <$> randomId
    let pt1 = pushToken GCM "test" t1 c1
    let pt2 = pt1 & tokenClient .~ c2 -- share the token
    _ <- registerPushToken u1 pt1 g
    _ <- registerPushToken u2 pt2 g

    -- Update the shared token
    t2 <- Token . T.decodeUtf8 . toByteString' <$> randomId
    let new = pushToken GCM "test" t2 c1
    _ <- registerPushToken u1 new g

    -- Check both tokens
    ts1 <- map (view token) <$> listPushTokens u1 g
    ts2 <- map (view token) <$> listPushTokens u2 g
    liftIO $ do
        [t2] @=? ts1
        [t2] @=? ts2

testLongPushToken :: TestSignature ()
testLongPushToken g _ b _ = do
    uid <- randomUser b
    clt <- randomClient g uid

    -- normal size APNS token should succeed
    tkn1 <- randomToken clt apnsToken
    registerPushTokenRequest uid tkn1 g !!! const 201 === statusCode

    -- APNS token over 400 bytes should fail (actual token sizes are twice the tSize)
    tkn2 <- randomToken clt apnsToken{tSize=256}
    registerPushTokenRequest uid tkn2 g !!! const 413 === statusCode

    -- normal size GCM token should succeed
    tkn3 <- randomToken clt gcmToken
    registerPushTokenRequest uid tkn3 g !!! const 201 === statusCode

    -- GCM token over 8192 bytes should fail (actual token sizes are twice the tSize)
    tkn4 <- randomToken clt gcmToken{tSize=5000}
    registerPushTokenRequest uid tkn4 g !!! const 413 === statusCode

-- * Helpers

registerUser :: HasCallStack => Gundeck -> Cannon -> Http (UserId, ConnId)
registerUser gu ca = do
    uid <- randomId
    con <- randomConnId
    void $ connectUser gu ca uid con
    ensurePresent gu uid 1
    return (uid, con)

ensurePresent :: HasCallStack => Gundeck -> UserId -> Int -> Http ()
ensurePresent gu u n =
    retryWhile ((n /=) . length . decodePresence) (getPresence gu (showUser u)) !!!
        (const n === length . decodePresence)

connectUser :: HasCallStack => Gundeck -> Cannon -> UserId -> ConnId -> Http (TChan ByteString)
connectUser gu ca uid con = do
    [(_, [ch])] <- connectUsersAndDevices gu ca [(uid, [con])]
    return ch

connectUsersAndDevices :: HasCallStack => Gundeck -> Cannon -> [(UserId, [ConnId])] -> Http [(UserId, [TChan ByteString])]
connectUsersAndDevices gu ca uidsAndConnIds = do
    chs <- forM uidsAndConnIds $ \(uid, conns) -> (uid,) <$> do
        forM conns $ \conn -> do
            ch <- liftIO $ atomically newTChan
            _ <- wsRun ca uid conn (wsReader ch)
            pure ch
    (\(uid, conns) -> wsAssertPresences gu uid (length conns)) `mapM_` uidsAndConnIds
    pure chs

-- | Sort 'PushToken's based on the actual 'token' values.
sortPushTokens :: [PushToken] -> [PushToken]
sortPushTokens = sortBy (compare `on` view token)

wsRun :: HasCallStack => Cannon -> UserId -> ConnId -> WS.ClientApp () -> Http (Async ())
wsRun ca uid (ConnId con) app = do
    liftIO $ async $ WS.runClientWith caHost caPort caPath caOpts caHdrs app
  where
    runCan = runCannon ca empty
    caHost = C.unpack $ Http.host runCan
    caPort = Http.port runCan
    caPath = "/await" ++ C.unpack (Http.queryString runCan)
    caOpts = WS.defaultConnectionOptions
    caHdrs = [ ("Z-User", showUser uid), ("Z-Connection", con) ]

wsAssertPresences :: HasCallStack => Gundeck -> UserId -> Int -> Http ()
wsAssertPresences gu uid numPres = do
    retryWhile ((numPres /=) . length . decodePresence) (getPresence gu $ showUser uid) !!!
        (const numPres === length . decodePresence)

wsCloser :: MVar () -> WS.ClientApp ()
wsCloser m conn = takeMVar m >> WS.sendClose conn C.empty >> putMVar m ()

wsReader :: TChan ByteString -> WS.ClientApp ()
wsReader ch conn = forever $ WS.receiveData conn >>= atomically . writeTChan ch

retryWhile :: (MonadIO m) => (a -> Bool) -> m a -> m a
retryWhile = retryWhileN 10

retryWhileN :: (MonadIO m) => Int -> (a -> Bool) -> m a -> m a
retryWhileN n f m = retrying (constantDelay 1000000 <> limitRetries n)
                             (const (return . f))
                             (const m)

waitForMessage :: TChan ByteString -> IO (Maybe ByteString)
waitForMessage = waitForMessage' 1000000

waitForMessage' :: Int -> TChan ByteString -> IO (Maybe ByteString)
waitForMessage' musecs = System.Timeout.timeout musecs . liftIO . atomically . readTChan

registerClient :: Gundeck -> UserId -> ClientId -> SignalingKeys -> Http (Response (Maybe BL.ByteString))
registerClient g uid cid keys = put $ runGundeck g
    . zUser uid
    . paths ["/i/clients", toByteString' cid]
    . contentJson
    . body (RequestBodyLBS $ encode keys)

unregisterClient :: Gundeck -> UserId -> ClientId -> Http (Response (Maybe BL.ByteString))
unregisterClient g uid cid = delete $ runGundeck g
    . zUser uid
    . paths ["/i/clients", toByteString' cid]

registerPushToken :: UserId -> PushToken -> Gundeck -> Http Token
registerPushToken u t g = do
    r <- registerPushTokenRequest u t g
    return $ Token (T.decodeUtf8 $ getHeader' "Location" r)

registerPushTokenRequest :: UserId -> PushToken -> Gundeck -> Http (Response (Maybe BL.ByteString))
registerPushTokenRequest u t g = do
    let p = RequestBodyLBS (encode t)
    post ( runGundeck g
         . path "/push/tokens"
         . contentJson
         . zUser u
         . zConn "random"
         . body p
         )

unregisterPushToken :: UserId -> Token -> Gundeck -> Http (Response (Maybe BL.ByteString))
unregisterPushToken u t g = do
    let p = RequestBodyLBS (encode t)
    delete ( runGundeck g
           . paths ["/push/tokens", toByteString' t]
           . contentJson
           . zUser u
           . zConn "random"
           . body p
           )

listPushTokens :: UserId -> Gundeck -> Http [PushToken]
listPushTokens u g = do
    rs <- get ( runGundeck g
              . path "/push/tokens"
              . zUser u
              . zConn "random"
              )
    maybe (error "Failed to decode push tokens")
          (return . pushTokens)
          (responseBody rs >>= decode)

listNotifications :: HasCallStack => UserId -> Maybe ClientId -> Gundeck -> Http [QueuedNotification]
listNotifications u c g = do
    rs <- getNotifications g u c <!! const 200 === statusCode
    case responseBody rs >>= decode of
        Nothing -> error "Failed to decode notifications"
        Just ns -> maybe (error "No timestamp on notifications list") -- cf. #47
                         (const $ pure (view queuedNotifications ns))
                         (view queuedTime ns)

getNotifications :: Gundeck -> UserId -> Maybe ClientId -> Http (Response (Maybe BL.ByteString))
getNotifications gu u c = get $ runGundeck gu
    . zUser u
    . path "notifications"
    . maybe id (queryItem "client" . toByteString') c

getLastNotification :: Gundeck -> UserId -> Maybe ClientId -> Http (Response (Maybe BL.ByteString))
getLastNotification gu u c = get $ runGundeck gu
    . zUser u
    . paths ["notifications", "last"]
    . maybe id (queryItem "client" . toByteString') c

sendPush :: HasCallStack => Gundeck -> Push -> Http ()
sendPush gu push = sendPushes gu [push]

sendPushes :: HasCallStack => Gundeck -> [Push] -> Http ()
sendPushes gu push =
    post ( runGundeck gu . path "i/push/v2" . json push ) !!! const 200 === statusCode

buildPush :: HasCallStack => UserId -> [(UserId, [ClientId])] -> List1 Object -> Push
buildPush sdr rcps pload =
    let rcps' = Set.fromList (map (uncurry rcpt) rcps)
    in newPush sdr (unsafeRange rcps') pload
  where
    rcpt u c = recipient u RouteAny & recipientClients .~ c

data TokenSpec = TokenSpec {trans:: Transport, tSize :: Int, tName :: AppName}

gcmToken :: TokenSpec
gcmToken = TokenSpec GCM 16 appName

apnsToken :: TokenSpec
apnsToken = TokenSpec APNSSandbox 32 appName

randomToken :: MonadIO m => ClientId -> TokenSpec -> m PushToken
randomToken c ts = liftIO $ do
    tok <- Token . T.decodeUtf8 <$> B16.encode <$> randomBytes (tSize ts)
    return $ pushToken (trans ts) (tName ts) tok c

showUser :: UserId -> ByteString
showUser = C.pack . show

zUser :: UserId -> Request -> Request
zUser = header "Z-User" . toByteString'

zConn :: ByteString -> Request -> Request
zConn = header "Z-Connection"

getPresence :: Gundeck -> ByteString -> Http (Response (Maybe BL.ByteString))
getPresence gu u = get (runGundeck gu . path ("/i/presences/" <> u))

setPresence :: Gundeck -> Presence -> Http (Response (Maybe BL.ByteString))
setPresence gu dat = post (runGundeck gu . path "/i/presences" . json dat)

decodePresence :: Response (Maybe BL.ByteString) -> [Presence]
decodePresence rs = fromMaybe (error "Failed to decode presences") $ responseBody rs >>= decode

randomUser :: Brig -> Http UserId
randomUser br = do
    e <- liftIO $ mkEmail "success" "simulator.amazonses.com"
    let p = object
            [ "name"     .= e
            , "email"    .= e
            , "password" .= ("secret" :: Text)
            ]
    r <- post (runBrig br . path "/i/users" . json p)
    return . readNote "unable to parse Location header"
           . C.unpack
           $ getHeader' "Location" r
  where
    mkEmail loc dom = do
        uid <- nextRandom
        return $ loc <> "+" <> UUID.toText uid <> "@" <> dom

randomClient :: HasCallStack => Gundeck -> UserId -> Http ClientId
randomClient g u = do
    c <- randomClientId
    s <- randomSignalingKeys
    void $ registerClient g u c s !!! const 200 === statusCode
    return c

deleteUser :: HasCallStack => Gundeck -> UserId -> Http ()
deleteUser g uid = delete (runGundeck g . zUser uid . path "/i/user") !!! const 200 === statusCode

toRecipients :: [UserId] -> Range 1 1024 (Set Recipient)
toRecipients = unsafeRange . Set.fromList . map (`recipient` RouteAny)

randomConnId :: MonadIO m => m ConnId
randomConnId = liftIO $ ConnId <$> do
    r <- randomIO :: IO Word32
    return $ C.pack $ show r

randomClientId :: MonadIO m => m ClientId
randomClientId = liftIO $ newClientId <$> (randomIO :: IO Word64)

randomBytes :: MonadIO m => Int -> m ByteString
randomBytes n = liftIO $ BS.pack <$> replicateM n (randomIO :: IO Word8)

randomSignalingKeys :: MonadIO m => m SignalingKeys
randomSignalingKeys = liftIO $ do
    enk <- EncKey <$> liftIO (randomBytes 32)
    mak <- MacKey <$> liftIO (randomBytes 32)
    return (SignalingKeys enk mak)

textPayload :: Text -> List1 Object
textPayload txt = List1.singleton (HashMap.fromList ["text" .= txt])

parseNotification :: Response (Maybe BL.ByteString) -> Maybe QueuedNotification
parseNotification = responseBody >=> decode

parseNotifications :: Response (Maybe BL.ByteString) -> Maybe [QueuedNotification]
parseNotifications = responseBody >=> (^? key "notifications") >=> fromJSON'

parseNotificationIds :: Response (Maybe BL.ByteString) -> Maybe [NotificationId]
parseNotificationIds r = map (view queuedNotificationId) <$> parseNotifications r

fromJSON' :: FromJSON a => Value -> Maybe a
fromJSON' v = case fromJSON v of
    Success a -> Just a
    _         -> Nothing
