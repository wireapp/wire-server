-- Disabling to stop warnings on HasCallStack
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

module API
  ( tests,
  )
where

import Bilge hiding (head)
import Bilge.Assert
import Control.Arrow ((&&&))
import Control.Concurrent.Async (Async, async, concurrently_, forConcurrently_, wait)
import Control.Concurrent.Async qualified as Async
import Control.Lens (view, (%~), (.~), (?~), (^.), (^?), _2)
import Control.Retry (constantDelay, limitRetries, recoverAll, retrying)
import Data.Aeson hiding (json)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Lens
import Data.Aeson.Types qualified as Aeson
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as B16
import Data.ByteString.Char8 qualified as C
import Data.ByteString.Conversion
import Data.ByteString.Lazy (fromStrict)
import Data.ByteString.Lazy qualified as BL
import Data.Id
import Data.List.NonEmpty qualified as NonEmpty
import Data.List1 (List1)
import Data.List1 qualified as List1
import Data.Range
import Data.Set qualified as Set
import Data.Text.Encoding qualified as T
import Data.UUID qualified as UUID
import Data.UUID.V4
import Gundeck.Options hiding (bulkPush)
import Gundeck.Options qualified as O
import Gundeck.Types
import Gundeck.Types.Common qualified
import Imports
import Network.HTTP.Client qualified as Http
import Network.URI (parseURI)
import Network.WebSockets qualified as WS
import Safe
import System.Random (randomIO)
import System.Timeout (timeout)
import Test.Tasty
import Test.Tasty.HUnit
import TestSetup
import Util (runRedisProxy, withSettingsOverrides)
import Wire.API.Internal.Notification
import Prelude qualified

tests :: IO TestSetup -> TestTree
tests s =
  testGroup
    "API tests"
    [ testGroup
        "Push"
        [ test s "Replace presence" replacePresence,
          test s "Remove stale presence" removeStalePresence,
          test s "Single user push" singleUserPush,
          test s "Single user push with large message" singleUserPushLargeMessage,
          test s "Push many to Cannon via bulkpush (via gundeck; group notif)" $ bulkPush False 50 8,
          test s "Push many to Cannon via bulkpush (via gundeck; e2e notif)" $ bulkPush True 50 8,
          test s "Send a push, ensure origin does not receive it" sendSingleUserNoPiggyback,
          test s "Targeted push by connection" targetConnectionPush,
          test s "Targeted push by client" targetClientPush,
          test s "Store notifications even when redis is down" storeNotificationsEvenWhenRedisIsDown
        ],
      testGroup
        "Notifications"
        [ test s "No notifications" testNoNotifs,
          test s "Fetch new notifications" testFetchNewNotifs,
          test s "No new notifications" testNoNewNotifs,
          test s "Missing notifications (until API Version 3)" testMissingNotifsV2,
          test s "Missing notifications (from API Version 3)" testMissingNotifsV3,
          test s "Fetch last notification" testFetchLastNotif,
          test s "No last notification" testNoLastNotif,
          test s "Bad 'since' parameter (until API Version 3)" testFetchNotifBadSinceV2,
          test s "Bad 'since' parameter (from API Version 3)" testFetchNotifBadSinceV3,
          test s "Fetch notification by ID" testFetchNotifById,
          test s "Filter notifications by client" testFilterNotifByClient,
          test s "Paging" testNotificationPaging
        ],
      testGroup
        "Clients"
        [ test s "unregister a client" testUnregisterClient
        ],
      testGroup
        "Tokens"
        [ test s "register a push token" testRegisterPushToken,
          test s "unregister a push token" testUnregisterPushToken
        ],
      testGroup
        "Websocket pingpong"
        [ test s "data-level pings produce pongs" testDataPingPong,
          test s "control pings with payload produce pongs with the same payload" testControlPingPongWithData,
          test s "data non-pings are ignored" testNoPingNoPong
        ],
      testGroup
        "Redis migration"
        [ test s "redis migration should work" testRedisMigration
        ],
      -- TODO: The following tests require (at the moment), the usage real AWS
      --       services so they are kept in a separate group to simplify testing
      testGroup
        "RealAWS"
        [ test s "Send a push to online and offline users" sendMultipleUsers,
          test s "register too many push tokens" testRegisterTooManyTokens,
          test s "share push token" testSharePushToken,
          test s "replace shared push token" testReplaceSharedPushToken,
          test s "fail on long push token" testLongPushToken
        ]
    ]

-----------------------------------------------------------------------------
-- Push

replacePresence :: TestM ()
replacePresence = do
  gu <- view tsGundeck
  ca <- view tsCannon
  uid <- randomId
  con <- randomConnId
  let localhost8080 = URI . fromJust $ parseURI "http://localhost:8080"
  let localhost8081 = URI . fromJust $ parseURI "http://localhost:8081"
  let pres1 = Presence uid (ConnId "dummy_dev") localhost8080 Nothing 0 ""
  let pres2 = Presence uid (ConnId "dummy_dev") localhost8081 Nothing 0 ""
  void $ connectUser ca uid con
  setPresence gu pres1 !!! const 201 === statusCode
  sendPush (push uid [uid])
  getPresence gu (showUser uid) !!! do
    const 2 === length . decodePresence
    assertTrue "Cannon is not removed" $
      elem localhost8080 . map resource . decodePresence
  setPresence gu pres2
    !!! const 201
      === statusCode
  getPresence gu (showUser uid) !!! do
    const 2 === length . decodePresence
    assertTrue "New Cannon" $
      elem localhost8081 . map resource . decodePresence
    assertTrue "Old Cannon is removed" $
      notElem localhost8080 . map resource . decodePresence
  where
    pload = List1.singleton $ KeyMap.fromList ["foo" .= (42 :: Int)]
    push u us = newPush (Just u) (toRecipients us) pload & pushOriginConnection ?~ ConnId "dev"

removeStalePresence :: TestM ()
removeStalePresence = do
  ca <- view tsCannon
  uid <- randomId
  con <- randomConnId
  void $ connectUser ca uid con
  ensurePresent uid 1
  sendPush (push (Just uid) [uid])
  m <- liftIO newEmptyMVar
  w <- wsRun ca uid con (wsCloser m)
  wsAssertPresences uid 1
  liftIO $ void $ putMVar m () >> wait w
  -- The websocket might take a few time units to drop so better to try a few pushes
  recoverAll (constantDelay 1000000 <> limitRetries 10) $ \_ -> do
    sendPush (push (Just uid) [uid])
    ensurePresent uid 0
  where
    pload = List1.singleton $ KeyMap.fromList ["foo" .= (42 :: Int)]
    push u us = newPush u (toRecipients us) pload & pushOriginConnection ?~ ConnId "dev"

singleUserPush :: TestM ()
singleUserPush = testSingleUserPush smallMsgPayload
  where
    -- JSON: {"foo":42}
    smallMsgPayload = List1.singleton $ KeyMap.fromList ["foo" .= (42 :: Int)]

testSingleUserPush :: List1 Object -> TestM ()
testSingleUserPush msgPayload = do
  ca <- view tsCannon
  uid <- randomId
  ch <- connectUser ca uid =<< randomConnId
  sendPush (push uid [uid])
  liftIO $ do
    msg <- waitForMessage ch
    assertBool "No push message received" (isJust msg)
    assertEqual
      "Payload altered during transmission"
      (Just msgPayload)
      (ntfPayload <$> (decode . fromStrict . fromJust) msg)
  where
    push u us = newPush (Just u) (toRecipients us) msgPayload & pushOriginConnection ?~ ConnId "dev"

singleUserPushLargeMessage :: TestM ()
singleUserPushLargeMessage = testSingleUserPush largeMsgPayload
  where
    -- JSON: {"list":["1","2", ... ,"10000"]}
    largeMsgPayload = List1.singleton $ KeyMap.fromList ["list" .= [show i | i <- [1 .. 10000] :: [Int]]]

-- | Create a number of users with a number of connections each, and connect each user's connections
-- | Create a number of users with a number of connections each, and connect each user's connections
-- to one of two cannons at random.  Push either encrypted notifications (@isE2E == True@) or
-- notifications from server (@isE2E == False@) to all connections, and make sure they all arrive at
-- the destination devices.  This also works if you pass the same 'Cannon' twice, even if 'Cannon'
-- is a k8s load balancer that dispatches requests to different replicas.
bulkPush :: Bool -> Int -> Int -> TestM ()
bulkPush isE2E numUsers numConnsPerUser = do
  ca <- view tsCannon
  ca2 <- view tsCannon
  uids@(uid : _) :: [UserId] <- replicateM numUsers randomId
  connids@((_ : _) : _) :: [[ConnId]] <- replicateM numUsers $ replicateM numConnsPerUser randomConnId
  let ucs :: [(UserId, [ConnId])] = zip uids connids
      ucs' :: [(UserId, [(ConnId, Bool)])] = toggle (mconcat $ repeat [True, False]) ucs
  chs <- do
    let (ucs1, ucs2) = splitAt (fromIntegral (length ucs `div` 2)) ucs
        (ucs1', ucs2') = splitAt (fromIntegral (length ucs `div` 2)) ucs'
    chs1 <- injectucs ca ucs1' . fmap snd <$> connectUsersAndDevices ca ucs1
    chs2 <- injectucs ca2 ucs2' . fmap snd <$> connectUsersAndDevices ca2 ucs2
    pure $ chs1 ++ chs2
  let pushData = mconcat . replicate 3 $ (if isE2E then pushE2E else pushGroup) uid ucs'
  sendPushes pushData
  liftIO $ forConcurrently_ chs $ replicateM 3 . checkMsg
  where
    -- associate chans with userid, connid.
    injectucs ::
      CannonR ->
      [(UserId, [(ConnId, Bool)])] ->
      [[TChan ByteString]] ->
      [(CannonR, UserId, ((ConnId, Bool), TChan ByteString))]
    injectucs ca_ ucs chs = mconcat $ zipWith (\(uid, connids) chs_ -> (ca_,uid,) <$> zip connids chs_) ucs chs
    -- will a notification actually be sent?
    toggle :: [Bool] -> [(UserId, [ConnId])] -> [(UserId, [(ConnId, Bool)])]
    toggle = f1
      where
        f1 _ [] = []
        f1 shoulds ((uid, connids) : ucs') = (uid, zip connids shoulds) : f1 shoulds' ucs'
          where
            shoulds' = drop (length connids) shoulds
    ploadGroup :: List1 Aeson.Object
    ploadGroup = List1.singleton $ KeyMap.fromList ["foo" .= (42 :: Int)]
    pushGroup :: UserId -> [(UserId, [(ConnId, Bool)])] -> [Push]
    pushGroup u ucs = [newPush (Just u) (toRecipients $ fst <$> ucs) ploadGroup & pushConnections .~ Set.fromList conns]
      where
        conns =
          [ connid | (_, cns) <- ucs, (connid, shouldSend) <- cns, shouldSend
          ]
    ploadE2E :: ConnId -> List1 Aeson.Object
    ploadE2E connid = List1.singleton $ KeyMap.fromList ["connid" .= connid]
    pushE2E :: UserId -> [(UserId, [(ConnId, Bool)])] -> [Push]
    pushE2E u ucs =
      targets <&> \(uid, connid) ->
        newPush (Just u) (toRecipients [uid]) (ploadE2E connid)
          & pushConnections .~ Set.singleton connid
      where
        targets :: [(UserId, ConnId)]
        targets =
          [ (uid, connid) | (uid, cns) <- ucs, (connid, shouldSend) <- cns, shouldSend
          ]
    checkMsg :: (cannon, userId, ((ConnId, Bool), TChan ByteString)) -> IO ()
    checkMsg (_ca, _uid, ((connid, shouldReceive), ch)) = do
      let timeoutmusecs = 1000000 + 10000 * numUsers * numConnsPerUser -- 10ms of extra timeout for every conn.
      msg <- waitForMessage' timeoutmusecs ch
      if shouldReceive
        then do
          assertBool "No push message received" (isJust msg)
          assertEqual
            "Payload altered during transmission"
            (Just $ if isE2E then ploadE2E connid else ploadGroup)
            (ntfPayload <$> (decode . fromStrict . fromJust) msg)
        else do
          assertBool "Unexpected push message received" (isNothing msg)

sendSingleUserNoPiggyback :: TestM ()
sendSingleUserNoPiggyback = do
  ca <- view tsCannon
  uid <- randomId
  did <- randomConnId
  ch <- connectUser ca uid did
  sendPush (push (Just uid) [uid] did)
  liftIO $ do
    msg <- waitForMessage ch
    assertBool "Push message received" (isNothing msg)
  where
    pload = List1.singleton $ KeyMap.fromList ["foo" .= (42 :: Int)]
    push u us d = newPush u (toRecipients us) pload & pushOriginConnection ?~ d

sendMultipleUsers :: TestM ()
sendMultipleUsers = do
  ca <- view tsCannon
  uid1 <- randomId -- offline and no native push
  uid2 <- randomId -- online
  uid3 <- randomId -- offline and native push
  clt <- randomClientId
  tok <- randomToken clt gcmToken
  _ <- registerPushToken uid3 tok
  ws <- connectUser ca uid2 =<< randomConnId
  sendPush (push uid1 [uid1, uid2, uid3])
  -- 'uid2' should get the push over the websocket
  liftIO $ do
    msg <- waitForMessage ws
    assertBool "No push message received" (isJust msg)
    assertEqual
      "Payload altered during transmission"
      (Just pload)
      (ntfPayload <$> (decode . fromStrict . fromJust) msg)
  -- We should get a 'DeliveryFailure' and / or 'EndpointUpdated'
  -- via SQS and thus remove the token.
  liftIO $ putStrLn "Waiting for SQS feedback to remove the token (~60-90s) ..."
  void $ retryWhileN 90 (not . null) (listPushTokens uid3)
  -- 'uid1' and 'uid2' should each have 1 notification
  ntfs1 <- listNotifications uid1 Nothing
  ntfs2 <- listNotifications uid2 Nothing
  liftIO . forM_ [ntfs1, ntfs2] $ \ntfs -> do
    assertEqual "Not exactly 1 notification" 1 (length ntfs)
    let p = view queuedNotificationPayload (Prelude.head ntfs)
    assertEqual "Wrong events in notification" (List1.toNonEmpty pload) p
  -- 'uid3' should have two notifications, one for the message and one
  -- for the removed token.
  ntfs3 <- listNotifications uid3 Nothing
  liftIO $ do
    assertBool "Not at least 2 notifications" (length ntfs3 >= 2)
    let (n1, nx) = checkNotifications ntfs3
    -- The first notification must be the test payload
    let p1 = view queuedNotificationPayload n1
    assertEqual "Wrong events in 1st notification" (List1.toNonEmpty pload) p1
    -- Followed by at least one notification for the token removal
    forM_ nx $ \n ->
      let p2 = fromJSON (Object (NonEmpty.head (n ^. queuedNotificationPayload)))
       in assertEqual "Wrong events in notification" (Success (PushRemove tok)) p2
  where
    checkNotifications [] = error "No notifications received!"
    checkNotifications (x : xs) = (x, xs)
    pload = List1.singleton pevent
    pevent = KeyMap.fromList ["foo" .= (42 :: Int)]
    push u us = newPush (Just u) (toRecipients us) pload & pushOriginConnection ?~ ConnId "dev"

targetConnectionPush :: TestM ()
targetConnectionPush = do
  ca <- view tsCannon
  uid <- randomId
  conn1 <- randomConnId
  c1 <- connectUser ca uid conn1
  c2 <- connectUser ca uid =<< randomConnId
  sendPush (push uid conn1)
  liftIO $ do
    e1 <- waitForMessage c1
    e2 <- waitForMessage c2
    assertBool "No push message received" (isJust e1)
    assertBool "Unexpected push message received" (isNothing e2)
  where
    pload = List1.singleton $ KeyMap.fromList ["foo" .= (42 :: Int)]
    push u t = newPush (Just u) (toRecipients [u]) pload & pushConnections .~ Set.singleton t

targetClientPush :: TestM ()
targetClientPush = do
  ca <- view tsCannon
  uid <- randomId
  cid1 <- randomClientId
  cid2 <- randomClientId
  let ca1 = CannonR (runCannonR ca . queryItem "client" (toByteString' cid1))
  let ca2 = CannonR (runCannonR ca . queryItem "client" (toByteString' cid2))
  c1 <- connectUser ca1 uid =<< randomConnId
  c2 <- connectUser ca2 uid =<< randomConnId
  -- Push only to the first client
  sendPush (push uid cid1)
  liftIO $ do
    e1 <- waitForMessage c1
    e2 <- waitForMessage c2
    assertBool "No push message received" (isJust e1)
    assertBool "Unexpected push message received" (isNothing e2)
  -- Push only to the second client
  sendPush (push uid cid2)
  liftIO $ do
    e1 <- waitForMessage c1
    e2 <- waitForMessage c2
    assertBool "Unexpected push message received" (isNothing e1)
    assertBool "No push message received" (isJust e2)
  -- Check the notification stream
  ns1 <- listNotifications uid (Just cid1)
  ns2 <- listNotifications uid (Just cid2)
  liftIO . forM_ [(ns1, cid1), (ns2, cid2)] $ \(ns, c) -> do
    assertEqual "Not exactly 1 notification" 1 (length ns)
    let p = view queuedNotificationPayload (Prelude.head ns)
    assertEqual "Wrong events in notification" (List1.toNonEmpty (pload c)) p
  where
    pevent c = KeyMap.fromList ["foo" .= clientToText c]
    pload c = List1.singleton (pevent c)
    rcpt u c =
      recipient u RouteAny
        & recipientClients .~ RecipientClientsSome (List1.singleton c)
    push u c = newPush (Just u) (unsafeRange (Set.singleton (rcpt u c))) (pload c)

storeNotificationsEvenWhenRedisIsDown :: TestM ()
storeNotificationsEvenWhenRedisIsDown = do
  ally <- randomId
  origRedisEndpoint <- view $ tsOpts . redis
  let proxyPort = 10112
  redisProxyServer <- liftIO . async $ runRedisProxy (origRedisEndpoint ^. O.host) (origRedisEndpoint ^. O.port) proxyPort
  withSettingsOverrides (redis .~ RedisEndpoint "localhost" proxyPort (origRedisEndpoint ^. connectionMode)) $ do
    let pload = textPayload "hello"
        push = buildPush ally [(ally, RecipientClientsAll)] pload
    gu <- view tsGundeck
    liftIO $ Async.cancel redisProxyServer
    post (runGundeckR gu . path "i/push/v2" . json [push]) !!! const 200 === statusCode

  ns <- listNotifications ally Nothing
  liftIO $ assertEqual ("Expected 1 notification, got: " <> show ns) 1 (length ns)

-----------------------------------------------------------------------------
-- Notifications

testNoNotifs :: TestM ()
testNoNotifs = do
  ally <- randomId
  ns <- listNotifications ally Nothing
  liftIO $ assertEqual "Unexpected notifications" 0 (length ns)

testFetchNewNotifs :: TestM ()
testFetchNewNotifs = do
  gu <- view tsGundeck
  ally <- randomId
  let pload = textPayload "hello"
  replicateM_ 4 (sendPush (buildPush ally [(ally, RecipientClientsAll)] pload))
  ns <- map (view queuedNotificationId) <$> listNotifications ally Nothing
  get
    ( runGundeckR gu
        . zUser ally
        . paths ["v3", "notifications"]
        . query [("since", Just (toByteString' (ns !! 1)))]
    )
    !!! do
      const 200 === statusCode
      const (Just $ drop 2 ns) === parseNotificationIds

testNoNewNotifs :: TestM ()
testNoNewNotifs = do
  gu <- view tsGundeck
  ally <- randomId
  sendPush (buildPush ally [(ally, RecipientClientsAll)] (textPayload "hello"))
  (n : _) <- map (view queuedNotificationId) <$> listNotifications ally Nothing
  get
    ( runGundeckR gu
        . zUser ally
        . paths ["v3", "notifications"]
        . query [("since", Just (toByteString' n))]
    )
    !!! do
      const 200 === statusCode
      const (Just []) === parseNotificationIds

testMissingNotifsV2 :: TestM ()
testMissingNotifsV2 = do
  testMissingNotifs "v2" $ \ns -> do
    const 404 === statusCode
    const (Just ns) === parseNotifications

testMissingNotifsV3 :: TestM ()
testMissingNotifsV3 =
  testMissingNotifs "v3" $
    const $ do
      const 404 === statusCode
      const Nothing === parseNotifications

testMissingNotifs :: ByteString -> ([QueuedNotification] -> Assertions ()) -> TestM ()
testMissingNotifs version checks =
  do
    gu <- view tsGundeck
    other <- randomId
    sendPush (buildPush other [(other, RecipientClientsAll)] (textPayload "hello"))
    (old : _) <- map (view queuedNotificationId) <$> listNotifications other Nothing
    ally <- randomId
    sendPush (buildPush ally [(ally, RecipientClientsAll)] (textPayload "hello"))
    ns <- listNotifications ally Nothing
    get
      ( runGundeckR gu
          . zUser ally
          . paths [version, "notifications"]
          . query [("since", Just (toByteString' old))]
      )
      !!! checks ns

testFetchLastNotif :: TestM ()
testFetchLastNotif = do
  gu <- view tsGundeck
  ally <- randomId
  sendPush (buildPush ally [(ally, RecipientClientsAll)] (textPayload "first"))
  sendPush (buildPush ally [(ally, RecipientClientsAll)] (textPayload "last"))
  [_, n] <- listNotifications ally Nothing
  get (runGundeckR gu . zUser ally . paths ["notifications", "last"]) !!! do
    const 200 === statusCode
    const (Just n) === parseNotification

testNoLastNotif :: TestM ()
testNoLastNotif = do
  gu <- view tsGundeck
  ally <- randomId
  get (runGundeckR gu . zUser ally . paths ["notifications", "last"]) !!! do
    const 404 === statusCode
    const (Just "not-found") =~= responseBody

testFetchNotifBadSinceV3 :: TestM ()
testFetchNotifBadSinceV3 = do
  gu <- view tsGundeck
  ally <- randomId
  sendPush (buildPush ally [(ally, RecipientClientsAll)] (textPayload "first"))
  get
    ( runGundeckR gu
        . zUser ally
        . paths ["v3", "notifications"]
        . query [("since", Just "jumberjack")]
    )
    !!! do
      const 400 === statusCode

testFetchNotifBadSinceV2 :: TestM ()
testFetchNotifBadSinceV2 = do
  gu <- view tsGundeck
  ally <- randomId
  sendPush (buildPush ally [(ally, RecipientClientsAll)] (textPayload "first"))
  ns <- listNotifications ally Nothing
  get
    ( runGundeckR gu
        . zUser ally
        . path "notifications"
        . query [("since", Just "jumberjack")]
    )
    !!! do
      const 404 === statusCode
      const (Just ns) === parseNotifications

testFetchNotifById :: TestM ()
testFetchNotifById = do
  gu <- view tsGundeck
  ally <- randomId
  c1 <- randomClientId
  c2 <- randomClientId
  sendPush
    ( buildPush
        ally
        [(ally, RecipientClientsSome (List1.singleton c1))]
        (textPayload "first")
    )
  sendPush
    ( buildPush
        ally
        [(ally, RecipientClientsSome (List1.singleton c2))]
        (textPayload "second")
    )
  [n1, n2] <- listNotifications ally Nothing
  forM_ [(n1, c1), (n2, c2)] $ \(n, c) ->
    let nid = toByteString' (view queuedNotificationId n)
        cid = toByteString' c
     in get
          ( runGundeckR gu
              . zUser ally
              . paths ["notifications", nid]
              . queryItem "client" cid
          )
          !!! do
            const 200 === statusCode
            const (Just n) === parseNotification

testFilterNotifByClient :: TestM ()
testFilterNotifByClient = do
  alice <- randomId
  clt1 <- randomClientId
  clt2 <- randomClientId
  clt3 <- randomClientId
  -- Add a notification for client 1
  sendPush
    ( buildPush
        alice
        [(alice, RecipientClientsSome (List1.singleton clt1))]
        (textPayload "first")
    )
  [n] <- listNotifications alice (Just clt1)
  -- get all for the first client
  getNotifications alice (Just clt1) !!! do
    const 200 === statusCode
    const (Just [n]) === parseNotifications
  -- get all for the second client
  getNotifications alice (Just clt2) !!! do
    const 200 === statusCode
    const (Just []) === parseNotifications
  -- get all for all clients
  getNotifications alice Nothing !!! do
    const 200 === statusCode
    const (Just [n]) === parseNotifications
  -- Add another notification for client 3
  sendPush
    ( buildPush
        alice
        [(alice, RecipientClientsSome (List1.singleton clt3))]
        (textPayload "last")
    )
  [n'] <- listNotifications alice (Just clt3)
  -- get last for the first client
  getLastNotification alice (Just clt1) !!! do
    const 200 === statusCode
    const (Just n) === parseNotification
  -- get last for a second client
  getLastNotification alice (Just clt2) !!! do
    const 404 === statusCode
    const (Just "not-found") =~= responseBody
  -- get last for a third client
  getLastNotification alice (Just clt3) !!! do
    const 200 === statusCode
    const (Just n') === parseNotification
  -- get last for any client
  getLastNotification alice Nothing !!! do
    const 200 === statusCode
    const (Just n') === parseNotification
  -- Add a lot of notifications for client 3
  replicateM_ 101 $
    sendPush
      ( buildPush
          alice
          [(alice, RecipientClientsSome (List1.singleton clt3))]
          (textPayload "final")
      )
  ns <- listNotifications alice (Just clt3)
  liftIO $ assertBool "notification count" (length ns == 102)
  -- last for the first client still unchanged
  getLastNotification alice (Just clt1) !!! do
    const 200 === statusCode
    const (Just n) === parseNotification
  -- still no notification for the second client
  getLastNotification alice (Just clt2) !!! do
    const 404 === statusCode
    const (Just "not-found") =~= responseBody
  -- last for the third client updated
  getLastNotification alice (Just clt3) !!! do
    const 200 === statusCode
    const (Just (last ns)) === parseNotification

testNotificationPaging :: TestM ()
testNotificationPaging = do
  -- Without client ID
  u1 <- randomId
  replicateM_ 399 (insert u1 RecipientClientsAll)
  paging u1 Nothing 399 399 [399, 0]
  paging u1 Nothing 399 100 [100, 100, 100, 99, 0]
  paging u1 Nothing 399 101 [101, 101, 101, 96, 0]
  -- With client ID
  u2 <- randomId
  clients@[c1, c2, c3] <- replicateM 3 randomClientId
  let numClients = length clients
  forM_ [0 .. 999] $ \i -> do
    let c = clients !! (i `mod` numClients)
    insert u2 (RecipientClientsSome (List1.singleton c))
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
  replicateM_ 90 $ insert u3 (RecipientClientsSome (List1.singleton c1))
  replicateM_ 20 $ insert u3 (RecipientClientsSome (List1.singleton c2))
  replicateM_ 20 $ insert u3 (RecipientClientsSome (List1.singleton c1))
  paging u3 (Just c1) 110 100 [100, 10, 0]
  paging u3 (Just c1) 110 110 [110, 0]
  paging u3 (Just c2) 20 100 [20, 0]
  where
    insert u c = sendPush (buildPush u [(u, c)] (textPayload "data"))
    paging u c total step = foldM_ (next u c (total, step)) (0, Nothing)
    next ::
      UserId ->
      Maybe ClientId ->
      (Int, Int) ->
      (Int, Maybe NotificationId) ->
      Int ->
      TestM (Int, Maybe NotificationId)
    next u c (total, step) (count, start) pageSize = do
      gu <- view tsGundeck
      let range =
            maybe id (queryItem "client" . toByteString') c
              . maybe id (queryItem "since" . toByteString') start
              . queryItem "size" (toByteString' step)
      r <- get (runGundeckR gu . paths ["v3", "notifications"] . zUser u . range) <!! const 200 === statusCode
      let rs = decode =<< responseBody r
      let (ns, more) = (fmap (view queuedNotifications) &&& fmap (view queuedHasMore)) rs
      let count' = count + step
      let start' = ns >>= fmap (view queuedNotificationId) . listToMaybe . reverse
      liftIO $ assertEqual "page size" (Just pageSize) (length <$> ns)
      liftIO $ assertEqual "has more" (Just (count' < total)) more
      pure (count', start')

-----------------------------------------------------------------------------
-- Client registration

testUnregisterClient :: TestM ()
testUnregisterClient = do
  g <- view tsGundeck
  uid <- randomId
  cid <- randomClientId
  unregisterClient g uid cid
    !!! const 200
      === statusCode

-----------------------------------------------------------------------------
-- Native push token registration
-- This expects the following SNS Platform applications to be present in AWS:

-- ${env}-test (FCM), ${env}-test (APNS_SANDBOX), ${env}-com.wire.ent (APNS_SANDBOX),
-- with ${env} normally being integration.

testRegisterPushToken :: TestM ()
testRegisterPushToken = do
  g <- view tsGundeck
  uid <- randomUser
  -- Client 1 with 4 distinct tokens
  c1 <- randomClientId
  t11 <- randomToken c1 apnsToken
  t11' <- randomToken c1 apnsToken -- overlaps
  t12 <- randomToken c1 apnsToken {tName = AppName "com.wire.ent"} -- different app
  t13 <- randomToken c1 gcmToken -- different transport

  -- Client 2 with 1 token
  c2 <- randomClientId
  t21 <- randomToken c2 apnsToken
  t22 <- randomToken c2 gcmToken -- different transport
  t22' <- randomToken c2 gcmToken -- overlaps

  -- Register non-overlapping tokens
  _ <- registerPushToken uid t11
  _ <- registerPushToken uid t12
  _ <- registerPushToken uid t13
  _ <- registerPushToken uid t21
  _ <- registerPushToken uid t22
  -- Check tokens
  _tokens <- sortPushTokens <$> listPushTokens uid
  let _expected = sortPushTokens [t11, t12, t13, t21, t22]
  liftIO $ assertEqual "unexpected tokens" _expected _tokens
  -- Register overlapping tokens. The previous overlapped
  -- tokens should be removed, but none of the others.
  _ <- registerPushToken uid t11'
  _ <- registerPushToken uid t22'
  -- Check tokens
  _tokens <- sortPushTokens <$> listPushTokens uid
  let _expected = sortPushTokens [t11', t12, t13, t21, t22']
  liftIO $ assertEqual "unexpected tokens" _expected _tokens
  -- Native push tokens are deleted together with the client
  unregisterClient g uid c1 !!! const 200 === statusCode
  unregisterClient g uid c1 !!! const 200 === statusCode -- (deleting a non-existing token is ok.)
  unregisterClient g uid c2 !!! const 200 === statusCode
  unregisterClient g uid c2 !!! const 200 === statusCode -- (deleting a non-existing token is ok.)
  _tokens <- listPushTokens uid
  liftIO $ assertEqual "unexpected tokens" [] _tokens

-- TODO: Try to make this test more performant, this test takes too long right now
testRegisterTooManyTokens :: TestM ()
testRegisterTooManyTokens = do
  -- create tokens for reuse with multiple users
  gcmTok <- Token . T.decodeUtf8 . toByteString' <$> randomId
  uids <- liftIO $ replicateM 55 randomId
  -- create 55 users with these tokens, which should succeed
  mapM_ (registerToken 201 gcmTok) uids
  -- should run out of space in endpoint metadata and fail with a 413 on number 56
  registerToken 413 gcmTok =<< randomId
  where
    registerToken status gcmTok uid = do
      con <- randomClientId
      let tkg = pushToken GCM "test" gcmTok con
      registerPushTokenRequest uid tkg !!! const status === statusCode

testUnregisterPushToken :: TestM ()
testUnregisterPushToken = do
  uid <- randomUser
  clt <- randomClientId
  tkn <- randomToken clt gcmToken
  void $ registerPushToken uid tkn
  void $ retryWhileN 12 null (listPushTokens uid)
  unregisterPushToken uid (tkn ^. token) !!! const 204 === statusCode
  void $ retryWhileN 12 (not . null) (listPushTokens uid)
  unregisterPushToken uid (tkn ^. token) !!! const 404 === statusCode

testDataPingPong :: TestM ()
testDataPingPong = do
  ca <- view tsCannon
  uid :: UserId <- randomId
  connid :: ConnId <- randomConnId
  [(_, [(chread, chwrite)] :: [(TChan ByteString, TChan ByteString)])] <-
    connectUsersAndDevicesWithSendingClients ca [(uid, [connid])]
  liftIO $ do
    atomically $ writeTChan chwrite "ping"
    msg <- waitForMessage chread
    assertBool "no pong" $ msg == Just "pong"

testControlPingPongWithData :: TestM ()
testControlPingPongWithData = do
  ca <- view tsCannon
  uid :: UserId <- randomId
  connid :: ConnId <- randomConnId
  [(_, [(chread, chPingWrite)] :: [(TChan WS.Message, TChan ByteString)])] <-
    connectUsersAndDevicesWithSendingClientsRaw ca [(uid, [connid])]
  liftIO $ do
    let pingPayload = "pi 3e4ac0590d55a24af7298b po"
    atomically $ writeTChan chPingWrite pingPayload
    _msg <- waitForMessageRaw chread -- this is a server-sent ping; we'll ignore this
    msg <- waitForMessageRaw chread
    let expected = Just (WS.ControlMessage $ WS.Pong $ fromStrict pingPayload)
    assertBool "no pong with the same payload" $ msg == expected

testNoPingNoPong :: TestM ()
testNoPingNoPong = do
  ca <- view tsCannon
  uid :: UserId <- randomId
  connid :: ConnId <- randomConnId
  [(_, [(chread, chwrite)] :: [(TChan ByteString, TChan ByteString)])] <-
    connectUsersAndDevicesWithSendingClients ca [(uid, [connid])]
  liftIO $ do
    atomically $ writeTChan chwrite "Wire is so much nicer with internet!"
    msg <- waitForMessage chread
    assertBool "unexpected response on non-ping" $ isNothing msg

testSharePushToken :: TestM ()
testSharePushToken = do
  gcmTok <- Token . T.decodeUtf8 . toByteString' <$> randomId
  apsTok <- Token . T.decodeUtf8 . B16.encode <$> randomBytes 32
  let tok1 = pushToken GCM "test" gcmTok
  let tok2 = pushToken APNSVoIP "com.wire.dev.ent" apsTok
  let tok3 = pushToken APNS "com.wire.int.ent" apsTok
  forM_ [tok1, tok2, tok3] $ \tk -> do
    u1 <- randomUser
    u2 <- randomUser
    c1 <- randomClientId
    c2 <- randomClientId
    let t1 = tk c1
    let t2 = tk c2
    t1' <- registerPushToken u1 t1
    t2' <- registerPushToken u2 t2 -- share the token with u1
    -- Unfortunately this fails locally :(
    -- "Duplicate endpoint token: 61d22005-af6e-4199-add9-899aae79c70a"
    -- Instead of getting something in the lines of
    -- "Invalid parameter: Token Reason: Endpoint <arn> " already exists with the same Token, but different attributes."
    liftIO $ assertEqual "token mismatch" (t1 ^. token) t1'
    liftIO $ assertEqual "token mismatch" (t2 ^. token) t2'
    liftIO $ assertEqual "token mismatch" t1' t2'
    ts1 <- retryWhile ((/= 1) . length) (listPushTokens u1)
    ts2 <- retryWhile ((/= 1) . length) (listPushTokens u2)
    liftIO $ assertEqual "token mismatch" [t1] ts1
    liftIO $ assertEqual "token mismatch" [t2] ts2
    unregisterPushToken u1 t1' !!! const 204 === statusCode
    unregisterPushToken u2 t2' !!! const 204 === statusCode

testReplaceSharedPushToken :: TestM ()
testReplaceSharedPushToken = do
  u1 <- randomUser
  u2 <- randomUser
  c1 <- randomClientId
  c2 <- randomClientId
  -- Set up a shared token
  t1 <- Token . T.decodeUtf8 . toByteString' <$> randomId
  let pt1 = pushToken GCM "test" t1 c1
  let pt2 = pt1 & tokenClient .~ c2 -- share the token
  _ <- registerPushToken u1 pt1
  _ <- registerPushToken u2 pt2
  -- Update the shared token
  t2 <- Token . T.decodeUtf8 . toByteString' <$> randomId
  let new = pushToken GCM "test" t2 c1
  _ <- registerPushToken u1 new
  -- Check both tokens
  ts1 <- map (view token) <$> listPushTokens u1
  ts2 <- map (view token) <$> listPushTokens u2
  liftIO $ do
    [t2] @=? ts1
    [t2] @=? ts2

testLongPushToken :: TestM ()
testLongPushToken = do
  uid <- randomUser
  clt <- randomClientId
  -- normal size APNS token should succeed
  tkn1 <- randomToken clt apnsToken
  registerPushTokenRequest uid tkn1 !!! const 201 === statusCode
  -- APNS token over 400 bytes should fail (actual token sizes are twice the tSize)
  tkn2 <- randomToken clt apnsToken {tSize = 256}
  registerPushTokenRequest uid tkn2 !!! const 413 === statusCode
  -- normal size GCM token should succeed
  tkn3 <- randomToken clt gcmToken
  registerPushTokenRequest uid tkn3 !!! const 201 === statusCode
  -- GCM token over 8192 bytes should fail (actual token sizes are twice the tSize)
  tkn4 <- randomToken clt gcmToken {tSize = 5000}
  registerPushTokenRequest uid tkn4 !!! const 413 === statusCode

-- * Redis Migration

testRedisMigration :: TestM ()
testRedisMigration = do
  uid <- randomUser
  con <- randomConnId
  cannonURI <- Gundeck.Types.Common.parse "http://cannon.example"
  let presence = Presence uid con cannonURI Nothing 1 ""
  redis2 <- view tsRedis2

  withSettingsOverrides (redisAdditionalWrite ?~ redis2) $ do
    g <- view tsGundeck
    setPresence g presence
      !!! const 201
        === statusCode
    retrievedPresence <-
      map resource . decodePresence <$> (getPresence g (toByteString' uid) <!! const 200 === statusCode)
    liftIO $ assertEqual "With both redises: presences should match the set presences" [cannonURI] retrievedPresence

  withSettingsOverrides (redis .~ redis2) $ do
    g <- view tsGundeck
    retrievedPresence <-
      map resource . decodePresence <$> (getPresence g (toByteString' uid) <!! const 200 === statusCode)
    liftIO $ assertEqual "With only second redis: presences should match the set presences" [cannonURI] retrievedPresence

-- * Helpers

ensurePresent :: HasCallStack => UserId -> Int -> TestM ()
ensurePresent u n = do
  gu <- view tsGundeck
  retryWhile ((n /=) . length . decodePresence) (getPresence gu (showUser u))
    !!! (const n === length . decodePresence)

connectUser :: HasCallStack => CannonR -> UserId -> ConnId -> TestM (TChan ByteString)
connectUser ca uid con = do
  [(_, [ch])] <- connectUsersAndDevices ca [(uid, [con])]
  pure ch

connectUsersAndDevices ::
  HasCallStack =>
  CannonR ->
  [(UserId, [ConnId])] ->
  TestM [(UserId, [TChan ByteString])]
connectUsersAndDevices ca uidsAndConnIds = do
  strip <$> connectUsersAndDevicesWithSendingClients ca uidsAndConnIds
  where
    strip = fmap (_2 %~ fmap fst)

connectUsersAndDevicesWithSendingClients ::
  HasCallStack =>
  CannonR ->
  [(UserId, [ConnId])] ->
  TestM [(UserId, [(TChan ByteString, TChan ByteString)])]
connectUsersAndDevicesWithSendingClients ca uidsAndConnIds = do
  forM uidsAndConnIds $ \(uid, conns) -> do
    chs <-
      (uid,) <$> do
        forM conns $ \conn -> do
          chread <- liftIO $ atomically newTChan
          chwrite <- liftIO $ atomically newTChan
          _ <- wsRun ca uid conn (wsReaderWriter chread chwrite)
          pure (chread, chwrite)
    assertPresences (uid, conns)
    pure chs

-- similar to the function above, but hooks
-- in a Ping Writer and gives access to 'WS.Message's
-- this can be used to test Ping/Pong behaviour on the control channel
connectUsersAndDevicesWithSendingClientsRaw ::
  HasCallStack =>
  CannonR ->
  [(UserId, [ConnId])] ->
  TestM [(UserId, [(TChan WS.Message, TChan ByteString)])]
connectUsersAndDevicesWithSendingClientsRaw ca uidsAndConnIds = do
  forM uidsAndConnIds $ \(uid, conns) -> do
    chs <-
      (uid,) <$> do
        forM conns $ \conn -> do
          chread <- liftIO $ atomically newTChan
          chwrite <- liftIO $ atomically newTChan
          _ <- wsRun ca uid conn (wsReaderWriterPing chread chwrite)
          pure (chread, chwrite)
    assertPresences (uid, conns)
    pure chs

assertPresences :: (UserId, [ConnId]) -> TestM ()
assertPresences (uid, conns) = wsAssertPresences uid (length conns)

-- | Sort 'PushToken's based on the actual 'token' values.
sortPushTokens :: [PushToken] -> [PushToken]
sortPushTokens = sortBy (compare `on` view token)

wsRun :: HasCallStack => CannonR -> UserId -> ConnId -> WS.ClientApp () -> TestM (Async ())
wsRun ca uid (ConnId con) app = do
  liftIO $ async $ WS.runClientWith caHost caPort caPath caOpts caHdrs app
  where
    runCan = runCannonR ca empty
    caHost = C.unpack $ Http.host runCan
    caPort = Http.port runCan
    caPath = "/await" ++ C.unpack (Http.queryString runCan)
    caOpts = WS.defaultConnectionOptions
    caHdrs = [("Z-User", showUser uid), ("Z-Connection", con)]

wsAssertPresences :: HasCallStack => UserId -> Int -> TestM ()
wsAssertPresences uid numPres = do
  gu <- view tsGundeck
  retryWhile ((numPres /=) . length . decodePresence) (getPresence gu $ showUser uid)
    !!! (const numPres === length . decodePresence)

wsCloser :: MVar () -> WS.ClientApp ()
wsCloser m conn = takeMVar m >> WS.sendClose conn C.empty >> putMVar m ()

wsReaderWriter :: TChan ByteString -> TChan ByteString -> WS.ClientApp ()
wsReaderWriter chread chwrite conn =
  concurrently_
    (forever $ WS.receiveData conn >>= atomically . writeTChan chread)
    (forever $ WS.sendTextData conn =<< atomically (readTChan chwrite))

wsReaderWriterPing :: TChan WS.Message -> TChan ByteString -> WS.ClientApp ()
wsReaderWriterPing chread chwrite conn =
  concurrently_
    (forever $ WS.receive conn >>= atomically . writeTChan chread)
    (forever $ WS.sendPing conn =<< atomically (readTChan chwrite))

retryWhile :: (MonadIO m) => (a -> Bool) -> m a -> m a
retryWhile = retryWhileN 10

retryWhileN :: (MonadIO m) => Int -> (a -> Bool) -> m a -> m a
retryWhileN n f m =
  retrying
    (constantDelay 1000000 <> limitRetries n)
    (const (pure . f))
    (const m)

waitForMessageRaw :: TChan WS.Message -> IO (Maybe WS.Message)
waitForMessageRaw = System.Timeout.timeout 3000000 . liftIO . atomically . readTChan

waitForMessage :: ToByteString a => TChan a -> IO (Maybe a)
waitForMessage = waitForMessage' 1000000

waitForMessage' :: ToByteString a => Int -> TChan a -> IO (Maybe a)
waitForMessage' musecs = System.Timeout.timeout musecs . liftIO . atomically . readTChan

unregisterClient :: GundeckR -> UserId -> ClientId -> TestM (Response (Maybe BL.ByteString))
unregisterClient g uid cid =
  delete $
    runGundeckR g
      . zUser uid
      . paths ["/i/clients", toByteString' cid]

registerPushToken :: UserId -> PushToken -> TestM Token
registerPushToken u t = do
  r <- registerPushTokenRequest u t
  pure $ Token (T.decodeUtf8 $ getHeader' "Location" r)

registerPushTokenRequest :: UserId -> PushToken -> TestM (Response (Maybe BL.ByteString))
registerPushTokenRequest u t = do
  g <- view tsGundeck
  let p = RequestBodyLBS (encode t)
  post
    ( runGundeckR g
        . path "/push/tokens"
        . contentJson
        . zUser u
        . zConn "random"
        . body p
    )

unregisterPushToken :: UserId -> Token -> TestM (Response (Maybe BL.ByteString))
unregisterPushToken u t = do
  g <- view tsGundeck
  let p = RequestBodyLBS (encode t)
  delete
    ( runGundeckR g
        . paths ["/push/tokens", toByteString' t]
        . contentJson
        . zUser u
        . zConn "random"
        . body p
    )

listPushTokens :: UserId -> TestM [PushToken]
listPushTokens u = do
  g <- view tsGundeck
  rs <-
    get
      ( runGundeckR g
          . path "/push/tokens"
          . zUser u
          . zConn "random"
      )
  maybe
    (error "Failed to decode push tokens")
    (pure . pushTokens)
    (responseBody rs >>= decode)

listNotifications :: HasCallStack => UserId -> Maybe ClientId -> TestM [QueuedNotification]
listNotifications u c = do
  rs <- getNotifications u c <!! const 200 === statusCode
  case responseBody rs >>= decode of
    Nothing -> error "Failed to decode notifications"
    Just ns ->
      maybe
        (error "No timestamp on notifications list") -- cf. #47
        (const $ pure (view queuedNotifications ns))
        (view queuedTime ns)

getNotifications :: UserId -> Maybe ClientId -> TestM (Response (Maybe BL.ByteString))
getNotifications u c =
  view tsGundeck >>= \gu ->
    get $
      runGundeckR gu
        . zUser u
        . paths ["v3", "notifications"]
        . maybe id (queryItem "client" . toByteString') c

getLastNotification :: UserId -> Maybe ClientId -> TestM (Response (Maybe BL.ByteString))
getLastNotification u c =
  view tsGundeck >>= \gu ->
    get $
      runGundeckR gu
        . zUser u
        . paths ["notifications", "last"]
        . maybe id (queryItem "client" . toByteString') c

sendPush :: HasCallStack => Push -> TestM ()
sendPush push = sendPushes [push]

sendPushes :: HasCallStack => [Push] -> TestM ()
sendPushes push = do
  gu <- view tsGundeck
  post (runGundeckR gu . path "i/push/v2" . json push) !!! const 200 === statusCode

buildPush ::
  HasCallStack =>
  UserId ->
  [(UserId, RecipientClients)] ->
  List1 Object ->
  Push
buildPush sdr rcps pload =
  let rcps' = Set.fromList (map (uncurry rcpt) rcps)
   in newPush (Just sdr) (unsafeRange rcps') pload
  where
    rcpt u c = recipient u RouteAny & recipientClients .~ c

appName :: AppName
appName = AppName "test"

data TokenSpec = TokenSpec {trans :: Transport, tSize :: Int, tName :: AppName}

gcmToken :: TokenSpec
gcmToken = TokenSpec GCM 16 appName

apnsToken :: TokenSpec
apnsToken = TokenSpec APNSSandbox 32 appName

randomToken :: MonadIO m => ClientId -> TokenSpec -> m PushToken
randomToken c ts = liftIO $ do
  tok <- (Token . T.decodeUtf8) Prelude.. B16.encode Prelude.<$> randomBytes (tSize ts)
  pure $ pushToken (trans ts) (tName ts) tok c

showUser :: UserId -> ByteString
showUser = C.pack . show

zUser :: UserId -> Request -> Request
zUser = header "Z-User" . toByteString'

zConn :: ByteString -> Request -> Request
zConn = header "Z-Connection"

getPresence :: GundeckR -> ByteString -> TestM (Response (Maybe BL.ByteString))
getPresence gu u = get (runGundeckR gu . path ("/i/presences/" <> u))

setPresence :: GundeckR -> Presence -> TestM (Response (Maybe BL.ByteString))
setPresence gu dat = post (runGundeckR gu . path "/i/presences" . json dat)

decodePresence :: Response (Maybe BL.ByteString) -> [Presence]
decodePresence rs = fromMaybe (error "Failed to decode presences") $ responseBody rs >>= decode

randomUser :: TestM UserId
randomUser = do
  br <- view tsBrig
  e <- liftIO $ mkEmail "success" "simulator.amazonses.com"
  let p =
        object
          [ "name" .= e,
            "email" .= e,
            "password" .= ("secret-8-chars-long-at-least" :: Text)
          ]
  r <- post (runBrigR br . path "/i/users" . json p)
  pure
    . readNote "unable to parse Location header"
    . C.unpack
    $ getHeader' "Location" r
  where
    mkEmail loc dom = do
      uid <- nextRandom
      pure $ loc <> "+" <> UUID.toText uid <> "@" <> dom

toRecipients :: [UserId] -> Range 1 1024 (Set Recipient)
toRecipients = unsafeRange . Set.fromList . map (`recipient` RouteAny)

randomConnId :: MonadIO m => m ConnId
randomConnId =
  liftIO $
    ConnId <$> do
      r <- randomIO :: IO Word32
      pure $ C.pack $ show r

randomClientId :: MonadIO m => m ClientId
randomClientId = liftIO $ ClientId <$> (randomIO :: IO Word64)

randomBytes :: MonadIO m => Int -> m ByteString
randomBytes n = liftIO $ BS.pack <$> replicateM n (randomIO :: IO Word8)

textPayload :: Text -> List1 Object
textPayload txt = List1.singleton (KeyMap.fromList ["text" .= txt])

parseNotification :: Response (Maybe BL.ByteString) -> Maybe QueuedNotification
parseNotification = responseBody >=> decode

parseNotifications :: Response (Maybe BL.ByteString) -> Maybe [QueuedNotification]
parseNotifications = responseBody >=> (^? key "notifications") >=> fromJSON'

parseNotificationIds :: Response (Maybe BL.ByteString) -> Maybe [NotificationId]
parseNotificationIds r = map (view queuedNotificationId) <$> parseNotifications r

fromJSON' :: FromJSON a => Value -> Maybe a
fromJSON' v = case fromJSON v of
  Success a -> Just a
  _ -> Nothing
