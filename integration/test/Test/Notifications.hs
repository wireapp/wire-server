{-# OPTIONS -Wno-ambiguous-fields #-}
module Test.Notifications where

import API.Brig
import API.Common
import API.Gundeck
import API.GundeckInternal
import Data.Time (UTCTime)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Notifications
import SetupHelpers
import Testlib.Prelude

examplePush :: (MakesValue u) => u -> App Value
examplePush u = do
  r <- recipient u
  pure
    $ object
      [ "recipients" .= [r],
        "payload" .= [object ["hello" .= "world"]]
      ]

testFetchAllNotifications :: App ()
testFetchAllNotifications = do
  user <- randomUserId OwnDomain
  push <- examplePush user

  let n = 10
  replicateM_ n
    $ bindResponse (postPush user [push])
    $ \res ->
      res.status `shouldMatchInt` 200

  let c :: Maybe String = Just "deadbeef"
  ns <- getNotifications user (def {client = c} :: GetNotifications) >>= getJSON 200

  expected <- replicateM n (push %. "payload")
  allNotifs <- ns %. "notifications" & asList
  actual <- traverse (%. "payload") allNotifs
  actual `shouldMatch` expected

  firstNotif <-
    getNotification
      user
      (def {client = c} :: GetNotification)
      (head allNotifs %. "id")
      >>= getJSON 200
  firstNotif `shouldMatch` head allNotifs

  lastNotif <-
    getLastNotification
      user
      (def {client = c} :: GetNotification)
      >>= getJSON 200
  lastNotif `shouldMatch` last allNotifs

testLastNotification :: App ()
testLastNotification = do
  user <- randomUserId OwnDomain
  userId <- user %. "id" & asString
  let push c =
        object
          [ "recipients"
              .= [ object
                     [ "user_id" .= userId,
                       "route" .= "any",
                       "clients" .= [c]
                     ]
                 ],
            "payload" .= [object ["client" .= c]]
          ]

  for_ ["a", "b", "c", "d", "e", "f"] $ \c ->
    bindResponse (postPush user [push c]) $ \res ->
      res.status `shouldMatchInt` 200

  lastNotif <- getLastNotification user def {client = Just "c"} >>= getJSON 200
  lastNotif %. "payload" `shouldMatch` [object ["client" .= "c"]]

testInvalidNotification :: (HasCallStack) => App ()
testInvalidNotification = do
  user <- randomUserId OwnDomain

  -- test uuid v4 as "since"
  do
    notifId <- randomId
    void
      $ getNotifications user def {since = Just notifId}
      >>= getJSON 400

  -- test arbitrary uuid v1 as "since"
  do
    notifId <- randomUUIDv1
    void
      $ getNotifications user def {since = Just notifId}
      >>= getJSON 404

-- | Check that client-add notifications use the V6 format:
-- @
--   "capabilities": { "capabilities": [..] }
-- @
--
-- Migration plan: clients must be able to parse both old and new schema starting from V7.  Once V6 is deprecated, the backend can start sending notifications in the new form.
testAddClientNotification :: (HasCallStack) => App ()
testAddClientNotification = do
  alice <- randomUser OwnDomain def

  e <- withWebSocket alice $ \ws -> do
    void $ addClient alice def
    n <- awaitMatch isUserClientAddNotif ws
    nPayload n

  void $ e %. "client.capabilities.capabilities" & asList

testGetServerTime :: (HasCallStack) => App ()
testGetServerTime = do
  user <- randomUser OwnDomain def
  formattedTimestampStr <-
    getServerTime user `bindResponse` \r -> do
      r.status `shouldMatchInt` 200
      r.json %. "time" & asString
  let valid = isJust $ iso8601ParseM @Maybe @UTCTime formattedTimestampStr
  valid `shouldMatch` True
