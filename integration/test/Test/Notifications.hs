module Test.Notifications where

import API.Gundeck
import SetupHelpers
import Testlib.Prelude

examplePush :: MakesValue u => u -> App Value
examplePush u = do
  userId <- u %. "id" & asString
  pure $
    object
      [ "recipients"
          .= [ object
                 [ "user_id" .= userId,
                   "route" .= "any",
                   "clients" .= ([] :: [String])
                 ]
             ],
        "payload" .= [object ["hello" .= "world"]]
      ]

testFetchAllNotifications :: App ()
testFetchAllNotifications = do
  user <- randomUserId OwnDomain
  push <- examplePush user

  let n = 10
  replicateM_ n $
    bindResponse (postPushV2 user [push]) $ \res ->
      res.status `shouldMatchInt` 200

  let client = "deadbeeef"
  ns <- getNotifications user client def >>= getJSON 200

  expected <- replicateM n (push %. "payload")
  allNotifs <- ns %. "notifications" & asList
  actual <- traverse (%. "payload") allNotifs
  actual `shouldMatch` expected

  firstNotif <- getNotification user client (head allNotifs %. "id") >>= getJSON 200
  firstNotif `shouldMatch` head allNotifs

  lastNotif <- getLastNotification user client >>= getJSON 200
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
    bindResponse (postPushV2 user [push c]) $ \res ->
      res.status `shouldMatchInt` 200

  lastNotif <- getLastNotification user "c" >>= getJSON 200
  lastNotif %. "payload" `shouldMatch` [object ["client" .= "c"]]
