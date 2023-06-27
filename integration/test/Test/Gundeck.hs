module Test.Gundeck where

import API.Gundeck
import SetupHelpers
import Testlib.Prelude

testFetchAllNotifs :: App ()
testFetchAllNotifs = do
  user <- randomUserId OwnDomain
  userId <- user %. "id" & asString

  let payload = [object ["hello" .= "world"]]
  let push =
        object
          [ "recipients"
              .= [ object
                     [ "user_id" .= userId,
                       "route" .= "any",
                       "clients" .= ([] :: [String])
                     ]
                 ],
            "payload" .= payload
          ]

  let n = 10
  replicateM_ n $
    bindResponse (postPushV2 user [push]) $ \res ->
      res.status `shouldMatchInt` 200

  let client = "deadbeeef"
  ns <- getNotifications user client def >>= getJSON 200

  let expected = replicate 10 payload
  actual <- ns %. "notifications" & asListOf (\v -> v %. "payload")
  actual `shouldMatch` expected

-- testFetchAllNotifs :: TestM ()
-- testFetchAllNotifs = do
--   ally <- randomId
--   let pload = textPayload "hello"
--   replicateM_ 10 (sendPush (buildPush ally [(ally, RecipientClientsAll)] pload))
--   ns <- listNotifications ally Nothing
--   liftIO $ assertEqual "Unexpected notification count" 10 (length ns)
--   liftIO $
--     assertEqual
--       "Unexpected notification payloads"
--       (replicate 10 (List1.toNonEmpty pload))
--       (map (view queuedNotificationPayload) ns)
