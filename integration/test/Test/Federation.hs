{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Test.Federation where

import Control.Monad.Codensity
import Control.Monad.Reader
import SetupHelpers
import Testlib.Prelude
import Testlib.ResourcePool

testNotificationsForOfflineBackends :: HasCallStack => App ()
testNotificationsForOfflineBackends = do
  resourcePool <- asks (.resourcePool)
  [ownUser, otherUser] <- createAndConnectUsers [OwnDomain, OtherDomain]

  runCodensity (acquireResources 1 resourcePool) $ \[beResource] -> do
    _thirdUser <- runCodensity (startDynamicBackend beResource mempty mempty) $ \_ -> do
      thirdUser <- randomUser beResource.berDomain def
      connectUsers ownUser thirdUser
      pure thirdUser

    deleteUser ownUser

    withWebSocket otherUser $ \ws -> do
      otherUserNotif <- nPayload $ awaitMatch 1 undefined ws
      otherUserNotif %. "type" `shouldMatch` "user.delete"

    -- -- assertThat otherUser gets a notification
    -- bindResponse (getLastNotification otherUser "90abcdef") $ \otherUserNotif -> do
    --   otherUserNotif.status `shouldMatchInt` 200
    --   otherUserNotif.json %. "type" `shouldMatch` "user.delete"

    -- bindResponse (getLastNotification otherUser "90abcdef") $ \thirdUserNotif -> do
    --   thirdUserNotif.status `shouldMatchInt` 200
    --   thirdUserNotif.json %. "type" `shouldMatch` "user.delete"

    -- start the third backend and assert that thirduser gets a notification
    undefined

-- key <- bindResponse (uploadAsset user) $ \resp -> do
--   resp.status `shouldMatchInt` 201
--   resp.json %. "key"

-- bindResponse (downloadAsset user key id) $ \resp -> do
--   resp.status `shouldMatchInt` 200
--   assertBool
--     ("Expect 'Hello World!' as text asset content. Got: " ++ show resp.body)
--     (resp.body == fromString "Hello World!")
