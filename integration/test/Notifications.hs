module Notifications where

import API.Gundeck
import Control.Monad.Extra
import Testlib.Prelude
import UnliftIO.Concurrent

awaitNotifications ::
  (HasCallStack, MakesValue user, MakesValue client) =>
  user ->
  client ->
  Maybe String ->
  -- | Timeout in seconds
  Int ->
  -- | Max no. of notifications
  Int ->
  -- | Selection function. Should not throw any exceptions
  (Value -> App Bool) ->
  App [Value]
awaitNotifications user client since0 tSecs n selector =
  assertAwaitResult =<< go tSecs since0 (AwaitResult False n [] [])
  where
    go 0 _ res = pure res
    go timeRemaining since res0 = do
      notifs <- bindResponse (getNotifications user client (GetNotifications since Nothing)) $ \resp -> asList (resp.json %. "notifications")
      lastNotifId <- case notifs of
        [] -> pure since
        _ -> Just <$> objId (last notifs)
      (matching, notMatching) <- partitionM selector notifs
      let matchesSoFar = res0.matches <> matching
          res =
            res0
              { matches = matchesSoFar,
                nonMatches = res0.nonMatches <> notMatching,
                success = length matchesSoFar >= res0.nMatchesExpected
              }
      if res.success
        then pure res
        else do
          threadDelay (1_000_000)
          go (timeRemaining - 1) lastNotifId res

awaitNotification ::
  (HasCallStack, MakesValue user, MakesValue client, MakesValue lastNotifId) =>
  user ->
  client ->
  Maybe lastNotifId ->
  Int ->
  (Value -> App Bool) ->
  App Value
awaitNotification user client lastNotifId tSecs selector = do
  since0 <- mapM objId lastNotifId
  head <$> awaitNotifications user client since0 tSecs 1 selector

isDeleteUserNotif :: MakesValue a => a -> App Bool
isDeleteUserNotif n =
  nPayload n %. "type" `isEqual` "user.delete"

isNewMessageNotif :: MakesValue a => a -> App Bool
isNewMessageNotif n = fieldEquals n "payload.0.type" "conversation.otr-message-add"

isMemberJoinNotif :: MakesValue a => a -> App Bool
isMemberJoinNotif n = fieldEquals n "payload.0.type" "conversation.member-join"

isConvLeaveNotif :: MakesValue a => a -> App Bool
isConvLeaveNotif n = fieldEquals n "payload.0.type" "conversation.member-leave"

isNotifConv :: (MakesValue conv, MakesValue a) => conv -> a -> App Bool
isNotifConv conv n = fieldEquals n "payload.0.qualified_conversation" (objQidObject conv)

isNotifForUser :: (MakesValue user, MakesValue a) => user -> a -> App Bool
isNotifForUser user n = fieldEquals n "payload.0.data.qualified_user_ids.0" (objQidObject user)

isConvNameChangeNotif :: (HasCallStack, MakesValue a, MakesValue c) => c -> a -> App Bool
isConvNameChangeNotif name n = (&&) <$> fieldType <*> fieldName
  where
    fieldType = fieldEquals n "payload.0.type" "conversation.rename"
    fieldName = fieldEquals n "payload.0.data.name" (asString name)
