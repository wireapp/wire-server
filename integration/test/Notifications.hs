{-# OPTIONS -Wno-ambiguous-fields #-}
module Notifications where

import API.Gundeck
import Control.Error (lastMay)
import Control.Monad.Extra
import Control.Monad.Reader (asks)
import Testlib.Prelude
import UnliftIO (timeout)
import UnliftIO.Concurrent

-- | assert that no notifications with the predicate happen within the timeout
assertNoNotifications ::
  (HasCallStack, MakesValue user, MakesValue client) =>
  -- | the user
  user ->
  -- | the client of that user
  client ->
  -- | the last notif
  Maybe String ->
  -- | the predicate
  (Value -> App Bool) ->
  App ()
assertNoNotifications u uc since0 p = do
  ucid <- objId uc
  let go since = do
        notifs <-
          getNotifications u def {client = Just ucid, since = since}
            `bindResponse` asList
            . (%. "notifications")
            . (.json)
        partitionM p notifs >>= \case
          ([], nonMatching) ->
            threadDelay 1_000 *> case nonMatching of
              (lastMay -> Just lst) -> objId lst >>= go . Just
              _ -> go Nothing
          (matching, _) -> do
            pj <- prettyJSON matching
            assertFailure $
              unlines
                [ "Expected no matching events  but got:",
                  pj
                ]
  Nothing <- asks timeOutSeconds >>= flip timeout (go since0)
  pure ()

awaitNotifications ::
  (HasCallStack, MakesValue user, MakesValue client) =>
  user ->
  client ->
  Maybe String ->
  -- | Max no. of notifications
  Int ->
  -- | Selection function. Should not throw any exceptions
  (Value -> App Bool) ->
  App [Value]
awaitNotifications user client since0 n selector = do
  tSecs <- asks ((* 1000) . timeOutSeconds)
  assertAwaitResult =<< go tSecs since0 (AwaitResult False n [] [])
  where
    go timeRemaining since res0
      | timeRemaining <= 0 = pure res0
      | otherwise =
          do
            c <- make client & asString
            notifs <-
              getNotifications
                user
                def {since = since, client = Just c}
                `bindResponse` \resp -> asList (resp.json %. "notifications")
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
                threadDelay 1_000
                go (timeRemaining - 1) lastNotifId res

awaitNotification ::
  (HasCallStack, MakesValue user, MakesValue client, MakesValue lastNotifId) =>
  user ->
  client ->
  Maybe lastNotifId ->
  (Value -> App Bool) ->
  App Value
awaitNotification user client lastNotifId selector = do
  since0 <- mapM objId lastNotifId
  head <$> awaitNotifications user client since0 1 selector

isDeleteUserNotif :: MakesValue a => a -> App Bool
isDeleteUserNotif n =
  nPayload n %. "type" `isEqual` "user.delete"

isNewMessageNotif :: MakesValue a => a -> App Bool
isNewMessageNotif n = fieldEquals n "payload.0.type" "conversation.otr-message-add"

isNewMLSMessageNotif :: MakesValue a => a -> App Bool
isNewMLSMessageNotif n = fieldEquals n "payload.0.type" "conversation.mls-message-add"

isWelcomeNotif :: MakesValue a => a -> App Bool
isWelcomeNotif n = fieldEquals n "payload.0.type" "conversation.mls-welcome"

isMemberJoinNotif :: MakesValue a => a -> App Bool
isMemberJoinNotif n = fieldEquals n "payload.0.type" "conversation.member-join"

isConvLeaveNotif :: MakesValue a => a -> App Bool
isConvLeaveNotif n = fieldEquals n "payload.0.type" "conversation.member-leave"

isNotifConv :: (MakesValue conv, MakesValue a, HasCallStack) => conv -> a -> App Bool
isNotifConv conv n = fieldEquals n "payload.0.qualified_conversation" (objQidObject conv)

isNotifForUser :: (MakesValue user, MakesValue a, HasCallStack) => user -> a -> App Bool
isNotifForUser user n = fieldEquals n "payload.0.data.qualified_user_ids.0" (objQidObject user)

isNotifFromUser :: (MakesValue user, MakesValue a, HasCallStack) => user -> a -> App Bool
isNotifFromUser user n = fieldEquals n "payload.0.qualified_from" (objQidObject user)

isConvNameChangeNotif :: (HasCallStack, MakesValue a) => a -> App Bool
isConvNameChangeNotif n = fieldEquals n "payload.0.type" "conversation.rename"

isMemberUpdateNotif :: (HasCallStack, MakesValue n) => n -> App Bool
isMemberUpdateNotif n = fieldEquals n "payload.0.type" "conversation.member-update"

isReceiptModeUpdateNotif :: (HasCallStack, MakesValue n) => n -> App Bool
isReceiptModeUpdateNotif n =
  fieldEquals n "payload.0.type" "conversation.receipt-mode-update"

isConvMsgTimerUpdateNotif :: (HasCallStack, MakesValue n) => n -> App Bool
isConvMsgTimerUpdateNotif n =
  fieldEquals n "payload.0.type" "conversation.message-timer-update"

isConvAccessUpdateNotif :: (HasCallStack, MakesValue n) => n -> App Bool
isConvAccessUpdateNotif n =
  fieldEquals n "payload.0.type" "conversation.access-update"

isConvCreateNotif :: MakesValue a => a -> App Bool
isConvCreateNotif n = fieldEquals n "payload.0.type" "conversation.create"

isConvDeleteNotif :: MakesValue a => a -> App Bool
isConvDeleteNotif n = fieldEquals n "payload.0.type" "conversation.delete"

notifTypeIsEqual :: MakesValue a => String -> a -> App Bool
notifTypeIsEqual typ n = nPayload n %. "type" `isEqual` typ

isTeamMemberLeaveNotif :: MakesValue a => a -> App Bool
isTeamMemberLeaveNotif = notifTypeIsEqual "team.member-leave"

isUserActivateNotif :: MakesValue a => a -> App Bool
isUserActivateNotif = notifTypeIsEqual "user.activate"

isUserClientAddNotif :: MakesValue a => a -> App Bool
isUserClientAddNotif = notifTypeIsEqual "user.client-add"

isUserClientRemoveNotif :: MakesValue a => a -> App Bool
isUserClientRemoveNotif = notifTypeIsEqual "user.client-remove"

isUserLegalholdRequestNotif :: MakesValue a => a -> App Bool
isUserLegalholdRequestNotif = notifTypeIsEqual "user.legalhold-request"

isUserLegalholdEnabledNotif :: MakesValue a => a -> App Bool
isUserLegalholdEnabledNotif = notifTypeIsEqual "user.legalhold-enable"

isUserLegalholdDisabledNotif :: MakesValue a => a -> App Bool
isUserLegalholdDisabledNotif = notifTypeIsEqual "user.legalhold-disable"

isUserConnectionNotif :: MakesValue a => a -> App Bool
isUserConnectionNotif = notifTypeIsEqual "user.connection"

isConnectionNotif :: MakesValue a => String -> a -> App Bool
isConnectionNotif status n =
  (&&)
    <$> nPayload n %. "type" `isEqual` "user.connection"
    <*> nPayload n %. "connection.status" `isEqual` status

assertLeaveNotification ::
  ( HasCallStack,
    MakesValue fromUser,
    MakesValue conv,
    MakesValue user,
    MakesValue kickedUser
  ) =>
  fromUser ->
  conv ->
  user ->
  String ->
  kickedUser ->
  App ()
assertLeaveNotification fromUser conv user client leaver =
  void $
    awaitNotification
      user
      client
      noValue
      ( allPreds
          [ isConvLeaveNotif,
            isNotifConv conv,
            isNotifForUser leaver,
            isNotifFromUser fromUser
          ]
      )

assertConvUserDeletedNotif :: MakesValue leaverId => WebSocket -> leaverId -> App ()
assertConvUserDeletedNotif ws leaverId = do
  n <- awaitMatch isConvLeaveNotif ws
  nPayload n %. "data.qualified_user_ids.0" `shouldMatch` leaverId
  nPayload n %. "data.reason" `shouldMatch` "user-deleted"
