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
