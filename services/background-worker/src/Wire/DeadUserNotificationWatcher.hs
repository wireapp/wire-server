{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}

module Wire.DeadUserNotificationWatcher where

import Cassandra
import Control.Monad.Trans.Maybe
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Conversion
import Data.Id
import Data.Map qualified as Map
import Imports
import Network.AMQP qualified as Q
import Network.AMQP.Extended
import Network.AMQP.Lifted qualified as QL
import Network.AMQP.Types
import System.Logger qualified as Log
import UnliftIO (async)
import Wire.API.Notification
import Wire.BackgroundWorker.Env
import Wire.BackgroundWorker.Util

getLastDeathQueue :: Maybe FieldTable -> Maybe ByteString
getLastDeathQueue (Just (FieldTable headers)) = do
  case Map.lookup "x-last-death-queue" headers of
    Just (FVString str) -> pure str
    _ -> Nothing
getLastDeathQueue Nothing = Nothing

-- FUTUREWORK: what happens if messages expire _after_ we checked against cassandra here?
-- Should we have an async notification terminate this?
startConsumer :: Q.Channel -> AppT IO Q.ConsumerTag
startConsumer chan = do
  env <- ask
  markAsWorking DeadUserNotificationWatcher

  cassandra <- asks (.cassandra)

  void . lift $ Q.declareQueue chan Q.newQueue {Q.queueName = userNotificationDlqName}
  QL.consumeMsgs chan userNotificationDlqName Q.Ack $ \(msg, envelope) ->
    if (msg.msgDeliveryMode == Just Q.NonPersistent)
      then do
        -- ignore transient messages, ack it so they don't clog the queue
        lift $ Q.ackEnv envelope
      else do
        -- forward non-transient messages to the respective client
        let dat = getLastDeathQueue msg.msgHeaders
        let vals = fmap (BS.split '.') dat
        case vals of
          Nothing -> logHeaderError env msg.msgHeaders
          Just ["user-notifications", uidBS, cidBS] -> do
            m <- runMaybeT $ do
              uid <- hoistMaybe $ fromByteString uidBS
              cid <- hoistMaybe $ fromByteString cidBS
              pure (uid, cid)
            (uid, cid) <- maybe (logParseError env dat) pure m
            markAsNeedsFullSync cassandra uid cid
            lift $ Q.ackEnv envelope
          _ -> void $ logParseError env dat
  where
    logHeaderError env headers = do
      Log.err
        env.logger
        ( Log.msg (Log.val "Could not find x-last-death-queue in headers")
            . Log.field "error_configuring_dead_letter_exchange" (show headers)
        )
      error "Could not find x-last-death-queue in headers"
    logParseError env dat = do
      Log.err env.logger $
        Log.msg (Log.val "Could not parse msgHeaders into uid/cid for dead letter exchange message")
          . Log.field "error_parsing_message" (show dat)
      error "Could not parse msgHeaders into uid/cid for dead letter exchange message"

markAsNeedsFullSync :: ClientState -> UserId -> ClientId -> AppT IO ()
markAsNeedsFullSync cassandra uid cid = do
  runClient cassandra do
    retry x1 $ write missedNotifications (params LocalQuorum (uid, cid))
  where
    missedNotifications :: PrepQuery W (UserId, ClientId) ()
    missedNotifications =
      [sql|
          INSERT INTO missed_notifications (user_id, client_id)
          VALUES (?, ?)
      |]

startWorker ::
  AmqpEndpoint ->
  AppT IO CleanupAction
startWorker amqp = do
  env <- ask
  cleanupRef <- newIORef Nothing
  markAsNotWorking DeadUserNotificationWatcher
  -- We can fire and forget this thread because it keeps respawning itself using the 'onConnectionClosedHandler'.
  void . async . openConnectionWithRetries env.logger amqp (Just "dead-user-notifications-watcher") $
    RabbitMqHooks
      { onNewChannel = \chan -> do
          consumerTag <- startConsumer chan
          atomicWriteIORef cleanupRef (Just (chan, consumerTag))
          -- Wait forever because when onNewChannel hooks finishes, it is
          -- assumed that the work for RabbitMQ is done and the connection with
          -- RabbitMQ is closed.
          forever $ threadDelay maxBound,
        onConnectionClose = do
          markAsNotWorking DeadUserNotificationWatcher
          atomicWriteIORef cleanupRef Nothing
          Log.err env.logger $
            Log.msg (Log.val "RabbitMQ Connection closed."),
        onChannelException = \e -> do
          markAsNotWorking DeadUserNotificationWatcher
          atomicWriteIORef cleanupRef Nothing
          unless (Q.isNormalChannelClose e) $
            Log.err env.logger $
              Log.msg (Log.val "Caught exception in RabbitMQ channel.")
                . Log.field "exception" (displayException e)
      }
  pure $ do
    readIORef cleanupRef >>= \case
      Just (chan, consumerTag) -> Q.cancelConsumer chan consumerTag
      Nothing -> pure ()
