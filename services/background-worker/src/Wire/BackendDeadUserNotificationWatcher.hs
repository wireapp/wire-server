{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}

module Wire.BackendDeadUserNotificationWatcher where

import Cassandra
import Control.Concurrent (putMVar)
import Control.Monad.Codensity
import Control.Monad.Trans.Maybe
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Conversion
import Data.Id
import Data.Map qualified as Map
import Imports hiding (putMVar)
import Network.AMQP qualified as Q
import Network.AMQP.Extended
import Network.AMQP.Lifted qualified as QL
import Network.AMQP.Types
import System.Logger qualified as Log
import UnliftIO.Exception (bracket)
import Wire.API.Notification
import Wire.BackgroundWorker.Env

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
  markAsWorking BackendDeadUserNoticationWatcher
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
            markAsNeedsFullSync env.cassandra uid cid
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

-- * start worker

startWorker ::
  AmqpEndpoint ->
  AppT IO ()
startWorker amqp = do
  env <- ask
  reconnectSignal :: MVar (Maybe Q.Connection) <- do
    -- If we put anything here, openConnection (below) will recurse.  Nothing means
    -- "connection died, open a new one"; Just conn means "connection is still ok, but channel
    -- died".
    --
    -- The only reason for `reconnectSignal` is that we can avoid re-opening the connection
    -- when channel crashes.
    newEmptyMVar

  -- This function will open a connection to rabbitmq and start the consumer.  We use
  -- `reconnectSignal` to signal when the connection or channel are closed so we can re-open
  -- it.
  let openConnection connM = do
        -- keep track of whether the connection is being closed normally
        closingAbnormallyRef <- newIORef True

        let -- `onCloseConn` is called in two situations: (1) bracket finalizer is called
            -- after in-between action has either terminated normally or received an
            -- exception; (2) the connection is closed unexpectedly by the amqp library.
            onCloseConn = do
              closingAbnormally <- readIORef closingAbnormallyRef
              if closingAbnormally
                then do
                  Log.err env.logger $
                    Log.msg (Log.val "BackendDeadUserNoticationWatcher: Connection closed unexpectedly.")
                else do
                  Log.info env.logger $
                    Log.msg (Log.val "BackendDeadUserNoticationWatcher: Connection closed normally.")
              putMVar reconnectSignal Nothing

            onBlockedConn msg = do
              Log.err env.logger $
                Log.field "amqp message" msg
                  . Log.msg (Log.val "BackendDeadUserNoticationWatcher: Connection blocked -- waiting for unblock.")

            onUnblockedConn = do
              Log.err env.logger $
                Log.msg (Log.val "BackendDeadUserNoticationWatcher: Connection unblocked.")

            onChannelException conn e = do
              unless (Q.isNormalChannelClose e) $
                Log.err env.logger $
                  Log.msg (Log.val "BackendDeadUserNoticationWatcher: Caught exception in RabbitMQ channel.")
                    . Log.field "exception" (displayException e)
              runAppT env $ markAsNotWorking BackendDeadUserNoticationWatcher
              putMVar reconnectSignal (Just conn)

        mConn <- lowerCodensity $ do
          -- Open amqp connection
          conn <- case connM of
            Nothing -> do
              -- Open the rabbit mq connection
              conn <-
                Codensity $
                  withConnectionWithClose
                    (\_ -> writeIORef closingAbnormallyRef False)
                    env.logger
                    "BackendDeadUserNoticationWatcher"
                    amqp

              -- We need to recover from connection closed by restarting it
              liftIO $ Q.addConnectionClosedHandler conn True onCloseConn
              liftIO $ Q.addConnectionBlockedHandler conn onBlockedConn onUnblockedConn

              runAppT env $
                -- there is a connection now, but it's not consuming yet.
                markAsNotWorking BackendDeadUserNoticationWatcher
              pure conn
            Just conn -> pure conn

          -- After starting the connection, open the channel
          chan <- Codensity $ bracket (liftIO $ Q.openChannel conn) (liftIO . Q.closeChannel)
          liftIO $ Q.addChannelExceptionHandler chan (onChannelException conn)

          -- Set up the consumer
          void $ Codensity $ bracket (startConsumer chan) (liftIO . Q.cancelConsumer chan)
          lift $
            -- block and waits for one of the above puts.
            takeMVar reconnectSignal
        openConnection mConn

  (openConnection Nothing)
