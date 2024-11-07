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
import UnliftIO hiding (bracket, putMVar)
import UnliftIO.Exception (bracket)
import Wire.API.Notification
import Wire.BackgroundWorker.Env

-- TODO: make this whole thing less awful
getLastDeathQueue :: Maybe FieldTable -> ByteString
getLastDeathQueue (Just (FieldTable headers)) = do
  case Map.lookup "x-last-death-queue" headers of
    Just (FVString str) -> str
    _ -> error "oh noes what now?"
getLastDeathQueue Nothing = error "oh noes!! sad"

-- TODO: what happens if messages expire _after_ we checked against cassandra here?
-- Should we have an async notification terminate this?
startConsumer :: Q.Channel -> AppT IO Q.ConsumerTag
startConsumer chan = do
  env <- ask
  markAsWorking BackendDeadUserNoticationWatcher

  cassandra <- asks (.cassandra)

  void . lift $ Q.declareQueue chan Q.newQueue {Q.queueName = userNotificationDlqName}
  QL.consumeMsgs chan userNotificationDlqName Q.Ack $ \(msg, envelope) ->
    if (msg.msgDeliveryMode == Just Q.NonPersistent)
      then do
        -- ignore transient messages, ack it so they don't clog the queue
        lift $ Q.ackEnv envelope
        pure ()
      else do
        -- forward non-transient messages to the respective client
        let dat = getLastDeathQueue msg.msgHeaders
        let vals = BS.split '.' dat
        case vals of
          ["user-notifications", uidBS, cidBS] -> do
            m <- runMaybeT $ do
              uid <- hoistMaybe $ fromByteString uidBS
              cid <- hoistMaybe $ fromByteString cidBS
              pure (uid, cid)
            (uid, cid) <- maybe (logParseError env dat) pure m
            markAsNeedsFullSync cassandra uid cid
            lift $ Q.ackEnv envelope
          _ -> void $ logParseError env dat
  where
    logParseError env dat = do
      Log.err env.logger $
        Log.msg (Log.val "Could not parse msgHeaders into uid/cid for dead letter exchange message")
          . Log.field "error_parsing_message" dat
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
  AppT IO (Async ())
startWorker AmqpEndpoint {..} = do
  env <- ask
  mVar <- newEmptyMVar

  -- Create settings
  (username, password) <- liftIO $ readCredsFromEnv
  mTlsSettings <- traverse (liftIO . (mkTLSSettings host)) tls
  let connOpts =
        Q.defaultConnectionOpts
          { Q.coServers = [(host, fromIntegral port)],
            Q.coVHost = vHost,
            Q.coAuth = [Q.plain username password],
            Q.coTLSSettings = Q.TLSCustom <$> mTlsSettings
          }

  let openConnection connM = do
        mConn <- lowerCodensity $ do
          conn <- case connM of
            Nothing -> do
              -- Open the rabbit mq connection
              conn <- Codensity $ bracket (liftIO $ Q.openConnection'' connOpts) (liftIO . Q.closeConnection)
              -- We need to recover from connection closed by restarting it
              liftIO $ Q.addConnectionClosedHandler conn True do
                Log.err env.logger $
                  Log.msg (Log.val "BackendDeadUserNoticationWatcher: Connection closed.")
                putMVar mVar Nothing
              pure conn
            Just conn -> pure conn

          -- After starting the connection, open the channel
          chan <- Codensity $ bracket (liftIO $ Q.openChannel conn) (liftIO . Q.closeChannel)

          -- If the channel stops, we need to re-open
          liftIO $ Q.addChannelExceptionHandler chan $ \e -> do
            Log.err env.logger $
              Log.msg (Log.val "BackendDeadUserNoticationWatcher: Caught exception in RabbitMQ channel.")
                . Log.field "exception" (displayException e)
            putMVar mVar (Just conn)

          -- Set up the consumer
          void $ Codensity $ bracket (startConsumer chan) (liftIO . Q.cancelConsumer chan)
          lift $ takeMVar mVar
        openConnection mConn

  async (openConnection Nothing)
