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
import Debug.Trace
import Imports hiding (putMVar)
import Network.AMQP qualified as Q
import Network.AMQP.Extended
import Network.AMQP.Lifted qualified as QL
import Network.AMQP.Types
import UnliftIO hiding (bracket, putMVar)
import UnliftIO.Exception (bracket)
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
  traceM $ "\n ---- Starting consumer"
  markAsWorking BackendDeadUserNoticationWatcher

  cassandra <- asks (.cassandra)

  -- TODO: replace bare string with the constant from the rabbitmq PR.
  let queueName = "dead-user-notifications"

  void . lift $ Q.declareQueue chan Q.newQueue {Q.queueName = queueName}
  QL.consumeMsgs chan queueName Q.Ack $ \(msg, envelope) -> do
    let dat = getLastDeathQueue msg.msgHeaders
    let vals = BS.split '.' dat
    case vals of
      ["user-notifications", uidBS, cidBS] -> do
        m <- runMaybeT $ do
          uid <- hoistMaybe $ fromByteString uidBS
          cid <- hoistMaybe $ fromByteString cidBS
          pure (uid, cid)
        (uid, cid) <- maybe handleParseErrors pure m
        markAsNeedsFullSync cassandra uid cid
        lift $ Q.ackEnv envelope

      -- TODO:
      _ -> error "invalid routing key, don't throw an error here, just log things"
  where
    -- TODO: use the logger instead of error
    handleParseErrors = error "Log: could not parse a user and client id from routing key."

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
                -- TODO: log
                traceM $ "\n would have caught this if we had logs"
                putMVar mVar Nothing
              pure conn
            Just conn -> pure conn

          -- After starting the connection, open the channel
          chan <- Codensity $ bracket (liftIO $ Q.openChannel conn) (liftIO . Q.closeChannel)

          -- If the channel stops, we need to re-open
          liftIO $ Q.addChannelExceptionHandler chan $ \_e -> do
            -- TODO: Do handling and logging
            putMVar mVar (Just conn)

          -- Set up the consumer
          void $ Codensity $ bracket (startConsumer chan) (liftIO . Q.cancelConsumer chan)
          lift $ takeMVar mVar
        openConnection mConn

  async (openConnection Nothing)

-- env <- ask
-- -- These are used in the POSIX signal handlers, so we need to make
-- -- cross thread references that we can use to cancel consumers and
-- -- wait for current processing steps to finish.
-- chanRef <- newIORef Nothing
-- let -- cleanup the refs when channels die
--     -- This is so we aren't trying to close consumers
--     -- that don't exist when the service is shutdown.
--     clearRefs = do
--       atomicWriteIORef chanRef Nothing
-- a <-
--   async $
--     liftIO $
--       openConnectionWithRetries env.logger rabbitmqOpts $
--         RabbitMqHooks
--           { -- The exception handling in `openConnectionWithRetries` won't open a new
--             -- connection on an explicit close call.
--             onNewChannel = \chan -> do
--               atomicWriteIORef chanRef $ pure chan
--               runAppT env $ startWatcher chan,
--             onChannelException = \_ -> do
--               clearRefs
--               runAppT env $ markAsNotWorking BackendDeadUserNoticationWatcher,
--             onConnectionClose = do
--               clearRefs
--               runAppT env $ markAsNotWorking BackendDeadUserNoticationWatcher
--           }
-- pure (chanRef, a)
